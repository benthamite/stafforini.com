#!/usr/bin/env python3
"""Run Netlify deploys with conservative recovery for post-upload rate limits."""

from __future__ import annotations

import argparse
import json
import os
import re
import shlex
import subprocess
import sys
import time
from pathlib import Path
from typing import Callable, Sequence


DEPLOY_ID_PATTERNS = (
    re.compile(r"deploy with id ([0-9a-f]{8,})", re.IGNORECASE),
    re.compile(r"/deploys/([0-9a-f]{8,})", re.IGNORECASE),
    re.compile(r'"id"\s*:\s*"([0-9a-f]{8,})"', re.IGNORECASE),
)

DEFAULT_STATUS_DELAYS = (60, 120, 180, 300, 300)
POST_UPLOAD_MARKERS = (
    "Finished uploading",
    "Waiting for deploy to go live",
    "Deploy path:",
)
RATE_LIMIT_MARKERS = (
    "429",
    "Too Many Requests",
    "rate limit",
    "rate-limit",
)


Runner = Callable[..., subprocess.CompletedProcess[str]]
Sleeper = Callable[[int], None]


def extract_deploy_id(output: str) -> str | None:
    for pattern in DEPLOY_ID_PATTERNS:
        match = pattern.search(output)
        if match:
            return match.group(1)
    return None


def looks_like_post_upload_rate_limit(output: str) -> bool:
    return any(marker in output for marker in POST_UPLOAD_MARKERS) and any(
        marker.lower() in output.lower() for marker in RATE_LIMIT_MARKERS
    )


def read_site_id(repo_root: Path) -> str:
    state_path = repo_root / ".netlify" / "state.json"
    try:
        state = json.loads(state_path.read_text())
    except FileNotFoundError as exc:
        raise RuntimeError(f"Cannot confirm Netlify deploy status: {state_path} not found") from exc
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"Cannot confirm Netlify deploy status: {state_path} is not valid JSON") from exc

    site_id = state.get("siteId")
    if not site_id:
        raise RuntimeError(f"Cannot confirm Netlify deploy status: {state_path} has no siteId")
    return site_id


def parse_delays(raw: str) -> tuple[int, ...]:
    delays = []
    for item in raw.split(","):
        item = item.strip()
        if item:
            delays.append(int(item))
    return tuple(delays)


def run_streaming(args: Sequence[str], **kwargs) -> subprocess.CompletedProcess[str]:
    kwargs.pop("capture_output", None)
    kwargs.pop("text", None)
    proc = subprocess.Popen(
        args,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        **kwargs,
    )
    output_parts = []
    assert proc.stdout is not None
    for line in proc.stdout:
        output_parts.append(line)
        print(line, end="")
    returncode = proc.wait()
    return subprocess.CompletedProcess(args, returncode, "".join(output_parts), "")


def get_deploy_status(
    netlify_command: Sequence[str],
    site_id: str,
    deploy_id: str,
    runner: Runner,
) -> dict:
    payload = json.dumps({"siteId": site_id, "deployId": deploy_id})
    result = runner(
        [*netlify_command, "api", "getSiteDeploy", "--data", payload],
        text=True,
        capture_output=True,
    )
    output = (result.stdout or "") + (result.stderr or "")
    if result.returncode != 0:
        raise RuntimeError(output.strip() or f"status command failed with exit code {result.returncode}")
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"status command returned non-JSON output: {output.strip()}") from exc


def confirm_deploy_status(
    netlify_command: Sequence[str],
    repo_root: Path,
    deploy_id: str,
    delays: Sequence[int],
    runner: Runner,
    sleeper: Sleeper,
) -> int:
    site_id = read_site_id(repo_root)
    print(
        f"Netlify deploy hit a post-upload API rate limit. "
        f"Checking deploy {deploy_id} with slower polling..."
    )

    last_state = "unknown"
    for delay in delays:
        if delay > 0:
            print(f"Waiting {delay}s before checking Netlify deploy status...")
        sleeper(delay)
        try:
            status = get_deploy_status(netlify_command, site_id, deploy_id, runner)
        except RuntimeError as exc:
            print(f"Netlify status check failed: {exc}", file=sys.stderr)
            continue

        last_state = str(status.get("state") or "unknown")
        if last_state == "ready":
            url = status.get("ssl_url") or status.get("url") or status.get("deploy_ssl_url") or ""
            print(f"Netlify deploy {deploy_id} is live{f': {url}' if url else ''}.")
            return 0
        if last_state == "error":
            message = status.get("error_message") or "deploy entered error state"
            print(f"Netlify deploy {deploy_id} failed remotely: {message}", file=sys.stderr)
            return 1
        print(f"Netlify deploy {deploy_id} is still {last_state}.")

    print(
        f"Netlify deploy {deploy_id} status is still {last_state}; remote status remains unknown.",
        file=sys.stderr,
    )
    return 1


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo-root", default=".", help="Repository root containing .netlify/state.json")
    parser.add_argument(
        "--netlify-command",
        default=os.environ.get("NETLIFY_COMMAND", "npx --no-install netlify"),
        help="Command used to invoke the pinned Netlify CLI",
    )
    parser.add_argument(
        "--status-delays",
        default=os.environ.get(
            "NETLIFY_DEPLOY_STATUS_DELAYS",
            ",".join(str(delay) for delay in DEFAULT_STATUS_DELAYS),
        ),
        help="Comma-separated seconds to wait before each post-429 status check",
    )
    parser.add_argument("netlify_args", nargs=argparse.REMAINDER)
    return parser


def main(
    argv: Sequence[str] | None = None,
    *,
    runner: Runner = run_streaming,
    sleeper: Sleeper = time.sleep,
) -> int:
    args = build_parser().parse_args(argv)
    netlify_args = list(args.netlify_args)
    if netlify_args and netlify_args[0] == "--":
        netlify_args = netlify_args[1:]
    if not netlify_args:
        raise SystemExit("missing Netlify CLI arguments")

    netlify_command = shlex.split(args.netlify_command)
    result = runner([*netlify_command, *netlify_args], text=True, capture_output=True)
    output = (result.stdout or "") + (result.stderr or "")
    if result.returncode == 0:
        return 0

    deploy_id = extract_deploy_id(output)
    if not deploy_id or not looks_like_post_upload_rate_limit(output):
        return result.returncode

    try:
        delays = parse_delays(args.status_delays)
    except ValueError as exc:
        raise SystemExit(f"invalid --status-delays value: {args.status_delays}") from exc

    return confirm_deploy_status(
        netlify_command,
        Path(args.repo_root),
        deploy_id,
        delays,
        runner,
        sleeper,
    )


if __name__ == "__main__":
    raise SystemExit(main())
