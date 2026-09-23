#!/usr/bin/env python3
"""Run bounded, serial agent reviews for existing books missing their PDF."""

from __future__ import annotations

import argparse
import fcntl
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import time

sys.dont_write_bytecode = True
SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_STATE = Path.home() / ".local/state/download-missing-pdfs"
DEFAULT_BIB = Path.home() / "My Drive/bibliography/old.bib"
LIBRARY = Path.home() / "My Drive/library-pdf"
CODEX = "/opt/homebrew/bin/codex"
EMACS_EVAL = Path.home() / "My Drive/dotfiles/bin/emacs-eval"


def adapter():
    spec = importlib.util.spec_from_file_location(
        "missing_pdf_inspection", SCRIPT_DIR / "download-missing-pdfs.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def read_state(path):
    if not path.exists():
        return {"version": 1, "books": {}}
    state = json.loads(path.read_text())
    if not isinstance(state, dict) or state.get("version") != 1 or not isinstance(state.get("books"), dict):
        raise ValueError(f"Invalid queue state: {path}")
    for key, record in state["books"].items():
        if (not isinstance(record, dict)
                or not isinstance(record.get("last_attempt"), (int, float))
                or not math.isfinite(record["last_attempt"])
                or record.get("status") not in {"running", "attached", "deferred", "error"}):
            raise ValueError(f"Invalid queue record: {key}")
    return state


def write_json(path, value):
    """Replace one private state file atomically on its own filesystem."""
    temporary = None
    try:
        with tempfile.NamedTemporaryFile("w", dir=path.parent, delete=False) as handle:
            temporary = Path(handle.name)
            json.dump(value, handle, indent=2)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        temporary.replace(path)
    finally:
        if temporary is not None and temporary.exists():
            temporary.unlink()


def select_books(books, state, limit, only=""):
    records = state["books"]
    selected = [book for book in books if not only or book["key"] == only]
    return sorted(selected, key=lambda book: (
        records.get(book["key"], {}).get("last_attempt", 0), book["key"]))[:limit]


def agent_environment(environ=None):
    env = dict(os.environ if environ is None else environ)
    for name in list(env):
        if name.endswith("_BUFFER_NAME") or name in {
            "CODEX_THREAD_ID", "CODEX_SESSION_ID", "CLAUDE_SESSION_ID",
            "CLAUDECODE", "CODEX_INTERNAL_ORIGINATOR_OVERRIDE",
        }:
            env.pop(name, None)
    # A persisted selection can name a pool, not a physical account directory.
    # Use the same configured routing as Emacs, which this job needs for Ebib.
    home = Path(env.get("HOME", str(Path.home())))
    marker = home / ".codex-current-account"
    if not env.get("CODEX_HOME") and marker.exists():
        account = "".join(marker.read_text().split())
        if not re.fullmatch(r"[A-Za-z0-9_-]+", account):
            raise ValueError("Invalid Codex account selector")
        expression = (
            f"(let* ((selection {json.dumps(account)}) "
            "(account (if (agent-account-pool-p 'codex selection) "
            "(agent-account-route 'codex selection) selection))) "
            "(agent-account-home 'codex account))")
        completed = subprocess.run(
            [sys.executable, str(EMACS_EVAL), expression],
            capture_output=True, text=True, timeout=20, env=env)
        if completed.returncode:
            raise ValueError("Could not resolve Codex account through Emacs")
        reply = json.loads(completed.stdout)
        if (not isinstance(reply, dict) or reply.get("ok") is not True
                or reply.get("truncated") is not False
                or not isinstance(reply.get("result"), str)):
            raise ValueError("Invalid Codex account response from Emacs")
        # emacs-eval returns a printed Lisp string; paths use JSON-compatible
        # quoting. Non-string replies (including nil) must fail closed.
        selected_path = json.loads(reply["result"])
        if not isinstance(selected_path, str) or not selected_path:
            raise ValueError("Codex account has no configured home")
        selected = Path(selected_path)
        if not selected.is_absolute() or not selected.is_dir():
            raise ValueError(f"Selected Codex account directory is missing: {selected}")
        env["CODEX_HOME"] = str(selected)
    return env


def run_agent(command, prompt, attempt, timeout, env):
    """Bound the owned child group; never signal the persistent Emacs server."""
    with (attempt / "events.jsonl").open("w") as stdout, (attempt / "stderr.log").open("w") as stderr:
        process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=stdout,
                                   stderr=stderr, cwd=attempt, env=env,
                                   start_new_session=True, text=True)
        try:
            process.communicate(prompt, timeout=timeout)
        except BaseException as exc:
            # Only this process group was created by this invocation. An Ebib
            # operation may still be pending: the unfinished ledger blocks retry.
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                pass
            # The leader exiting does not prove its tools exited. Kill any
            # remaining members of this owned group, including TERM-ignoring
            # descendants, before releasing the batch lock.
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
            if isinstance(exc, subprocess.TimeoutExpired):
                raise RuntimeError(f"Agent timed out after {timeout}s; reconcile pending Ebib operation") from exc
            raise
    if process.returncode:
        raise RuntimeError(f"Agent exited {process.returncode}; see {attempt / 'stderr.log'}")


def verify_operation(operation_id, key, bib):
    """Read the actual retained Ebib operation, independently of model output."""
    operation = json.dumps(operation_id)
    target = json.dumps(key)
    bibliography = json.dumps(str(bib.resolve()))
    expression = (
        f"(let ((s (ebib-extras-operation-status-for-id {operation}))) "
        f"(and (equal (plist-get s :key) {target}) "
        f"(equal (file-truename (plist-get s :bibfile)) {bibliography}) "
        "(eq (plist-get s :status) 'complete) "
        "(zerop (plist-get s :pending)) (null (plist-get s :errors))))")
    completed = subprocess.run([sys.executable, str(EMACS_EVAL), expression],
                               capture_output=True, text=True, timeout=20)
    if completed.returncode:
        raise ValueError("Could not verify the retained Ebib operation")
    actual = json.loads(completed.stdout)
    if (not isinstance(actual, dict) or actual.get("ok") is not True
            or actual.get("result") != "t" or actual.get("truncated") is not False):
        raise ValueError("Ebib operation is not complete for this exact entry and bibliography")


def artifact_path(value, attempt):
    if not isinstance(value, str) or not value:
        raise ValueError("Review artifact path is missing")
    path = Path(value)
    if (not path.is_absolute() or not path.resolve().is_relative_to(attempt.resolve())
            or not path.is_file()):
        raise ValueError("Review artifact must be a real file inside this attempt")
    return path


def validate_review(result, attempt, inspection):
    """Recheck retained byte-bound review structure, not semantic judgments."""
    evidence = json.loads(artifact_path(result["evidence"], attempt).read_text())
    if not isinstance(evidence, dict):
        raise ValueError("Review evidence must be a JSON object")
    for name, expected in (("installed_file", result["file"]),
                           ("installed_sha256", result["sha256"]),
                           ("operation_id", result["operation_id"])):
        if evidence.get(name) != expected:
            raise ValueError(f"Review evidence disagrees with result: {name}")
    inventory_path = artifact_path(evidence.get("inventory"), attempt)
    reviews_path = artifact_path(evidence.get("reviews"), attempt)
    selection_path = artifact_path(evidence.get("selection"), attempt)
    reviews = json.loads(reviews_path.read_text())
    selection = json.loads(selection_path.read_text())
    if (not isinstance(selection, dict) or selection.get("status") != "ok"
            or not isinstance(selection.get("selected"), dict)):
        raise ValueError("Saved book selection is not successful")
    selected = selection["selected"]
    artifact_path(selected.get("file"), attempt)
    library = inspection.paper_fetch
    try:
        inventory = library.read_book_candidates(inventory_path)
        # Shared selection validates matching targets, all required verified
        # checks, and each candidate's retained PDF against its review hashes.
        actual = library.select_book_candidate(inventory, reviews)
    except library.PaperFetchError as exc:
        raise ValueError(f"Invalid retained book review: {exc}") from exc
    if actual.get("status") != "ok" or not actual.get("selected"):
        raise ValueError("Retained reviews no longer select an eligible PDF")
    for name in ("md5", "sha256", "size_bytes", "file"):
        if name not in selected or selected[name] != actual["selected"].get(name):
            raise ValueError(f"Saved selection disagrees with retained reviewed PDF: {name}")


def validate_result(result, book, bib, attempt, inspection):
    fields = {"status", "key", "reason", "file", "sha256", "operation_id", "evidence"}
    if (not isinstance(result, dict) or set(result) != fields
            or any(not isinstance(result[field], str) for field in fields)):
        raise ValueError("Agent result has invalid fields")
    if result["key"] != book["key"] or result["status"] not in {"attached", "deferred", "error"}:
        raise ValueError("Agent result has wrong key or status")
    if not result["reason"].strip():
        raise ValueError("Agent result requires an explicit reason")
    evidence = Path(result["evidence"])
    if not evidence.is_absolute() or not evidence.resolve().is_relative_to(attempt.resolve()) or not evidence.is_file():
        raise ValueError("Agent evidence must be a real file inside this attempt")
    if result["status"] != "attached":
        return
    pdf = Path(result["file"])
    if not pdf.is_absolute() or pdf.suffix.lower() != ".pdf" or not pdf.is_file() or pdf.stat().st_size == 0:
        raise ValueError("Attachment is not a nonempty absolute PDF path")
    if not result["operation_id"].strip():
        raise ValueError("Attached result lacks an Ebib operation ID")
    validate_review(result, attempt, inspection)
    verify_operation(result["operation_id"], book["key"], bib)
    current = [entry for entry in inspection.parse_bib_books(bib) if entry["key"] == book["key"]]
    if len(current) != 1:
        raise ValueError("Cannot read back exactly one bibliography entry")
    attached = inspection.extract_pdf_path(current[0]["file"])
    if attached is None or attached.resolve() != pdf.resolve():
        raise ValueError("Saved bibliography attachment does not match agent result")
    with pdf.open("rb") as handle:
        if handle.read(5) != b"%PDF-":
            raise ValueError("Attachment lacks a PDF header")
        handle.seek(0)
        digest = hashlib.file_digest(handle, "sha256").hexdigest()
    if digest != result["sha256"]:
        raise ValueError("Saved attachment SHA-256 does not match reviewed bytes")


def execute(args, inspection, state):
    unfinished = [key for key, record in state["books"].items()
                  if record.get("status") == "running"
                  or (record.get("status") in {"error", "deferred"} and record.get("operation_id"))]
    if unfinished:
        raise RuntimeError("Incomplete prior attempt; reconcile evidence and pending Ebib operations before retry: " + ", ".join(unfinished))
    books = inspection.books_missing_pdf(inspection.parse_bib_books(args.bib), include_broken=True)
    queue = select_books(books, state, args.limit, args.only)
    if args.dry_run:
        print(json.dumps({"books": queue}, indent=2))
        return 0
    env = agent_environment()
    template = (SCRIPT_DIR / "download-missing-pdfs-task.md").read_text()
    for book in queue:
        attempt = Path(tempfile.mkdtemp(prefix="attempt-", dir=args.state_dir))
        write_json(attempt / "input.json", {"book": book, "bibliography": str(args.bib), "attempt": str(attempt)})
        record = {"last_attempt": time.time(), "status": "running", "attempt": str(attempt)}
        state["books"][book["key"]] = record
        write_json(args.state_dir / "state.json", state)
        prompt = template + "\n\nRead the exact job input at: " + str(attempt / "input.json") + "\n"
        (attempt / "prompt.md").write_text(prompt)
        command = [CODEX, "exec", "--sandbox", "workspace-write", "-c", 'approval_policy="never"',
                   "-c", "sandbox_workspace_write.network_access=true", "--skip-git-repo-check",
                   "--cd", str(attempt), "--add-dir", str(args.state_dir),
                   "--add-dir", str(args.bib.parent), "--add-dir", str(LIBRARY),
                   "--json", "--output-schema", str(SCRIPT_DIR / "download-missing-pdfs-result.schema.json"),
                   "--output-last-message", str(attempt / "result.json"), "-"]
        run_agent(command, prompt, attempt, args.timeout, env)
        result = json.loads((attempt / "result.json").read_text())
        validate_result(result, book, args.bib, attempt, inspection)
        if result["status"] != "attached" and result["operation_id"]:
            record.update(operation_id=result["operation_id"], reason=result["reason"])
            write_json(args.state_dir / "state.json", state)
            raise RuntimeError(f"Unfinished attachment operation for {book['key']}; reconcile {attempt}")
        record.update(status=result["status"], reason=result["reason"], operation_id=result["operation_id"])
        write_json(args.state_dir / "state.json", state)
        print(json.dumps(result), flush=True)
        if result["status"] == "error":
            return 1
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bib", type=Path, default=DEFAULT_BIB)
    parser.add_argument("--state-dir", type=Path, default=DEFAULT_STATE)
    parser.add_argument("--limit", type=int, default=5)
    parser.add_argument("--only", default="")
    parser.add_argument("--timeout", type=float, default=1200)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
    if args.limit < 1 or args.timeout <= 0:
        parser.error("--limit and --timeout must be positive")
    args.bib = args.bib.expanduser().resolve()
    args.state_dir = args.state_dir.expanduser().resolve()
    if args.state_dir.is_relative_to((Path.home() / "My Drive").resolve()):
        parser.error("--state-dir must be outside Google Drive")
    try:
        inspection = adapter()
        if args.dry_run:
            return execute(args, inspection, read_state(args.state_dir / "state.json"))
        os.umask(0o077)
        args.state_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
        with (args.state_dir / "lock").open("a") as lock:
            try:
                fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError:
                print("Another PDF acquisition batch is already running")
                return 0
            return execute(args, inspection, read_state(args.state_dir / "state.json"))
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
        print(f"PDF batch failed: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
