#!/usr/bin/env python3
"""Report `_redirects` rules that land on a page which does not exist.

A rule whose target was later renamed or deleted keeps returning its 301, so
nothing about the redirects file looks wrong. The fault surfaces weeks later
as a Search Console "Not found (404)" entry for the *target*, which is easy to
misread as an unrelated missing page.

Placeholder targets (`:splat`, `:slug`) and absolute URLs on other hosts are
out of scope: neither can be resolved against a rendered tree.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from urllib.parse import unquote

from lib import REPO_ROOT

EXTERNAL_SCHEMES = ("http://", "https://")


def iter_rules(redirects_file: Path):
    """Yield (line number, source, target) for each local, literal rule."""
    text = redirects_file.read_text(errors="replace")
    for lineno, raw in enumerate(text.splitlines(), 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        fields = line.split()
        if len(fields) < 2:
            continue
        # Query-param rules put conditions between source and target, as in
        # "/quotes/  p=3919  /quotes/slug/  301!", so take the first field
        # after the source that actually looks like a destination.
        target = next(
            (f for f in fields[1:] if f.startswith("/") or f.startswith(EXTERNAL_SCHEMES)),
            None,
        )
        if target is None or not target.startswith("/"):
            continue
        if ":" in target or "*" in target:
            continue
        yield lineno, fields[0], target


def target_exists(site_dir: Path, target: str) -> bool:
    path = unquote(target.split("#", 1)[0].split("?", 1)[0])
    rel = path.lstrip("/")
    if not rel or path.endswith("/"):
        return (site_dir / rel / "index.html").exists()
    return (site_dir / rel).exists() or (site_dir / rel / "index.html").exists()


def audit(site_dir: Path, redirects_file: Path | None = None):
    redirects_file = redirects_file or REPO_ROOT / "static" / "_redirects"
    if not redirects_file.exists():
        return []
    return [
        (lineno, source, target)
        for lineno, source, target in iter_rules(redirects_file)
        if not target_exists(site_dir, target)
    ]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dir", type=Path, required=True, help="Rendered site to resolve against")
    parser.add_argument(
        "--redirects",
        type=Path,
        default=None,
        help="Redirects file to audit (default: static/_redirects)",
    )
    args = parser.parse_args()

    dead = audit(args.dir, args.redirects)
    if not dead:
        print("Every _redirects target resolves.")
        return

    print(f"{len(dead)} _redirects rule(s) point at a page that does not exist:", file=sys.stderr)
    for lineno, source, target in dead:
        print(f"  - line {lineno}: {source} -> {target}", file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    main()
