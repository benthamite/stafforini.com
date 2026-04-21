#!/usr/bin/env python3
"""Smoke-test rendered Hugo output.

This catches the class of failures where the pipeline exits successfully but
the rendered site is visibly wrong, such as the home page losing its recent
quotes section.
"""

from __future__ import annotations

import argparse
import html
import re
import subprocess
import sys
import tempfile
import tomllib
from datetime import date, datetime
from pathlib import Path

from lib import REPO_ROOT, cite_key_to_slug, load_excluded_works


def read_toml_front_matter(path: Path) -> dict:
    text = path.read_text()
    match = re.match(r"\+\+\+\n(.*?)\n\+\+\+", text, re.DOTALL)
    if not match:
        return {}
    return tomllib.loads(match.group(1))


def quote_sort_key(item: tuple[Path, dict]) -> datetime:
    raw = item[1].get("date")
    if isinstance(raw, datetime):
        return raw
    if isinstance(raw, date):
        return datetime.combine(raw, datetime.min.time())
    if isinstance(raw, str):
        try:
            return datetime.fromisoformat(raw)
        except ValueError:
            return datetime.min
    return datetime.min


def expected_recent_quote_labels(limit: int = 5) -> list[str]:
    quotes_dir = REPO_ROOT / "content" / "quotes"
    works_path = REPO_ROOT / "data" / "works.json"

    if not quotes_dir.exists():
        return []

    import json

    works = json.loads(works_path.read_text()) if works_path.exists() else {}
    diary_quotes: list[tuple[Path, dict]] = []
    for path in quotes_dir.glob("*.md"):
        fm = read_toml_front_matter(path)
        if fm.get("diary") is True:
            diary_quotes.append((path, fm))

    labels: list[str] = []
    for _path, fm in sorted(diary_quotes, key=quote_sort_key, reverse=True)[:limit]:
        work_slug = fm.get("work")
        work = works.get(work_slug, {}) if isinstance(work_slug, str) else {}
        labels.append(work.get("author") or fm.get("title") or "")
    return [label for label in labels if label]


def verify_excluded_works() -> list[str]:
    """Enforce the takedown blocklist across sources and rendered static assets.

    This runs before the rendered-site check so that a leaked work is caught
    even if no Hugo build happened (e.g. during an incremental export).  Each
    excluded cite key is resolved to its kebab-case slug and we fail if the
    corresponding source or asset still exists:

      - content/works/{slug}.md               (work page)
      - content/quotes/*.md with work={slug}  (any quote citing the work)
      - static/pdfs/{slug}.pdf                (local source for R2 upload)
      - static/pdf-thumbnails/{slug}.png      (local source for R2 upload)

    The generators themselves remove these when the blocklist is consulted,
    but this gate protects against bugs or manual edits that bypass them.
    """
    errors: list[str] = []
    excluded = load_excluded_works()
    if not excluded:
        return errors

    excluded_slugs = {cite_key_to_slug(k) for k in excluded.keys()}

    for slug in sorted(excluded_slugs):
        work_md = REPO_ROOT / "content" / "works" / f"{slug}.md"
        if work_md.exists():
            errors.append(f"takedown-blocked work still in source: {work_md}")
        pdf = REPO_ROOT / "static" / "pdfs" / f"{slug}.pdf"
        if pdf.exists():
            errors.append(f"takedown-blocked PDF still in source: {pdf}")
        thumb = REPO_ROOT / "static" / "pdf-thumbnails" / f"{slug}.png"
        if thumb.exists():
            errors.append(f"takedown-blocked thumbnail still in source: {thumb}")

    quotes_dir = REPO_ROOT / "content" / "quotes"
    if quotes_dir.exists():
        work_re = re.compile(r'^work\s*[=:]\s*"([^"]+)"', re.MULTILINE)
        for md_file in quotes_dir.glob("*.md"):
            if md_file.name == "_index.md":
                continue
            head = md_file.read_text(errors="replace")[:2000]
            match = work_re.search(head)
            if match and match.group(1) in excluded_slugs:
                errors.append(
                    f"takedown-blocked quote still in source: {md_file} "
                    f"(work={match.group(1)})"
                )

    return errors


def verify_excluded_works_rendered(site_dir: Path) -> list[str]:
    """Belt-and-suspenders check on the rendered public/ tree."""
    errors: list[str] = []
    excluded = load_excluded_works()
    if not excluded:
        return errors
    excluded_slugs = {cite_key_to_slug(k) for k in excluded.keys()}
    for slug in sorted(excluded_slugs):
        work_page = site_dir / "works" / slug / "index.html"
        if work_page.exists():
            errors.append(f"takedown-blocked work rendered: {work_page}")
    return errors


def verify_built_site(site_dir: Path) -> list[str]:
    errors: list[str] = list(verify_excluded_works_rendered(site_dir))
    index_path = site_dir / "index.html"
    if not index_path.exists():
        errors.append(f"missing rendered homepage: {index_path}")
        return errors

    index = html.unescape(index_path.read_text(errors="replace"))

    required_text = [
        "Recently updated notes",
        "Recently published quotes",
    ]
    for text in required_text:
        if text not in index:
            errors.append(f"homepage missing {text!r}")

    if 'href="/notes/' not in index and "href=/notes/" not in index:
        errors.append("homepage has no note links")
    if 'href="/quotes/' not in index and "href=/quotes/" not in index:
        errors.append("homepage has no quote links")

    quote_pages = list((site_dir / "quotes").glob("*/index.html"))
    if not quote_pages:
        errors.append("rendered site has no quote pages")

    for label in expected_recent_quote_labels():
        if label not in index:
            errors.append(f"homepage missing recent quote label {label!r}")

    return errors


def build_dev_site(destination: Path) -> None:
    subprocess.run(
        [
            "hugo",
            "--config",
            "hugo.toml,hugo.dev.toml",
            "--destination",
            str(destination),
            "--noBuildLock",
            "--quiet",
        ],
        cwd=REPO_ROOT,
        check=True,
        timeout=120,
    )


def main() -> None:
    parser = argparse.ArgumentParser(description="Verify rendered Hugo output")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--dir", type=Path, help="Verify an existing rendered site")
    group.add_argument("--build", choices=["dev"], help="Build and verify a site profile")
    args = parser.parse_args()

    source_errors = verify_excluded_works()

    if args.dir:
        site_dir = args.dir
        errors = source_errors + verify_built_site(site_dir)
    else:
        with tempfile.TemporaryDirectory(prefix="stafforini-site-") as tmp:
            site_dir = Path(tmp)
            build_dev_site(site_dir)
            errors = source_errors + verify_built_site(site_dir)

    if errors:
        print("Rendered site verification failed:", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        sys.exit(1)

    print("Rendered site verification OK.")


if __name__ == "__main__":
    main()
