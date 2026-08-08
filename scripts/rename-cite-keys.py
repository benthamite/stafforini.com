#!/usr/bin/env python3
"""Rename cite keys across bibs, bibliographic-notes org files, content/
quote markdown, and the redirects file.

Pass a mapping of old → new cite keys via the RENAMES dict at the bottom
or via stdin (one ``old<TAB>new`` per line). The script:

  1. Renames every ``@<type>{old,`` to ``@<type>{new,`` across the bib
     files and updates ``crossref = {old}`` → ``crossref = {new}``.
  2. Renames the matching ``~/My Drive/bibliographic-notes/<old>.org``
     file to ``<new>.org`` and replaces every ``[cite:@old]`` and
     ``Custom_ID: old`` inside it.
  3. Updates ``:work "<old-slug>"`` properties in the org file.
  4. Updates ``work = "<old-slug>"`` in matching ``content/quotes/*.md``
     files.
  5. Updates any ``/works/<old-slug>/`` redirect targets in
     ``static/_redirects`` and adds new ``<old-slug>`` → ``<new-slug>``
     redirects.

Slugs are derived from cite keys via the same rule as
``scripts/lib.cite_key_to_slug``.
"""

import re
import shutil
import sys
import urllib.parse
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))
from lib import cite_key_to_slug  # noqa: E402

BIB_FILES = [
    Path.home() / "My Drive/bibliography/new.bib",
    Path.home() / "My Drive/bibliography/old.bib",
    Path.home() / "My Drive/bibliography/migration.bib",
    Path.home() / "repos/babel-refs/bib/fluid.bib",
    Path.home() / "repos/babel-refs/bib/stable.bib",
    Path.home() / "repos/babel-refs/bib/db.bib",
]
BIBNOTES = Path.home() / "My Drive/bibliographic-notes"
NOTES = Path.home() / "My Drive/notes"
REDIRECTS = REPO / "static/_redirects"
QUOTES = REPO / "content/quotes"


def rename_in_bib(text: str, old: str, new: str) -> tuple[str, int]:
    """Rename @<type>{old,…} and crossref = {old} occurrences."""
    count = 0
    # Cite key in entry header (preserve the @type and braces)
    pat = re.compile(r"(^@\w+\s*\{\s*)" + re.escape(old) + r"(\s*,)", re.MULTILINE)
    text, n = pat.subn(rf"\1{new}\2", text)
    count += n
    # crossref = {old}
    pat = re.compile(r"(crossref\s*=\s*\{)" + re.escape(old) + r"(\})")
    text, n = pat.subn(rf"\1{new}\2", text)
    count += n
    return text, count


def rename_in_org(text: str, old: str, new: str,
                  old_slug: str, new_slug: str) -> tuple[str, int]:
    """Rewrite Custom_ID, ROAM_REFS, [cite:@old], and :work "old-slug"."""
    count = 0
    for pat_text, repl_text in [
        (rf"\bCustom_ID:\s+{re.escape(old)}\b", f"Custom_ID: {new}"),
        (rf"\[cite:@{re.escape(old)}\b", f"[cite:@{new}"),
        (rf'"{re.escape(old_slug)}"', f'"{new_slug}"'),
    ]:
        new_text, n = re.subn(pat_text, repl_text, text)
        text = new_text
        count += n
    return text, count


def rename_quote_md(text: str, old_slug: str, new_slug: str) -> tuple[str, int]:
    pat = re.compile(rf'^work\s*=\s*"{re.escape(old_slug)}"', re.MULTILINE)
    return pat.subn(f'work = "{new_slug}"', text)


def encode_work_path(slug: str) -> str:
    """Return the ``/works/<slug>/`` path as Netlify will see it.

    Netlify matches ``_redirects`` rules against the percent-encoded
    request path, so a rule written with raw UTF-8 in the source column
    never fires. Googlebot requests ``/works/j%C3%B8rgensen-.../``, which
    does not match a literal ``/works/jørgensen-.../`` rule. Encoding here
    keeps generated rules matchable; ASCII slugs pass through unchanged.

    ``safe`` keeps the RFC 3986 sub-delimiters raw, because that is how a
    browser sends them. Encoding ``;`` to ``%3B`` here would break rules
    for cite keys with multiple semicolon-separated authors.
    """
    return "/works/" + urllib.parse.quote(slug, safe="!$&'()*+,;=:@") + "/"


def _split_rule(line: str) -> tuple[str, str] | None:
    """Return (source, target) for a redirect rule line, else None."""
    if not line.strip() or line.lstrip().startswith("#"):
        return None
    parts = line.split()
    if len(parts) < 2:
        return None
    return parts[0], parts[1]


def rename_redirects(text: str, old_slug: str, new_slug: str) -> tuple[str, int, bool]:
    """Update existing redirect targets and add an old→new redirect."""
    new_path = encode_work_path(new_slug)
    # Rules may spell the old slug either raw or percent-encoded, depending
    # on when they were written.
    old_paths = {f"/works/{old_slug}/", encode_work_path(old_slug)}

    count = 0
    already_present = False
    lines = text.split("\n")
    for i, line in enumerate(lines):
        rule = _split_rule(line)
        if rule is None:
            continue
        source, target = rule
        if source in old_paths and target == new_path:
            already_present = True
            continue
        if target not in old_paths:
            continue
        # Rewrite the target column only. Substituting across the whole
        # line would also rewrite a source that happens to be the old
        # slug, turning that rule into a self-redirect.
        start = line.index(target, line.index(source) + len(source))
        lines[i] = line[:start] + new_path + line[start + len(target):]
        count += 1

    new_text = "\n".join(lines)
    added = False
    if not already_present:
        rename_line = f"{encode_work_path(old_slug)}  {new_path}  301"
        new_text = new_text.rstrip() + "\n" + rename_line + "\n"
        added = True
    return new_text, count, added


def apply_rename(old: str, new: str) -> dict:
    """Apply one rename and return a stats dict."""
    old_slug = cite_key_to_slug(old)
    new_slug = cite_key_to_slug(new)
    stats = {
        "old": old, "new": new,
        "old_slug": old_slug, "new_slug": new_slug,
        "bib_changes": {}, "org_renamed": False, "org_changes": 0,
        "md_files_changed": [], "redirect_changes": 0,
        "redirect_added": False,
    }

    # Bib files
    for path in BIB_FILES:
        if not path.exists():
            continue
        text = path.read_text(encoding="utf-8")
        new_text, n = rename_in_bib(text, old, new)
        if n > 0:
            path.write_text(new_text, encoding="utf-8")
            stats["bib_changes"][path.name] = n

    # Bibliographic notes org file
    old_org = BIBNOTES / f"{old}.org"
    new_org = BIBNOTES / f"{new}.org"
    if old_org.exists():
        text = old_org.read_text(encoding="utf-8")
        new_text, n = rename_in_org(text, old, new, old_slug, new_slug)
        if old_org != new_org:
            old_org.rename(new_org)
            stats["org_renamed"] = True
        new_org.write_text(new_text, encoding="utf-8")
        stats["org_changes"] = n

    # ~/My Drive/notes/**/*.org — replace [cite:@old] anywhere in the
    # user's note tree. Skip the claude-logs/ directory (transcripts).
    if NOTES.exists():
        cite_pat = re.compile(rf"\[cite([^]]*?):@{re.escape(old)}\b")
        for org in NOTES.rglob("*.org"):
            if "claude-logs" in str(org) or "/.git/" in str(org):
                continue
            try:
                text = org.read_text(encoding="utf-8")
            except Exception:
                continue
            new_text, n = cite_pat.subn(rf"[cite\1:@{new}", text)
            if n > 0:
                org.write_text(new_text, encoding="utf-8")
                stats.setdefault("notes_changed", []).append(org.name)

    # content/quotes/*.md — find ones using the old slug
    if QUOTES.exists():
        for md in QUOTES.glob("*.md"):
            text = md.read_text(encoding="utf-8")
            new_text, n = rename_quote_md(text, old_slug, new_slug)
            if n > 0:
                md.write_text(new_text, encoding="utf-8")
                stats["md_files_changed"].append(md.name)

    # static/_redirects
    if REDIRECTS.exists():
        text = REDIRECTS.read_text(encoding="utf-8")
        new_text, n, added = rename_redirects(text, old_slug, new_slug)
        if n > 0 or added:
            REDIRECTS.write_text(new_text, encoding="utf-8")
            stats["redirect_changes"] = n
            stats["redirect_added"] = added

    return stats


def load_renames_from_stdin() -> dict[str, str]:
    out = {}
    for line in sys.stdin:
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split("\t")
        if len(parts) != 2:
            print(f"  skipping malformed line: {line!r}", file=sys.stderr)
            continue
        out[parts[0]] = parts[1]
    return out


def main() -> int:
    if sys.stdin.isatty():
        print("Reading renames from RENAMES dict in source.")
        renames = RENAMES
    else:
        renames = load_renames_from_stdin()

    print(f"Applying {len(renames)} cite-key renames...")
    print()
    for old, new in renames.items():
        stats = apply_rename(old, new)
        bib_summary = ", ".join(f"{k}={v}" for k, v in stats["bib_changes"].items()) or "none"
        org_note = "renamed" if stats["org_renamed"] else ("modified" if stats["org_changes"] else "—")
        md_note = ", ".join(stats["md_files_changed"]) or "—"
        red_note = (f"{stats['redirect_changes']} target(s) updated"
                    if stats["redirect_changes"]
                    else "no targets")
        if stats["redirect_added"]:
            red_note += " + slug-rename redirect added"
        print(f"  {old}")
        print(f"    → {new}")
        print(f"    bib: {bib_summary}; org: {org_note}; md: {md_note}; redirects: {red_note}")

    return 0


# Default rename set — edit before running, or pipe via stdin.
RENAMES: dict[str, str] = {}


if __name__ == "__main__":
    raise SystemExit(main())
