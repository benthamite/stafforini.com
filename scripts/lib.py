"""Shared library for stafforini.com build and migration scripts.

Centralizes duplicated utility functions and constants so that each
script imports from a single source of truth rather than maintaining
its own copy.
"""

import os
import re
import subprocess
import unicodedata
from pathlib import Path


# === Constants ===


# Canonical list of all bib files used across scripts.
# Individual scripts select subsets as needed.
BIB_FILES = [
    # Personal bibliography: actively maintained entries
    Path.home() / "My Drive/bibliography/new.bib",
    # Personal bibliography: older/archival entries
    Path.home() / "My Drive/bibliography/old.bib",
    # Shared bibliography (babel-refs): entries still being edited
    Path.home() / "My Drive/repos/babel-refs/bib/fluid.bib",
    # Shared bibliography (babel-refs): stable/finalized entries
    Path.home() / "My Drive/repos/babel-refs/bib/stable.bib",
    # Shared bibliography (babel-refs): database-sourced entries
    Path.home() / "My Drive/repos/babel-refs/bib/db.bib",
    # Auto-generated entries from WordPress quote migration
    Path.home() / "My Drive/bibliography/migration.bib",
]

# BIB_FILES minus auto-generated migration entries. Most scripts want
# this subset; only the migration pipeline itself needs migration.bib.
CORE_BIB_FILES = [f for f in BIB_FILES if "migration" not in f.name]

# Base set of English stop words shared across scripts.
# This is the intersection of common English stop words used by
# create-bib-entries.py, match-quotes-to-bib.py, and write-quotes-to-org.py.
# Scripts that need additional stop words (e.g. multilingual, pronouns)
# can extend with: MY_STOP_WORDS = STOP_WORDS | {"neue", "i", "me", ...}
STOP_WORDS = {
    "a", "an", "the", "of", "in", "on", "and", "or", "for", "to", "with",
    "from", "by", "at", "its", "is", "are", "was", "were", "be", "been",
    "being", "have", "has", "had", "do", "does", "did", "not", "but",
    "that", "this", "these", "those", "as", "if", "than", "so",
}


# macOS SF_DATALESS flag (from sys/stat.h, documented in stat(2) man page).
# Set by FileProvider on cloud-evicted (cloud-only) Google Drive files.
# Reading a dataless file blocks indefinitely waiting for hydration.
SF_DATALESS = 0x40000000


def is_dataless(path: Path) -> bool:
    """Check if a file has the macOS SF_DATALESS flag (cloud-evicted)."""
    try:
        return bool(os.stat(path).st_flags & SF_DATALESS)
    except (OSError, AttributeError):
        return False


def safe_remove(path: Path) -> None:
    """Move a file to the macOS Trash via the `trash` CLI instead of deleting it.

    Falls back to Path.unlink() if the `trash` command is not available.
    """
    try:
        subprocess.run(["trash", str(path)], check=True, capture_output=True)
    except FileNotFoundError:
        # `trash` command not installed; fall back to permanent deletion
        import logging
        logging.warning("trash CLI not found; permanently deleting %s", path)
        path.unlink(missing_ok=True)


# Epsilon for floating-point mtime comparisons. Filesystem timestamps lose
# precision through JSON round-trips (float64 → decimal string → float64),
# so we treat mtimes within 1 ms as equal.
MTIME_EPSILON = 0.001


def atomic_write_json(path, data, **kwargs):
    """Write *data* as JSON to *path* atomically (temp file + rename).

    Extra keyword arguments are forwarded to ``json.dump`` (e.g. ``indent``,
    ``ensure_ascii``).
    """
    import json
    import tempfile
    kwargs.setdefault("indent", 2)
    tmp_fd, tmp_path = tempfile.mkstemp(dir=str(Path(path).parent), suffix=".tmp")
    try:
        with os.fdopen(tmp_fd, "w") as f:
            json.dump(data, f, **kwargs)
            f.write("\n")
        os.replace(tmp_path, str(path))
    except BaseException:
        os.unlink(tmp_path)
        raise


def atomic_write_text(path, text):
    """Write *text* to *path* atomically (temp file + rename)."""
    import tempfile
    tmp_fd, tmp_path = tempfile.mkstemp(dir=str(Path(path).parent), suffix=".tmp")
    try:
        with os.fdopen(tmp_fd, "w") as f:
            f.write(text)
        os.replace(tmp_path, str(path))
    except BaseException:
        os.unlink(tmp_path)
        raise


def strip_accents(text):
    """Remove diacritics/accents but preserve case and punctuation.

    This is a subset of ``normalize()`` (which also lowercases and strips
    punctuation). Use when you need accent-insensitive comparison without
    changing case.
    """
    text = unicodedata.normalize("NFD", text)
    return "".join(c for c in text if unicodedata.category(c) != "Mn")


# === Utility functions ===


def normalize(text):
    """Lowercase, strip accents, remove punctuation, collapse whitespace."""
    if not text:
        return ""
    text = unicodedata.normalize("NFD", text)
    text = "".join(c for c in text if unicodedata.category(c) != "Mn")
    text = text.lower()
    text = re.sub(r"[^\w\s-]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


def extract_pdf_path(file_field: str) -> Path | None:
    """Extract the first PDF path from a BibTeX ``file`` field.

    Handles the formats found in our bib files:
      - Simple:  ``{~/My Drive/library-pdf/Author2020.pdf}``
      - Multi:   ``{file1.html;~/My Drive/library-pdf/Author2020.pdf}``
      - Zotero:  ``{Title\\: Subtitle:filename.pdf:application/pdf}``

    Returns the expanded ``Path`` for the first ``.pdf`` entry, or ``None``.
    """
    if not file_field:
        return None

    # Strip outer braces
    field = file_field.strip().strip("{}")

    # Split on semicolons (multi-file entries)
    parts = field.split(";")

    for part in parts:
        part = part.strip()
        if not part:
            continue

        # Zotero-style: "Title:path:mimetype" — split on unescaped colons
        segments = re.split(r"(?<!\\):", part)
        if len(segments) >= 2:
            # Try the second segment (path) first, then fall back to the whole part
            candidate = segments[1].strip() if segments[1].strip() else part
        else:
            candidate = part

        if not candidate.lower().endswith(".pdf"):
            # If the candidate didn't match, try the whole part
            if part.lower().endswith(".pdf"):
                candidate = part
            else:
                continue

        # Expand tilde
        expanded = Path(candidate).expanduser()
        return expanded

    return None


def cite_key_to_slug(cite_key: str) -> str:
    """Convert CamelCase cite key to kebab-case slug.

    Singer1972FamineAffluence -> singer-1972-famine-affluence
    """
    # Insert hyphens at boundaries: letter->digit, digit->letter, lower->upper
    s = re.sub(r"([a-zA-Z])(\d)", r"\1-\2", cite_key)
    s = re.sub(r"(\d)([a-zA-Z])", r"\1-\2", s)
    s = re.sub(r"([a-z])([A-Z])", r"\1-\2", s)
    return s.lower()


def escape_yaml_string(s: str) -> str:
    """Escape a string for YAML double-quoted value."""
    s = s.replace("\\", "\\\\")
    s = s.replace('"', '\\"')
    s = s.replace("\n", "\\n")
    s = s.replace("\t", "\\t")
    return s


# TOML and YAML use the same double-quote escaping rules for our purposes.
escape_toml_string = escape_yaml_string


def parse_bib_entries(
    bib_path,
    strip_braces=False,
    extra_fields=None,
    field_fallbacks=None,
):
    """Parse a .bib file into a list of entry dicts.

    Args:
        bib_path: Path to the .bib file.
        strip_braces: If True, strip curly braces from ALL field values
            during parsing. If a list/set of field names, strip braces
            only from those fields. If False, no brace stripping.
        extra_fields: Additional field names to extract beyond the base
            set (cite_key, entry_type, author, editor, title, year).
            For example: ["shorttitle", "booktitle", "journaltitle"].
        field_fallbacks: Dict mapping field names to fallback field names.
            If the primary field is empty, the fallback is tried.
            For example: {"journaltitle": "journal", "location": "address"}.

    Returns:
        List of dicts, each with at least cite_key, entry_type, author,
        editor, title, year, plus any extra_fields requested.
    """
    if extra_fields is None:
        extra_fields = []
    if field_fallbacks is None:
        field_fallbacks = {}

    entries = []
    text = bib_path.read_text(errors="replace")
    entry_starts = list(re.finditer(r"@(\w+)\s*\{([^,\s]+)\s*,", text))

    # Determine which fields to strip braces from
    if strip_braces is True:
        strip_all = True
        strip_fields = set()
    elif strip_braces is False:
        strip_all = False
        strip_fields = set()
    else:
        # strip_braces is a list/set of field names
        strip_all = False
        strip_fields = set(strip_braces)

    for idx, match in enumerate(entry_starts):
        entry_type = match.group(1).lower()
        cite_key = match.group(2).strip()
        # Skip BibTeX structural types (not actual bibliographic entries)
        if entry_type in ("comment", "preamble", "string"):
            continue

        start_pos = match.end()
        end_pos = (
            entry_starts[idx + 1].start() if idx + 1 < len(entry_starts) else len(text)
        )
        body = text[start_pos:end_pos]

        fields = {}
        # Match BibTeX field = value in three forms:
        #   field = {value with {nested {braces}}}  (up to 4 levels)
        #   field = "quoted value"
        #   field = 1234  (bare integer)
        for field_match in re.finditer(
            r"(\w+)\s*=\s*(?:\{((?:[^{}]|\{(?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*\})*)\}|\"([^\"]*)\"|(\d+))",
            body,
        ):
            field_name = field_match.group(1).lower()
            field_value = (
                field_match.group(2) or field_match.group(3) or field_match.group(4) or ""
            )
            if strip_all:
                field_value = field_value.replace("{", "").replace("}", "")
            elif strip_fields and field_name in strip_fields:
                field_value = field_value.replace("{", "").replace("}", "")
            fields[field_name] = field_value.strip()

        # Year extraction: explicit year field, or derive from date
        year = fields.get("year", "")
        if not year and "date" in fields:
            ym = re.search(r"(\d{4})", fields["date"])
            if ym:
                year = ym.group(1)

        # Build the entry dict with base fields
        entry = {
            "cite_key": cite_key,
            "entry_type": entry_type,
            "author": fields.get("author", ""),
            "editor": fields.get("editor", ""),
            "title": fields.get("title", ""),
            "year": year,
        }

        # Add extra fields, applying fallbacks where configured
        for field_name in extra_fields:
            value = fields.get(field_name, "")
            if not value and field_name in field_fallbacks:
                value = fields.get(field_fallbacks[field_name], "")
            entry[field_name] = value

        # Also resolve fallbacks for base fields that might have them
        for field_name in ("author", "editor", "title"):
            if not entry[field_name] and field_name in field_fallbacks:
                entry[field_name] = fields.get(field_fallbacks[field_name], "")

        entries.append(entry)

    return entries


def markdown_to_org_emphasis(text: str) -> str:
    """Convert markdown emphasis to org-mode emphasis.

    **text** → *text* (bold)
    *text*  → /text/ (italic)

    Bold must be converted before italic so that ** markers
    are consumed first and don't get partially matched as *.
    """
    # Bold: **text** → *text*
    text = re.sub(r"\*\*(.+?)\*\*", r"*\1*", text)
    # Italic: *text* → /text/
    text = re.sub(r"(?<!\*)\*([^*]+?)\*(?!\*)", r"/\1/", text)
    return text


def escape_org_text(text: str) -> str:
    """Escape text for safe inclusion in an org-mode quote block.

    Prefixes lines starting with ``*`` or ``#+`` with a zero-width space
    so org-mode does not interpret them as headings or keywords.
    """
    lines = text.split("\n")
    escaped = []
    for line in lines:
        if line.startswith("*") or line.startswith("#"):
            line = "\u200B" + line  # zero-width space prefix
        escaped.append(line)
    return "\n".join(escaped)


def tag_to_filename(tag: str) -> str:
    """Convert a tag to a kebab-case .org filename.

    "artificial intelligence" -> "artificial-intelligence.org"
    "Arnold Schwarzenegger." -> "arnold-schwarzenegger.org"
    """
    slug = tag.lower().rstrip(".")
    slug = re.sub(r"[^a-z0-9\u00e0-\u00ff-]+", "-", slug)
    slug = re.sub(r"-+", "-", slug).strip("-")
    return slug + ".org"
