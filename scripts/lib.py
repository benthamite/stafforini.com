"""Shared library for stafforini.com build and migration scripts.

Centralizes duplicated utility functions and constants so that each
script imports from a single source of truth rather than maintaining
its own copy.
"""

import re
import unicodedata
from pathlib import Path


# === Constants ===


# Canonical list of all bib files used across scripts.
# Individual scripts select subsets as needed.
BIB_FILES = [
    # Personal bibliography: actively maintained entries
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/new.bib",
    # Personal bibliography: older/archival entries
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/old.bib",
    # Shared bibliography (babel-refs): entries still being edited
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/fluid.bib",
    # Shared bibliography (babel-refs): stable/finalized entries
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/stable.bib",
    # Shared bibliography (babel-refs): database-sourced entries
    Path.home() / "Library/CloudStorage/Dropbox/repos/babel-refs/bib/db.bib",
    # Auto-generated entries from WordPress quote migration
    Path.home() / "Library/CloudStorage/Dropbox/bibliography/migration.bib",
]

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


# === Utility functions ===


def normalize(text):
    """Lowercase, strip accents, remove punctuation, collapse whitespace."""
    text = unicodedata.normalize("NFD", text)
    text = "".join(c for c in text if unicodedata.category(c) != "Mn")
    text = text.lower()
    text = re.sub(r"[^\w\s-]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


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
    return s.replace("\\", "\\\\").replace('"', '\\"')


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
        if entry_type in ("comment", "preamble", "string"):
            continue

        start_pos = match.end()
        end_pos = (
            entry_starts[idx + 1].start() if idx + 1 < len(entry_starts) else len(text)
        )
        body = text[start_pos:end_pos]

        fields = {}
        for field_match in re.finditer(
            r"(\w+)\s*=\s*(?:\{((?:[^{}]|\{[^{}]*\})*)\}|\"([^\"]*)\"|(\d+))",
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
