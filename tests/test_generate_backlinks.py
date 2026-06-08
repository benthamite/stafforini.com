"""Tests for scripts/generate-backlinks.py — utility functions.

Covers: strip_elisp_quotes, file_to_slug.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

import importlib
import importlib.util

_SCRIPT = Path(__file__).parent.parent / "scripts" / "generate-backlinks.py"
_spec = importlib.util.spec_from_file_location("generate_backlinks", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

strip_elisp_quotes = _mod.strip_elisp_quotes
file_to_slug = _mod.file_to_slug
discover_exported_slugs = _mod.discover_exported_slugs


# ---------------------------------------------------------------------------
# strip_elisp_quotes
# ---------------------------------------------------------------------------

class TestStripElispQuotes:
    def test_quoted_string(self):
        assert strip_elisp_quotes('"hello"') == "hello"

    def test_unquoted_string(self):
        assert strip_elisp_quotes("hello") == "hello"

    def test_empty_quotes(self):
        assert strip_elisp_quotes('""') == ""

    def test_non_string_passthrough(self):
        assert strip_elisp_quotes(42) == 42

    def test_none_passthrough(self):
        assert strip_elisp_quotes(None) is None

    def test_single_quote_not_stripped(self):
        # Only double quotes are stripped
        assert strip_elisp_quotes("'hello'") == "'hello'"

    def test_inner_quotes_preserved(self):
        # strip('"') only removes outer quotes; inner content preserved
        assert strip_elisp_quotes('"nested"quote"') == 'nested"quote'


# ---------------------------------------------------------------------------
# file_to_slug
# ---------------------------------------------------------------------------

class TestFileToSlug:
    def test_basic_path(self):
        assert file_to_slug("/path/to/notes/effective-altruism.org") == "effective-altruism"

    def test_quoted_path(self):
        assert file_to_slug('"/path/to/notes/my-note.org"') == "my-note"

    def test_no_extension(self):
        assert file_to_slug("/path/to/file") == "file"

    def test_just_filename(self):
        assert file_to_slug("note.org") == "note"

    def test_path_with_spaces(self):
        assert file_to_slug("/path to/notes/my note.org") == "my note"


# ---------------------------------------------------------------------------
# exported page discovery
# ---------------------------------------------------------------------------

class TestDiscoverExportedSlugs:
    def test_unlisted_pages_are_not_listed_sources(self, tmp_path):
        notes_dir = tmp_path / "notes"
        notes_dir.mkdir()
        (notes_dir / "listed.md").write_text(
            '+++\ntitle = "Listed"\n+++\n\nBody\n',
            encoding="utf-8",
        )
        (notes_dir / "unlisted.md").write_text(
            '+++\ntitle = "Unlisted"\nunlisted = true\n+++\n\nBody\n',
            encoding="utf-8",
        )

        exported_slugs, listed_source_slugs = discover_exported_slugs([notes_dir])

        assert exported_slugs == {"listed", "unlisted"}
        assert listed_source_slugs == {"listed"}
