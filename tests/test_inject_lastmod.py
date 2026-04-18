"""Tests for scripts/inject-lastmod.py — front matter fixups.

Covers: get_front_matter_date, apply_front_matter_fixups.
"""

import re
import sys
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

import importlib
import importlib.util

_SCRIPT = Path(__file__).parent.parent / "scripts" / "inject-lastmod.py"
_spec = importlib.util.spec_from_file_location("inject_lastmod", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

get_front_matter_date = _mod.get_front_matter_date
apply_front_matter_fixups = _mod.apply_front_matter_fixups
find_org_file = _mod.find_org_file
process_single_file = _mod.process_single_file
load_output_to_source_map = _mod.load_output_to_source_map


# ---------------------------------------------------------------------------
# get_front_matter_date
# ---------------------------------------------------------------------------

class TestGetFrontMatterDate:
    def test_standard_date(self):
        text = 'title = "Test"\ndate = 2024-01-15\nlastmod = 2024-02-01'
        assert get_front_matter_date(text) == "2024-01-15"

    def test_quoted_date(self):
        text = 'date = "2024-01-15"'
        assert get_front_matter_date(text) == "2024-01-15"

    def test_no_date(self):
        text = 'title = "Test"\nlastmod = 2024-02-01'
        assert get_front_matter_date(text) is None

    def test_date_with_time(self):
        text = "date = 2024-01-15T10:30:00Z"
        assert get_front_matter_date(text) == "2024-01-15T10:30:00Z"

    def test_empty_text(self):
        assert get_front_matter_date("") is None


# ---------------------------------------------------------------------------
# apply_front_matter_fixups
# ---------------------------------------------------------------------------

class TestApplyFrontMatterFixups:
    def test_injects_lastmod(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text('+++\ntitle = "Test"\ndate = 2024-01-15\n+++\n\nBody.\n')

        with patch.object(_mod, "get_org_title", return_value=None):
            result = apply_front_matter_fixups(md, lastmod_date="2024-06-01")

        assert result["lastmod_updated"] is True
        content = md.read_text()
        assert "lastmod = 2024-06-01" in content

    def test_updates_existing_lastmod(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text('+++\ntitle = "Test"\ndate = 2024-01-15\nlastmod = 2024-02-01\n+++\n\nBody.\n')

        with patch.object(_mod, "get_org_title", return_value=None):
            result = apply_front_matter_fixups(md, lastmod_date="2024-06-01")

        assert result["lastmod_updated"] is True
        content = md.read_text()
        assert "lastmod = 2024-06-01" in content
        assert "lastmod = 2024-02-01" not in content

    def test_no_change_when_lastmod_matches(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text('+++\ntitle = "Test"\ndate = 2024-01-15\nlastmod = 2024-06-01\n+++\n\nBody.\n')

        with patch.object(_mod, "get_org_title", return_value=None):
            result = apply_front_matter_fixups(md, lastmod_date="2024-06-01")

        assert result["lastmod_updated"] is False

    def test_no_toml_front_matter(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text("---\ntitle: Test\n---\n\nBody.\n")

        with patch.object(_mod, "get_org_title", return_value=None):
            result = apply_front_matter_fixups(md, lastmod_date="2024-06-01")

        # TOML regex won't match YAML front matter
        assert result["lastmod_updated"] is False

    def test_injects_title_when_missing(self, tmp_path):
        md = tmp_path / "test-note.md"
        md.write_text("+++\ndate = 2024-01-15\n+++\n\nBody.\n")

        with patch.object(_mod, "get_org_title", return_value="Test Note"):
            result = apply_front_matter_fixups(md)

        assert result["title_injected"] is True
        content = md.read_text()
        assert 'title = "Test Note"' in content

    def test_preserves_body(self, tmp_path):
        md = tmp_path / "test.md"
        original = '+++\ntitle = "Test"\ndate = 2024-01-15\n+++\n\nBody with **markup**.\n'
        md.write_text(original)

        with patch.object(_mod, "get_org_title", return_value=None):
            apply_front_matter_fixups(md, lastmod_date="2024-06-01")

        content = md.read_text()
        assert "Body with **markup**." in content

    def test_title_markup_fix_escapes_quotes(self, tmp_path):
        md = tmp_path / "test.md"
        md.write_text('+++\ntitle = "Old"\n+++\n\nBody.\n')

        with patch.object(_mod, "get_org_title", return_value='The "Good" Life'):
            result = apply_front_matter_fixups(md)

        assert result["title_markup_fixed"] is True
        content = md.read_text()
        assert 'title = "The \\"Good\\" Life"' in content
        assert 'title = "The "Good" Life"' not in content

    def test_write_false_reports_change_without_writing(self, tmp_path):
        md = tmp_path / "test.md"
        original = '+++\ntitle = "Test"\ndate = 2024-01-15\n+++\n\nBody.\n'
        md.write_text(original)

        with patch.object(_mod, "get_org_title", return_value=None):
            result = apply_front_matter_fixups(md, lastmod_date="2024-06-01", write=False)

        assert result["lastmod_updated"] is True
        assert md.read_text() == original


class TestFindOrgFile:
    def test_prefers_output_map_over_duplicate_basename(self, tmp_path):
        direct = tmp_path / "anki.org"
        nested_dir = tmp_path / "pablos-miscellany"
        nested_dir.mkdir()
        nested = nested_dir / "anki.org"
        direct.write_text("* Direct\n")
        nested.write_text("* Nested\n")

        with patch.object(_mod, "ORG_DIR", tmp_path):
            result = find_org_file("anki", {"anki.md": nested})

        assert result == nested

    def test_returns_none_for_ambiguous_fallback(self, tmp_path):
        (tmp_path / "anki.org").write_text("* Direct\n")
        nested_dir = tmp_path / "pablos-miscellany"
        nested_dir.mkdir()
        (nested_dir / "anki.org").write_text("* Nested\n")

        with patch.object(_mod, "ORG_DIR", tmp_path):
            result = find_org_file("anki")

        assert result is None


class TestLoadOutputToSourceMap:
    def test_builds_map_from_export_file_name(self, tmp_path):
        org = tmp_path / "source.org"
        org.write_text("* Source\n:PROPERTIES:\n:EXPORT_FILE_NAME: custom-slug\n:END:\n")

        with patch.object(_mod, "ORG_DIR", tmp_path):
            result = load_output_to_source_map()

        assert result == {"custom-slug.md": org}

    def test_duplicate_export_file_name_raises(self, tmp_path):
        (tmp_path / "one.org").write_text("* One\n:PROPERTIES:\n:EXPORT_FILE_NAME: same\n:END:\n")
        (tmp_path / "two.org").write_text("* Two\n:PROPERTIES:\n:EXPORT_FILE_NAME: same\n:END:\n")

        with patch.object(_mod, "ORG_DIR", tmp_path):
            try:
                load_output_to_source_map()
            except ValueError as exc:
                assert "Duplicate EXPORT_FILE_NAME" in str(exc)
            else:
                raise AssertionError("expected duplicate EXPORT_FILE_NAME to raise")


class TestProcessSingleFile:
    def test_dry_run_does_not_write(self, tmp_path):
        md = tmp_path / "test.md"
        org = tmp_path / "test.org"
        original = '+++\ntitle = "Test"\ndate = 2024-01-15\n+++\n\nBody.\n'
        md.write_text(original)
        org.write_text("* Test\n")

        with patch.object(_mod, "load_output_to_source_map", return_value={"test.md": org}), \
             patch.object(_mod, "_get_single_file_git_date", return_value="2024-06-01"), \
             patch.object(_mod, "_get_single_file_first_date", return_value="2024-01-15"):
            process_single_file(md, dry_run=True)

        assert md.read_text() == original
