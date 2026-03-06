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
