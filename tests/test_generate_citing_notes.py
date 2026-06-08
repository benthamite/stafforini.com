"""Tests for scripts/generate-citing-notes.py — cite shortcode extraction.

Covers: CITE_RE regex pattern and cite_key_to_slug integration.
"""

import re
import sys
from pathlib import Path
import importlib.util

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

from lib import cite_key_to_slug

_SCRIPT = Path(__file__).parent.parent / "scripts" / "generate-citing-notes.py"
_spec = importlib.util.spec_from_file_location("generate_citing_notes", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

# Reproduce the regex from generate-citing-notes.py
CITE_RE = re.compile(r'\{\{<\s*cite\s+"([^"]+)"')
collect_citing_notes = _mod.collect_citing_notes


# ---------------------------------------------------------------------------
# CITE_RE regex
# ---------------------------------------------------------------------------

class TestCiteRegex:
    def test_standard_shortcode(self):
        text = '{{< cite "Singer1972FamineAffluence" >}}'
        matches = CITE_RE.findall(text)
        assert matches == ["Singer1972FamineAffluence"]

    def test_multiple_cites(self):
        text = '{{< cite "A2020" >}} and {{< cite "B2021" >}}'
        matches = CITE_RE.findall(text)
        assert matches == ["A2020", "B2021"]

    def test_extra_whitespace(self):
        text = '{{<  cite  "Key2020" >}}'
        matches = CITE_RE.findall(text)
        assert matches == ["Key2020"]

    def test_no_match_without_quotes(self):
        text = "{{< cite Key2020 >}}"
        matches = CITE_RE.findall(text)
        assert matches == []

    def test_cite_in_prose(self):
        text = 'As argued by {{< cite "Author2020" >}}, this is important.'
        matches = CITE_RE.findall(text)
        assert matches == ["Author2020"]

    def test_no_match_in_plain_text(self):
        text = "There is no cite shortcode here."
        matches = CITE_RE.findall(text)
        assert matches == []

    def test_short_cite_with_hyphen(self):
        text = '{{< cite "-Author2020" >}}'
        matches = CITE_RE.findall(text)
        assert matches == ["-Author2020"]

    def test_slug_from_extracted_key(self):
        """Integration: extracted key converts to correct slug."""
        text = '{{< cite "Singer1972FamineAffluence" >}}'
        key = CITE_RE.findall(text)[0]
        assert cite_key_to_slug(key) == "singer-1972-famine-affluence"


# ---------------------------------------------------------------------------
# citing note collection
# ---------------------------------------------------------------------------

class TestCollectCitingNotes:
    def test_unlisted_notes_are_excluded(self, tmp_path):
        (tmp_path / "listed.md").write_text(
            '+++\ntitle = "Listed note"\n+++\n\n{{< cite "Singer1972FamineAffluence" >}}\n',
            encoding="utf-8",
        )
        (tmp_path / "unlisted.md").write_text(
            '+++\ntitle = "Unlisted note"\nunlisted = true\n+++\n\n'
            '{{< cite "Singer1972FamineAffluence" >}}\n',
            encoding="utf-8",
        )

        citing_notes = collect_citing_notes(tmp_path)

        assert citing_notes == {
            "singer-1972-famine-affluence": {("listed", "Listed note")}
        }
