"""Tests for scripts/generate-citing-notes.py — cite shortcode extraction.

Covers: CITE_RE regex pattern and cite_key_to_slug integration.
"""

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

from lib import cite_key_to_slug

# Reproduce the regex from generate-citing-notes.py
CITE_RE = re.compile(r'\{\{<\s*cite\s+"([^"]+)"')


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
