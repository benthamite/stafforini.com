"""Tests for scripts/rename-cite-keys.py — redirect rule generation.

Netlify matches _redirects rules against the percent-encoded request
path, so a rule whose source column holds raw UTF-8 never fires. These
tests pin the encoding, since the failure is silent in production.
"""

import importlib.util
from pathlib import Path

_SCRIPT = Path(__file__).parent.parent / "scripts" / "rename-cite-keys.py"
_spec = importlib.util.spec_from_file_location("rename_cite_keys", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

encode_work_path = _mod.encode_work_path
rename_redirects = _mod.rename_redirects


class TestEncodeWorkPath:
    def test_ascii_slug_is_unchanged(self):
        assert encode_work_path("guzey-2022-theses-sleep") == (
            "/works/guzey-2022-theses-sleep/")

    def test_accented_character_is_percent_encoded(self):
        assert encode_work_path("jørgensen-2008-encyclopedia-ecology") == (
            "/works/j%C3%B8rgensen-2008-encyclopedia-ecology/")

    def test_dot_is_left_alone(self):
        # `.` is unreserved; encoding it would break existing rules.
        assert encode_work_path("gov.uk-2022-government-review-uk") == (
            "/works/gov.uk-2022-government-review-uk/")

    def test_semicolon_is_left_alone(self):
        # Multi-author cite keys separate names with `;`, which browsers
        # send raw. Encoding it here would stop the rule matching.
        slug = "daron-acemoglu;giuseppe-de-feo-war-socialism-rise"
        assert encode_work_path(slug) == f"/works/{slug}/"


class TestRenameRedirects:
    def test_adds_an_encoded_rule_for_the_old_slug(self):
        text, _, added = rename_redirects(
            "", "jørgensen-2008-encyclopedia-ecology",
            "jorgensen-2008-encyclopedia-ecology")
        assert added
        assert text.strip() == (
            "/works/j%C3%B8rgensen-2008-encyclopedia-ecology/  "
            "/works/jorgensen-2008-encyclopedia-ecology/  301")

    def test_rewrites_an_existing_raw_target(self):
        original = "/docs/x.pdf  /works/jørgensen-2008-encyclopedia-ecology/  301\n"
        text, count, _ = rename_redirects(
            original, "jørgensen-2008-encyclopedia-ecology",
            "jorgensen-2008-encyclopedia-ecology")
        assert count == 1
        assert "/docs/x.pdf  /works/jorgensen-2008-encyclopedia-ecology/  301" in text
        assert "jørgensen" not in text.split("\n")[0]

    def test_rewrites_an_existing_encoded_target(self):
        original = ("/docs/x.pdf  "
                    "/works/j%C3%B8rgensen-2008-encyclopedia-ecology/  301\n")
        text, count, _ = rename_redirects(
            original, "jørgensen-2008-encyclopedia-ecology",
            "jorgensen-2008-encyclopedia-ecology")
        assert count == 1
        assert "/docs/x.pdf  /works/jorgensen-2008-encyclopedia-ecology/  301" in text

    def test_does_not_duplicate_an_existing_rule(self):
        first, _, _ = rename_redirects("", "smith-2020-ab", "smith-2020-a")
        second, _, added = rename_redirects(first, "smith-2020-ab", "smith-2020-a")
        assert not added
        assert second.count("/works/smith-2020-ab/") == 1

    def test_does_not_clobber_a_rule_whose_source_is_the_old_slug(self):
        # The old slug appears in the source column of an earlier rename.
        # Only the target column may be rewritten, or that rule becomes a
        # self-redirect.
        original = "/works/smith-2020-ab/  /works/smith-2020-a/  301\n"
        text, _, _ = rename_redirects(original, "smith-2020-a", "smith-2020")
        assert "/works/smith-2020-ab/  /works/smith-2020/  301" in text
        assert "/works/smith-2020-a/  /works/smith-2020-a/" not in text
