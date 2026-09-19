"""Tests for scripts/rename-cite-keys.py — redirect rule generation.

Netlify matches _redirects rules against the percent-encoded request
path, so a rule whose source column holds raw UTF-8 never fires. These
tests pin the encoding, since the failure is silent in production.
"""

import importlib.util
import io
from pathlib import Path

import pytest

_SCRIPT = Path(__file__).parent.parent / "scripts" / "rename-cite-keys.py"
_spec = importlib.util.spec_from_file_location("rename_cite_keys", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

encode_work_path = _mod.encode_work_path
rename_redirects = _mod.rename_redirects


@pytest.fixture
def rename_sources(tmp_path, monkeypatch):
    bib = tmp_path / "source.bib"
    bib.write_text("@book{Old2020,\n title = {Original}\n}\n")
    monkeypatch.setattr(_mod, "BIB_FILES", [bib])
    for name in ("BIBNOTES", "NOTES", "QUOTES"):
        directory = tmp_path / name
        directory.mkdir()
        monkeypatch.setattr(_mod, name, directory)
    monkeypatch.setattr(_mod, "REDIRECTS", tmp_path / "redirects")
    (_mod.BIBNOTES / "Old2020.org").write_text("Original note\n")
    return tmp_path, bib


class TestRenameCollisions:
    def test_existing_note_is_preserved_before_bib_changes(self, rename_sources):
        root, bib = rename_sources
        destination = _mod.BIBNOTES / "New2020.org"
        destination.write_text("Unique destination content\n")
        before = {p: p.read_bytes() for p in root.rglob("*") if p.is_file()}
        with pytest.raises(ValueError, match="Destination note already exists"):
            _mod.apply_rename("Old2020", "New2020")
        assert {p: p.read_bytes() for p in root.rglob("*") if p.is_file()} == before

    def test_existing_bib_key_is_rejected(self, rename_sources):
        _, bib = rename_sources
        bib.write_text(bib.read_text() + "@book{New2020,\n title = {Existing}\n}\n")
        before = bib.read_bytes()
        with pytest.raises(ValueError, match="Destination cite key already exists"):
            _mod.apply_rename("Old2020", "New2020")
        assert bib.read_bytes() == before
        assert (_mod.BIBNOTES / "Old2020.org").exists()

    @pytest.mark.parametrize("mapping", [
        {"Old2020": "New2020", "Other": "New2020"},
        {"Old2020": "New2020", "New2020": "Last"},
        {"Old2020": "New2020", "New2020": "Old2020"},
    ])
    def test_batch_collisions_fail_before_any_write(self, rename_sources, monkeypatch, mapping):
        root, _ = rename_sources
        before = {p: p.read_bytes() for p in root.rglob("*") if p.is_file()}
        monkeypatch.setattr(_mod.sys, "stdin", io.StringIO(
            "".join(f"{old}\t{new}\n" for old, new in mapping.items())))
        assert _mod.main() == 1
        assert {p: p.read_bytes() for p in root.rglob("*") if p.is_file()} == before

    def test_later_existing_destination_blocks_entire_batch(self, rename_sources, monkeypatch):
        root, _ = rename_sources
        (_mod.BIBNOTES / "Taken.org").write_text("Keep this note\n")
        before = {p: p.read_bytes() for p in root.rglob("*") if p.is_file()}
        monkeypatch.setattr(_mod.sys, "stdin", io.StringIO(
            "Old2020\tNew2020\nOther\tTaken\n"))
        assert _mod.main() == 1
        assert {p: p.read_bytes() for p in root.rglob("*") if p.is_file()} == before

    def test_noncolliding_rename_updates_sources(self, rename_sources):
        _, bib = rename_sources
        _mod.apply_rename("Old2020", "New2020")
        assert "@book{New2020," in bib.read_text()
        assert not (_mod.BIBNOTES / "Old2020.org").exists()
        assert (_mod.BIBNOTES / "New2020.org").read_text() == "Original note\n"

    def test_identity_rename_does_not_add_a_self_redirect(self, rename_sources):
        root, _ = rename_sources
        before = {p: p.read_bytes() for p in root.rglob("*") if p.is_file()}
        _mod.apply_rename("Old2020", "Old2020")
        assert {p: p.read_bytes() for p in root.rglob("*") if p.is_file()} == before


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
