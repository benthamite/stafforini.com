"""Tests for scripts/extract-non-diary-quotes.py — slug-rename detection.

The fault this guards against: a non-diary quote's slug hashes its heading
:ID:, falling back to the quote text when the heading has none. A heading
that gets an ID after its first export silently moves to a new slug and the
old URL 404s (bibliographic-notes 45e22f08 did this to 23 quotes on
2026-08-02). The manifest keys quotes by content so the move is detected and
a redirect rule is emitted.
"""

import importlib.util
from pathlib import Path

import pytest

MODULE_PATH = Path(__file__).parent.parent / "scripts" / "extract-non-diary-quotes.py"


@pytest.fixture(scope="module")
def mod():
    spec = importlib.util.spec_from_file_location("extract_non_diary_quotes", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


ORG_WITHOUT_ID = """\
#+title: Notation as a tool of thought
* Notation as a tool of thought
:PROPERTIES:
:ROAM_REFS: [cite:@Iverson1980NotationAsTool]
:END:

** executability and universality  :public:
#+begin_quote
The thesis of the present paper is that the advantages of executability can be combined.
#+end_quote
"""

ORG_WITH_ID = ORG_WITHOUT_ID.replace(
    "** executability and universality  :public:\n",
    "** executability and universality  :public:\n"
    ":PROPERTIES:\n:ID:       E20B180D-E47F-4D09-8D0F-116DAAF76A2C\n:END:\n",
)


def _quotes(mod, tmp_path, text, name="Iverson1980NotationAsTool.org"):
    p = tmp_path / name
    p.write_text(text)
    return mod.process_org_file(p, set())


class TestContentKey:
    def test_stable_under_whitespace_changes(self, mod):
        a = mod.content_key("w", "one  two\nthree")
        b = mod.content_key("w", "one two three")
        assert a == b

    def test_differs_across_works(self, mod):
        assert mod.content_key("w1", "text") != mod.content_key("w2", "text")


class TestAddingAnIdMovesTheSlug:
    """Replay of the 2026-08-02 event: same quote, ID added, slug changes."""

    def test_slug_changes_but_content_key_does_not(self, mod, tmp_path):
        before = _quotes(mod, tmp_path, ORG_WITHOUT_ID)
        after = _quotes(mod, tmp_path, ORG_WITH_ID)
        assert len(before) == len(after) == 1
        assert before[0]["slug"] != after[0]["slug"]
        assert before[0]["content_key"] == after[0]["content_key"]

    def test_rename_is_detected(self, mod, tmp_path):
        before = _quotes(mod, tmp_path, ORG_WITHOUT_ID)[0]
        after = _quotes(mod, tmp_path, ORG_WITH_ID)[0]
        manifest = {before["content_key"]: before["slug"]}
        renames, new_manifest = mod.detect_slug_renames(
            manifest, {after["content_key"]: after["slug"]}
        )
        assert renames == [(before["slug"], after["slug"])]
        assert new_manifest == {after["content_key"]: after["slug"]}


class TestDetectSlugRenames:
    def test_no_change_no_renames(self, mod):
        renames, new = mod.detect_slug_renames({"k": "s"}, {"k": "s"})
        assert renames == [] and new == {"k": "s"}

    def test_new_quote_is_not_a_rename(self, mod):
        renames, new = mod.detect_slug_renames({}, {"k": "s"})
        assert renames == [] and new == {"k": "s"}

    def test_deleted_quote_is_dropped_not_redirected(self, mod):
        renames, new = mod.detect_slug_renames({"k": "s"}, {})
        assert renames == [] and new == {}


class TestAppendRedirectRules:
    def test_creates_block_at_end(self, mod):
        out = mod.append_redirect_rules("/a  /b  301\n", [("old-q-1", "new-q-1")])
        assert out.startswith("/a  /b  301\n")
        assert mod.REDIRECTS_BLOCK_BEGIN in out
        assert "/quotes/old-q-1/  /quotes/new-q-1/  301\n" in out
        assert out.rstrip("\n").endswith(mod.REDIRECTS_BLOCK_END)

    def test_appends_inside_existing_block(self, mod):
        first = mod.append_redirect_rules("", [("o1", "n1")])
        second = mod.append_redirect_rules(first, [("o2", "n2")])
        begin = second.index(mod.REDIRECTS_BLOCK_BEGIN)
        end = second.index(mod.REDIRECTS_BLOCK_END)
        block = second[begin:end]
        assert "/quotes/o1/  /quotes/n1/  301" in block
        assert "/quotes/o2/  /quotes/n2/  301" in block
        assert second.count(mod.REDIRECTS_BLOCK_BEGIN) == 1

    def test_skips_sources_already_ruled_anywhere(self, mod):
        text = "# hand-written\n/quotes/o1/  /quotes/n1/  301\n"
        assert mod.append_redirect_rules(text, [("o1", "n1")]) == text

    def test_no_renames_leaves_text_untouched(self, mod):
        assert mod.append_redirect_rules("/a  /b  301\n", []) == "/a  /b  301\n"
