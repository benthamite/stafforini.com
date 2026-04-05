"""Tests for scripts/lib.py — shared utility functions.

Covers: cite_key_to_slug, normalize, strip_accents, extract_pdf_path,
escape_yaml_string, tag_to_filename, markdown_to_org_emphasis,
escape_org_text, parse_bib_entries, atomic_write_json, atomic_write_text.
"""

import json
from pathlib import Path

import lib
import pytest


# ---------------------------------------------------------------------------
# cite_key_to_slug
# ---------------------------------------------------------------------------

class TestCiteKeyToSlug:
    def test_standard_camel_case(self):
        assert lib.cite_key_to_slug("Singer1972FamineAffluence") == "singer-1972-famine-affluence"

    def test_short_cite_prefix_stripped(self):
        assert lib.cite_key_to_slug("-Singer1972FamineAffluence") == "singer-1972-famine-affluence"

    def test_single_author_no_title(self):
        assert lib.cite_key_to_slug("Parfit1984") == "parfit-1984"

    def test_all_lowercase(self):
        assert lib.cite_key_to_slug("smith2020") == "smith-2020"

    def test_consecutive_uppercase(self):
        # All-caps "WHO" stays together — only lower→upper inserts a hyphen
        result = lib.cite_key_to_slug("WHO2020Report")
        assert result == "who-2020-report"

    def test_numbers_in_middle(self):
        assert lib.cite_key_to_slug("Author2020SomeTitle") == "author-2020-some-title"

    def test_already_lowercase_with_digits(self):
        assert lib.cite_key_to_slug("smith2020example") == "smith-2020-example"

    def test_empty_string(self):
        assert lib.cite_key_to_slug("") == ""

    def test_only_hyphen_prefix(self):
        assert lib.cite_key_to_slug("-") == ""

    def test_multiple_hyphen_prefixes(self):
        # lstrip("-") removes all leading hyphens
        assert lib.cite_key_to_slug("--Smith2020") == "smith-2020"


# ---------------------------------------------------------------------------
# build_unique_slug_map
# ---------------------------------------------------------------------------

class TestBuildUniqueSlugMap:
    def test_unique_cite_keys(self):
        result = lib.build_unique_slug_map(["Singer1972FamineAffluence", "Parfit1984"])
        assert result == {
            "singer-1972-famine-affluence": "Singer1972FamineAffluence",
            "parfit-1984": "Parfit1984",
        }

    def test_collision_raises(self):
        with pytest.raises(ValueError, match="foo-2020-bar"):
            lib.build_unique_slug_map(["Foo2020Bar", "foo2020Bar"])


# ---------------------------------------------------------------------------
# normalize
# ---------------------------------------------------------------------------

class TestNormalize:
    def test_basic(self):
        assert lib.normalize("Hello World") == "hello world"

    def test_accents_stripped(self):
        assert lib.normalize("café") == "cafe"

    def test_punctuation_removed(self):
        assert lib.normalize("Hello, World!") == "hello world"

    def test_whitespace_collapsed(self):
        assert lib.normalize("  too   many   spaces  ") == "too many spaces"

    def test_empty_string(self):
        assert lib.normalize("") == ""

    def test_none_like_falsy(self):
        assert lib.normalize("") == ""

    def test_hyphens_preserved(self):
        # Hyphens are kept by the regex [^\w\s-]
        assert lib.normalize("well-known") == "well-known"

    def test_mixed_unicode(self):
        assert lib.normalize("Über Résumé") == "uber resume"


# ---------------------------------------------------------------------------
# strip_accents
# ---------------------------------------------------------------------------

class TestStripAccents:
    def test_preserves_case(self):
        assert lib.strip_accents("Café") == "Cafe"

    def test_preserves_punctuation(self):
        assert lib.strip_accents("résumé!") == "resume!"

    def test_no_accents(self):
        assert lib.strip_accents("hello") == "hello"

    def test_empty(self):
        assert lib.strip_accents("") == ""

    def test_german_umlauts(self):
        assert lib.strip_accents("über") == "uber"


# ---------------------------------------------------------------------------
# extract_pdf_path
# ---------------------------------------------------------------------------

class TestExtractPdfPath:
    def test_simple_path(self):
        result = lib.extract_pdf_path("{~/My Drive/library-pdf/Author2020.pdf}")
        assert result is not None
        assert result.name == "Author2020.pdf"

    def test_simple_path_no_braces(self):
        result = lib.extract_pdf_path("~/My Drive/library-pdf/Author2020.pdf")
        assert result is not None
        assert result.name == "Author2020.pdf"

    def test_multi_file_picks_pdf(self):
        result = lib.extract_pdf_path("{file1.html;~/My Drive/library-pdf/Author2020.pdf}")
        assert result is not None
        assert result.name == "Author2020.pdf"

    def test_zotero_format(self):
        result = lib.extract_pdf_path("{Title\\: Subtitle:Author2020.pdf:application/pdf}")
        assert result is not None
        assert result.name == "Author2020.pdf"

    def test_empty_string(self):
        assert lib.extract_pdf_path("") is None

    def test_no_pdf(self):
        assert lib.extract_pdf_path("{file.html}") is None

    def test_tilde_expanded(self):
        result = lib.extract_pdf_path("~/test.pdf")
        assert result is not None
        assert str(result).startswith(str(Path.home()))

    def test_none_input(self):
        # extract_pdf_path guards against empty input
        assert lib.extract_pdf_path("") is None

    def test_multi_file_first_is_pdf(self):
        result = lib.extract_pdf_path("{~/first.pdf;~/second.pdf}")
        assert result is not None
        assert result.name == "first.pdf"

    def test_semicolon_only_non_pdf(self):
        assert lib.extract_pdf_path("{file.html;file.txt}") is None


# ---------------------------------------------------------------------------
# escape_yaml_string
# ---------------------------------------------------------------------------

class TestEscapeYamlString:
    def test_quotes_escaped(self):
        assert lib.escape_yaml_string('say "hello"') == 'say \\"hello\\"'

    def test_backslash_escaped(self):
        assert lib.escape_yaml_string("back\\slash") == "back\\\\slash"

    def test_newline_escaped(self):
        assert lib.escape_yaml_string("line1\nline2") == "line1\\nline2"

    def test_tab_escaped(self):
        assert lib.escape_yaml_string("col1\tcol2") == "col1\\tcol2"

    def test_plain_text_unchanged(self):
        assert lib.escape_yaml_string("plain text") == "plain text"

    def test_escape_order_backslash_before_quotes(self):
        # A backslash before a quote should become \\" not \\\\"
        assert lib.escape_yaml_string('\\"') == '\\\\\\"'

    def test_empty(self):
        assert lib.escape_yaml_string("") == ""


# ---------------------------------------------------------------------------
# tag_to_filename
# ---------------------------------------------------------------------------

class TestTagToFilename:
    def test_basic(self):
        assert lib.tag_to_filename("artificial intelligence") == "artificial-intelligence.org"

    def test_trailing_period_stripped(self):
        assert lib.tag_to_filename("Arnold Schwarzenegger.") == "arnold-schwarzenegger.org"

    def test_special_characters(self):
        assert lib.tag_to_filename("C++ programming") == "c-programming.org"

    def test_accented_characters_preserved(self):
        # Latin-1 extended range (\u00e0-\u00ff) is kept
        assert lib.tag_to_filename("café latte") == "café-latte.org"

    def test_multiple_spaces(self):
        assert lib.tag_to_filename("too  many   spaces") == "too-many-spaces.org"

    def test_leading_trailing_hyphens_stripped(self):
        assert lib.tag_to_filename(" hello ") == "hello.org"


# ---------------------------------------------------------------------------
# markdown_to_org_emphasis
# ---------------------------------------------------------------------------

class TestMarkdownToOrgEmphasis:
    def test_bold_conversion(self):
        # Bold markdown becomes org bold, not italic.
        assert lib.markdown_to_org_emphasis("**bold**") == "*bold*"

    def test_italic_conversion(self):
        assert lib.markdown_to_org_emphasis("*italic*") == "/italic/"

    def test_bold_and_italic_together(self):
        # Bold stays bold, italic becomes org italic.
        result = lib.markdown_to_org_emphasis("**bold** and *italic*")
        assert result == "*bold* and /italic/"

    def test_no_emphasis(self):
        assert lib.markdown_to_org_emphasis("plain text") == "plain text"

    def test_adjacent_bold_markers_not_split(self):
        # Verify ** is consumed as a unit, not as two separate *
        result = lib.markdown_to_org_emphasis("**bold** then *italic*")
        assert "*bold*" in result
        assert "/italic/" in result

    def test_empty(self):
        assert lib.markdown_to_org_emphasis("") == ""


# ---------------------------------------------------------------------------
# escape_org_text
# ---------------------------------------------------------------------------

class TestEscapeOrgText:
    def test_heading_escaped(self):
        result = lib.escape_org_text("* Heading")
        assert result.startswith("\u200B")

    def test_keyword_escaped(self):
        result = lib.escape_org_text("#+title: Hello")
        assert result.startswith("\u200B")

    def test_normal_text_unchanged(self):
        assert lib.escape_org_text("normal text") == "normal text"

    def test_multiline(self):
        result = lib.escape_org_text("normal\n* heading\nmore text")
        lines = result.split("\n")
        assert lines[0] == "normal"
        assert lines[1].startswith("\u200B")
        assert lines[2] == "more text"

    def test_hash_without_plus_escaped(self):
        # The code checks line.startswith("#"), so bare # is also escaped
        result = lib.escape_org_text("# comment")
        assert result.startswith("\u200B")


# ---------------------------------------------------------------------------
# parse_bib_entries
# ---------------------------------------------------------------------------

class TestParseBibEntries:
    def test_basic_entry(self, tmp_bib):
        path = tmp_bib("""
@article{Smith2020Example,
  author = {Smith, John},
  title = {An Example Article},
  year = {2020},
}
""")
        entries = lib.parse_bib_entries(path)
        assert len(entries) == 1
        e = entries[0]
        assert e["cite_key"] == "Smith2020Example"
        assert e["entry_type"] == "article"
        assert e["author"] == "Smith, John"
        assert e["title"] == "An Example Article"
        assert e["year"] == "2020"

    def test_multiple_entries(self, tmp_bib):
        path = tmp_bib("""
@article{A2020, author={A}, title={First}, year={2020}}
@book{B2021, author={B}, title={Second}, year={2021}}
""")
        entries = lib.parse_bib_entries(path)
        assert len(entries) == 2
        assert entries[0]["entry_type"] == "article"
        assert entries[1]["entry_type"] == "book"

    def test_skips_comment_preamble_string(self, tmp_bib):
        path = tmp_bib("""
@comment{This is a comment}
@preamble{"\\newcommand{\\noopsort}[1]{}"}
@string{jan = "January"}
@article{Real2020, author={X}, title={Real Entry}, year={2020}}
""")
        entries = lib.parse_bib_entries(path)
        assert len(entries) == 1
        assert entries[0]["cite_key"] == "Real2020"

    def test_entry_type_lowercased(self, tmp_bib):
        path = tmp_bib("""
@Article{Test2020, author={X}, title={T}, year={2020}}
""")
        entries = lib.parse_bib_entries(path)
        assert entries[0]["entry_type"] == "article"

    def test_extra_fields(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = {A},
  title = {T},
  year = {2020},
  file = {~/test.pdf},
  translator = {Someone},
}
""")
        entries = lib.parse_bib_entries(path, extra_fields=["file", "translator"])
        e = entries[0]
        assert e["file"] == "~/test.pdf"
        assert e["translator"] == "Someone"

    def test_field_fallbacks(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = {A},
  title = {T},
  year = {2020},
  journal = {Some Journal},
}
""")
        entries = lib.parse_bib_entries(
            path,
            extra_fields=["journaltitle"],
            field_fallbacks={"journaltitle": "journal"},
        )
        assert entries[0]["journaltitle"] == "Some Journal"

    def test_year_from_date_fallback(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = {A},
  title = {T},
  date = {2020-05-15},
}
""")
        entries = lib.parse_bib_entries(path)
        assert entries[0]["year"] == "2020"

    def test_strip_braces_true(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = {{World Health Organization}},
  title = {A {Nested} Title},
  year = {2020},
}
""")
        entries = lib.parse_bib_entries(path, strip_braces=True)
        assert "{" not in entries[0]["author"]
        assert "{" not in entries[0]["title"]

    def test_strip_braces_selective(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = {{WHO}},
  title = {A {Nested} Title},
  year = {2020},
}
""")
        entries = lib.parse_bib_entries(path, strip_braces=["title"])
        # title should have braces stripped
        assert entries[0]["title"] == "A Nested Title"
        # author should still have braces (not in strip list)
        assert entries[0]["author"] == "{WHO}"

    def test_quoted_field_value(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = "Smith, John",
  title = "A Title",
  year = "2020",
}
""")
        entries = lib.parse_bib_entries(path)
        assert entries[0]["author"] == "Smith, John"

    def test_bare_integer_year(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = {A},
  title = {T},
  year = 2020,
}
""")
        entries = lib.parse_bib_entries(path)
        assert entries[0]["year"] == "2020"

    def test_nested_braces(self, tmp_bib):
        path = tmp_bib("""
@article{Test2020,
  author = {A},
  title = {A title with {nested {braces}} inside},
  year = {2020},
}
""")
        entries = lib.parse_bib_entries(path)
        assert "nested {braces}" in entries[0]["title"]

    def test_crossref_field(self, tmp_bib):
        path = tmp_bib("""
@incollection{Child2020,
  author = {Child},
  title = {A Chapter},
  crossref = {Parent2020},
}
@collection{Parent2020,
  editor = {Editor},
  title = {The Collection},
  year = {2020},
}
""")
        entries = lib.parse_bib_entries(path, extra_fields=["crossref"])
        child = [e for e in entries if e["cite_key"] == "Child2020"][0]
        assert child["crossref"] == "Parent2020"

    def test_empty_file(self, tmp_bib):
        path = tmp_bib("")
        entries = lib.parse_bib_entries(path)
        assert entries == []


# ---------------------------------------------------------------------------
# atomic_write_json / atomic_write_text
# ---------------------------------------------------------------------------

class TestAtomicWrite:
    def test_atomic_write_json(self, tmp_path):
        path = tmp_path / "test.json"
        data = {"key": "value", "number": 42}
        lib.atomic_write_json(path, data)
        assert path.exists()
        loaded = json.loads(path.read_text())
        assert loaded == data

    def test_atomic_write_json_trailing_newline(self, tmp_path):
        path = tmp_path / "test.json"
        lib.atomic_write_json(path, {"a": 1})
        assert path.read_text().endswith("\n")

    def test_atomic_write_text(self, tmp_path):
        path = tmp_path / "test.txt"
        lib.atomic_write_text(path, "hello world")
        assert path.read_text() == "hello world"

    def test_atomic_write_overwrites(self, tmp_path):
        path = tmp_path / "test.txt"
        lib.atomic_write_text(path, "first")
        lib.atomic_write_text(path, "second")
        assert path.read_text() == "second"
