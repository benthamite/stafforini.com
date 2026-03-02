"""Tests for scripts/generate-work-pages.py — work page generation.

Covers: bib_author_to_display, strip_citation_from_content, resolve_crossrefs,
generate_work_page (YAML front matter generation).
"""

import re
import sys
from pathlib import Path

# Import the module under test
sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

# We need to handle the module's top-level imports that reference paths
# that may not exist in the test environment. Import individual functions.
import importlib
import importlib.util

_SCRIPT = Path(__file__).parent.parent / "scripts" / "generate-work-pages.py"
_spec = importlib.util.spec_from_file_location("generate_work_pages", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)

# Patch constants that reference filesystem paths before executing the module
import types
_mod.HUGO_ROOT = Path("/tmp/fake-hugo")
_mod.QUOTES_DIR = Path("/tmp/fake-quotes")

_spec.loader.exec_module(_mod)

bib_author_to_display = _mod.bib_author_to_display
strip_citation_from_content = _mod.strip_citation_from_content
resolve_crossrefs = _mod.resolve_crossrefs
generate_work_page = _mod.generate_work_page


# ---------------------------------------------------------------------------
# bib_author_to_display
# ---------------------------------------------------------------------------

class TestBibAuthorToDisplay:
    def test_single_author_last_first(self):
        assert bib_author_to_display("Smith, John") == "John Smith"

    def test_two_authors(self):
        result = bib_author_to_display("Smith, John and Doe, Jane")
        assert result == "John Smith and Jane Doe"

    def test_three_authors(self):
        result = bib_author_to_display("A, X and B, Y and C, Z")
        assert result == "X A, Y B, and Z C"

    def test_four_authors_et_al(self):
        result = bib_author_to_display("A, W and B, X and C, Y and D, Z")
        assert result == "W A et al."

    def test_braces_stripped(self):
        result = bib_author_to_display("{World Health Organization}")
        assert result == "World Health Organization"

    def test_multi_word_surname(self):
        result = bib_author_to_display("de Tocqueville, Alexis")
        assert result == "Alexis de Tocqueville"

    def test_no_comma_passthrough(self):
        # Names without commas are used as-is (e.g., organizational names)
        result = bib_author_to_display("World Health Organization")
        assert result == "World Health Organization"

    def test_empty_string(self):
        assert bib_author_to_display("") == ""

    def test_whitespace_only(self):
        assert bib_author_to_display("   ") == ""

    def test_surname_only(self):
        # "Surname," with trailing comma but no given name
        result = bib_author_to_display("Aristotle,")
        assert result == "Aristotle"

    def test_preserves_particles(self):
        result = bib_author_to_display("van Gogh, Vincent")
        assert result == "Vincent van Gogh"


# ---------------------------------------------------------------------------
# strip_citation_from_content
# ---------------------------------------------------------------------------

class TestStripCitationFromContent:
    def test_strips_citation_after_blockquote(self):
        text = "> Quote text here.\n\n(Author, 2020, p. 42)\n"
        result = strip_citation_from_content(text)
        assert "> Quote text here." in result
        assert "Author, 2020" not in result

    def test_preserves_full_blockquote(self):
        text = "> Line 1\n> Line 2\n>\n> Line 3\n\n(Citation)\n"
        result = strip_citation_from_content(text)
        assert "> Line 1" in result
        assert "> Line 2" in result
        assert "> Line 3" in result

    def test_preserves_topics_line(self):
        text = "> Quote.\n\n(Citation)\n\nTopics: philosophy, ethics\n"
        result = strip_citation_from_content(text)
        assert "Topics: philosophy, ethics" in result

    def test_no_blockquote_returns_unchanged(self):
        text = "No blockquote here.\n"
        result = strip_citation_from_content(text)
        assert result == text

    def test_ends_with_single_newline(self):
        text = "> Quote.\n\n(Citation)\n"
        result = strip_citation_from_content(text)
        assert result.endswith("\n")
        assert not result.endswith("\n\n")

    def test_multiline_blockquote(self):
        text = "> First paragraph.\n>\n> Second paragraph.\n\n(Author, Year)\n"
        result = strip_citation_from_content(text)
        assert "> Second paragraph." in result
        assert "Author, Year" not in result


# ---------------------------------------------------------------------------
# resolve_crossrefs
# ---------------------------------------------------------------------------

class TestResolveCrossrefs:
    def test_inherits_booktitle_from_parent(self):
        bib = {
            "child": {"cite_key": "child", "crossref": "parent", "booktitle": "", "title": "Chapter"},
            "parent": {"cite_key": "parent", "crossref": "", "title": "The Book", "booktitle": ""},
        }
        resolve_crossrefs(bib)
        # booktitle should fall back to parent's title
        assert bib["child"]["booktitle"] == "The Book"

    def test_inherits_year_from_parent(self):
        bib = {
            "child": {"cite_key": "child", "crossref": "parent", "year": ""},
            "parent": {"cite_key": "parent", "crossref": "", "year": "2020"},
        }
        resolve_crossrefs(bib)
        assert bib["child"]["year"] == "2020"

    def test_does_not_overwrite_existing_field(self):
        bib = {
            "child": {"cite_key": "child", "crossref": "parent", "year": "2019", "location": "London"},
            "parent": {"cite_key": "parent", "crossref": "", "year": "2020", "location": "New York"},
        }
        resolve_crossrefs(bib)
        assert bib["child"]["year"] == "2019"
        assert bib["child"]["location"] == "London"

    def test_missing_parent_no_crash(self):
        bib = {
            "child": {"cite_key": "child", "crossref": "nonexistent"},
        }
        resolve_crossrefs(bib)  # Should not raise

    def test_inherits_editor_when_child_has_author(self):
        bib = {
            "child": {"cite_key": "child", "crossref": "parent", "author": "Writer", "editor": ""},
            "parent": {"cite_key": "parent", "crossref": "", "editor": "Editor Person", "author": ""},
        }
        resolve_crossrefs(bib)
        assert bib["child"]["editor"] == "Editor Person"


# ---------------------------------------------------------------------------
# generate_work_page
# ---------------------------------------------------------------------------

class TestGenerateWorkPage:
    def test_basic_book(self):
        entry = {
            "title": "The Art of War",
            "author": "Tzu, Sun",
            "editor": "",
            "year": "500",
            "entry_type": "book",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "",
            "abstract": "",
        }
        result = generate_work_page(entry)
        assert '---' in result
        assert 'title: "The Art of War"' in result
        assert 'author: "Sun Tzu"' in result
        assert 'entry_type: "book"' in result

    def test_shorttitle_from_colon(self):
        entry = {
            "title": "Main Title: A Subtitle",
            "author": "Smith, John",
            "editor": "",
            "year": "2020",
            "entry_type": "book",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "",
            "abstract": "",
        }
        result = generate_work_page(entry)
        assert 'shorttitle: "Main Title"' in result

    def test_bibtex_escapes_cleaned(self):
        entry = {
            "title": "R\\&D in the 21st Century",
            "author": "Smith, John",
            "editor": "",
            "year": "2020",
            "entry_type": "article",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "",
            "abstract": "",
        }
        result = generate_work_page(entry)
        assert "R&D" in result
        assert "R\\&D" not in result

    def test_braces_stripped_from_title(self):
        entry = {
            "title": "A {LaTeX} Title",
            "author": "A, B",
            "editor": "",
            "year": "2020",
            "entry_type": "article",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "",
            "abstract": "",
        }
        result = generate_work_page(entry)
        assert "A LaTeX Title" in result
        assert "{" not in result.split("---")[1]  # No braces in front matter

    def test_pub_date_included_when_full_date(self):
        entry = {
            "title": "T",
            "author": "A, B",
            "editor": "",
            "year": "2020",
            "entry_type": "article",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "2020-05-15",
            "abstract": "",
        }
        result = generate_work_page(entry)
        assert 'pub_date: "2020-05-15"' in result

    def test_pub_date_omitted_for_year_only(self):
        entry = {
            "title": "T",
            "author": "A, B",
            "editor": "",
            "year": "2020",
            "entry_type": "article",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "2020",
            "abstract": "",
        }
        result = generate_work_page(entry)
        assert "pub_date" not in result

    def test_abstract_in_body(self):
        entry = {
            "title": "T",
            "author": "A, B",
            "editor": "",
            "year": "2020",
            "entry_type": "article",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "",
            "abstract": "This is the abstract.",
        }
        result = generate_work_page(entry)
        # Abstract should appear after the closing ---
        parts = result.split("---")
        body = parts[-1]
        assert "This is the abstract." in body

    def test_editor_as_author_fallback(self):
        entry = {
            "title": "T",
            "author": "",
            "editor": "Editor, A",
            "year": "2020",
            "entry_type": "collection",
            "shorttitle": "",
            "location": "",
            "booktitle": "",
            "journaltitle": "",
            "volume": "",
            "number": "",
            "pages": "",
            "url": "",
            "date": "",
            "abstract": "",
        }
        result = generate_work_page(entry)
        assert 'author: "A Editor"' in result
