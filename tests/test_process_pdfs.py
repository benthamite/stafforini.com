"""Tests for scripts/process-pdfs.py — PDF processing and copyright filtering.

Covers: _is_excluded_book, resolve_crossref_pdfs, needs_processing.
"""

from pathlib import Path
from unittest.mock import patch

from process_pdfs_helpers import _is_excluded_book, resolve_crossref_pdfs, BOOK_LIKE_TYPES, BOOK_YEAR_CUTOFF


# ---------------------------------------------------------------------------
# _is_excluded_book
# ---------------------------------------------------------------------------

class TestIsExcludedBook:
    """Test the copyright-policy book filter."""

    def test_book_2000_excluded(self):
        assert _is_excluded_book({"entry_type": "book", "year": "2000"}) is True

    def test_book_2020_excluded(self):
        assert _is_excluded_book({"entry_type": "book", "year": "2020"}) is True

    def test_book_1999_allowed(self):
        assert _is_excluded_book({"entry_type": "book", "year": "1999"}) is False

    def test_book_1800_allowed(self):
        assert _is_excluded_book({"entry_type": "book", "year": "1800"}) is False

    def test_collection_2005_excluded(self):
        assert _is_excluded_book({"entry_type": "collection", "year": "2005"}) is True

    def test_reference_2010_excluded(self):
        assert _is_excluded_book({"entry_type": "reference", "year": "2010"}) is True

    def test_mvbook_2001_excluded(self):
        assert _is_excluded_book({"entry_type": "mvbook", "year": "2001"}) is True

    def test_all_book_like_types_covered(self):
        for t in BOOK_LIKE_TYPES:
            assert _is_excluded_book({"entry_type": t, "year": "2020"}) is True

    def test_article_not_excluded(self):
        assert _is_excluded_book({"entry_type": "article", "year": "2020"}) is False

    def test_incollection_not_excluded(self):
        assert _is_excluded_book({"entry_type": "incollection", "year": "2020"}) is False

    def test_inproceedings_not_excluded(self):
        assert _is_excluded_book({"entry_type": "inproceedings", "year": "2020"}) is False

    def test_missing_year_conservative(self):
        """Unknown year on a book-like type → excluded (conservative)."""
        assert _is_excluded_book({"entry_type": "book", "year": ""}) is True

    def test_no_year_key_conservative(self):
        assert _is_excluded_book({"entry_type": "book"}) is True

    def test_unparseable_year_conservative(self):
        assert _is_excluded_book({"entry_type": "book", "year": "forthcoming"}) is True

    def test_missing_entry_type(self):
        assert _is_excluded_book({"year": "2020"}) is False

    def test_empty_entry(self):
        assert _is_excluded_book({}) is False

    def test_boundary_year_1999(self):
        assert _is_excluded_book({"entry_type": "book", "year": "1999"}) is False

    def test_boundary_year_2000(self):
        assert _is_excluded_book({"entry_type": "book", "year": "2000"}) is True

    def test_whitespace_in_year(self):
        assert _is_excluded_book({"entry_type": "book", "year": " 2005 "}) is True


# ---------------------------------------------------------------------------
# resolve_crossref_pdfs
# ---------------------------------------------------------------------------

class TestResolveCrossrefPdfs:
    """Test crossref PDF inheritance with copyright filtering."""

    def test_basic_inheritance(self):
        entries = {
            "child": {"cite_key": "child", "entry_type": "incollection", "crossref": "parent"},
            "parent": {"cite_key": "parent", "entry_type": "collection", "year": "1990", "file": "parent.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 1
        assert entries["child"]["file"] == "parent.pdf"

    def test_no_inheritance_when_child_has_file(self):
        entries = {
            "child": {"cite_key": "child", "entry_type": "incollection", "crossref": "parent", "file": "own.pdf"},
            "parent": {"cite_key": "parent", "entry_type": "collection", "year": "1990", "file": "parent.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 0
        assert entries["child"]["file"] == "own.pdf"

    def test_no_inheritance_from_excluded_book(self):
        """Post-2000 collection PDF should not be inherited."""
        entries = {
            "child": {"cite_key": "child", "entry_type": "incollection", "crossref": "parent"},
            "parent": {"cite_key": "parent", "entry_type": "collection", "year": "2005", "file": "parent.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 0
        assert "file" not in entries["child"]

    def test_inheritance_from_pre2000_book(self):
        entries = {
            "child": {"cite_key": "child", "entry_type": "inbook", "crossref": "parent"},
            "parent": {"cite_key": "parent", "entry_type": "book", "year": "1995", "file": "old-book.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 1
        assert entries["child"]["file"] == "old-book.pdf"

    def test_missing_parent_no_crash(self):
        entries = {
            "child": {"cite_key": "child", "entry_type": "incollection", "crossref": "nonexistent"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 0

    def test_parent_without_file(self):
        entries = {
            "child": {"cite_key": "child", "entry_type": "incollection", "crossref": "parent"},
            "parent": {"cite_key": "parent", "entry_type": "collection", "year": "1990", "file": ""},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 0

    def test_no_crossref_field(self):
        entries = {
            "standalone": {"cite_key": "standalone", "entry_type": "article", "file": "paper.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 0

    def test_article_parent_allowed(self):
        """Non-book parent types have no year restriction."""
        entries = {
            "child": {"cite_key": "child", "entry_type": "article", "crossref": "parent"},
            "parent": {"cite_key": "parent", "entry_type": "proceedings", "year": "2020", "file": "proc.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 1

    def test_multiple_children_one_parent(self):
        entries = {
            "ch1": {"cite_key": "ch1", "entry_type": "incollection", "crossref": "parent"},
            "ch2": {"cite_key": "ch2", "entry_type": "incollection", "crossref": "parent"},
            "parent": {"cite_key": "parent", "entry_type": "collection", "year": "1990", "file": "book.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 2
        assert entries["ch1"]["file"] == "book.pdf"
        assert entries["ch2"]["file"] == "book.pdf"

    def test_whitespace_crossref_key(self):
        entries = {
            "child": {"cite_key": "child", "entry_type": "incollection", "crossref": "  parent  "},
            "parent": {"cite_key": "parent", "entry_type": "collection", "year": "1990", "file": "book.pdf"},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 1

    def test_empty_crossref_field(self):
        entries = {
            "child": {"cite_key": "child", "entry_type": "incollection", "crossref": ""},
        }
        count = resolve_crossref_pdfs(entries)
        assert count == 0
