"""Tests for scripts/download-missing-pdfs.py — Bib parsing and scoring."""

import sys
from pathlib import Path

import importlib.util

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

_SCRIPT = Path(__file__).parent.parent / "scripts" / "download-missing-pdfs.py"
_spec = importlib.util.spec_from_file_location("download_missing_pdfs", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

parse_bib_books = _mod.parse_bib_books
score_result = _mod.score_result
select_best_result = _mod.select_best_result


class TestParseBibBooks:
    def test_multiline_title_preserved(self, tmp_bib):
        path = tmp_bib("""
@book{Key2020,
  title = {A Title
    With Subtitle},
  author = {Smith, John},
  isbn = {9781234567890},
  year = {2020},
}
""")
        books = parse_bib_books(path)
        assert len(books) == 1
        assert books[0]["title"] == "A Title\n    With Subtitle"

    def test_editor_used_when_author_missing(self, tmp_bib):
        path = tmp_bib("""
@book{Key2020,
  title = {Collected Essays},
  editor = {Doe, Jane},
  year = {2020},
}
""")
        books = parse_bib_books(path)
        assert books[0]["author"] == "Doe, Jane"


class TestScoreResult:
    def test_accepts_author_only_match_when_title_missing(self):
        target = {"title": "", "author": "Smith, John"}
        result = {
            "format": "pdf",
            "size_bytes": 5 * 1024 * 1024,
            "title": "Different Title",
            "authors": "John Smith",
        }

        assert score_result(result, target) == (0, 5 * 1024 * 1024)
        assert select_best_result([result], target) == result
