"""Legacy book entry point delegates to shared acquisition; it never attaches."""

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest

_SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "download-missing-pdfs.py"
_spec = importlib.util.spec_from_file_location("download_missing_pdfs", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)
pf = _mod.paper_fetch


def test_multiline_title_and_editor_are_preserved(tmp_bib):
    path = tmp_bib("""@book{Example1960,
 title = {An Example Book:
   Collected Essays},
 editor = {Smith, Alice},
 date = {1960},
 isbn = {9780262033848},
}
""")
    original = path.read_bytes()
    book = _mod.parse_bib_books(path)[0]
    assert book["title"] == "An Example Book:\n   Collected Essays"
    assert book["author"] == "Smith, Alice"
    assert book["edition"] == ""  # no implicit first-edition claim
    assert path.read_bytes() == original


def test_non_pdf_attachment_does_not_hide_a_missing_pdf(tmp_path):
    html = tmp_path / "book.html"
    html.write_text("book")
    book = {"key": "Example1960", "file": str(html)}
    assert _mod.books_missing_pdf([book]) == [book]
    pdf = tmp_path / "book.pdf"
    pdf.write_bytes(b"%PDF-fixture")
    assert _mod.books_missing_pdf([{**book, "file": str(pdf)}]) == []
    missing = {**book, "file": str(tmp_path / "missing.pdf")}
    assert _mod.books_missing_pdf([missing], include_broken=True) == [missing]


@pytest.mark.parametrize("arguments", [[], ["--resume"], ["--update-bib"], ["--key", "fixture-private"]])
def test_legacy_mutating_modes_stop_before_files_services_or_credentials(monkeypatch, capsys, arguments):
    def forbidden(*args, **kwargs):
        raise AssertionError("No service, credential or bibliography access permitted")
    monkeypatch.setattr(pf, "Http", forbidden)
    monkeypatch.setattr(pf, "annas_secret_key", forbidden)
    monkeypatch.setattr(_mod, "parse_bib_books", forbidden)
    assert _mod.main(arguments) == 5
    result = json.loads(capsys.readouterr().out)
    assert result["status"] == "needs-review"
    assert "fixture-private" not in result["message"]
    assert "Zotra/Ebib" in result["message"]


def test_dry_run_delegates_discovery_and_writes_nothing(monkeypatch, tmp_bib, capsys):
    path = tmp_bib("""@book{Example1960,
 title = {An Example Book},
 author = {Smith, Alice},
 isbn = {9780262033848},
 date = {1960},
}
""")
    original = path.read_bytes()
    calls = []
    monkeypatch.setattr(_mod, "BIB_FILE", path)
    monkeypatch.setattr(pf, "Http", lambda: object())
    monkeypatch.setattr(pf, "resolve_annas_hosts", lambda *args: ["annas-archive.gl"])
    def lookup(http, isbn, *, strict):
        calls.append((isbn, strict))
        return [{"md5": "a" * 32, "format": "pdf", "scanned": True, "size_bytes": None}]
    monkeypatch.setattr(pf, "libgen_book_results", lookup)
    monkeypatch.setattr(pf, "search_annas_books", lambda *args: [])
    monkeypatch.setattr(pf, "annas_secret_key", lambda: pytest.fail("Discovery read a key"))
    assert _mod.main(["--dry-run", "--only", "Example1960"]) == 5
    result = json.loads(capsys.readouterr().out)
    assert calls == [("9780262033848", True)]
    assert result["books"][0]["candidates"][0]["scanned"] is True
    assert "selected" not in result["books"][0]
    assert path.read_bytes() == original


def test_unknown_provider_response_is_reported_as_unknown(monkeypatch):
    monkeypatch.setattr(pf, "libgen_book_results", lambda *args, **kwargs: [])
    def unknown(*args):
        raise pf.PaperFetchError("Anna's book search returned an unrecognized page")
    monkeypatch.setattr(pf, "search_annas_books", unknown)
    book = {"key": "Example1960", "isbn": "9780262033848", "title": "Book", "author": "Smith"}
    result = _mod.discover_existing_book(object(), book, "annas-archive.gl")
    assert result["search_complete"] is False
    assert result["attempts"][1]["status"] == "unknown"
    assert result["status"] == "needs-review"


def test_review_selection_is_read_only_and_uses_current_pdf_bytes(tmp_path, monkeypatch, capsys):
    content = b"%PDF-1.7 disposable test bytes"
    pdf = tmp_path / "candidate.pdf"
    pdf.write_bytes(content)
    md5 = hashlib.md5(content).hexdigest()
    target = {"title": "An Example Book", "author": "Smith, Alice", "year": "1960",
              "edition": "first", "language": "english"}
    inventory = tmp_path / "candidates.json"
    inventory.write_text(json.dumps({"version": 1, "target": target,
                                     "candidates": [{"md5": md5, "format": "pdf"}]}))
    review = {"file": str(pdf), "sha256": hashlib.sha256(content).hexdigest(),
              "identity": {"status": "verified", "evidence": "Title and author checked."},
              "edition": {"status": "verified", "evidence": "Copyright page checked."},
              "language": {"status": "verified", "evidence": "Interior text checked."},
              "completeness": {"status": "verified", "evidence": "Full extent checked."},
              "physical_pages": {"status": "verified", "evidence": "Printed pagination checked."}}
    reviews = tmp_path / "reviews.json"
    reviews.write_text(json.dumps({"version": 1, "target": target, "candidates": {md5: review}}))
    monkeypatch.setattr(pf, "Http", lambda: pytest.fail("Selection made a network client"))
    monkeypatch.setattr(_mod, "parse_bib_books", lambda *args: pytest.fail("Selection read a bibliography"))
    assert _mod.main(["--candidates", str(inventory), "--reviews", str(reviews)]) == 0
    result = json.loads(capsys.readouterr().out)
    assert result["selected"]["file"] == str(pdf)
    assert pdf.read_bytes() == content
    assert sorted(path.name for path in tmp_path.iterdir()) == ["candidate.pdf", "candidates.json", "reviews.json"]
    pdf.write_bytes(content + b"changed")
    assert _mod.main(["--candidates", str(inventory), "--reviews", str(reviews)]) == 5
    assert json.loads(capsys.readouterr().out)["selected"] is None


def test_no_second_downloader_or_bib_writer_remains():
    source = _SCRIPT.read_text()
    for old_definition in ("def score_result", "def _parse_search_results", "def update_bib_entry", "def _download_once"):
        assert old_definition not in source
    assert "paper_fetch.select_book_candidate" in source
    assert "paper_fetch.search_annas_books" in source
