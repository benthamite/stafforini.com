"""Inventory helpers the nightly PDF batch uses to choose and read back books."""

import missing_pdf_inventory as inventory


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
    book = inventory.parse_bib_books(path)[0]
    assert book["title"] == "An Example Book:\n   Collected Essays"
    assert book["author"] == "Smith, Alice"
    assert book["edition"] == ""  # no implicit first-edition claim
    assert path.read_bytes() == original


def test_non_pdf_attachment_does_not_hide_a_missing_pdf(tmp_path):
    html = tmp_path / "book.html"
    html.write_text("book")
    book = {"key": "Example1960", "file": str(html)}
    assert inventory.books_missing_pdf([book]) == [book]
    pdf = tmp_path / "book.pdf"
    pdf.write_bytes(b"%PDF-fixture")
    assert inventory.books_missing_pdf([{**book, "file": str(pdf)}]) == []
    missing = {**book, "file": str(tmp_path / "missing.pdf")}
    assert inventory.books_missing_pdf([missing], include_broken=True) == [missing]


def test_exposes_the_shared_paper_fetch_library():
    assert callable(inventory.paper_fetch.select_book_candidate)
    assert callable(inventory.paper_fetch.read_book_candidates)
