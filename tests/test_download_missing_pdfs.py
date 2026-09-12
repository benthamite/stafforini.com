"""Tests for scripts/download-missing-pdfs.py — Bib parsing and scoring."""

import sys
from pathlib import Path

import importlib.util
import io
import json
import urllib.error

import pytest

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


class TestArticleLookup:
    def test_cli_reports_failed_http_lookup_without_claiming_absence(
        self, monkeypatch, capsys, tmp_path
    ):
        def denied(_url):
            raise urllib.error.HTTPError(
                "https://example.invalid/private", 403, "Forbidden", None, None
            )

        monkeypatch.setattr(_mod, "_fetch_url", denied)
        monkeypatch.setattr(sys, "argv", [
            str(_SCRIPT), "--article-doi", "10.5840/jphil202011725",
            "--base-url", "https://example.invalid/", "--key", "fixture-key",
            "--out", str(tmp_path),
        ])
        with pytest.raises(SystemExit) as exited:
            _mod.main()
        assert exited.value.code == 1
        output = capsys.readouterr()
        assert "HTTP 403" in output.err
        assert "availability unknown" in output.err.lower()
        assert "No Anna's Archive record" not in output.err

    @pytest.mark.parametrize("body", ["<html>ddos-guard</html>", "Just a moment", "captcha"])
    def test_challenge_is_not_an_empty_lookup(self, monkeypatch, capsys, tmp_path, body):
        monkeypatch.setattr(_mod, "_fetch_url", lambda _: body)
        assert _mod.download_article_by_doi(
            "10.1/example", "fixture-key", "https://example.invalid/", tmp_path
        ) is None
        output = capsys.readouterr().err
        assert "availability unknown" in output.lower()
        assert "No Anna's Archive record" not in output

    def test_successful_link_free_response_is_distinct(self, monkeypatch, capsys, tmp_path):
        monkeypatch.setattr(_mod, "_fetch_url", lambda _: (
            '<div role="main"><div class="mt-4 px-2">'
            '<span class="font-bold">No files found.</span>'
            ' Try fewer or different search terms and filters.</div></div>'
        ))
        assert _mod.download_article_by_doi(
            "10.1/example", "fixture-key", "https://example.invalid/", tmp_path
        ) is None
        output = capsys.readouterr().err
        assert "no downloadable record links" in output.lower()
        assert "lookup failed" not in output.lower()

    def test_no_results_ignores_unrelated_recent_download_links(self, monkeypatch):
        unrelated = "a" * 32
        monkeypatch.setattr(_mod, "_fetch_url", lambda _: (
            '<div role="main"><div class="mt-4 px-2">'
            '<span class="font-bold">No files found.</span>'
            ' Try fewer or different search terms and filters.</div>'
            '<h2>Recent Downloads</h2>'
            f'<a href="/md5/{unrelated}">Unrelated book</a></div>'
        ))
        assert _mod.fetch_scidb_md5s("10.1/example", "https://example.invalid/") == []

    def test_unknown_page_cannot_select_an_unverified_md5(self, monkeypatch):
        monkeypatch.setattr(_mod, "_fetch_url", lambda _: (
            '<div role="main">doi:10.1/example'
            f'<a href="/md5/{"a" * 32}">Unrelated book</a></div>'
        ))
        with pytest.raises(_mod.ArticleLookupError, match="identity"):
            _mod.fetch_scidb_md5s("10.1/example", "https://example.invalid/")

    def test_only_doi_matching_result_card_is_selected(self, monkeypatch):
        doi = "10.1186/s12864-020-6502-7"
        wanted = "32266d50efb5c62460a064d685ad8e81"
        unrelated = "a" * 32
        monkeypatch.setattr(_mod, "_fetch_url", lambda _: (
            '<div role="main"><div class="js-aarecord-list-outer">'
            '<div class="flex pt-3 pb-3 border-b last:border-b-0 border-gray-100">'
            f'<a href="/md5/{wanted}"><img src="cover.jpg"></a>'
            '<div><div class="line-clamp-[2] overflow-hidden break-words text-[9px] text-gray-500 font-mono">'
            f'scihub/{doi}.pdf</div>'
            f'<a href="/md5/{wanted}" class="js-vim-focus custom-a">'
            'Choice of library size normalization and statistical methods</a>'
            '</div></div></div><h2>Recent Downloads</h2>'
            f'<a href="/md5/{unrelated}" class="js-vim-focus">Unrelated book</a></div>'
        ))
        assert _mod.fetch_scidb_md5s(doi, "https://example.invalid/") == [wanted]

    def test_doi_elsewhere_cannot_validate_a_result_card(self, monkeypatch):
        monkeypatch.setattr(_mod, "_fetch_url", lambda _: (
            '<div role="main">scihub/10.1/example.pdf'
            '<div class="js-aarecord-list-outer"><div class="flex border-b">'
            '<div>scihub/10.1/different.pdf</div>'
            f'<a class="js-vim-focus" href="/md5/{"a" * 32}">Different paper</a>'
            '</div></div></div>'
        ))
        with pytest.raises(_mod.ArticleLookupError, match="identity"):
            _mod.fetch_scidb_md5s("10.1/example", "https://example.invalid/")

    def test_ambiguous_record_card_is_rejected(self, monkeypatch):
        monkeypatch.setattr(_mod, "_fetch_url", lambda _: (
            '<div class="js-aarecord-list-outer"><div class="flex border-b">'
            '<div>scihub/10.1/example.pdf</div>'
            f'<a class="js-vim-focus" href="/md5/{"a" * 32}">First record</a>'
            f'<a class="js-vim-focus" href="/md5/{"b" * 32}">Second record</a>'
            '</div></div>'
        ))
        with pytest.raises(_mod.ArticleLookupError, match="Ambiguous"):
            _mod.fetch_scidb_md5s("10.1/example", "https://example.invalid/")


class TestDownloadLogging:
    def test_verbose_success_does_not_print_api_key_or_signed_url(
        self, monkeypatch, capsys, tmp_path
    ):
        credential = "fixture-private-key"
        signed = "https://example.invalid/private-path?token=fixture-signed-token"
        replies = iter([
            json.dumps({"download_url": signed}).encode(), b"%PDF-1.7 fixture",
        ])
        monkeypatch.setattr(_mod.urllib.request, "urlopen", lambda *a, **k: io.BytesIO(next(replies)))
        assert _mod._download_once("0" * 32, credential, "https://a.invalid/", tmp_path / "a.pdf", verbose=True)
        output = capsys.readouterr().err
        assert credential not in output
        assert "key=" not in output
        assert "private-path" not in output
        assert "fixture-signed-token" not in output

    @pytest.mark.parametrize("phase", ["api", "download", "api-error"])
    def test_remote_errors_do_not_echo_sensitive_urls(self, monkeypatch, capsys, tmp_path, phase):
        private = "https://example.invalid/private-path?key=fixture-private-key"
        calls = 0

        def respond(*_args, **_kwargs):
            nonlocal calls
            calls += 1
            if phase == "api-error":
                return io.BytesIO(json.dumps({"error": private}).encode())
            if phase == "download" and calls == 1:
                return io.BytesIO(json.dumps({"download_url": private}).encode())
            raise urllib.error.URLError(private)

        monkeypatch.setattr(_mod.urllib.request, "urlopen", respond)
        assert not _mod._download_once(
            "0" * 32, "fixture-private-key", "https://a.invalid/", tmp_path / "a.pdf", verbose=True
        )
        output = capsys.readouterr().err
        assert "private-path" not in output
        assert "fixture-private-key" not in output
