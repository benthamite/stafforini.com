"""Tests for scripts/download-missing-pdfs.py — bib parsing, scoring, shared Anna's Archive client."""

import sys
from pathlib import Path

import importlib.util
import json

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

_SCRIPT = Path(__file__).parent.parent / "scripts" / "download-missing-pdfs.py"
_spec = importlib.util.spec_from_file_location("download_missing_pdfs", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

parse_bib_books = _mod.parse_bib_books
score_result = _mod.score_result
select_best_result = _mod.select_best_result
pf = _mod.paper_fetch


class FakeHttp:
    """Serves canned responses by URL substring and records every request."""

    def __init__(self, routes):
        self.routes = routes
        self.calls = []

    def get(self, url, *, params=None, headers=None, timeout=None):
        self.calls.append((url, params or {}))
        for needle, response in self.routes:
            if needle in url:
                return response
        return pf.Response(404, b"", url)


def html(body, status=200):
    return pf.Response(status, body.encode(), "https://a.invalid/", "text/html")


@pytest.fixture
def fake_http(monkeypatch):
    def install(routes):
        http = FakeHttp(routes)
        monkeypatch.setattr(_mod, "_http", lambda: http)
        return http
    return install


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


class TestSharedClient:
    def test_article_mode_is_gone(self):
        """Papers are paper-fetch's job; this script must not grow a second SciDB client."""
        source = _SCRIPT.read_text()
        assert "--article-doi" not in source
        assert "scidb" not in source.lower()
        assert not hasattr(_mod, "download_article_by_doi")

    def test_credential_lookup_is_the_shared_one(self, monkeypatch):
        monkeypatch.setattr(pf, "annas_secret_key", lambda: "fixture-key")
        assert _mod.get_secret_key() == "fixture-key"

    @pytest.mark.parametrize("body,status", [
        ("<title>DDoS-Guard</title>", 403), ("Just a moment...", 503), ("<h1>No files found.</h1>", 500),
    ])
    def test_blocked_search_is_unknown_not_empty(self, fake_http, capsys, body, status):
        fake_http([("search", html(body, status))])
        assert _mod.search_annas_archive("9781234567890", "https://a.invalid/") is None
        assert "availability unknown" in capsys.readouterr().err.lower()

    def test_empty_search_page_is_an_empty_result(self, fake_http):
        fake_http([("search", html("<div>No files found.</div>"))])
        assert _mod.search_annas_archive("9781234567890", "https://a.invalid/") == []

    def test_host_override_must_be_annas_archive(self, monkeypatch, capsys):
        monkeypatch.setattr(sys, "argv", [str(_SCRIPT), "--dry-run", "--base-url", "https://evil.invalid/"])
        with pytest.raises(SystemExit) as exited:
            _mod.main()
        assert exited.value.code == 1
        assert "refusing" in capsys.readouterr().err.lower()


class TestDownloadLogging:
    def test_verbose_success_does_not_print_api_key_or_signed_url(self, fake_http, capsys, tmp_path):
        credential = "fixture-private-key"
        signed = "https://example.invalid/private-path?token=fixture-signed-token"
        http = fake_http([
            ("fast_download", html(json.dumps({"download_url": signed}))),
            ("private-path", pf.Response(200, b"%PDF-1.7 fixture" + b" " * 3000, signed, "application/pdf")),
        ])
        assert _mod._download_once("0" * 32, credential, "https://annas-archive.gl/", tmp_path / "a.pdf", verbose=True)
        assert (tmp_path / "a.pdf").read_bytes().startswith(b"%PDF-")
        output = capsys.readouterr().err
        assert credential not in output
        assert "key=" not in output
        assert "private-path" not in output
        assert "fixture-signed-token" not in output
        api_calls = [p for u, p in http.calls if "fast_download" in u]
        assert api_calls and api_calls[0]["key"] == credential

    @pytest.mark.parametrize("phase", ["api-error", "download-html", "not-member"])
    def test_remote_errors_do_not_echo_sensitive_urls(self, fake_http, capsys, tmp_path, phase):
        private = "https://example.invalid/private-path?key=fixture-private-key"
        if phase == "api-error":
            routes = [("fast_download", html(json.dumps({"download_url": None, "error": private}), 500))]
        elif phase == "not-member":
            routes = [("fast_download", html(json.dumps({"download_url": None, "error": "Not a member"}), 403))]
        else:
            routes = [("fast_download", html(json.dumps({"download_url": private}))),
                      ("private-path", html("<html>challenge</html>"))]
        fake_http(routes)
        assert not _mod._download_once(
            "0" * 32, "fixture-private-key", "https://annas-archive.gl/", tmp_path / "a.pdf", verbose=True
        )
        output = capsys.readouterr().err
        assert "private-path" not in output
        assert "fixture-private-key" not in output
        assert not (tmp_path / "a.pdf").exists()

    def test_quota_raises_rate_limit(self, fake_http, tmp_path):
        fake_http([("fast_download", html(json.dumps({"download_url": None, "error": "Daily quota exceeded"}), 429))])
        with pytest.raises(_mod.RateLimitError):
            _mod._download_once("0" * 32, "k", "https://annas-archive.gl/", tmp_path / "a.pdf")

    def test_key_never_leaves_annas_archive_hosts(self, fake_http, tmp_path, capsys):
        fake_http([])
        assert not _mod._download_once("0" * 32, "k", "https://evil.invalid/", tmp_path / "a.pdf")
        assert "refused" in capsys.readouterr().err.lower()
