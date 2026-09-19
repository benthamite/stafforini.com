import importlib.util
from pathlib import Path
from urllib.error import HTTPError

import pytest


SPEC = importlib.util.spec_from_file_location(
    "verify_live_quote", Path(__file__).resolve().parents[1] / "scripts/verify-live-quote.py"
)
live = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(live)


def page(text):
    return f'<main><div class="quote-body"><blockquote>{text}</blockquote><div>Author</div></div></main>'


@pytest.fixture
def built(tmp_path):
    path = tmp_path / "public/quotes/test-quote/index.html"
    path.parent.mkdir(parents=True)
    path.write_text(page("New quote &amp; attribution"))
    return tmp_path


def test_live_quote_checks_current_content_and_retries_404(built, capsys):
    responses = [HTTPError("url", 404, "Not Found", {}, None), page("Old quote"),
                 page("New quote &amp; attribution")]
    waits = []

    def fetch(url):
        assert url == "https://stafforini.com/quotes/test-quote/"
        result = responses.pop(0)
        if isinstance(result, Exception):
            raise result
        return result

    live.verify("test-quote", root=built, fetch=fetch, sleep=waits.append, attempts=3)
    assert waits == [5, 5]
    assert "Published and verified live:" in capsys.readouterr().out


@pytest.mark.parametrize("html", [page("Old quote"), "<h1>Not found</h1>",
                                page("New quote &amp; attribution").replace("Author", "Wrong author")])
def test_live_quote_rejects_wrong_content(built, html, capsys):
    with pytest.raises(ValueError, match="Publication NOT verified"):
        live.verify("test-quote", root=built, fetch=lambda _: html, attempts=1)
    assert "Published and verified" not in capsys.readouterr().out


def test_missing_expected_quote_fails_before_network(tmp_path):
    path = tmp_path / "public/quotes/test-quote/index.html"
    path.parent.mkdir(parents=True)
    path.write_text("<h1>Not found</h1>")
    with pytest.raises(ValueError, match="Built page has no quote body"):
        live.verify("test-quote", root=tmp_path, fetch=lambda _: pytest.fail("Unexpected network"))


def test_invalid_slug_fails_before_reading_files():
    with pytest.raises(ValueError, match="Invalid quote slug"):
        live.verify("../../etc/passwd")


def test_live_quote_rejects_stale_attribution_link_with_same_text(built):
    path = built / "public/quotes/test-quote/index.html"
    expected = page('Quote <a href="/works/correct/">Author</a>')
    path.write_text(expected)
    with pytest.raises(ValueError, match="Publication NOT verified"):
        live.verify("test-quote", root=built, attempts=1,
                    fetch=lambda _: expected.replace("/works/correct/", "/works/wrong/"))
