"""Regression checks for sidenote collision and hover behavior."""

from pathlib import Path
import re


REPO_ROOT = Path(__file__).resolve().parents[1]
SIDENOTES_CSS = REPO_ROOT / "assets" / "css" / "_sidenotes.css"
CONTENT_CSS = REPO_ROOT / "assets" / "css" / "_content.css"
SIDENOTES_JS = REPO_ROOT / "assets" / "js" / "sidenotes.js"


def _css_block(css: str, selector: str) -> str:
    pattern = re.compile(rf"{re.escape(selector)}\s*\{{(?P<body>.*?)\}}", re.S)
    match = pattern.search(css)
    assert match is not None, f"Missing CSS block for {selector}"
    return match.group("body")


def _z_index(css: str, selector: str) -> int:
    body = _css_block(css, selector)
    match = re.search(r"\bz-index\s*:\s*(-?\d+)\s*;", body)
    assert match is not None, f"Missing z-index for {selector}"
    return int(match.group(1))


def test_sidenote_column_paints_above_code_masks():
    """Hover masks for code blocks must not cover neighboring sidenotes."""
    css = SIDENOTES_CSS.read_text()

    assert _z_index(css, ".sidenote-column") > _z_index(css, ".sidenote-code-mask")


def test_closed_details_code_is_not_a_sidenote_obstacle():
    """Hidden code geometry must not truncate neighboring sidenotes."""
    js = SIDENOTES_JS.read_text()
    skip = js.index("if (pre.closest('details:not([open])')) return;")
    measure = js.index("var rect = pre.getBoundingClientRect();", skip)

    assert skip < measure


def test_footnote_reference_does_not_expand_body_line_box():
    """Superscript references must preserve the body-text baseline rhythm."""
    css = CONTENT_CSS.read_text()
    body = _css_block(css, 'sup[id^="fnref:"]')

    assert re.search(r"\bline-height\s*:\s*0\s*;", body)
