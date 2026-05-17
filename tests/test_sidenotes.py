"""Regression checks for sidenote hover layering."""

from pathlib import Path
import re


REPO_ROOT = Path(__file__).resolve().parents[1]
SIDENOTES_CSS = REPO_ROOT / "assets" / "css" / "_sidenotes.css"


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
