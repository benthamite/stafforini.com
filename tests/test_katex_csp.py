"""Keep production's font policy compatible with its KaTeX stylesheet."""

from pathlib import Path
import re
import tomllib
from urllib.parse import urlsplit


def test_production_csp_allows_katex_stylesheet_font_origin():
    root = Path(__file__).resolve().parents[1]
    config = tomllib.loads((root / "netlify.toml").read_text())
    policy = next(header["values"]["Content-Security-Policy"]
                  for header in config["headers"] if header["for"] == "/*")
    font_sources = next(part.strip().split()[1:] for part in policy.split(";")
                        if part.strip().startswith("font-src "))
    head = (root / "layouts/partials/head.html").read_text()
    css = re.search(r'href="(https://[^\"]+/katex.min.css)"', head).group(1)
    origin = urlsplit(css)
    # KaTeX distributes its fonts beside the stylesheet, in ./fonts/.
    assert f"{origin.scheme}://{origin.netloc}" in font_sources
