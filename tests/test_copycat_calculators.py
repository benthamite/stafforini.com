import json
import math
import re
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
CALCULATORS = (
    ROOT / "static/images/sa-lp-calculator.html",
    ROOT / "static/images/value-aligned-research-advisors-calculator.html",
)


def _mode_data(html, mode):
    marker = f"{mode}:"
    start = html.index("[", html.index(marker))
    rows, _ = json.JSONDecoder().raw_decode(html[start:])
    return rows


@pytest.mark.parametrize("path", CALCULATORS)
def test_calculator_exposes_all_three_proxies(path):
    html = path.read_text()
    options = re.findall(
        r'<option value="([^"]+)"(?: selected)?>([^<]+)</option>', html
    )

    assert options == [
        ("equity_only", "Equity proxy"),
        ("scaled", "Scaled equity proxy (30%)"),
        ("full", "Option proxy"),
    ]
    assert "document.getElementById('mode').value = 'equity_only';" in html
    assert "mode !== 'full' && c.type === 'call'" in html
    assert "mode !== 'full' && c.type === 'put'" in html


@pytest.mark.parametrize("path", CALCULATORS)
def test_scaled_proxy_reduces_option_sizing_before_renormalizing(path):
    html = path.read_text()
    modes = {
        mode: {
            (row["ticker"], row["type"]): row
            for row in _mode_data(html, mode)
        }
        for mode in ("equity_only", "scaled", "full")
    }

    assert modes["equity_only"].keys() == modes["scaled"].keys()
    assert modes["equity_only"].keys() == modes["full"].keys()

    for key, equity in modes["equity_only"].items():
        scaled = modes["scaled"][key]
        option = modes["full"][key]
        assert scaled["reported_value"] == equity["reported_value"]
        assert option["reported_value"] == equity["reported_value"]
        assert option["target_basis"] == equity["reported_value"]

        if key[1] in ("call", "put"):
            for field in ("target_basis", "capital_basis", "cutoff_basis"):
                assert scaled[field] == pytest.approx(
                    equity[field] * 0.30, rel=1e-9
                )
            assert scaled["instrument"] == "stock"
            expected_direction = "short" if key[1] == "put" else "long"
            assert scaled["direction"] == expected_direction
        else:
            for field in ("target_basis", "capital_basis", "cutoff_basis"):
                assert scaled[field] == equity[field]

    for mode in ("equity_only", "scaled", "full"):
        rows = modes[mode].values()
        assert sum(row["weight"] for row in rows) == pytest.approx(1, abs=1e-5)

    bankroll = 100_000
    for mode in ("equity_only", "scaled"):
        rows = list(modes[mode].values())
        total_basis = sum(row["capital_basis"] for row in rows)
        target_total = sum(
            row["target_basis"] * bankroll / total_basis for row in rows
        )
        assert math.isclose(target_total, bankroll, rel_tol=1e-12)


@pytest.mark.parametrize("path", CALCULATORS)
def test_scaled_proxy_uses_share_mode_presentation(path):
    html = path.read_text()

    assert "var showOptionDetails = mode === 'full';" in html
    assert "mode === 'scaled'" in html
    assert "option rows are held at 30% of reported notional" in html
    assert "r.target_basis * scale" in html
