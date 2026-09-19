"""Exercise quote-date selection without network or optional finance packages."""

import ast
from datetime import datetime, timedelta
from pathlib import Path
from types import SimpleNamespace

import pytest


ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = (
    ROOT / "static/code/situational-awareness-lp.py",
    ROOT / "static/code/value-aligned-research-advisors.py",
)


class Index(list):
    def __ge__(self, value):
        return [item >= value for item in self]

    def __le__(self, value):
        return [item <= value for item in self]


class Series:
    def __init__(self, rows):
        self.rows = rows
        self.index = Index([date for date, _ in rows])
        self.iloc = [price for _, price in rows]
        self.empty = not rows

    def dropna(self):
        return self

    def __getitem__(self, mask):
        return Series([row for row, keep in zip(self.rows, mask) if keep])


class MultiIndex:
    def get_level_values(self, level):
        return ["Close"]


class CloseFrame:
    columns = ["SPY"]

    def __init__(self, series):
        self.series = series

    def copy(self):
        return self

    def __getitem__(self, ticker):
        assert ticker == "SPY"
        return self.series


class DownloadFrame:
    empty = False
    columns = MultiIndex()

    def __init__(self, series):
        self.close = CloseFrame(series)

    def __getitem__(self, metric):
        assert metric == "Close"
        return self.close


def model_functions(path, rows):
    """Load only reviewed quote functions, including VARA's embedded SALP copy."""
    tree = ast.parse(path.read_text())
    functions = {}
    for node in ast.walk(tree):
        if isinstance(node, ast.FunctionDef) and node.name in (
            "get_prices", "_resolve_price_date"
        ):
            functions.setdefault(node.name, []).append(node)
    for get_prices, resolve in zip(
        functions["get_prices"], functions["_resolve_price_date"]
    ):
        downloads = []

        def download(*args, **kwargs):
            downloads.append((args, kwargs))
            return DownloadFrame(Series(rows))

        namespace = {
            "datetime": datetime,
            "timedelta": timedelta,
            "yf": SimpleNamespace(download=download),
            "pd": SimpleNamespace(
                MultiIndex=MultiIndex,
                Timestamp=lambda value: value,
                to_numeric=lambda series, **kwargs: series,
            ),
        }
        exec(compile(ast.Module(body=[get_prices, resolve], type_ignores=[]),
                     str(path), "exec"), namespace)
        yield namespace, downloads


@pytest.mark.parametrize("path", SCRIPTS, ids=lambda p: p.stem)
@pytest.mark.parametrize(
    "requested,expected",
    (("2026-09-18", "2026-09-18"),
     ("2026-09-19", "2026-09-18"),
     ("2026-09-13", "2026-09-14")),
)
def test_equal_closes_preserve_selected_quote_timestamp(path, requested, expected):
    rows = [(datetime.fromisoformat(date), 600.0) for date in
            ("2026-09-10", "2026-09-14", "2026-09-18")]
    for namespace, downloads in model_functions(path, rows):
        prices, dates = namespace["get_prices"](
            ["SPY"], [requested], return_dates=True)
        assert prices == {"SPY": {requested: 600.0}}
        assert namespace["_resolve_price_date"](dates, requested) == expected
        # Resolution must use the original selection, not another download.
        assert len(downloads) == 1
        assert namespace["get_prices"](["SPY"], [requested]) == prices
