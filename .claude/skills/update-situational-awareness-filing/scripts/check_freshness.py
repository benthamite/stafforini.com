#!/usr/bin/env python3
"""Check local full-13F identity and boundaries, not refresh provenance or prices."""

from __future__ import annotations

import argparse
import calendar
from collections import defaultdict
from datetime import date
from decimal import Decimal
from html.parser import HTMLParser
import json
from pathlib import Path
import re
import sys


DEFAULT_NOTE = Path(
    "/Users/pablostafforini/My Drive/notes/public/situational-awareness-lp.org"
)
DEFAULT_SITE = Path("/Users/pablostafforini/repos/stafforini.com")
QUARTER = r"Q[1-4]_[0-9]{4}"
DATE = r"[0-9]{4}-[0-9]{2}-[0-9]{2}"
ACCESSION = r"[0-9]{10}-[0-9]{2}-[0-9]{6}"
MODES = ("equity_only", "scaled", "full")


def need(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def iso_date(value: str) -> str:
    need(isinstance(value, str) and re.fullmatch(DATE, value) is not None,
         "date must be YYYY-MM-DD")
    date.fromisoformat(value)
    return value


def quarter_end(value: str) -> str:
    need(isinstance(value, str) and re.fullmatch(QUARTER, value) is not None,
         "quarter must be Q1_YYYY through Q4_YYYY")
    year, month = int(value[3:]), int(value[1]) * 3
    return date(year, month, calendar.monthrange(year, month)[1]).isoformat()


def accession(value: str) -> str:
    need(isinstance(value, str) and re.fullmatch(ACCESSION, value) is not None,
         "accession must have the form 0000000000-00-000000")
    return value


def nonnegative_int(value: str) -> int:
    need(re.fullmatch(r"[0-9]+", value) is not None, "must be a nonnegative integer")
    return int(value)


def cli_type(validate):
    def parse(value):
        try:
            return validate(value)
        except ValueError as error:
            raise argparse.ArgumentTypeError(str(error)) from error
    return parse


def result_block(text: str, name: str) -> str:
    # Match a complete name, including Org's optional cached-result hash.
    headers = list(re.finditer(
        rf"(?im)^#\+RESULTS(?:\[[^]\r\n]+\])?:[ \t]+{re.escape(name)}[ \t]*$",
        text))
    need(len(headers) == 1, f"expected exactly one #+RESULTS: {name}; found {len(headers)}")
    lines = text[headers[0].end():].splitlines()
    while lines and not lines[0].strip():
        lines.pop(0)
    need(bool(lines), f"empty result for {name}")
    first = lines[0]
    opening = re.fullmatch(r"#\+begin_(example|export)(?:[ \t]+[^\r\n]+)?[ \t]*",
                           first, re.IGNORECASE)
    if opening:
        end = "#+end_" + opening[1].lower()
        for i, line in enumerate(lines[1:], 1):
            if line.lower().rstrip() == end:
                payload = "\n".join(lines[1:i])
                need(bool(payload.strip()), f"empty result for {name}")
                return payload
            need(not re.match(r"#\+(?:begin_|end_|RESULTS)", line, re.IGNORECASE),
                 f"malformed result for {name}")
        raise ValueError(f"unterminated result for {name}")
    if first.startswith(":"):
        captured = []
        for line in lines:
            if not line.startswith(":"):
                break
            need(line == ":" or line.startswith(": "), f"malformed verbatim result for {name}")
            captured.append(line[2:] if line.startswith(": ") else "")
        payload = "\n".join(captured)
        need(bool(payload.strip()), f"empty result for {name}")
        return payload
    # Current producers use verbatim JSON and example-wrapped printed results.
    raise ValueError(f"unsupported result format for {name}; expected verbatim or example/export")


def unique_object(pairs):
    obj = {}
    for key, value in pairs:
        need(key not in obj, f"duplicate JSON key: {key}")
        obj[key] = value
    return obj


def no_constant(value):
    raise ValueError(f"non-finite JSON number: {value}")


DECODER = json.JSONDecoder(parse_float=Decimal, object_pairs_hook=unique_object,
                           parse_constant=no_constant)


def parse_data(block: str) -> dict:
    data = DECODER.decode(block)
    need(isinstance(data, dict), "sa-data must be a JSON object")
    return data


def holdings_signature(rows: list[dict], value_key: str, *, aggregate=True) -> dict:
    need(isinstance(rows, list), "holdings must be a list")
    totals = defaultdict(int)
    for row in rows:
        need(isinstance(row, dict), "each holding must be an object")
        ticker, kind, value = row.get("ticker"), row.get("type"), row.get(value_key)
        need(isinstance(ticker, str) and bool(ticker.strip()) and ticker == ticker.strip(),
             "holding ticker must be a nonempty trimmed string")
        need(kind in ("long", "call", "put"), "unsupported holding type")
        # Full 13F values are integer dollars. JSON 100.0 is equivalent to 100;
        # fractions, booleans, strings and non-finite numbers cannot be rounded away.
        need(type(value) is int or isinstance(value, Decimal), "holding value must be numeric")
        need(value >= 0 and value == int(value), "holding value must be a nonnegative integer")
        key = (ticker, kind)
        need(aggregate or key not in totals, "duplicate calculator ticker/type row")
        totals[key] += int(value)
    return dict(totals)


def target_filing(data: dict, args) -> tuple[list, dict]:
    filings = data.get("filings")
    need(isinstance(filings, list) and bool(filings), "sa-data filings must be a nonempty list")
    quarters, accessions, ends = set(), set(), []
    for filing in filings:
        need(isinstance(filing, dict), "each filing must be an object")
        q, acc = filing.get("quarter"), accession(filing.get("accession"))
        qe = quarter_end(q)
        need(filing.get("quarter_end") == qe, "quarter_end must match its report quarter")
        need(q not in quarters and acc not in accessions, "duplicate quarter or accession in sa-data")
        quarters.add(q)
        accessions.add(acc)
        ends.append(qe)
        need(filing.get("form") in ("13F-HR", "13F-HR/A"), "checker supports full 13F records only")
        model_date = iso_date(filing.get("filing_date"))
        source_date = iso_date(filing.get("source_filing_date"))
        need(qe <= model_date <= source_date, "inconsistent report/model/source filing dates")
        holdings_signature(filing.get("holdings"), "value")
    # This is also the producer's ordering contract; accepting arbitrary order can
    # leave a stale active calculator even if a target happens to be last.
    need(ends == sorted(ends), "sa-data filings must be ordered by report quarter")
    targets = [f for f in filings if f["accession"] == args.accession]
    need(len(targets) == 1, "sa-data must contain exactly one target accession")
    target = targets[0]
    need(target["quarter"] == args.quarter, "target accession has the wrong quarter")
    need(target["quarter_end"] == max(ends), "target is not the latest report quarter in sa-data")
    need(target["source_filing_date"] == args.filing_date, "target official source filing date differs")
    need(target["filing_date"] == args.rebalance_date, "target stored model rebalance date differs")
    need(len(target["holdings"]) == args.holding_count, "target holding count differs")
    need(sum(holdings_signature(target["holdings"], "value").values()) == args.reported_total,
         "target reported total differs")
    return filings, target


def check_performance(block: str, quarter: str, effective: str, filings: list) -> None:
    rows = []
    cumulative = []
    row_pattern = rf"(\S+)(?:[ \t]+(†))?[ \t]+({DATE})[ \t]+to[ \t]+({DATE})[ \t]+(.+)"
    for line in block.splitlines():
        if not re.match(r"(?:Q[0-9]_\S+|13[GD]_\S+|Cumulative)\s", line):
            continue
        match = re.fullmatch(row_pattern, line.strip())
        need(match is not None, "malformed sa-perf period row")
        label, active, start, end, values = match.groups()
        need(label == "Cumulative" or re.fullmatch(
            rf"(?:{QUARTER}|13[GD]_[0-9]{{4}}_[0-9]{{2}}_[0-9]{{2}})", label) is not None,
            "unsupported sa-perf period label")
        iso_date(start)
        iso_date(end)
        need(start <= end, "sa-perf period has reversed dates")
        metrics = values.split()
        need(len(metrics) == 4 and all(re.fullmatch(r"(?:[+-]?[0-9]+(?:\.[0-9]+)?%|N/A)", v)
                                     for v in metrics), "malformed sa-perf return cells")
        row = (label, bool(active), start, end)
        (cumulative if label == "Cumulative" else rows).append(row)
    need(bool(rows) and len(cumulative) == 1, "sa-perf needs periods and one cumulative row")
    need(len({r[0] for r in rows}) == len(rows), "duplicate sa-perf period")
    need(sum(r[1] for r in rows) == 1 and rows[-1][1], "sa-perf must have one final active period")
    need(rows[-1][0] == quarter and rows[-1][2] == effective,
         "sa-perf final active quarter must start at the target effective date")
    by_label = {r[0]: r for r in rows}
    need({r[0] for r in rows if r[0].startswith("Q")} == {f["quarter"] for f in filings},
         "sa-perf full-quarter coverage differs from sa-data")
    for filing in filings:
        expected_start = effective if filing["quarter"] == quarter else filing["filing_date"]
        need(by_label[filing["quarter"]][2] == expected_start,
             "sa-perf full-quarter start differs from its model date")
    for prior, following in zip(rows, rows[1:]):
        need(prior[3] == following[2], "sa-perf period boundaries are not contiguous")
    need(cumulative[0][2:] == (rows[0][2], rows[-1][3]),
         "sa-perf cumulative dates do not match its periods")
    # The producer prints a final dagger-marked row even when start == end.
    # A previous row ending on the new date is not, by itself, a refreshed result.


def check_delay(block: str, effective: str, filings: list) -> None:
    windows = re.findall(rf"(?m)^Window: ({DATE}) to ({DATE}) \(([0-9]+) trading days\)$", block)
    need(len(windows) == 1 and len(re.findall(r"(?m)^Window:", block)) == 1,
         "sa-delay must contain exactly one valid window")
    start, end, _ = windows[0]
    need(iso_date(start) <= iso_date(end) == effective, "sa-delay window has the wrong boundary")
    need(start == filings[0]["filing_date"], "sa-delay window has the wrong starting filing")
    transitions = re.findall(r"(?m)^Quarters modelled: ([0-9]+) transitions$", block)
    need(len(transitions) == 1 and int(transitions[0]) == len(filings) - 1,
         "sa-delay transition count does not match full filings")


class ArtifactHTML(HTMLParser):
    """Extract generated scripts and metadata text; never evaluate JavaScript."""

    def __init__(self, text):
        super().__init__(convert_charrefs=True)
        self.scripts = []
        self.meta = []
        self._script = None
        self._meta = None
        self._meta_depth = 0
        self.feed(text)
        self.close()
        need(self._script is None and self._meta is None, "unterminated artifact HTML")

    def handle_starttag(self, tag, attrs):
        if tag == "script":
            need(self._script is None, "nested script")
            self._script = []
        if self._meta is not None:
            self._meta_depth += 1
        elif tag == "div" and "meta" in (dict(attrs).get("class") or "").split():
            self._meta, self._meta_depth = [], 1

    def handle_endtag(self, tag):
        if tag == "script" and self._script is not None:
            self.scripts.append("".join(self._script))
            self._script = None
        if self._meta is not None:
            self._meta_depth -= 1
            if not self._meta_depth:
                self.meta.append(" ".join("".join(self._meta).split()))
                self._meta = None

    def handle_data(self, data):
        if self._script is not None:
            self._script.append(data)
        elif self._meta is not None:
            self._meta.append(data)


def script_match(artifact: ArtifactHTML, pattern: str, label: str):
    matches = [(body, match) for body in artifact.scripts
               for match in re.finditer(pattern, body)]
    need(len(matches) == 1, f"expected exactly one generated {label}")
    return matches[0]


def json_argument(body: str, offset: int):
    while offset < len(body) and body[offset].isspace():
        offset += 1
    value, end = DECODER.raw_decode(body, offset)
    return value, end


def parse_calculator_rows(text: str) -> dict:
    artifact = ArtifactHTML(text)
    body, match = script_match(artifact, r"\bvar\s+DATA\s*=\s*\{", "calculator DATA object")
    offset, modes = match.end(), {}
    # The generator deliberately writes a JS object with JSON-valued arrays,
    # not a JSON object and not arbitrary executable expressions.
    for i, mode in enumerate(MODES):
        key = re.match(r"\s*" + mode + r"\s*:\s*", body[offset:])
        need(key is not None, f"calculator is missing its {mode} array")
        rows, offset = json_argument(body, offset + key.end())
        need(isinstance(rows, list), f"calculator {mode} must be a list")
        modes[mode] = rows
        delimiter = re.match(r"\s*" + ("," if i < len(MODES) - 1 else r"}\s*;"), body[offset:])
        need(delimiter is not None, "malformed calculator DATA object")
        offset += delimiter.end()
    return modes


def check_calculator(text: str, target: dict, rebalance: str) -> None:
    artifact = ArtifactHTML(text)
    labels = [part.split(" · ", 1)[0] for part in artifact.meta
              if part.startswith("Latest disclosed portfolio:")]
    expected = f"Latest disclosed portfolio: {target['quarter'].replace('_', ' ')} 13F filed {rebalance}"
    need(labels == [expected], "calculator current-portfolio metadata differs or is ambiguous")
    expected_holdings = holdings_signature(target["holdings"], "value")
    for mode, rows in parse_calculator_rows(text).items():
        need(holdings_signature(rows, "reported_value", aggregate=False) == expected_holdings,
             f"calculator {mode} holdings differ from the target's aggregate holdings")


def check_chart(text: str, rebalance: str) -> None:
    artifact = ArtifactHTML(text)
    body, match = script_match(artifact, r"\bPlotly\.newPlot\s*\(", "Plotly.newPlot call")
    offset = match.end()
    values = []
    for _ in range(3):
        value, offset = json_argument(body, offset)
        values.append(value)
        comma = re.match(r"\s*,", body[offset:])
        need(comma is not None, "unsupported generated Plotly argument format")
        offset += comma.end()
    plot_id, traces, layout = values
    need(isinstance(plot_id, str) and isinstance(traces, list) and isinstance(layout, dict),
         "malformed Plotly plot data/layout")
    shapes = layout.get("shapes")
    need(isinstance(shapes, list), "chart has no layout shapes")
    need(any(isinstance(s, dict) and s.get("type") == "line"
             and s.get("xref") == "x" and s.get("yref") == "y domain"
             and s.get("x0") == rebalance and s.get("x1") == rebalance
             and s.get("y0") == 0 and s.get("y1") == 1 for s in shapes),
         "chart has no target model-date vertical rebalance marker")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--quarter", required=True, type=cli_type(lambda q: (quarter_end(q), q)[1]))
    parser.add_argument("--filing-date", required=True, type=cli_type(iso_date),
                        help="Official source filing date (not a substituted model date)")
    parser.add_argument("--rebalance-date", type=cli_type(iso_date),
                        help="Stored model/label date; defaults to filing date; not amendment validation")
    parser.add_argument("--effective-date", type=cli_type(iso_date),
                        help="Performance/delay boundary; defaults to rebalance date")
    parser.add_argument("--accession", required=True, type=cli_type(accession))
    parser.add_argument("--holding-count", required=True, type=cli_type(nonnegative_int))
    parser.add_argument("--reported-total", required=True, type=cli_type(nonnegative_int))
    parser.add_argument("--note", type=Path, default=DEFAULT_NOTE)
    parser.add_argument("--site-repo", type=Path, default=DEFAULT_SITE)
    args = parser.parse_args()
    args.rebalance_date = args.rebalance_date or args.filing_date
    effective = args.effective_date or args.rebalance_date
    if not quarter_end(args.quarter) <= args.rebalance_date <= args.filing_date:
        parser.error("report, rebalance and official source dates are inconsistent")
    if effective < args.rebalance_date:
        parser.error("effective date cannot precede model rebalance date")
    successes, failures = [], []

    def check(label, fn):
        try:
            fn()
            successes.append(label)
        except (OSError, UnicodeError, ValueError, RecursionError) as error:
            failures.append(f"{label}: {error}")

    try:
        note = args.note.read_text(encoding="utf-8")
        results = {name: result_block(note, name) for name in
                   ("sa-data", "sa-perf", "sa-delay", "sa-sensitivity")}
        filings, target = target_filing(parse_data(results["sa-data"]), args)
    except (OSError, UnicodeError, ValueError, RecursionError) as error:
        print(f"FAIL: cannot establish target filing: {error}", file=sys.stderr)
        return 1

    successes.append("sa-data target identity, dates, full-filing order, count and integer-dollar total")
    check("sa-perf final active period and contiguous boundaries",
          lambda: check_performance(results["sa-perf"], args.quarter, effective, filings))
    check("sa-delay full-filing window and transition count",
          lambda: check_delay(results["sa-delay"], effective, filings))
    check("sa-sensitivity nonempty result without literal err cells",
          lambda: need(re.search(r"\berr\b", results["sa-sensitivity"], re.IGNORECASE) is None,
                       "result contains err"))
    for label, filename in (("returns chart", "sa-lp-returns.html"),
                            ("AIS chart", "sa-lp-returns-ais.html")):
        path = args.site_repo / "static/images" / filename
        check(f"{label} model-date marker",
              lambda path=path: check_chart(path.read_text(encoding="utf-8"), args.rebalance_date))
    calculator = args.site_repo / "static/images/sa-lp-calculator.html"
    check("calculator current label and all three modes' aggregate holdings",
          lambda: check_calculator(calculator.read_text(encoding="utf-8"), target, args.rebalance_date))
    for item in successes:
        print(f"PASS: {item}")
    for item in failures:
        print(f"FAIL: {item}", file=sys.stderr)
    print("LIMIT: local identity/boundary checks only; no SEC completeness, model/amendment "
          "correctness, refresh provenance, sensitivity-rerun, price, browser or publication proof.")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
