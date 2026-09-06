"""Offline, explicit-path checks of the paired filing identity checker."""

import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
HELPERS = [ROOT / tool / "skills/update-situational-awareness-filing/scripts/check_freshness.py"
           for tool in (".claude", ".codex")]
SPEC = importlib.util.spec_from_file_location("sa_freshness", HELPERS[0])
CHECKER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECKER)
ACCESSION = "0000935836-26-000418"


class FreshnessTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="sa-freshness-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.note = self.root / "note.org"
        self.site = self.root / "site"
        self.images = self.site / "static/images"
        self.images.mkdir(parents=True)
        self.holdings = [{"ticker": "TEST", "type": "long", "value": 100}]
        self.filings = [
            {"quarter": "Q1_2026", "quarter_end": "2026-03-31",
             "filing_date": "2026-05-18", "source_filing_date": "2026-05-18",
             "form": "13F-HR", "accession": "0002045724-26-000008", "holdings": []},
            {"quarter": "Q2_2026", "quarter_end": "2026-06-30",
             "filing_date": "2026-08-14", "source_filing_date": "2026-08-14",
             "form": "13F-HR", "accession": ACCESSION, "holdings": self.holdings},
        ]
        self.perf = ("Q1_2026          2026-05-18 to 2026-08-14 +1% +1% +1% +1%\n"
                     "Q2_2026 †        2026-08-14 to 2026-09-04 +1% +1% +1% +1%\n"
                     "Cumulative       2026-05-18 to 2026-09-04 +1% +1% +1% +1%")
        self.delay = "Window: 2026-05-18 to 2026-08-14 (63 trading days)\nQuarters modelled: 1 transitions"
        self.sensitivity = "SENSITIVITY\n0.15 +1%"
        self.write_note()
        self.write_calculator()
        self.write_charts()

    def write_note(self):
        data = ": " + json.dumps({"filings": self.filings})
        blocks = [("sa-data", data)] + [
            (name, "#+begin_example\n" + value + "\n#+end_example")
            for name, value in (("sa-perf", self.perf), ("sa-delay", self.delay),
                                ("sa-sensitivity", self.sensitivity))]
        self.note.write_text("\n\n".join("#+RESULTS: " + name + "\n" + value
                                       for name, value in blocks) + "\n", encoding="utf-8")

    def write_calculator(self, rows=None, label_date="2026-08-14", legacy=False):
        if rows is None:
            totals = {}
            for row in self.holdings:
                key = row["ticker"], row["type"]
                totals[key] = totals.get(key, 0) + row["value"]
            rows = [{"ticker": t, "type": kind, "reported_value": value}
                    for (t, kind), value in totals.items()]
        modes = ["equity_only", "full"] if legacy else ["equity_only", "scaled", "full"]
        data = ",\n  ".join(mode + ": " + json.dumps(rows) for mode in modes)
        text = ('<div class="meta">Latest disclosed portfolio: Q2 2026 13F filed '
                + label_date + ' &middot; underlying prices as of 2026-09-04</div>\n'
                '<script>var DATA = {\n  ' + data + '\n};</script>')
        (self.images / "sa-lp-calculator.html").write_text(text, encoding="utf-8")

    def write_charts(self, date="2026-08-14", comment_only=False):
        layout = {"shapes": [{"x0": date, "x1": date, "xref": "x",
                               "y0": 0, "y1": 1, "yref": "y domain", "type": "line"}]}
        text = '<script>Plotly.newPlot("owned-plot", [], ' + json.dumps(layout) + ', {});</script>'
        if comment_only:
            text = '<!-- "x0":"2026-08-14","x1":"2026-08-14" -->'
        for name in ("sa-lp-returns.html", "sa-lp-returns-ais.html"):
            (self.images / name).write_text(text, encoding="utf-8")

    def run_checker(self, *extra, helper=None):
        command = [sys.executable, "-I", "-B", str(helper or HELPERS[0]),
                   "--quarter", "Q2_2026", "--filing-date", "2026-08-14",
                   "--accession", ACCESSION, "--holding-count", str(len(self.holdings)),
                   "--reported-total", "100", "--note", str(self.note),
                   "--site-repo", str(self.site), *extra]
        return subprocess.run(command, text=True, capture_output=True, timeout=10,
                              cwd=self.root, env={"PATH": "/usr/bin:/bin"})

    def assert_rejected(self, *extra):
        result = self.run_checker(*extra)
        self.assertNotEqual(result.returncode, 0, result.stdout)
        self.assertNotIn("Traceback", result.stderr)
        return result

    def test_current_three_mode_producer_and_pair(self):
        for helper in HELPERS:
            with self.subTest(helper=helper):
                result = self.run_checker(helper=helper)
                self.assertEqual(result.returncode, 0, result.stderr)

    def test_checker_does_not_write_inputs_or_other_files(self):
        before = {str(p.relative_to(self.root)): p.read_bytes()
                  for p in self.root.rglob("*") if p.is_file()}
        self.assertEqual(self.run_checker().returncode, 0)
        self.assert_rejected("--holding-count", "999")
        after = {str(p.relative_to(self.root)): p.read_bytes()
                 for p in self.root.rglob("*") if p.is_file()}
        self.assertEqual(after, before)

    def test_exact_result_name(self):
        with self.assertRaises(ValueError):
            CHECKER.result_block("#+RESULTS: sa-data-stale\n: {}\n", "sa-data")

    def test_duplicate_results(self):
        with self.assertRaises(ValueError):
            CHECKER.result_block("#+RESULTS: sa-data\n: {}\n\n#+RESULTS: sa-data\n: {}\n", "sa-data")

    def test_unterminated_result(self):
        with self.assertRaises(ValueError):
            CHECKER.result_block("#+RESULTS: sa-perf\n#+begin_example\nwrong\n", "sa-perf")

    def test_case_insensitive_org_keywords(self):
        self.assertIn("payload", CHECKER.result_block(
            "#+results: sa-perf\n#+BEGIN_EXAMPLE\npayload\n#+END_EXAMPLE\n", "sa-perf"))

    def test_result_with_org_cache_hash(self):
        self.assertIn("{}", CHECKER.result_block("#+RESULTS[deadbeef]: sa-data\n: {}\n", "sa-data"))

    def test_empty_or_wrong_result_rejected(self):
        for block in ("", "#+begin_example\n#+end_example", "#+begin_src python\n{}\n#+end_src"):
            with self.subTest(block=block), self.assertRaises(ValueError):
                CHECKER.result_block("#+RESULTS: sa-data\n" + block + "\n", "sa-data")

    def test_fractional_value_not_rounded(self):
        self.write_calculator([{"ticker": "TEST", "type": "long", "reported_value": 100.1}])
        self.assert_rejected()

    def test_duplicate_source_rows_aggregate(self):
        self.holdings[:] = [{"ticker": "TEST", "type": "long", "value": 40},
                            {"ticker": "TEST", "type": "long", "value": 60}]
        self.write_note()
        self.write_calculator()
        result = self.run_checker()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_duplicate_calculator_rows_rejected(self):
        self.write_calculator([{"ticker": "TEST", "type": "long", "reported_value": 40},
                               {"ticker": "TEST", "type": "long", "reported_value": 60}])
        self.assert_rejected()

    def test_each_mode_is_checked(self):
        path = self.images / "sa-lp-calculator.html"
        original = path.read_text()
        for mode in ("equity_only", "scaled", "full"):
            path.write_text(original.replace(mode + ': [{"ticker": "TEST"', mode + ': [{"ticker": "STALE"'))
            with self.subTest(mode=mode):
                self.assert_rejected()

    def test_reordered_filings_cannot_make_old_target_latest(self):
        future = copy.deepcopy(self.filings[-1])
        future.update(quarter="Q3_2026", quarter_end="2026-09-30", filing_date="2026-11-13",
                      source_filing_date="2026-11-13", accession="0000935836-26-000500")
        self.filings.insert(0, future)
        self.write_note()
        self.assert_rejected()

    def test_unsorted_filings_rejected_even_when_target_is_latest(self):
        self.filings.reverse()
        self.write_note()
        self.assert_rejected()

    def test_source_date_must_match(self):
        self.filings[-1]["source_filing_date"] = "2026-08-17"
        self.write_note()
        self.assert_rejected()

    def test_model_date_must_match(self):
        self.filings[-1]["filing_date"] = "2026-08-13"
        self.write_note()
        self.assert_rejected()

    def test_amendment_dates_distinguished(self):
        self.filings[-1].update(source_filing_date="2026-08-17", form="13F-HR/A")
        self.write_note()
        result = self.run_checker("--filing-date", "2026-08-17", "--rebalance-date", "2026-08-14")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_invalid_cli_values_fail_cleanly(self):
        for args in (("--quarter", "Q5_2026"), ("--filing-date", "2026-02-30"),
                     ("--filing-date", "20260814"), ("--effective-date", "2026-08-13"),
                     ("--accession", "418"), ("--holding-count", "-1"),
                     ("--reported-total", "-100"), ("--reported-total", "1.1")):
            with self.subTest(args=args):
                self.assert_rejected(*args)

    def test_bad_input_schema_fails_cleanly(self):
        for bad in (None, [], "filings", {"filings": None}, {"filings": []}, {"filings": [None]}):
            self.write_note()
            text = self.note.read_text()
            text = text.replace(": " + json.dumps({"filings": self.filings}), ": " + json.dumps(bad))
            self.note.write_text(text)
            with self.subTest(bad=bad):
                self.assert_rejected()

    def test_invalid_holding_values(self):
        for value in (None, True, -1, 100.9, "100", float("nan"), float("inf")):
            self.holdings[0]["value"] = value
            self.write_note()
            with self.subTest(value=value):
                self.assert_rejected()

    def test_duplicate_json_key(self):
        self.note.write_text(self.note.read_text().replace('"value": 100', '"value": 999, "value": 100'))
        self.assert_rejected()

    def test_duplicate_target_accession(self):
        self.filings.append(copy.deepcopy(self.filings[-1]))
        self.write_note()
        self.assert_rejected()

    def test_malformed_quarter_end(self):
        self.filings[-1]["quarter_end"] = "2026-06-29"
        self.write_note()
        self.assert_rejected()

    def test_stale_prior_performance_boundary(self):
        self.perf = self.perf.replace("2026-05-18 to 2026-08-14", "2026-05-18 to 2026-08-13")
        self.write_note()
        self.assert_rejected()

    def test_target_is_final_active_performance_row(self):
        self.perf = self.perf.replace("Q2_2026 †", "Q2_2026  ")
        self.write_note()
        self.assert_rejected()

    def test_multiple_active_periods_rejected(self):
        self.perf = self.perf.replace("Q1_2026 ", "Q1_2026 †")
        self.write_note()
        self.assert_rejected()

    def test_same_day_zero_period_is_valid(self):
        self.perf = self.perf.replace("2026-09-04", "2026-08-14")
        self.write_note()
        result = self.run_checker()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_same_day_missing_target_is_not_a_refreshed_result(self):
        self.perf = self.perf.replace("Q2_2026 †        2026-08-14 to 2026-09-04 +1% +1% +1% +1%\n", "")
        self.perf = self.perf.replace("2026-09-04", "2026-08-14")
        self.write_note()
        self.assert_rejected()

    def test_missing_prior_quarter_cannot_pass(self):
        self.perf = self.perf.split("\n", 1)[1].replace(
            "Cumulative       2026-05-18", "Cumulative       2026-08-14")
        self.write_note()
        self.assert_rejected()

    def test_earlier_full_quarter_start_must_match_model(self):
        self.perf = self.perf.replace("2026-05-18", "2026-05-19")
        self.write_note()
        self.assert_rejected()

    def test_cumulative_date_consistency(self):
        self.perf = self.perf.replace("Cumulative       2026-05-18 to 2026-09-04", "Cumulative       2026-05-18 to 2026-08-13")
        self.write_note()
        self.assert_rejected()

    def test_historical_layered_period_is_valid(self):
        self.perf = self.perf.replace("Q1_2026          2026-05-18 to 2026-08-14",
            "Q1_2026          2026-05-18 to 2026-05-27 +1% +1% +1% +1%\n13G_2026_05_27   2026-05-27 to 2026-08-14")
        self.write_note()
        result = self.run_checker()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_active_layered_period_is_not_a_full_filing_pass(self):
        self.perf = self.perf.replace("Q2_2026 †", "Q2_2026  ").replace("Cumulative", "13G_2026_09_04 † 2026-09-04 to 2026-09-04 +1% +1% +1% +1%\nCumulative")
        self.write_note()
        self.assert_rejected()

    def test_delay_duplicate_windows_rejected(self):
        self.delay += "\nWindow: 2026-05-18 to 2026-08-13 (62 trading days)"
        self.write_note()
        self.assert_rejected()

    def test_delay_transition_count(self):
        self.delay = self.delay.replace("1 transitions", "0 transitions")
        self.write_note()
        self.assert_rejected()

    def test_delay_start_matches_first_filing(self):
        self.delay = self.delay.replace("2026-05-18", "2026-05-19")
        self.write_note()
        self.assert_rejected()

    def test_sensitivity_err_rejected(self):
        self.sensitivity = "SENSITIVITY\n0.15 err"
        self.write_note()
        self.assert_rejected()

    def test_marker_in_html_comment_is_not_a_chart(self):
        self.write_charts(comment_only=True)
        self.assert_rejected()

    def test_marker_must_be_a_vertical_line(self):
        path = self.images / "sa-lp-returns.html"
        path.write_text(path.read_text().replace('"type": "line"', '"type": "rect"'))
        self.assert_rejected()

    def test_stale_chart_rejected(self):
        self.write_charts(date="2026-08-13")
        self.assert_rejected()

    def test_commented_current_label_does_not_hide_stale_visible_label(self):
        path = self.images / "sa-lp-calculator.html"
        self.write_calculator(label_date="2026-05-18")
        path.write_text(path.read_text() + '<!-- Latest disclosed portfolio: Q2 2026 13F filed 2026-08-14 -->')
        self.assert_rejected()

    def test_duplicate_current_labels_fail(self):
        path = self.images / "sa-lp-calculator.html"
        path.write_text(path.read_text() + '<div class="meta">Latest disclosed portfolio: Q1 2026 13F filed 2026-05-18</div>')
        self.assert_rejected()

    def test_duplicate_data_object_fails(self):
        path = self.images / "sa-lp-calculator.html"
        path.write_text(path.read_text() + '<script>var DATA = {};</script>')
        self.assert_rejected()

    def test_boolean_html_class_attribute_fails_cleanly(self):
        path = self.images / "sa-lp-calculator.html"
        path.write_text(path.read_text().replace('class="meta"', "class"))
        self.assert_rejected()

    def test_filesystem_and_encoding_errors_are_clean(self):
        for mode in ("missing", "directory", "encoding"):
            if mode == "missing":
                args = ("--note", str(self.root / "missing.org"))
            elif mode == "directory":
                args = ("--note", str(self.root))
            else:
                self.note.write_bytes(b"\xff")
                args = ()
            with self.subTest(mode=mode):
                self.assert_rejected(*args)

    def test_missing_asset_is_clean(self):
        self.assert_rejected("--site-repo", str(self.root / "missing-site"))


if __name__ == "__main__":
    unittest.main()
