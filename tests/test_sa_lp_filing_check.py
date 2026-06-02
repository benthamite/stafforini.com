"""Tests for the SA LP SEC filing watcher."""

import importlib.util
from pathlib import Path

_SCRIPT = Path(__file__).parent.parent / "scripts" / "sa-lp-13f-check.py"
_spec = importlib.util.spec_from_file_location("sa_lp_filing_check", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)


def test_recent_watched_filings_includes_13f_and_confirmed_13g(monkeypatch):
    submissions = {
        "filings": {
            "recent": {
                "form": ["8-K", "13F-HR", "13F-HR/A"],
                "filingDate": ["2026-05-30", "2026-05-18", "2026-05-19"],
                "reportDate": ["", "2026-03-31", "2026-03-31"],
                "accessionNumber": [
                    "0000000000-26-000001",
                    "0002045724-26-000008",
                    "0002045724-26-000009",
                ],
            }
        }
    }
    search_results = [
        {
            "form": "SC 13G",
            "filed": "2026-05-27",
            "entity": "Nebius Group N.V.",
            "cik": "1513845",
            "accession": "0000935836-26-000303",
            "document_url": "https://www.sec.gov/Archives/edgar/data/1513845/000093583626000303/primary_doc.xml",
        },
        {
            "form": "SC 13G",
            "filed": "2026-05-28",
            "entity": "False Positive Inc.",
            "cik": "9999999",
            "accession": "0000999999-26-000001",
            "document_url": "https://www.sec.gov/Archives/edgar/data/9999999/000099999926000001/primary_doc.xml",
        },
    ]
    documents = {
        search_results[0]["document_url"]: (
            "Situational Awareness LP\n"
            "SAF AI GP LP\n"
            "Nebius Group N.V."
        ),
        search_results[1]["document_url"]: "Situational awareness is important.",
    }

    def fake_json(url):
        if "submissions" in url:
            return submissions
        raise AssertionError(url)

    monkeypatch.setattr(_mod, "http_get_json", fake_json)
    monkeypatch.setattr(_mod, "search_recent_13g_filings", lambda: search_results)
    monkeypatch.setattr(_mod, "http_get_text", lambda url: documents[url])

    filings = _mod.recent_watched_filings()

    assert [filing["accession"] for filing in filings] == [
        "0002045724-26-000008",
        "0002045724-26-000009",
        "0000935836-26-000303",
    ]
    assert filings[-1]["kind"] == "13G"
    assert filings[-1]["issuer"] == "Nebius Group N.V."


def test_load_state_migrates_legacy_last_accession(tmp_path, monkeypatch):
    state_file = tmp_path / "sa-lp-state.json"
    state_file.write_text(
        '{\n'
        '  "last_accession": "0002045724-26-000008",\n'
        '  "last_notified": "2026-05-18T00:00:00+00:00"\n'
        '}\n'
    )
    monkeypatch.setattr(_mod, "STATE_FILE", state_file)

    state = _mod.load_state()

    assert state["notified_accessions"] == ["0002045724-26-000008"]


def test_save_state_preserves_existing_accessions_and_adds_new(tmp_path, monkeypatch):
    state_file = tmp_path / "sa-lp-state.json"
    state_file.write_text(
        '{\n'
        '  "notified_accessions": ["0002045724-26-000008"]\n'
        '}\n'
    )
    monkeypatch.setattr(_mod, "STATE_FILE", state_file)

    _mod.save_state(
        [
            {"accession": "0002045724-26-000008"},
            {"accession": "0000935836-26-000303"},
        ]
    )

    state = _mod.load_state()
    assert state["notified_accessions"] == [
        "0002045724-26-000008",
        "0000935836-26-000303",
    ]


def test_legacy_last_accession_marks_older_13fs_as_notified():
    state = {
        "notified_accessions": ["0002045724-26-000008"],
        "legacy_last_accession": "0002045724-26-000008",
    }
    filings = [
        {"kind": "13F", "accession": "0000935836-25-000120"},
        {"kind": "13F", "accession": "0002045724-25-000002"},
        {"kind": "13F", "accession": "0002045724-26-000008"},
        {"kind": "13G", "accession": "0000935836-26-000303"},
    ]

    assert _mod.notified_accessions(state, filings) == {
        "0000935836-25-000120",
        "0002045724-25-000002",
        "0002045724-26-000008",
    }
