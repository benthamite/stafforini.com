"""Tests for the multi-fund SEC filing watcher."""

import importlib.util
from pathlib import Path

_SCRIPT = Path(__file__).parent.parent / "scripts" / "sa-lp-13f-check.py"
_spec = importlib.util.spec_from_file_location("sa_lp_filing_check", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

SA_LP = _mod.FUNDS[0]
VARA = _mod.FUNDS[1]


def test_fund_table_lists_sa_lp_and_vara():
    assert SA_LP["key"] == "sa-lp"
    assert SA_LP["cik_pad"] == "0002045724"
    assert SA_LP["watch_13g"] is True
    assert VARA["key"] == "vara"
    assert VARA["cik_pad"] == "0001963565"
    assert VARA["watch_13g"] is False
    assert VARA["post_url"].endswith("/value-aligned-research-advisors/")


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
            "issuer": "Nebius Group N.V.",
            "kind": "13G",
            "fund_name": SA_LP["name"],
            "post_url": SA_LP["post_url"],
            "cik_int": SA_LP["cik_int"],
            "accession": "0000935836-26-000303",
            "document_url": "https://www.sec.gov/Archives/edgar/data/1513845/000093583626000303/primary_doc.xml",
        },
        {
            "form": "SC 13G",
            "filed": "2026-05-28",
            "issuer": "False Positive Inc.",
            "kind": "13G",
            "fund_name": SA_LP["name"],
            "post_url": SA_LP["post_url"],
            "cik_int": SA_LP["cik_int"],
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
    monkeypatch.setattr(
        _mod, "search_recent_13g_filings", lambda fund: search_results
    )
    monkeypatch.setattr(_mod, "http_get_text", lambda url: documents[url])

    filings = _mod.recent_watched_filings(SA_LP)

    assert [filing["accession"] for filing in filings] == [
        "0002045724-26-000008",
        "0002045724-26-000009",
        "0000935836-26-000303",
    ]
    assert filings[-1]["kind"] == "13G"
    assert filings[-1]["issuer"] == "Nebius Group N.V."
    assert all(filing["fund_name"] == SA_LP["name"] for filing in filings)


def test_recent_watched_filings_skips_13g_search_for_vara(monkeypatch):
    submissions = {
        "filings": {
            "recent": {
                "form": ["13F-HR"],
                "filingDate": ["2026-08-17"],
                "reportDate": ["2026-06-30"],
                "accessionNumber": ["0001963565-26-000005"],
            }
        }
    }

    monkeypatch.setattr(_mod, "http_get_json", lambda url: submissions)
    monkeypatch.setattr(
        _mod,
        "search_recent_13g_filings",
        lambda fund: (_ for _ in ()).throw(
            AssertionError("VARA should not search 13G filings")
        ),
    )

    filings = _mod.recent_watched_filings(VARA)

    assert [filing["accession"] for filing in filings] == [
        "0001963565-26-000005"
    ]
    assert filings[0]["fund_name"] == VARA["name"]
    assert filings[0]["post_url"] == VARA["post_url"]


def test_load_state_migrates_legacy_last_accession(tmp_path, monkeypatch):
    monkeypatch.setattr(_mod, "DATA_DIR", tmp_path)
    (tmp_path / SA_LP["state_file"]).write_text(
        '{\n'
        '  "last_accession": "0002045724-26-000008",\n'
        '  "last_notified": "2026-05-18T00:00:00+00:00"\n'
        '}\n'
    )

    state = _mod.load_state(SA_LP)

    assert state["notified_accessions"] == ["0002045724-26-000008"]


def test_save_state_preserves_existing_accessions_and_adds_new(tmp_path, monkeypatch):
    monkeypatch.setattr(_mod, "DATA_DIR", tmp_path)
    (tmp_path / SA_LP["state_file"]).write_text(
        '{\n'
        '  "notified_accessions": ["0002045724-26-000008"]\n'
        '}\n'
    )

    _mod.save_state(
        SA_LP,
        [
            {"accession": "0002045724-26-000008"},
            {"accession": "0000935836-26-000303"},
        ],
    )

    state = _mod.load_state(SA_LP)
    assert state["notified_accessions"] == [
        "0002045724-26-000008",
        "0000935836-26-000303",
    ]


def test_state_files_are_per_fund(tmp_path, monkeypatch):
    monkeypatch.setattr(_mod, "DATA_DIR", tmp_path)

    _mod.save_state(SA_LP, [{"accession": "0002045724-26-000008"}])
    _mod.save_state(VARA, [{"accession": "0001963565-26-000005"}])

    assert _mod.load_state(SA_LP)["notified_accessions"] == [
        "0002045724-26-000008"
    ]
    assert _mod.load_state(VARA)["notified_accessions"] == [
        "0001963565-26-000005"
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


def test_test_alert_is_labeled_and_uses_post_url():
    filing = _mod.build_test_alert()

    assert filing["kind"] == "TEST"
    assert filing["form"] == "TEST ALERT"
    assert filing["url"] == SA_LP["post_url"]
    assert _mod.notification_subject(filing).startswith("TEST: ")
    assert "This is a test alert" in _mod.notification_body(filing)
    assert SA_LP["post_url"] in _mod.notification_body(filing)


def test_test_alert_mode_sends_without_polling_or_writing(monkeypatch):
    sent = []

    monkeypatch.setattr(_mod, "recent_watched_filings", lambda fund: (_ for _ in ()).throw(
        AssertionError("test alert should not poll SEC")
    ))
    monkeypatch.setattr(_mod, "save_state", lambda fund, _filings: (_ for _ in ()).throw(
        AssertionError("test alert should not write state")
    ))
    monkeypatch.setattr(_mod, "send_private_notifications", sent.append)
    monkeypatch.setattr(_mod.sys, "argv", ["sa-lp-13f-check.py", "--test-alert"])

    assert _mod.main() == 0
    assert len(sent) == 1
    assert sent[0]["kind"] == "TEST"


def test_feed_form_kind_accepts_ownership_forms_only():
    assert _mod.feed_form_kind("13F-HR") == "13F"
    assert _mod.feed_form_kind("13F-HR/A") == "13F"
    assert _mod.feed_form_kind("SCHEDULE 13D") == "13D"
    assert _mod.feed_form_kind("SCHEDULE 13D/A") == "13D"
    assert _mod.feed_form_kind("SC 13D/A") == "13D"
    assert _mod.feed_form_kind("SCHEDULE 13G/A") == "13G"
    assert _mod.feed_form_kind("SC 13G") == "13G"
    for form in ("3", "4", "4/A", "5"):
        assert _mod.feed_form_kind(form) == "Section 16"
    for form in ("N-PX", "8-K", "40-F", "424B3", "SC 13E3", "13F-NT"):
        assert _mod.feed_form_kind(form) is None


def test_recent_watched_filings_reads_13d_13g_and_section_16_from_feed(monkeypatch):
    submissions = {
        "filings": {
            "recent": {
                "form": ["SCHEDULE 13D", "N-PX", "SCHEDULE 13G/A", "4", "13F-HR"],
                "filingDate": [
                    "2026-08-28", "2026-08-28", "2026-08-14", "2026-07-02", "2026-08-14",
                ],
                "reportDate": ["", "2026-06-30", "", "2026-06-30", "2026-06-30"],
                "accessionNumber": [
                    "0000935836-26-000468",
                    "0000935836-26-000464",
                    "0000935836-26-000416",
                    "0000935836-26-000339",
                    "0000935836-26-000418",
                ],
            }
        }
    }
    search_results = [
        {
            "kind": "13G",
            "form": "SCHEDULE 13G/A",
            "filed": "2026-08-14",
            "period": "2026-06-30",
            "accession": "0000935836-26-000416",
            "issuer": "SharonAI Holdings Inc.",
            "fund_name": SA_LP["name"],
            "post_url": SA_LP["post_url"],
            "cik_int": SA_LP["cik_int"],
            "document_url": "https://www.sec.gov/unused",
        },
    ]

    monkeypatch.setattr(_mod, "http_get_json", lambda url: submissions)
    monkeypatch.setattr(
        _mod, "search_recent_13g_filings", lambda fund: search_results
    )
    monkeypatch.setattr(
        _mod,
        "http_get_text",
        lambda url: (_ for _ in ()).throw(
            AssertionError("feed filings need no document name check")
        ),
    )

    filings = _mod.recent_watched_filings(SA_LP)

    by_accession = {filing["accession"]: filing for filing in filings}
    assert list(by_accession) == [
        "0000935836-26-000339",
        "0000935836-26-000416",
        "0000935836-26-000418",
        "0000935836-26-000468",
    ]
    assert by_accession["0000935836-26-000468"]["kind"] == "13D"
    assert by_accession["0000935836-26-000468"]["period"] == "2026-08-28"
    assert by_accession["0000935836-26-000339"]["kind"] == "Section 16"
    assert by_accession["0000935836-26-000416"]["issuer"] == "SharonAI Holdings Inc."
    assert "document_url" not in by_accession["0000935836-26-000416"]
