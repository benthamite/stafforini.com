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
    drafts = []
    deleted = []

    monkeypatch.setattr(_mod, "recent_watched_filings", lambda fund: (_ for _ in ()).throw(
        AssertionError("test alert should not poll SEC")
    ))
    monkeypatch.setattr(_mod, "save_state", lambda fund, _filings: (_ for _ in ()).throw(
        AssertionError("test alert should not write state")
    ))
    monkeypatch.setattr(_mod, "add_feed_entry", lambda fund, _filing: (_ for _ in ()).throw(
        AssertionError("test alert should not write the public feed")
    ))
    monkeypatch.setattr(_mod, "send_private_notifications", sent.append)
    monkeypatch.setattr(
        _mod, "create_newsletter_draft", lambda filing: drafts.append(filing) or "em_test"
    )
    monkeypatch.setattr(_mod, "delete_newsletter_email", deleted.append)
    monkeypatch.setattr(_mod.sys, "argv", ["sa-lp-13f-check.py", "--test-alert"])

    assert _mod.main() == 0
    assert len(sent) == 1
    assert sent[0]["kind"] == "TEST"
    assert drafts[0]["kind"] == "TEST"
    assert deleted == ["em_test"]


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


FEED_TEMPLATE = """<?xml version='1.0' encoding='utf-8'?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <id>https://stafforini.com/feed/sa-lp-13f.xml</id>
  <title>Situational Awareness LP — SEC filings</title>
  <link href="https://stafforini.com/feed/sa-lp-13f.xml" rel="self" />
  <updated>2026-04-18T00:00:00+00:00</updated>
</feed>
"""


def _filing(accession, form="SCHEDULE 13D", filed="2026-08-28", **extra):
    return {
        "kind": "13D",
        "form": form,
        "filed": filed,
        "period": filed,
        "accession": accession,
        "issuer": "see SEC filing index",
        "fund_name": SA_LP["name"],
        "post_url": SA_LP["post_url"],
        "cik_int": SA_LP["cik_int"],
        **extra,
    }


def test_add_feed_entry_prepends_newest_and_is_idempotent(tmp_path, monkeypatch):
    monkeypatch.setattr(_mod, "FEED_DIR", tmp_path)
    (tmp_path / SA_LP["feed_file"]).write_text(FEED_TEMPLATE)

    _mod.add_feed_entry(SA_LP, _filing("0000935836-26-000416", form="SCHEDULE 13G/A",
                                       filed="2026-08-14", issuer="SharonAI Holdings Inc."))
    _mod.add_feed_entry(SA_LP, _filing("0000935836-26-000468"))
    _mod.add_feed_entry(SA_LP, _filing("0000935836-26-000468"))

    import xml.etree.ElementTree as ET
    ns = {"a": _mod.ATOM_NS}
    root = ET.parse(tmp_path / SA_LP["feed_file"]).getroot()
    entries = root.findall("a:entry", ns)
    assert [e.findtext("a:id", namespaces=ns) for e in entries] == [
        "urn:sec:accession:0000935836-26-000468",
        "urn:sec:accession:0000935836-26-000416",
    ]
    assert entries[1].findtext("a:title", namespaces=ns) == (
        "SCHEDULE 13G/A (SharonAI Holdings Inc.), filed 2026-08-14"
    )
    assert entries[0].find("a:link", ns).get("href").endswith(
        "/000093583626000468/0000935836-26-000468-index.html"
    )
    assert root.findtext("a:updated", namespaces=ns) != "2026-04-18T00:00:00+00:00"


def test_create_newsletter_draft_requests_draft_status(monkeypatch):
    calls = []

    def fake_request(method, url, api_key, payload=None):
        calls.append((method, url, api_key, payload))
        return b'{"id": "em_123", "status": "draft"}'

    monkeypatch.setenv("BUTTONDOWN_API_KEY", "key")
    monkeypatch.setattr(_mod, "buttondown_request", fake_request)

    assert _mod.create_newsletter_draft(_filing("0000935836-26-000468")) == "em_123"
    method, url, api_key, payload = calls[0]
    assert (method, url, api_key) == ("POST", _mod.BUTTONDOWN_EMAILS_API, "key")
    assert payload["status"] == "draft"
    assert payload["subject"] == "Situational Awareness LP filed a new SCHEDULE 13D"
    assert SA_LP["post_url"] in payload["body"]


def test_create_newsletter_draft_rejects_non_draft_response(monkeypatch):
    monkeypatch.setenv("BUTTONDOWN_API_KEY", "key")
    monkeypatch.setattr(
        _mod, "buttondown_request",
        lambda *args, **kwargs: b'{"id": "em_123", "status": "about_to_send"}',
    )

    import pytest
    with pytest.raises(RuntimeError, match="not 'draft'"):
        _mod.create_newsletter_draft(_filing("0000935836-26-000468"))


def test_create_newsletter_draft_requires_api_key(monkeypatch):
    monkeypatch.delenv("BUTTONDOWN_API_KEY", raising=False)

    import pytest
    with pytest.raises(RuntimeError, match="BUTTONDOWN_API_KEY"):
        _mod.create_newsletter_draft(_filing("0000935836-26-000468"))


def test_check_fund_drafts_notifies_and_updates_feed_before_saving_state(monkeypatch):
    events = []
    new = _filing("0000935836-26-000468")

    monkeypatch.setattr(_mod, "recent_watched_filings", lambda fund: [new])
    monkeypatch.setattr(_mod, "load_state", lambda fund: {"notified_accessions": []})
    monkeypatch.setattr(_mod, "create_newsletter_draft", lambda f: events.append("draft"))
    monkeypatch.setattr(_mod, "send_private_notifications", lambda f: events.append("notify"))
    monkeypatch.setattr(_mod, "add_feed_entry", lambda fund, f: events.append("feed"))
    monkeypatch.setattr(_mod, "save_state", lambda fund, filings: events.append("state"))

    assert _mod.check_fund(SA_LP, dry_run=False) == 0
    assert events == ["draft", "notify", "feed", "state"]


def test_affiliate_13g_links_to_its_document_not_the_fund_index():
    filing = _filing(
        "0000999999-26-000001",
        document_url="https://www.sec.gov/Archives/edgar/data/9999999/000099999926000001/doc.xml",
    )
    assert _mod.filing_link(filing) == filing["document_url"]
