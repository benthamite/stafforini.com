#!/usr/bin/env python3
"""Poll SEC EDGAR for new 13F filings by Situational Awareness LP.

On each run:
  1. Fetch the latest 13F-HR accession number from SEC EDGAR.
  2. Compare against ``data/sa-lp-13f-state.json``.
  3. If it differs, send an email via the Buttondown API and prepend
     an entry to ``static/feed/sa-lp-13f.xml``.
  4. Update the state file.

Designed to run idempotently from GitHub Actions. Exits non-zero on
unrecoverable errors so the workflow run is flagged.

Usage:
    BUTTONDOWN_API_KEY=... python3 scripts/sa-lp-13f-check.py
    python3 scripts/sa-lp-13f-check.py --dry-run    # no API call, no writes
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET
from datetime import datetime, timezone
from pathlib import Path

CIK_PAD = "0002045724"
CIK_INT = "2045724"
# SEC requires a real identifying User-Agent; a generic UA gets 403'd.
UA = "SA-LP 13F watcher (Pablo Stafforini pablo@stafforini.com)"

REPO_ROOT = Path(__file__).resolve().parents[1]
STATE_FILE = REPO_ROOT / "data" / "sa-lp-13f-state.json"
FEED_FILE = REPO_ROOT / "static" / "feed" / "sa-lp-13f.xml"
FEED_URL = "https://stafforini.com/feed/sa-lp-13f.xml"
POST_URL = "https://stafforini.com/notes/situational-awareness-lp/"
BUTTONDOWN_API = "https://api.buttondown.com/v1/emails"
ATOM_NS = "http://www.w3.org/2005/Atom"


def http_get_json(url: str) -> dict:
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as resp:
        return json.loads(resp.read())


def latest_13f() -> dict | None:
    """Return the most recent 13F-HR (or 13F-HR/A) filing, or None."""
    data = http_get_json(f"https://data.sec.gov/submissions/CIK{CIK_PAD}.json")
    recent = data["filings"]["recent"]
    rows = zip(
        recent["form"],
        recent["filingDate"],
        recent["reportDate"],
        recent["accessionNumber"],
    )
    for form, filed, period, accession in rows:
        if form.startswith("13F-HR"):
            return {
                "form": form,
                "filed": filed,
                "period": period,
                "accession": accession,
            }
    return None


def filing_url(accession: str) -> str:
    acc_nd = accession.replace("-", "")
    return (
        f"https://www.sec.gov/Archives/edgar/data/{CIK_INT}/"
        f"{acc_nd}/{accession}-index.html"
    )


def send_email(api_key: str, filing: dict) -> None:
    subject = (
        f"Situational Awareness LP filed a new {filing['form']} "
        f"(period {filing['period']})"
    )
    body = (
        f"A new {filing['form']} has been filed with the SEC by "
        f"Situational Awareness LP.\n\n"
        f"- Period: {filing['period']}\n"
        f"- Filed:  {filing['filed']}\n"
        f"- Accession: {filing['accession']}\n\n"
        f"SEC filing index:\n{filing_url(filing['accession'])}\n\n"
        f"Portfolio calculator and strategy context:\n{POST_URL}\n"
    )
    payload = json.dumps({"subject": subject, "body": body}).encode()
    req = urllib.request.Request(
        BUTTONDOWN_API,
        data=payload,
        headers={
            "Authorization": f"Token {api_key}",
            "Content-Type": "application/json",
            "User-Agent": UA,
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            resp_body = resp.read().decode()
    except urllib.error.HTTPError as err:
        raise RuntimeError(
            f"Buttondown API returned HTTP {err.code}: {err.read()!r}"
        ) from err
    print(f"Buttondown accepted the email: {resp_body[:200]}")


def load_state() -> dict:
    if STATE_FILE.exists():
        return json.loads(STATE_FILE.read_text())
    return {"last_accession": None}


def save_state(accession: str) -> None:
    STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
    state = {
        "last_accession": accession,
        "last_notified": datetime.now(timezone.utc).isoformat(timespec="seconds"),
    }
    STATE_FILE.write_text(json.dumps(state, indent=2) + "\n")


def _new_feed() -> ET.Element:
    ET.register_namespace("", ATOM_NS)
    feed = ET.Element(f"{{{ATOM_NS}}}feed")
    ET.SubElement(feed, f"{{{ATOM_NS}}}id").text = FEED_URL
    ET.SubElement(feed, f"{{{ATOM_NS}}}title").text = (
        "Situational Awareness LP — 13F filings"
    )
    ET.SubElement(feed, f"{{{ATOM_NS}}}subtitle").text = (
        "New SEC 13F-HR filings by Situational Awareness LP."
    )
    self_link = ET.SubElement(feed, f"{{{ATOM_NS}}}link")
    self_link.set("href", FEED_URL)
    self_link.set("rel", "self")
    site_link = ET.SubElement(feed, f"{{{ATOM_NS}}}link")
    site_link.set("href", POST_URL)
    ET.SubElement(feed, f"{{{ATOM_NS}}}updated").text = (
        datetime.now(timezone.utc).isoformat(timespec="seconds")
    )
    return feed


def prepend_entry(filing: dict) -> None:
    ET.register_namespace("", ATOM_NS)
    if FEED_FILE.exists():
        tree = ET.parse(FEED_FILE)
        feed = tree.getroot()
    else:
        feed = _new_feed()

    now_iso = datetime.now(timezone.utc).isoformat(timespec="seconds")
    filed_iso = f"{filing['filed']}T12:00:00+00:00"

    entry = ET.Element(f"{{{ATOM_NS}}}entry")
    ET.SubElement(entry, f"{{{ATOM_NS}}}id").text = (
        f"urn:sec:accession:{filing['accession']}"
    )
    ET.SubElement(entry, f"{{{ATOM_NS}}}title").text = (
        f"{filing['form']} — period {filing['period']}"
    )
    link = ET.SubElement(entry, f"{{{ATOM_NS}}}link")
    link.set("href", filing_url(filing["accession"]))
    ET.SubElement(entry, f"{{{ATOM_NS}}}updated").text = filed_iso
    ET.SubElement(entry, f"{{{ATOM_NS}}}published").text = filed_iso
    ET.SubElement(entry, f"{{{ATOM_NS}}}summary").text = (
        f"Situational Awareness LP filed {filing['form']} for period "
        f"{filing['period']} on {filing['filed']}."
    )

    entry_tag = f"{{{ATOM_NS}}}entry"
    insert_at = len(list(feed))
    for i, child in enumerate(feed):
        if child.tag == entry_tag:
            insert_at = i
            break
    feed.insert(insert_at, entry)

    for child in feed:
        if child.tag == f"{{{ATOM_NS}}}updated":
            child.text = now_iso
            break

    FEED_FILE.parent.mkdir(parents=True, exist_ok=True)
    tree = ET.ElementTree(feed)
    ET.indent(tree, space="  ")
    tree.write(FEED_FILE, encoding="utf-8", xml_declaration=True)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print what would happen; do not call Buttondown or write files.",
    )
    args = parser.parse_args()

    filing = latest_13f()
    if not filing:
        print("No 13F filings found in SEC submissions feed.", file=sys.stderr)
        return 1

    state = load_state()
    if filing["accession"] == state.get("last_accession"):
        print(
            f"No new filing. Latest is still {filing['accession']} "
            f"(period {filing['period']}, filed {filing['filed']})."
        )
        return 0

    print(
        f"New filing detected: {filing['form']} "
        f"period={filing['period']} filed={filing['filed']} "
        f"accession={filing['accession']}"
    )

    if args.dry_run:
        print("--dry-run: skipping Buttondown send and file writes.")
        return 0

    api_key = os.environ.get("BUTTONDOWN_API_KEY")
    if not api_key:
        print("BUTTONDOWN_API_KEY is not set.", file=sys.stderr)
        return 1

    send_email(api_key, filing)
    prepend_entry(filing)
    save_state(filing["accession"])
    print("Email sent, feed updated, state saved.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
