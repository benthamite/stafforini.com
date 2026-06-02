#!/usr/bin/env python3
"""Poll SEC EDGAR for new filings by Situational Awareness LP.

On each run:
  1. Fetch recent 13F-HR filings from SA LP's SEC submissions feed.
  2. Search recent Schedule 13G filings that mention watched SA LP names.
  3. Compare accessions against ``data/sa-lp-13f-state.json``.
  4. Notify Pablo about unseen filings.
  5. Update state so notifications are not repeated.

Designed to run idempotently from GitHub Actions. Exits non-zero on
unrecoverable errors so the workflow run is flagged.

Usage:
    TELEGRAM_BOT_TOKEN=... TELEGRAM_CHAT_ID=... python3 scripts/sa-lp-13f-check.py
    python3 scripts/sa-lp-13f-check.py --dry-run    # no notification, no writes
"""

from __future__ import annotations

import argparse
import json
import os
import smtplib
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import date, datetime, timedelta, timezone
from email.message import EmailMessage
from pathlib import Path

CIK_PAD = "0002045724"
CIK_INT = "2045724"
# SEC requires a real identifying User-Agent; a generic UA gets 403'd.
UA = "SA-LP filing watcher (Pablo Stafforini pablo@stafforini.com)"

REPO_ROOT = Path(__file__).resolve().parents[1]
STATE_FILE = REPO_ROOT / "data" / "sa-lp-13f-state.json"
POST_URL = "https://stafforini.com/notes/situational-awareness-lp/"
TELEGRAM_API = "https://api.telegram.org"
EFTS_SEARCH_URL = "https://efts.sec.gov/LATEST/search-index"
WATCHED_13G_NAMES = [
    "Situational Awareness LP",
    "Situational Awareness, LP",
    "SAF AI GP LP",
    "Situational Awareness LLC",
    "Situational Awareness Partners LP",
    "Leopold Aschenbrenner",
    "Carl Shulman",
]
SEARCH_13G_NAMES = [
    "Situational Awareness, LP",
    "Situational Awareness LLC",
    "Situational Awareness Partners LP",
]


def http_get_json(url: str) -> dict:
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as resp:
        return json.loads(resp.read())


def http_get_text(url: str) -> str:
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as resp:
        return resp.read().decode("utf-8", errors="replace")


def recent_13f_filings() -> list[dict]:
    """Return recent 13F-HR and 13F-HR/A filings from SA LP's feed."""
    data = http_get_json(f"https://data.sec.gov/submissions/CIK{CIK_PAD}.json")
    recent = data["filings"]["recent"]
    rows = zip(
        recent["form"],
        recent["filingDate"],
        recent["reportDate"],
        recent["accessionNumber"],
    )
    filings = []
    for form, filed, period, accession in rows:
        if form.startswith("13F-HR"):
            filings.append({
                "kind": "13F",
                "form": form,
                "filed": filed,
                "period": period,
                "accession": accession,
                "issuer": "Situational Awareness LP",
            })
    return filings


def latest_13f() -> dict | None:
    """Return the most recent 13F-HR (or 13F-HR/A) filing, or None."""
    filings = recent_13f_filings()
    return filings[0] if filings else None


def filing_url(accession: str) -> str:
    acc_nd = accession.replace("-", "")
    return (
        f"https://www.sec.gov/Archives/edgar/data/{CIK_INT}/"
        f"{acc_nd}/{accession}-index.html"
    )


def efts_document_url(hit: dict) -> str:
    source = hit["_source"]
    accession, filename = hit["_id"].split(":", 1)
    cik = int(source["ciks"][0])
    return (
        f"https://www.sec.gov/Archives/edgar/data/{cik}/"
        f"{accession.replace('-', '')}/{filename}"
    )


def search_recent_13g_filings() -> list[dict]:
    """Search SEC full text for recent Schedule 13G filings naming SA LP."""
    end = date.today()
    start = end - timedelta(days=14)
    seen: set[str] = set()
    filings: list[dict] = []

    for form in ("SCHEDULE 13G", "SCHEDULE 13G/A"):
        for name in SEARCH_13G_NAMES:
            query = {
                "q": f'"{name}"',
                "forms": form,
                "startdt": start.isoformat(),
                "enddt": end.isoformat(),
            }
            url = f"{EFTS_SEARCH_URL}?{urllib.parse.urlencode(query)}"
            try:
                data = http_get_json(url)
            except urllib.error.HTTPError as err:
                print(
                    f"Skipping SEC 13G search query after HTTP {err.code}: "
                    f"form={form!r} name={name!r}",
                    file=sys.stderr,
                )
                continue
            for hit in data.get("hits", {}).get("hits", []):
                accession = hit["_id"].split(":", 1)[0]
                if accession in seen:
                    continue
                seen.add(accession)
                source = hit.get("_source", {})
                filings.append({
                    "kind": "13G",
                    "form": source.get("form", form),
                    "filed": source.get("file_date", ""),
                    "period": source.get("period_ending") or source.get("file_date", ""),
                    "accession": accession,
                    "issuer": first_display_name(source.get("display_names", [])),
                    "document_url": efts_document_url(hit),
                })

    return filings


def first_display_name(display_names: list[str]) -> str:
    if not display_names:
        return "unknown issuer"
    name = display_names[0].split(" (CIK ", 1)[0]
    return name.strip() or "unknown issuer"


def confirms_watched_13g(filing: dict) -> bool:
    text = http_get_text(filing["document_url"])
    return any(name in text for name in WATCHED_13G_NAMES)


def recent_watched_filings() -> list[dict]:
    filings = recent_13f_filings()
    for filing in search_recent_13g_filings():
        if confirms_watched_13g(filing):
            filing.setdefault("kind", "13G")
            filing.setdefault("issuer", filing.get("entity", "unknown issuer"))
            filing.setdefault("period", filing.get("filed", ""))
            filings.append(filing)
    return sorted(filings, key=lambda filing: (filing["filed"], filing["accession"]))


def notification_subject(filing: dict) -> str:
    return (
        f"Situational Awareness LP filed a new {filing['form']} "
        f"(period {filing['period']})"
    )


def notification_body(filing: dict) -> str:
    return (
        f"A new {filing['form']} involving Situational Awareness LP has "
        f"been filed with the SEC.\n\n"
        f"- Kind: {filing['kind']}\n"
        f"- Issuer: {filing['issuer']}\n"
        f"- Period: {filing['period']}\n"
        f"- Filed:  {filing['filed']}\n"
        f"- Accession: {filing['accession']}\n\n"
        f"SEC filing index:\n{filing_url(filing['accession'])}\n\n"
        f"Next steps:\n"
        f"1. Inspect the filing and update the Situational Awareness LP note.\n"
        f"2. Review the updated post.\n"
        f"3. Send the newsletter message manually.\n\n"
        f"Post:\n{POST_URL}\n"
    )


def send_telegram(bot_token: str, chat_id: str, filing: dict) -> None:
    payload = json.dumps(
        {
            "chat_id": chat_id,
            "text": f"{notification_subject(filing)}\n\n{notification_body(filing)}",
            "disable_web_page_preview": True,
        }
    ).encode()
    req = urllib.request.Request(
        f"{TELEGRAM_API}/bot{bot_token}/sendMessage",
        data=payload,
        headers={
            "Content-Type": "application/json",
            "User-Agent": UA,
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            resp.read()
    except urllib.error.HTTPError as err:
        raise RuntimeError(
            f"Telegram API returned HTTP {err.code}: {err.read()!r}"
        ) from err
    print("Telegram notification sent.")


def send_smtp_email(filing: dict) -> None:
    host = os.environ.get("SMTP_HOST")
    username = os.environ.get("SMTP_USERNAME")
    password = os.environ.get("SMTP_PASSWORD")
    sender = os.environ.get("NOTIFY_EMAIL_FROM") or username
    recipient = os.environ.get("NOTIFY_EMAIL_TO")
    if not all([host, username, password, sender, recipient]):
        raise RuntimeError(
            "SMTP notification requires SMTP_HOST, SMTP_USERNAME, "
            "SMTP_PASSWORD, NOTIFY_EMAIL_FROM or SMTP_USERNAME, and "
            "NOTIFY_EMAIL_TO."
        )

    port = int(os.environ.get("SMTP_PORT", "587"))
    msg = EmailMessage()
    msg["Subject"] = notification_subject(filing)
    msg["From"] = sender
    msg["To"] = recipient
    msg.set_content(notification_body(filing))

    with smtplib.SMTP(host, port, timeout=30) as smtp:
        smtp.starttls()
        smtp.login(username, password)
        smtp.send_message(msg)
    print("SMTP email notification sent.")


def send_private_notifications(filing: dict) -> None:
    sent = False

    bot_token = os.environ.get("TELEGRAM_BOT_TOKEN")
    chat_id = os.environ.get("TELEGRAM_CHAT_ID")
    if bot_token or chat_id:
        if not (bot_token and chat_id):
            raise RuntimeError(
                "Telegram notification requires both TELEGRAM_BOT_TOKEN "
                "and TELEGRAM_CHAT_ID."
            )
        send_telegram(bot_token, chat_id, filing)
        sent = True

    if os.environ.get("SMTP_HOST") or os.environ.get("NOTIFY_EMAIL_TO"):
        send_smtp_email(filing)
        sent = True

    if not sent:
        raise RuntimeError(
            "No private notification channel configured. Set "
            "TELEGRAM_BOT_TOKEN and TELEGRAM_CHAT_ID, or configure SMTP_HOST, "
            "SMTP_USERNAME, SMTP_PASSWORD, and NOTIFY_EMAIL_TO."
        )


def build_legacy_buttondown_email(filing: dict) -> tuple[str, str]:
    """Return the old public-newsletter message for manual reuse if needed."""
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
    return subject, body


def load_state() -> dict:
    if STATE_FILE.exists():
        state = json.loads(STATE_FILE.read_text())
    else:
        state = {}

    if "notified_accessions" not in state:
        notified = []
        if state.get("last_accession"):
            notified.append(state["last_accession"])
            state["legacy_last_accession"] = state["last_accession"]
        state["notified_accessions"] = notified
    return state


def notified_accessions(state: dict, filings: list[dict]) -> set[str]:
    notified = set(state.get("notified_accessions", []))
    legacy_last = state.get("legacy_last_accession")
    if legacy_last:
        for filing in filings:
            if filing.get("kind") == "13F":
                notified.add(filing["accession"])
            if filing["accession"] == legacy_last:
                break
    return notified


def save_state(filings: list[dict]) -> None:
    STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
    existing = load_state().get("notified_accessions", [])
    accessions = list(dict.fromkeys(existing + [filing["accession"] for filing in filings]))
    state = {
        "notified_accessions": accessions,
        "last_notified": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "_comment": "Auto-updated by scripts/sa-lp-13f-check.py after private notification.",
    }
    STATE_FILE.write_text(json.dumps(state, indent=2) + "\n")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print what would happen; do not send notifications or write files.",
    )
    args = parser.parse_args()

    filings = recent_watched_filings()
    if not filings:
        print("No watched filings found in SEC feeds.", file=sys.stderr)
        return 1

    state = load_state()
    notified = notified_accessions(state, filings)
    new_filings = [filing for filing in filings if filing["accession"] not in notified]
    if not new_filings:
        latest = filings[-1]
        print(
            f"No new filing. Latest watched filing is still {latest['accession']} "
            f"({latest['form']}, period {latest['period']}, filed {latest['filed']})."
        )
        return 0

    for filing in new_filings:
        print(
            f"New filing detected: {filing['form']} "
            f"period={filing['period']} filed={filing['filed']} "
            f"accession={filing['accession']}"
        )

    if args.dry_run:
        for filing in new_filings:
            print(notification_subject(filing))
            print()
            print(notification_body(filing))
        print("--dry-run: skipping private notification and file writes.")
        return 0

    for filing in new_filings:
        send_private_notifications(filing)
    save_state(filings)
    print("Private notification sent and state saved.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
