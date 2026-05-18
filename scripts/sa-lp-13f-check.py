#!/usr/bin/env python3
"""Poll SEC EDGAR for new 13F filings by Situational Awareness LP.

On each run:
  1. Fetch the latest 13F-HR accession number from SEC EDGAR.
  2. Compare against ``data/sa-lp-13f-state.json``.
  3. If it differs, send a private notification to Pablo.
  4. Update the state file so the private notification is not repeated.

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
import urllib.request
from datetime import datetime, timezone
from email.message import EmailMessage
from pathlib import Path

CIK_PAD = "0002045724"
CIK_INT = "2045724"
# SEC requires a real identifying User-Agent; a generic UA gets 403'd.
UA = "SA-LP 13F watcher (Pablo Stafforini pablo@stafforini.com)"

REPO_ROOT = Path(__file__).resolve().parents[1]
STATE_FILE = REPO_ROOT / "data" / "sa-lp-13f-state.json"
POST_URL = "https://stafforini.com/notes/situational-awareness-lp/"
TELEGRAM_API = "https://api.telegram.org"


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


def notification_subject(filing: dict) -> str:
    return (
        f"Situational Awareness LP filed a new {filing['form']} "
        f"(period {filing['period']})"
    )


def notification_body(filing: dict) -> str:
    return (
        f"A new {filing['form']} has been filed with the SEC by "
        f"Situational Awareness LP.\n\n"
        f"- Period: {filing['period']}\n"
        f"- Filed:  {filing['filed']}\n"
        f"- Accession: {filing['accession']}\n\n"
        f"SEC filing index:\n{filing_url(filing['accession'])}\n\n"
        f"Next steps:\n"
        f"1. Update the Situational Awareness LP note and calculator.\n"
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
        return json.loads(STATE_FILE.read_text())
    return {"last_accession": None}


def save_state(accession: str) -> None:
    STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
    state = {
        "last_accession": accession,
        "last_notified": datetime.now(timezone.utc).isoformat(timespec="seconds"),
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
        print(notification_subject(filing))
        print()
        print(notification_body(filing))
        print("--dry-run: skipping private notification and file writes.")
        return 0

    send_private_notifications(filing)
    save_state(filing["accession"])
    print("Private notification sent and state saved.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
