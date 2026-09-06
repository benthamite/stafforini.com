#!/usr/bin/env python3
"""Extract GSC issue links; unresolved messages produce explicit rows and exit 1.

Only c.gle receives an unauthenticated, non-following GET. Direct Search Console
links are validated syntactically, not fetched or authenticated. Neither result
proves the sender, account, property, report contents, or indexing state.
"""

from __future__ import annotations

import argparse
import base64
import binascii
from email.message import Message
from html.parser import HTMLParser
import http.client
import json
import re
import subprocess
import sys
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any


GMAIL = Path.home() / "My Drive/dotfiles/claude/bin/gmail.py"
RAW_TIMEOUT = 30
LINK_TIMEOUT = 20  # urllib blocking-operation timeout, not a total batch deadline.
MAX_RAW_BYTES = 32 * 1024 * 1024
MAX_PARTS = 2000
MAX_DEPTH = 50
MAX_LINKS = 100
REDIRECT_CODES = {301, 302, 303, 307, 308}
SUCCESS_STATUSES = {"resolved", "direct"}


class ExtractionError(Exception):
    """A bounded public error code, never subprocess output or email contents."""


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def decode_body(data: str, charset: str = "utf-8") -> str:
    if not re.fullmatch(r"[A-Za-z0-9_-]*={0,2}", data):
        raise ExtractionError("invalid_body_encoding")
    try:
        padded = data + ("=" * (-len(data) % 4))
        return base64.b64decode(padded, altchars=b"-_", validate=True).decode(charset)
    except (ValueError, binascii.Error, UnicodeError, LookupError) as exc:
        raise ExtractionError("invalid_body_encoding") from exc


def iter_parts(part: dict[str, Any]):
    pending = [(part, 0)]
    count = 0
    while pending:
        current, depth = pending.pop()
        count += 1
        if count > MAX_PARTS or depth > MAX_DEPTH:
            raise ExtractionError("mime_limit_exceeded")
        if not isinstance(current, dict):
            raise ExtractionError("invalid_message_shape")
        parts = current.get("parts", [])
        headers = current.get("headers", [])
        body = current.get("body", {})
        if (
            not isinstance(parts, list)
            or not isinstance(headers, list)
            or not isinstance(body, dict)
            or not isinstance(current.get("mimeType", ""), str)
            or any(not isinstance(h, dict) or not isinstance(h.get("name"), str)
                   or not isinstance(h.get("value"), str) for h in headers)
            or ("data" in body and not isinstance(body["data"], str))
            or ("size" in body and (type(body["size"]) is not int or body["size"] < 0))
            or ("attachmentId" in body and not isinstance(body["attachmentId"], str))
        ):
            raise ExtractionError("invalid_message_shape")
        if len(parts) + count > MAX_PARTS:
            raise ExtractionError("mime_limit_exceeded")
        yield current
        pending.extend((child, depth + 1) for child in reversed(parts))


def validate_message(msg: Any, message_id: str) -> dict[str, Any]:
    if (not isinstance(msg, dict) or msg.get("id") != message_id
            or not isinstance(msg.get("threadId"), str) or not msg["threadId"]
            or not isinstance(msg.get("payload"), dict)):
        raise ExtractionError("invalid_message_shape")
    # Validate the entire structure before a matching link can cause any GET.
    for _ in iter_parts(msg["payload"]):
        pass
    return msg


def raw_message(message_id: str, account: str) -> dict[str, Any]:
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]{0,255}", message_id):
        raise ExtractionError("invalid_message_id")
    # Keep raw mail out of error output and avoid buffering child output in
    # memory. The private anonymous file closes on all paths; do not retain it.
    try:
        with tempfile.TemporaryFile(dir="/tmp") as output:
            subprocess.run(
                [sys.executable, str(GMAIL), "raw", message_id, "--account", account],
                stdout=output, stderr=subprocess.DEVNULL, check=True,
                timeout=RAW_TIMEOUT,
            )
            if output.tell() > MAX_RAW_BYTES:
                raise ExtractionError("message_too_large")
            output.seek(0)
            msg = json.loads(output.read().decode("utf-8"))
    except subprocess.TimeoutExpired as exc:
        raise ExtractionError("gmail_timeout") from exc
    except (subprocess.CalledProcessError, OSError) as exc:
        raise ExtractionError("gmail_failed") from exc
    except (ValueError, UnicodeError, RecursionError) as exc:
        raise ExtractionError("invalid_message_json") from exc
    return validate_message(msg, message_id)


def part_header(part: dict[str, Any], name: str) -> str:
    for item in part.get("headers", []):
        if item["name"].lower() == name.lower():
            return item["value"]
    return ""


def header(msg: dict[str, Any], name: str) -> str:
    return part_header(msg["payload"], name)


def bodies(msg: dict[str, Any], mime_type: str) -> list[str]:
    out = []
    for part in iter_parts(msg["payload"]):
        if part.get("mimeType", "").lower() != mime_type:
            continue
        body = part.get("body", {})
        data = body.get("data", "")
        if not data and (body.get("attachmentId") or body.get("size", 0) > 0):
            raise ExtractionError("body_requires_attachment")
        if data:
            content_type = Message()
            content_type["Content-Type"] = part_header(part, "Content-Type")
            charset = content_type.get_content_charset() or "utf-8"
            out.append(decode_body(data, charset))
    return out


def issue_label(msg: dict[str, Any]) -> str:
    text = "\n".join(bodies(msg, "text/plain"))
    match = re.search(r"issue:\s*'([^']+)'", text)
    return match.group(1) if match else (header(msg, "Subject") or "(unknown)")


def property_name(msg: dict[str, Any]) -> str:
    text = "\n".join(bodies(msg, "text/plain"))
    match = re.search(r"property,\s*([^\s.]+(?:\.[^\s.]+)+)", text)
    if match:
        return match.group(1).rstrip(".")
    match = re.search(r"site\s+(https?://[^\s]+|[^\s]+)", header(msg, "Subject"))
    return match.group(1).rstrip("/") if match else ""


class IssueAnchors(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.links: list[str] = []
        self.href: str | None = None
        self.label: list[str] = []
        self.hidden = 0

    def handle_starttag(self, tag, attrs):
        if tag in {"script", "style"}:
            self.hidden += 1
        if tag == "a":
            # A second start tag abandons an unclosed/malformed first anchor.
            hrefs = [value for key, value in attrs if key == "href"]
            self.href = hrefs[0] if len(hrefs) == 1 and hrefs[0] is not None else ""
            self.label = []

    def handle_data(self, data):
        if self.href is not None and not self.hidden:
            self.label.append(data)

    def handle_endtag(self, tag):
        if tag in {"script", "style"} and self.hidden:
            self.hidden -= 1
        if tag == "a" and self.href is not None:
            label = " ".join(self.label)
            if re.search(r"\bView\s+issue\s+details\b", label, re.IGNORECASE):
                if self.href not in self.links:
                    self.links.append(self.href)
                    if len(self.links) > MAX_LINKS:
                        raise ExtractionError("link_limit_exceeded")
            self.href = None
            self.label = []


def view_issue_links(msg: dict[str, Any]) -> list[str]:
    links: list[str] = []
    for body in bodies(msg, "text/html"):
        parser = IssueAnchors()
        parser.feed(body)
        parser.close()
        links.extend(link for link in parser.links if link not in links)
        if len(links) > MAX_LINKS:
            raise ExtractionError("link_limit_exceeded")
    return links


def safe_url_characters(url: str) -> bool:
    return (isinstance(url, str) and bool(url) and len(url) <= 8192
            and url.isascii() and "\\" not in url
            and not any(c.isspace() or ord(c) < 32 or ord(c) == 127 for c in url))


def link_kind(url: str) -> str:
    if not safe_url_characters(url):
        raise ExtractionError("unsafe_url")
    try:
        parsed = urllib.parse.urlsplit(url)
        if (parsed.scheme != "https" or parsed.port not in {None, 443}
                or parsed.username is not None or parsed.password is not None):
            raise ExtractionError("unsafe_url")
        decoded_path = urllib.parse.unquote(parsed.path, errors="strict")
        if ("\\" in decoded_path or re.search(r"(^|/)\.\.?(/|$)", decoded_path)
                or any(ord(c) < 32 or ord(c) == 127 for c in decoded_path)):
            raise ExtractionError("unsafe_url")
        if parsed.hostname == "c.gle" and parsed.path not in {"", "/"}:
            return "short"
        if (parsed.hostname == "search.google.com"
                and re.match(r"/(?:u/[0-9]+/)?search-console(?:/|$)", parsed.path)):
            return "direct"
    except ValueError as exc:
        raise ExtractionError("unsafe_url") from exc
    raise ExtractionError("unsafe_url")


def resolve_once(url: str) -> str:
    if link_kind(url) == "direct":
        return url
    opener = urllib.request.build_opener(NoRedirect)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    try:
        with opener.open(req, timeout=LINK_TIMEOUT):
            # A short link returning HTML is not a Search Console issue URL.
            raise ExtractionError("no_redirect")
    except urllib.error.HTTPError as exc:
        try:
            if exc.code not in REDIRECT_CODES:
                raise ExtractionError(f"http_{exc.code}") from exc
            locations = exc.headers.get_all("Location", []) if exc.headers else []
            if len(locations) != 1:
                raise ExtractionError("missing_or_ambiguous_location") from exc
            if not safe_url_characters(locations[0]):
                raise ExtractionError("unsafe_url") from exc
            try:
                target = urllib.parse.urljoin(url, locations[0])
            except ValueError as invalid:
                raise ExtractionError("unsafe_url") from invalid
            if link_kind(target) != "direct":
                raise ExtractionError("unsupported_redirect_chain") from exc
            return target
        finally:
            exc.close()
    except (urllib.error.URLError, OSError, http.client.HTTPException) as exc:
        raise ExtractionError("link_request_failed") from exc


def message_rows(message_id: str, account: str) -> list[dict[str, str]]:
    base = dict.fromkeys(
        ["message_id", "thread_id", "subject", "issue", "property",
         "c_gle_url", "issue_url", "status", "error"], "",
    )
    base["message_id"] = message_id
    try:
        msg = raw_message(message_id, account)
        base.update(thread_id=msg["threadId"], subject=header(msg, "Subject"),
                    issue=issue_label(msg), property=property_name(msg))
        links = view_issue_links(msg)
    except ExtractionError as exc:
        return [dict(base, status="message_error", error=str(exc))]
    if not links:
        return [dict(base, status="no_link", error="no_issue_anchor")]
    rows = []
    for link in links:
        row = dict(base, c_gle_url=link)
        try:
            kind = link_kind(link)
        except ExtractionError as exc:
            row.update(status="rejected_link", error=str(exc))
        else:
            try:
                row.update(issue_url=resolve_once(link),
                           status="direct" if kind == "direct" else "resolved")
            except ExtractionError as exc:
                row.update(status="resolution_error", error=str(exc))
        rows.append(row)
    return rows


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("message_ids", nargs="+")
    parser.add_argument("--account", default="personal", choices=["personal", "epoch"])
    parser.add_argument("--json", action="store_true", help="Emit JSON instead of TSV.")
    args = parser.parse_args()
    rows = [row for message_id in args.message_ids
            for row in message_rows(message_id, args.account)]
    if args.json:
        # ASCII escaping prevents malformed Unicode/control bytes in mail
        # headers from breaking terminal output; JSON readers recover Unicode.
        print(json.dumps(rows, indent=2, ensure_ascii=True))
    else:
        fields = ["message_id", "thread_id", "issue", "property", "issue_url",
                  "subject", "status", "error"]
        print("\t".join(fields))
        for row in rows:
            print("\t".join(re.sub(r"[\x00-\x1f\x7f-\x9f\u2028\u2029]", " ",
                                  row[field].encode("utf-8", "backslashreplace").decode("utf-8"))
                            for field in fields))
    return 1 if any(row["status"] not in SUCCESS_STATUSES for row in rows) else 0


if __name__ == "__main__":
    sys.exit(main())
