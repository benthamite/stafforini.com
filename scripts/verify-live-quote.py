#!/usr/bin/env python3
"""Confirm that a published quote serves the freshly built quote and attribution."""

import argparse
from html.parser import HTMLParser
from pathlib import Path
import re
import sys
import time
from urllib.error import URLError
from urllib.request import Request, urlopen


REPO_ROOT = Path(__file__).resolve().parents[1]
SITE = "https://stafforini.com"


class QuoteBodyParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.depth = 0
        self.parts = []
        self.targets = []

    def handle_starttag(self, tag, attrs):
        if tag == "div":
            if self.depth:
                self.depth += 1
            elif "quote-body" in dict(attrs).get("class", "").split():
                self.depth = 1
        if self.depth and tag == "img":
            self.parts.append(dict(attrs).get("alt", ""))
        if self.depth and tag in {"a", "img"}:
            attribute = "href" if tag == "a" else "src"
            self.targets.append((tag, dict(attrs).get(attribute, "")))

    def handle_endtag(self, tag):
        if tag == "div" and self.depth:
            self.depth -= 1

    def handle_data(self, data):
        if self.depth:
            self.parts.append(data)


def quote_content(html):
    parser = QuoteBodyParser()
    parser.feed(html)
    return " ".join(" ".join(parser.parts).split()), tuple(parser.targets)


def fetch_quote(url):
    request = Request(url, headers={"Cache-Control": "no-cache",
                                    "User-Agent": "stafforini-publish-quote/1.0"})
    with urlopen(request, timeout=20) as response:
        if response.status != 200:
            raise ValueError(f"HTTP {response.status}")
        if response.url.rstrip("/") != url.rstrip("/"):
            raise ValueError(f"Quote redirected to {response.url}")
        return response.read().decode("utf-8")


def verify(slug, *, root=REPO_ROOT, fetch=fetch_quote, sleep=time.sleep, attempts=6):
    if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", slug):
        raise ValueError("Invalid quote slug")
    expected = quote_content((root / "public" / "quotes" / slug / "index.html").read_text())
    if not expected[0]:
        raise ValueError("Built page has no quote body; refusing to report publication")
    url = f"{SITE}/quotes/{slug}/"
    failure = "No live verification attempt completed"
    for attempt in range(attempts):
        try:
            if quote_content(fetch(url)) == expected:
                print(f"Published and verified live: {url}", flush=True)
                return
            failure = "Live quote or attribution differs from the freshly built page"
        except (URLError, TimeoutError, ValueError) as exc:
            failure = str(exc)
        if attempt + 1 < attempts:
            print(f"Waiting for live quote ({attempt + 1}/{attempts}): {failure}", flush=True)
            sleep(5)
    raise ValueError(f"Publication NOT verified at {url}: {failure}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--slug", required=True)
    args = parser.parse_args()
    try:
        verify(args.slug)
    except (OSError, ValueError) as exc:
        print(str(exc), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
