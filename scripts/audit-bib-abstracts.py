#!/usr/bin/env python3
"""Find and optionally clear `abstract` fields that are not abstracts.

generate-work-pages.py copies each bib entry's `abstract` into the body of its
content/works page, so anything wrong in the field is published verbatim. Three
kinds of junk are in there now, all of it apparently from an automated
summarising pass that was never checked:

  * refusal text -- the summariser was handed an empty input and its apology was
    saved as the abstract ("Please provide the text you would like me to
    summarize", and the same in French on two Assimil entries)
  * meta-commentary about the act of summarising rather than the work
  * scraping debris -- zero-width characters and LaTeX escapes

Reports by default and changes nothing. With --apply it clears the flagged
fields, writing a .bak beside each file first. Clearing rather than rewriting is
deliberate: an absent abstract simply omits the page body, whereas a guessed one
would publish a fabrication.

    python3 scripts/audit-bib-abstracts.py
    python3 scripts/audit-bib-abstracts.py --apply
"""

from __future__ import annotations

import argparse
import re
import shutil
import sys
import unicodedata
from pathlib import Path

BIB_DIR = Path("/Users/pablostafforini/My Drive/bibliography")

# Phrases that only appear when a summariser was asked to summarise nothing.
# Matching is on a normalised copy, so LaTeX escapes and zero-width characters
# do not let a refusal slip through.
REFUSALS = [
    r"please provide the text",
    r"you would like me to summari[sz]e",
    r"veuillez ins[ée]rer le contenu",
    r"le bloc de texte que vous avez fourni est vide",
    r"le texte [àa] r[ée]sumer n['’]a pas [ée]t[ée] inclus",
    r"no text (?:was )?provided",
    r"i (?:cannot|can't) summari[sz]e",
    r"the space between the triple backt",
    r"as an ai language model",
    r"necessitates a sober and objective tone",
]

# Talking about how an abstract should be written, instead of being one.
META = [
    r"the creation of a scientific abstract",
    r"this (?:summary|abstract) (?:should|must|will)",
]

ZERO_WIDTH = {0x200b, 0x200c, 0x200d, 0xfeff, 0x00ad}
LATEX = {
    r"\textbar": "|", r"\textgreater": ">", r"\textless": "<",
    r"\textbackslash": "\\", r"\textbullet": "•", r"\textasciitilde": "~",
    r"\textasciicircum": "^", r"\&": "&", r"\%": "%",
}

ENTRY = re.compile(r"@\w+\{([^,]+),", re.M)
# BibTeX abstracts are brace-delimited and often contain nested braces, so the
# field is matched by walking braces rather than with a flat regex.
FIELD = re.compile(r"(\n\s*abstract\s*=\s*)\{", re.I)


def normalise(text: str) -> str:
    for macro, char in LATEX.items():
        text = text.replace(macro, char)
    text = "".join(c for c in text if ord(c) not in ZERO_WIDTH)
    return re.sub(r"\s+", " ", unicodedata.normalize("NFC", text)).strip().lower()


def abstract_spans(text: str):
    """Yield each complete abstract field, including its separator comma."""
    for match in FIELD.finditer(text):
        open_brace = match.end() - 1
        depth = 0
        for i in range(open_brace, len(text)):
            if text[i] == "{":
                depth += 1
            elif text[i] == "}":
                depth -= 1
                if depth == 0:
                    end = i + 1
                    separator = re.match(r"\s*,", text[end:])
                    if separator:
                        end += separator.end()
                    yield match.start(), end, text[open_brace + 1 : i]
                    break


def citekey_before(text: str, position: int) -> str:
    keys = [m.group(1) for m in ENTRY.finditer(text, 0, position)]
    return keys[-1] if keys else "?"


def classify(value: str) -> str | None:
    flat = normalise(value)
    if not flat:
        return "empty"
    for pattern in REFUSALS:
        if re.search(pattern, flat):
            return "summariser refusal"
    for pattern in META:
        if re.search(pattern, flat):
            return "meta-commentary, not an abstract"
    # Deliberately no length rule. Video entries legitimately carry a runtime and
    # rating as their whole abstract ("1h 45m | R"), and a short-abstract check
    # flags ~200 of those as junk when they are exactly what the page should show.
    return None


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--apply", action="store_true",
                        help="clear the flagged fields (writes a .bak first)")
    args = parser.parse_args()

    total = flagged = 0
    for path in sorted(BIB_DIR.glob("*.bib")):
        text = path.read_text(errors="replace")
        hits = []
        for start, end, value in abstract_spans(text):
            total += 1
            reason = classify(value)
            if reason:
                hits.append((start, end, value, reason, citekey_before(text, start)))
        if not hits:
            continue
        flagged += len(hits)
        print(f"\n{path.name}: {len(hits)} flagged")
        for _, _, value, reason, key in hits:
            excerpt = re.sub(r"\s+", " ", value).strip()[:88]
            print(f"  [{reason}] {key}\n      {excerpt}")

        if args.apply:
            shutil.copy2(path, path.with_suffix(path.suffix + ".bak"))
            # Rewrite back-to-front so earlier offsets stay valid.
            for start, end, *_ in sorted(hits, key=lambda h: -h[0]):
                text = text[:start] + text[end:]
            path.write_text(text)
            print(f"  -> cleared; backup at {path.name}.bak")

    print(f"\n{flagged} of {total} abstract fields flagged")
    if not args.apply and flagged:
        print("re-run with --apply to clear them")
    return 0


if __name__ == "__main__":
    sys.exit(main())
