#!/usr/bin/env python3
"""Create org-roam stub files for unlinked WordPress quote tags.

Reads unlinked tags from wp-quotes-tags-linked.json and creates minimal
org-roam files in:
  - ~/Dropbox/notes/tags/   (topics)
  - ~/Dropbox/people/tags/  (people)

Each stub has a #+title:, a level-1 heading with :note: or :person: tag,
and an :ID: property — just enough for org-roam to index them as link
targets for the backlinks-as-taxonomy system.
"""

import json
import re
import uuid
from pathlib import Path

SCRIPTS_DIR = Path(__file__).parent
INPUT_JSON = SCRIPTS_DIR / "wp-quotes-tags-linked.json"

NOTES_TAGS_DIR = Path.home() / "Library/CloudStorage/Dropbox/notes/tags"
PEOPLE_TAGS_DIR = Path.home() / "Library/CloudStorage/Dropbox/people/tags"

# Words that, when appearing as whole words in a multi-word tag,
# indicate it's NOT a person name.
NON_PERSON_WORDS = {
    "civil", "college", "crisis", "east", "final", "germany",
    "islands", "lectures", "missile", "obediencia", "punto",
    "report", "revolution", "union", "university", "war", "west",
}


def is_person(tag: str) -> bool:
    """Classify a tag as person vs. topic.

    A tag is likely a person if it has 2+ words, most are capitalized,
    and none are common non-person words.
    """
    words = tag.split()
    if len(words) < 2:
        return False

    # Reject if it starts with "The" or "Ley"
    if words[0].lower() in ("the", "ley"):
        return False

    # Reject if any word (lowercased) is in the non-person blocklist
    if any(w.lower() in NON_PERSON_WORDS for w in words):
        return False

    # Require at least 2 capitalized words (ignoring particles)
    particles = {"de", "von", "van", "el", "al", "la", "du", "le", "di", "y", "and"}
    capitalized = sum(1 for w in words if w[0].isupper() and w.lower() not in particles)
    return capitalized >= 2


def tag_to_filename(tag: str) -> str:
    """Convert a tag to a kebab-case .org filename.

    "artificial intelligence" -> "artificial-intelligence.org"
    "Arnold Schwarzenegger." -> "arnold-schwarzenegger.org"
    """
    # Lowercase
    slug = tag.lower()
    # Remove trailing punctuation
    slug = slug.rstrip(".")
    # Replace non-alphanumeric (except hyphens) with hyphens
    slug = re.sub(r"[^a-z0-9\u00e0-\u00ff-]+", "-", slug)
    # Collapse multiple hyphens and strip edges
    slug = re.sub(r"-+", "-", slug).strip("-")
    return slug + ".org"


def make_org_stub(title: str, org_tag: str) -> str:
    """Generate the content of a minimal org-roam stub file."""
    node_id = str(uuid.uuid4()).upper()
    return (
        f"#+title: {title}\n"
        f"\n"
        f"* {title} :{org_tag}:\n"
        f":PROPERTIES:\n"
        f":ID:       {node_id}\n"
        f":END:\n"
    )


def main():
    print("Loading unlinked tags...")
    data = json.loads(INPUT_JSON.read_text())
    unlinked = data["unlinked"]
    print(f"  {len(unlinked)} unlinked tags")

    # Reclassify with improved heuristic
    people = []
    topics = []
    for item in unlinked:
        tag = item["tag"]
        if is_person(tag):
            people.append(tag)
        else:
            topics.append(tag)

    print(f"  {len(people)} people, {len(topics)} topics")

    # Create directories
    NOTES_TAGS_DIR.mkdir(parents=True, exist_ok=True)
    PEOPLE_TAGS_DIR.mkdir(parents=True, exist_ok=True)
    print(f"\nDirectories:")
    print(f"  {NOTES_TAGS_DIR}")
    print(f"  {PEOPLE_TAGS_DIR}")

    # Create topic stubs
    created_topics = 0
    skipped_topics = 0
    for tag in sorted(topics):
        filename = tag_to_filename(tag)
        path = NOTES_TAGS_DIR / filename
        if path.exists():
            skipped_topics += 1
            continue
        content = make_org_stub(tag, "note")
        path.write_text(content, encoding="utf-8")
        created_topics += 1

    # Create people stubs
    created_people = 0
    skipped_people = 0
    for tag in sorted(people):
        filename = tag_to_filename(tag)
        path = PEOPLE_TAGS_DIR / filename
        if path.exists():
            skipped_people += 1
            continue
        content = make_org_stub(tag, "person")
        path.write_text(content, encoding="utf-8")
        created_people += 1

    print(f"\nCreated:")
    print(f"  {created_topics} topic stubs in {NOTES_TAGS_DIR.name}/")
    print(f"  {created_people} people stubs in {PEOPLE_TAGS_DIR.name}/")
    if skipped_topics or skipped_people:
        print(f"Skipped (already exist):")
        print(f"  {skipped_topics} topics, {skipped_people} people")
    print(f"  Total: {created_topics + created_people} files created")


if __name__ == "__main__":
    main()
