"""Assert that every path this project reads from outside the repo resolves.

The 2026-08-03 move from ``~/My Drive/repos/`` to ``~/repos/`` broke three
external dependencies, and each one failed silently rather than loudly:

- the two note-attachment Hugo mounts, which made 90 MB of archived files
  vanish from every build (fixed in 0cfde38 / ee0fc4d);
- three of the five bibliography sources, which made
  ``generate-work-pages.py`` judge 8,891 work pages stale and delete them.

Both were found only after the fact, because a missing source is not an
error to Hugo or to a "warn and continue" parser -- the build just produces
less. These tests turn that class of fault into a failing suite.

They deliberately touch the real filesystem. That makes them dependent on
this machine and on Google Drive being mounted, which is acceptable here:
deploys are only ever driven from this machine (``netlify.toml`` exits 1 to
disable Netlify's own CI), so an environment that cannot see these paths is
an environment that must not deploy.
"""

from __future__ import annotations

import re
import subprocess
from pathlib import Path

import pytest

import lib

REPO_ROOT = Path(__file__).resolve().parent.parent
MOUNT_BLOCK_RE = re.compile(r"\[\[module\.mounts\]\](.*?)(?=\[\[|\Z)", re.S)
SOURCE_RE = re.compile(r'source\s*=\s*"([^"]+)"')

# The move left this prefix behind in several files. Nothing should name it.
STALE_PREFIX = "My Drive/repos/"


def _mount_sources() -> list[tuple[str, str]]:
    """Yield (config file name, mount source) for every Hugo config."""
    found = []
    for config in sorted(REPO_ROOT.glob("hugo*.toml")):
        for block in MOUNT_BLOCK_RE.finditer(config.read_text()):
            match = SOURCE_RE.search(block.group(1))
            if match:
                found.append((config.name, match.group(1)))
    return found


def test_mount_sources_are_discoverable():
    """Guard the parser itself: a config format change must not empty these tests."""
    sources = _mount_sources()
    assert len(sources) > 10, f"expected many mounts, parsed {len(sources)}"
    assert any("notes/public" in src for _, src in sources), (
        "the note-attachment mounts are missing -- they are the ones that broke"
    )


@pytest.mark.parametrize("config_name,source", _mount_sources())
def test_mount_source_resolves(config_name, source):
    """A mount whose source is absent is not an error to Hugo; the files just go."""
    path = Path(source) if source.startswith("/") else REPO_ROOT / source
    assert path.exists(), (
        f"{config_name} mounts {source!r}, which does not exist. "
        f"Hugo will build successfully and silently omit it, and the next "
        f"deploy will remove it from production."
    )


@pytest.mark.parametrize("bib_path", lib.BIB_FILES, ids=lambda p: p.name)
def test_bibliography_source_resolves(bib_path):
    """A missing bib makes every work page sourced from it look stale."""
    assert bib_path.exists(), (
        f"{bib_path} is missing. generate-work-pages.py would treat every work "
        f"page from this source as stale and delete it."
    )


@pytest.mark.parametrize(
    "name", ["BIBLIO_NOTES_DIR", "NOTES_DIR", "ORGROAM_DB_PATH"]
)
def test_lib_path_constant_resolves(name):
    assert getattr(lib, name).exists(), f"lib.{name} does not exist"


def test_no_source_file_names_the_pre_move_repos_path():
    """Catch a stale ``~/My Drive/repos/`` reference before it ships.

    Tracked files only. Generated caches are out of scope: ``.netlify/``
    records a pre-move absolute publish path, but ``deploy.sh`` passes
    ``--dir`` explicitly, so the cached value is never consulted.
    """
    tracked = subprocess.run(
        ["git", "ls-files"], cwd=REPO_ROOT, capture_output=True, text=True, check=True
    ).stdout.split()
    offenders = []
    for name in tracked:
        path = REPO_ROOT / name
        if path.suffix not in {".py", ".sh", ".el", ".toml"} or not path.is_file():
            continue
        # This file names the stale prefix on purpose, to test for it.
        if path.name == Path(__file__).name:
            continue
        if STALE_PREFIX in path.read_text(errors="replace"):
            offenders.append(name)
    assert not offenders, (
        f"these files still name the pre-move path {STALE_PREFIX!r}: "
        + ", ".join(str(p) for p in offenders)
    )
