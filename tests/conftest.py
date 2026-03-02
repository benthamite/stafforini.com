"""Shared fixtures for stafforini.com test suite."""

import sys
from pathlib import Path

import pytest

# Add scripts/ to sys.path so tests can import the modules directly
SCRIPTS_DIR = Path(__file__).parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS_DIR))


@pytest.fixture
def tmp_bib(tmp_path):
    """Create a temporary .bib file from a string.

    Usage:
        def test_something(tmp_bib):
            path = tmp_bib('''
                @article{Smith2020Example,
                  author = {Smith, John},
                  title = {An Example},
                  year = {2020},
                }
            ''')
    """
    def _make(content: str) -> Path:
        p = tmp_path / "test.bib"
        p.write_text(content)
        return p
    return _make


@pytest.fixture
def tmp_md(tmp_path):
    """Create a temporary markdown file from a string."""
    counter = [0]

    def _make(content: str, name: str | None = None) -> Path:
        if name is None:
            counter[0] += 1
            name = f"test-{counter[0]}.md"
        p = tmp_path / name
        p.write_text(content)
        return p
    return _make
