"""Tests for scripts/audit-redirect-targets.py — dead redirect destinations.

The fault this guards against: a rule keeps 301-ing to a page that was later
renamed or deleted, so the redirect looks healthy and only the target shows up
as a 404 in Search Console.
"""

import importlib.util
from pathlib import Path

import pytest

MODULE_PATH = Path(__file__).parent.parent / "scripts" / "audit-redirect-targets.py"


@pytest.fixture(scope="module")
def audit_module():
    spec = importlib.util.spec_from_file_location("audit_redirect_targets", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def site(tmp_path):
    """A rendered tree with one real page and one real file."""
    root = tmp_path / "site"
    (root / "works" / "kant-1785-groundwork").mkdir(parents=True)
    (root / "works" / "kant-1785-groundwork" / "index.html").write_text("<html></html>")
    (root / "images").mkdir()
    (root / "images" / "chart.png").write_text("png")
    (root / "docs").mkdir()
    (root / "docs" / "Kant - Groundwork.pdf").write_text("pdf")
    return root


def write_redirects(tmp_path, *lines):
    path = tmp_path / "_redirects"
    path.write_text("\n".join(lines) + "\n")
    return path


def test_live_target_is_not_reported(audit_module, site, tmp_path):
    rules = write_redirects(
        tmp_path, "/old-groundwork/  /works/kant-1785-groundwork/  301"
    )
    assert audit_module.audit(site, rules) == []


def test_deleted_target_is_reported(audit_module, site, tmp_path):
    rules = write_redirects(tmp_path, "/old-quote/  /quotes/deleted-quote/  301")
    assert audit_module.audit(site, rules) == [(1, "/old-quote/", "/quotes/deleted-quote/")]


def test_query_param_rule_target_is_found_past_the_condition(audit_module, site, tmp_path):
    """`/quotes/  p=3919  /quotes/slug/  301!` puts the target in field 3."""
    rules = write_redirects(tmp_path, "/quotes/  p=3919  /quotes/gone/  301!")
    assert audit_module.audit(site, rules) == [(1, "/quotes/", "/quotes/gone/")]


def test_percent_encoded_target_resolves_to_the_decoded_path(audit_module, site, tmp_path):
    """Netlify matches encoded paths; the rendered filename is decoded."""
    rules = write_redirects(
        tmp_path, "/old.pdf  /docs/Kant%20-%20Groundwork.pdf  301"
    )
    assert audit_module.audit(site, rules) == []


def test_file_target_without_trailing_slash_resolves(audit_module, site, tmp_path):
    rules = write_redirects(tmp_path, "/old-chart.png  /images/chart.png  301")
    assert audit_module.audit(site, rules) == []


def test_placeholder_and_splat_targets_are_skipped(audit_module, site, tmp_path):
    """`:splat` and `:slug` cannot be resolved against a rendered tree."""
    rules = write_redirects(
        tmp_path,
        "/txt/*  /docs/:splat  301",
        "/tango/:a/feed/  /tango/:a/  301",
    )
    assert audit_module.audit(site, rules) == []


def test_external_targets_are_skipped(audit_module, site, tmp_path):
    rules = write_redirects(
        tmp_path, "/paper.pdf  https://pdf.stafforini.com/paper.pdf  301"
    )
    assert audit_module.audit(site, rules) == []


def test_comments_and_blank_lines_are_skipped(audit_module, site, tmp_path):
    rules = write_redirects(
        tmp_path,
        "# Legacy WordPress mappings",
        "",
        "/old-groundwork/  /works/kant-1785-groundwork/  301",
    )
    assert audit_module.audit(site, rules) == []


def test_reported_line_numbers_match_the_file(audit_module, site, tmp_path):
    rules = write_redirects(
        tmp_path,
        "# header",
        "/old-groundwork/  /works/kant-1785-groundwork/  301",
        "/other/  /works/gone/  301",
    )
    assert audit_module.audit(site, rules) == [(3, "/other/", "/works/gone/")]
