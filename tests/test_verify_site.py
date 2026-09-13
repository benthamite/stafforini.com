"""Smoke tests for rendered-site verification."""

from __future__ import annotations

import importlib.util
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

VERIFY_PATH = Path(__file__).parent.parent / "scripts" / "verify-site.py"


@pytest.fixture(scope="module")
def verify_module():
    spec = importlib.util.spec_from_file_location("verify_site", VERIFY_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_redirect_target_check_reports_dead_targets(verify_module, tmp_path):
    """A rule pointing at a page absent from the render must fail verification."""
    site = tmp_path / "site"
    (site / "works" / "kept").mkdir(parents=True)
    (site / "works" / "kept" / "index.html").write_text("<html></html>")

    redirects = tmp_path / "_redirects"
    redirects.write_text(
        "/old-kept/  /works/kept/  301!\n"
        "/old-gone/  /works/gone/  301!\n"
    )

    errors = verify_module.verify_redirect_targets(site, redirects)
    assert len(errors) == 1, errors
    assert "/works/gone/" in errors[0]
    assert "line 2" in errors[0]


def test_redirect_target_check_passes_when_every_target_resolves(verify_module, tmp_path):
    site = tmp_path / "site"
    (site / "works" / "kept").mkdir(parents=True)
    (site / "works" / "kept" / "index.html").write_text("<html></html>")

    redirects = tmp_path / "_redirects"
    redirects.write_text("/old-kept/  /works/kept/  301!\n")

    assert verify_module.verify_redirect_targets(site, redirects) == []


def test_redirect_target_check_is_wired_into_the_full_profile():
    """Guard the wiring itself, not just the audit it delegates to."""
    source = VERIFY_PATH.read_text()
    assert "verify_redirect_targets" in source
    full_branch = source.split('if args.profile == "full":', 1)[1]
    assert "verify_redirect_targets(site_dir)" in full_branch.split("else:", 1)[0], (
        "the redirect audit must run in the full profile"
    )


@pytest.fixture
def pdf_preview_site(verify_module, tmp_path, monkeypatch):
    repo = tmp_path / "repo"
    (repo / "static" / "pdf-thumbnails").mkdir(parents=True)
    (repo / "content" / "works").mkdir(parents=True)
    (repo / "hugo.deploy.toml").write_text(
        '[params]\npdfBaseURL = "https://pdf.example.test"\n'
        'thumbBaseURL = "https://thumb.example.test"\n'
    )
    monkeypatch.setattr(verify_module, "REPO_ROOT", repo)
    site = tmp_path / "site"

    def add_work(slug, markup, thumbnail=True):
        (repo / "content" / "works" / f"{slug}.md").touch()
        if thumbnail:
            (repo / "static" / "pdf-thumbnails" / f"{slug}.png").touch()
        page = site / "works" / slug / "index.html"
        page.parent.mkdir(parents=True)
        page.write_text(markup)

    return repo, site, add_work


def test_work_pdf_previews_use_source_inventory_and_production_urls(
    verify_module, pdf_preview_site
):
    repo, site, add_work = pdf_preview_site
    add_work("present", '<a href="https://pdf.example.test/present.pdf">'
             '<img src="https://thumb.example.test/present.png"></a>')
    add_work("no-pdf", "<h1>No local PDF</h1>", thumbnail=False)
    (repo / "static" / "pdf-thumbnails" / "stray.png").touch()
    # No rendered static thumbnail directory: production serves these from R2.
    assert verify_module.verify_work_pdf_previews(site) == []


@pytest.mark.parametrize(
    "markup, expected",
    [
        ("<h1>Work</h1>", ["PDF link", "PDF preview image"]),
        ('<a href="/pdfs/work.pdf"><img src="/pdf-thumbnails/work.png"></a>',
         ["PDF link", "PDF preview image"]),
        ('<a href="https://pdf.example.test/work.pdf">PDF</a>', ["PDF preview image"]),
        ('<img src="https://thumb.example.test/work.png">', ["PDF link"]),
        ('<link href="https://pdf.example.test/work.pdf">'
         '<a href="https://thumb.example.test/work.png">Preview</a>',
         ["PDF link", "PDF preview image"]),
    ],
)
def test_work_pdf_previews_reject_missing_or_wrong_targets(
    verify_module, pdf_preview_site, markup, expected
):
    _repo, site, add_work = pdf_preview_site
    add_work("work", markup)
    errors = verify_module.verify_work_pdf_previews(site)
    assert len(errors) == len(expected)
    for kind, error in zip(expected, errors):
        assert f"missing expected {kind}: work" in error


def test_work_pdf_previews_report_missing_rendered_page(verify_module, pdf_preview_site):
    repo, site, _add_work = pdf_preview_site
    (repo / "content" / "works" / "missing.md").touch()
    (repo / "static" / "pdf-thumbnails" / "missing.png").touch()
    assert verify_module.verify_work_pdf_previews(site) == [
        "1 work page(s) missing expected rendered page: missing"
    ]


def test_work_pdf_previews_fail_on_missing_source(verify_module, pdf_preview_site):
    repo, site, _add_work = pdf_preview_site
    (repo / "static" / "pdf-thumbnails").rmdir()
    errors = verify_module.verify_work_pdf_previews(site)
    assert len(errors) == 1
    assert "missing work PDF verification source:" in errors[0]
    assert "pdf-thumbnails" in errors[0]


def test_work_pdf_preview_errors_are_aggregated(verify_module, pdf_preview_site):
    _repo, site, add_work = pdf_preview_site
    for i in range(20):
        add_work(f"work-{i:02}", "<h1>Work</h1>")
    errors = verify_module.verify_work_pdf_previews(site)
    assert len(errors) == 2
    assert all(error.startswith("20 work page(s)") for error in errors)
    assert all("work-04" in error and "work-05" not in error for error in errors)


@pytest.mark.parametrize("profile, expected_calls", [("full", 1), ("fast-note", 0)])
def test_work_pdf_preview_check_runs_only_for_full_profile(
    verify_module, monkeypatch, tmp_path, profile, expected_calls
):
    from unittest.mock import Mock

    for name in ("verify_excluded_works", "verify_built_site", "verify_sitemap",
                 "verify_internal_links", "verify_redirect_targets"):
        monkeypatch.setattr(verify_module, name, Mock(return_value=[]))
    check = Mock(return_value=[])
    monkeypatch.setattr(verify_module, "verify_work_pdf_previews", check)
    monkeypatch.setattr(sys, "argv", ["verify-site.py", "--dir", str(tmp_path),
                                   "--profile", profile])
    verify_module.main()
    assert check.call_count == expected_calls


def test_dev_site_smoke_check_passes():
    if not shutil.which("hugo"):
        pytest.skip("hugo executable not available")

    result = subprocess.run(
        [sys.executable, "scripts/verify-site.py", "--build", "dev"],
        text=True,
        capture_output=True,
        timeout=120,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "Rendered site verification OK." in result.stdout
