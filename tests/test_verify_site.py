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
