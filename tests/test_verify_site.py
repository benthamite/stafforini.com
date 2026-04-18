"""Smoke tests for rendered-site verification."""

from __future__ import annotations

import shutil
import subprocess
import sys


def test_dev_site_smoke_check_passes():
    if not shutil.which("hugo"):
        import pytest

        pytest.skip("hugo executable not available")

    result = subprocess.run(
        [sys.executable, "scripts/verify-site.py", "--build", "dev"],
        text=True,
        capture_output=True,
        timeout=120,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "Rendered site verification OK." in result.stdout
