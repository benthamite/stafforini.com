"""Execute the deployment boundary without running refreshes or live deploys."""

import os
import subprocess
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]


@pytest.mark.parametrize("name", ("sa-lp-refresh", "vara-refresh"))
@pytest.mark.parametrize("deployment_exit,dry_run", ((0, False), (7, False), (0, True)))
def test_freshness_requires_successful_deployment(tmp_path, name, deployment_exit, dry_run):
    source = (ROOT / "scripts" / f"{name}.sh").read_text()
    boundary = source[source.index('echo "--- Deploying to Netlify'):]
    scripts = tmp_path / "site" / "scripts"
    scripts.mkdir(parents=True)
    (scripts / "deploy.sh").write_text(f"exit {deployment_exit}\n")
    state = tmp_path / "state"
    env = dict(os.environ, XDG_STATE_HOME=str(state), DRY_RUN=str(int(dry_run)))
    result = subprocess.run(
        ["bash", "-c", 'set -euo pipefail\nSTAFFORINI_REPO="$1"\n' + boundary,
         "refresh-boundary", str(scripts.parent)],
        env=env, capture_output=True, text=True,
    )
    assert result.returncode == (0 if dry_run else deployment_exit)
    heartbeat = state / "launchd" / f"com.stafforini.{name}.success"
    assert heartbeat.exists() == (deployment_exit == 0 and not dry_run)
