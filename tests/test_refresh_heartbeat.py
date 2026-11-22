"""Execute the deployment boundary without running refreshes or live deploys."""

import os
import subprocess
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
BOUNDARY = 'if [[ "$DEPLOY" -eq 0 ]]; then'


def run_boundary(tmp_path, name, deploy, dry_run, deployment_exit=0):
    source = (ROOT / "scripts" / f"{name}.sh").read_text()
    boundary = source[source.index(BOUNDARY):]
    scripts = tmp_path / "site" / "scripts"
    scripts.mkdir(parents=True)
    calls = tmp_path / "deploy-calls"
    (scripts / "deploy.sh").write_text(f'echo "$@" >> "{calls}"\nexit {deployment_exit}\n')
    marker = tmp_path / "completed"
    env = dict(os.environ, DRY_RUN=str(int(dry_run)), REFRESH_COMPLETED_FILE=str(marker))
    result = subprocess.run(
        ["bash", "-c", f'set -euo pipefail\nSTAFFORINI_REPO="$1"\nDEPLOY={int(deploy)}\n' + boundary,
         "refresh-boundary", str(scripts.parent)],
        env=env, capture_output=True, text=True,
    )
    deploys = calls.read_text().splitlines() if calls.exists() else []
    return result, deploys, marker.exists()


@pytest.mark.parametrize("name", ("sa-lp-refresh", "vara-refresh"))
def test_no_deploy_marks_completion_without_deploying(tmp_path, name):
    result, deploys, completed = run_boundary(tmp_path, name, deploy=False, dry_run=False)
    assert result.returncode == 0, result.stderr
    assert deploys == []
    assert completed


@pytest.mark.parametrize("name", ("sa-lp-refresh", "vara-refresh"))
@pytest.mark.parametrize("deployment_exit,dry_run", ((0, False), (7, False), (0, True)))
def test_standalone_run_deploys_itself(tmp_path, name, deployment_exit, dry_run):
    result, deploys, completed = run_boundary(
        tmp_path, name, deploy=True, dry_run=dry_run, deployment_exit=deployment_exit)
    assert result.returncode == (0 if dry_run else deployment_exit)
    assert deploys == ([] if dry_run else ["--fast-note"])
    assert not completed


def run_wrapper(tmp_path, outcomes, deployment_exit=0, dry_run=False):
    """OUTCOMES maps each refresh to "complete", "skip" or "fail"."""
    scripts = tmp_path / "site" / "scripts"
    scripts.mkdir(parents=True)
    log = tmp_path / "calls"
    for name, outcome in outcomes.items():
        body = f'echo "{name} $*" >> "{log}"\n'
        if outcome == "complete":
            body += 'touch "$REFRESH_COMPLETED_FILE"\n'
        body += "exit 3\n" if outcome == "fail" else "exit 0\n"
        (scripts / f"{name}.sh").write_text(body)
    (scripts / "deploy.sh").write_text(f'echo "deploy $*" >> "{log}"\nexit {deployment_exit}\n')
    stubs = tmp_path / "bin"
    stubs.mkdir()
    osascript = stubs / "osascript"
    osascript.write_text(f'#!/bin/sh\necho notify >> "{log}"\n')
    osascript.chmod(0o755)
    state = tmp_path / "state"
    env = dict(os.environ, XDG_STATE_HOME=str(state), STAFFORINI_REPO=str(scripts.parent),
               DRY_RUN=str(int(dry_run)), HOME=str(tmp_path),
               PATH=f"{stubs}:{os.environ['PATH']}")
    result = subprocess.run(["bash", str(ROOT / "scripts" / "model-refresh.sh")],
                            env=env, capture_output=True, text=True)
    calls = log.read_text().splitlines() if log.exists() else []
    heartbeat = state / "launchd" / "com.stafforini.model-refresh.success"
    return result, calls, heartbeat.exists()


@pytest.mark.parametrize(
    "salp,vara,deployment_exit,dry_run,exit_code,deploys,fresh",
    (
        ("complete", "complete", 0, False, 0, True, True),
        ("complete", "complete", 7, False, 1, True, False),
        ("complete", "complete", 0, True, 0, False, False),
        ("fail", "complete", 0, False, 1, True, False),
        ("complete", "fail", 0, False, 1, True, False),
        ("skip", "complete", 0, False, 0, True, False),
        ("skip", "skip", 0, False, 0, False, False),
        ("fail", "fail", 0, False, 1, False, False),
    ),
)
def test_wrapper_deploys_once(tmp_path, salp, vara, deployment_exit, dry_run,
                              exit_code, deploys, fresh):
    result, calls, heartbeat = run_wrapper(
        tmp_path, {"sa-lp-refresh": salp, "vara-refresh": vara},
        deployment_exit=deployment_exit, dry_run=dry_run)
    assert result.returncode == exit_code, result.stdout + result.stderr
    assert calls[:2] == ["sa-lp-refresh --no-deploy", "vara-refresh --no-deploy"]
    assert calls[2:] == (["deploy --fast-note"] if deploys else []) + (["notify"] if exit_code else [])
    assert heartbeat == fresh
