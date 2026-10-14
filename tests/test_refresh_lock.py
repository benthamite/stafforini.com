"""The SA-LP and VARA refreshes serialize on one lock and bound the credential read."""

import os
import shutil
import subprocess
import time
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
NAMES = ["sa-lp-refresh", "vara-refresh"]

pytestmark = pytest.mark.skipif(shutil.which("timeout") is None,
                                reason="GNU timeout is not installed")


def preamble(name):
    """The real lock and credential-read code of NAME, with notifications stubbed."""
    source = (ROOT / "scripts" / f"{name}.sh").read_text()
    section = source[source.index("REFRESH_LOCK="):source.index("export MARKETDATA_KEY")]
    return ("set -euo pipefail\nnotify_failure() { :; }\n" + section
            + 'echo "key: $MARKETDATA_KEY"\n')


def make_env(tmp_path, reader_body, timeout_seconds="3"):
    home = tmp_path / "home"
    (home / "bin").mkdir(parents=True, exist_ok=True)
    reader = home / "bin" / "op-automations"
    reader.write_text("#!/bin/bash\n" + reader_body + "\n")
    reader.chmod(0o755)
    return dict(os.environ, HOME=str(home), XDG_STATE_HOME=str(tmp_path / "state"),
                REFRESH_CREDENTIAL_TIMEOUT_SECONDS=timeout_seconds)


def lock_dir(tmp_path):
    return tmp_path / "state" / "stafforini.com" / "model-refresh.lock"


def start(name, env):
    return subprocess.Popen(["bash", "-c", preamble(name)], env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@pytest.mark.parametrize("name", NAMES)
def test_hung_credential_read_times_out_and_releases_lock(tmp_path, name):
    env = make_env(tmp_path, "sleep 60")
    started = time.monotonic()
    result = subprocess.run(["bash", "-c", preamble(name)], env=env,
                            capture_output=True, text=True, timeout=30)
    assert result.returncode == 1
    assert "timed out after 3s" in result.stderr
    assert time.monotonic() - started < 20
    assert not lock_dir(tmp_path).exists()


def test_refreshes_wait_for_each_other(tmp_path):
    env = make_env(tmp_path, "sleep 12; echo fixture-key", timeout_seconds="30")
    first = start("sa-lp-refresh", env)
    pid_file = lock_dir(tmp_path) / "pid"
    deadline = time.monotonic() + 10
    while not pid_file.exists():
        assert time.monotonic() < deadline, "first refresh never took the lock"
        time.sleep(0.1)
    second = start("vara-refresh", env)
    first_out, first_err = first.communicate(timeout=60)
    second_out, second_err = second.communicate(timeout=60)
    assert first.returncode == 0, first_err
    assert second.returncode == 0, second_err
    assert "key: fixture-key" in first_out and "key: fixture-key" in second_out
    # Announced once although the first refresh outlives several polls.
    assert second_out.count("waiting for another") == 1
    assert f"waiting for another SA-LP/VARA refresh (pid {first.pid})" in second_out
    assert "took the refresh lock" in second_out
    assert not lock_dir(tmp_path).exists()


def test_stale_lock_is_taken_over(tmp_path):
    env = make_env(tmp_path, "echo fixture-key")
    dead = subprocess.Popen(["true"])
    dead.wait()
    lock_dir(tmp_path).mkdir(parents=True)
    (lock_dir(tmp_path) / "pid").write_text(f"{dead.pid}\n")
    result = subprocess.run(["bash", "-c", preamble("vara-refresh")], env=env,
                            capture_output=True, text=True, timeout=30)
    assert result.returncode == 0, result.stderr
    assert "waiting for another" not in result.stdout
    assert not lock_dir(tmp_path).exists()
