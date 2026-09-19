"""Refresh commits must preserve unrelated staged and unstaged work."""

import os
import subprocess
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]


@pytest.mark.parametrize("name", ["sa-lp-refresh", "vara-refresh"])
@pytest.mark.parametrize("changed", [False, True])
def test_refresh_commit_is_scoped(tmp_path, name, changed):
    repo = tmp_path / "repo"
    repo.mkdir()
    env = dict(os.environ, GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_NOSYSTEM="1",
               GIT_AUTHOR_NAME="Fixture", GIT_AUTHOR_EMAIL="fixture@example.invalid",
               GIT_COMMITTER_NAME="Fixture", GIT_COMMITTER_EMAIL="fixture@example.invalid",
               DRY_RUN="0")

    def git(*args):
        return subprocess.check_output(["git", "-C", str(repo), *args],
                                       env=env, text=True, stderr=subprocess.PIPE)

    git("init")
    git("config", "core.hooksPath", os.devnull)
    for filename in ("refresh.txt", "unrelated.txt"):
        (repo / filename).write_text("original\n")
    git("add", ".")
    git("commit", "-m", "fixture")
    original_head = git("rev-parse", "HEAD")
    (repo / "unrelated.txt").write_text("unrelated staged work\n")
    git("add", "unrelated.txt")
    (repo / "unrelated.txt").write_text("unrelated unstaged work\n")
    staged = git("diff", "--cached", "--", "unrelated.txt")
    unstaged = git("diff", "--", "unrelated.txt")
    if changed:
        (repo / "refresh.txt").write_text("new refresh output\n")

    # Execute the actual shell function, replacing only the external push.
    site = tmp_path / "site"
    (site / "scripts").mkdir(parents=True)
    (site / "scripts" / "push-refresh.py").write_text(
        'from pathlib import Path\nimport sys\n'
        'Path(sys.argv[1], "push-called").touch()\n')
    source = (ROOT / "scripts" / f"{name}.sh").read_text()
    function = source[source.index("commit_if_changed() {"):
                      source.index("\nNOTES_COMMIT_MESSAGE=")]
    result = subprocess.run(
        ["bash", "-c", 'set -euo pipefail\nSTAFFORINI_REPO="$1"\n' + function
         + '\ncommit_if_changed "$2" "refresh fixture" refresh.txt\n',
         "refresh-fixture", str(site), str(repo)],
        env=env, capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert git("diff", "--cached", "--", "unrelated.txt") == staged
    assert git("diff", "--", "unrelated.txt") == unstaged
    assert (repo / "push-called").exists() == changed
    if changed:
        assert git("show", "--pretty=", "--name-only", "HEAD").splitlines() == ["refresh.txt"]
        assert git("show", "HEAD:refresh.txt") == "new refresh output\n"
    else:
        assert git("rev-parse", "HEAD") == original_head
