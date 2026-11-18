"""Exercise bounded retries, including an actual push through local HTTP."""

import importlib.util
import os
import subprocess
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("push_refresh", ROOT / "scripts/push-refresh.py")
push_refresh = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(push_refresh)


@pytest.mark.parametrize("diagnostic,retries", [
    ("fatal: unable to access 'https://example.invalid/': Could not resolve host: example.invalid", 3),
    ("fatal: unable to access 'https://example.invalid/': Failed to connect to example.invalid port 443", 3),
    ("fatal: unable to access 'https://example.invalid/': The requested URL returned error: 403", 0),
    ("fatal: Authentication failed for 'https://example.invalid/'", 0),
    ("fatal: not a git repository", 0),
])
def test_retry_policy(monkeypatch, diagnostic, retries):
    calls, delays = [], []

    def run(command, **kwargs):
        calls.append(command)
        return subprocess.CompletedProcess(command, 128, "", diagnostic)

    monkeypatch.setattr(push_refresh.subprocess, "run", run)
    monkeypatch.setattr(push_refresh.time, "sleep", delays.append)
    assert push_refresh.push_refresh("/fixture") == 128
    assert calls == [["git", "-C", "/fixture", "push"]] * (retries + 1)
    assert delays == ([5, 10, 20] if retries else [])


def test_real_http_push_recovers_after_unavailable_response(tmp_path, monkeypatch):
    # All writes stay in these disposable repos; no account or external server.
    env = dict(os.environ, GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_NOSYSTEM="1")

    def git(*args):
        return subprocess.check_output(["git", *map(str, args)], env=env, stderr=subprocess.PIPE).strip()

    remote, local = tmp_path / "remote.git", tmp_path / "local"
    git("init", "--bare", remote)
    git("-C", remote, "config", "http.receivepack", "true")
    git("init", "-b", "main", local)
    git("-C", local, "config", "user.name", "Fixture")
    git("-C", local, "config", "user.email", "fixture@example.invalid")
    git("-C", local, "config", "core.hooksPath", os.devnull)
    (local / "refresh.txt").write_text("refreshed data\n")
    git("-C", local, "add", "refresh.txt")
    git("-C", local, "commit", "-m", "Refresh fixture")
    attempts = []

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            attempts.append(self.path)
            if len(attempts) == 1:
                self.send_error(503)
                return
            self.backend()

        def do_POST(self):
            self.backend()

        def backend(self):
            path, _, query = self.path.partition("?")
            body = self.rfile.read(int(self.headers.get("Content-Length", "0")))
            result = subprocess.run(
                ["git", "http-backend"], input=body, capture_output=True, check=True,
                env=dict(env, GIT_PROJECT_ROOT=str(tmp_path), GIT_HTTP_EXPORT_ALL="1",
                         PATH_INFO=path, QUERY_STRING=query, REQUEST_METHOD=self.command,
                         CONTENT_TYPE=self.headers.get("Content-Type", ""),
                         CONTENT_LENGTH=str(len(body)), REMOTE_USER="fixture"),
            )
            headers, _, content = result.stdout.partition(b"\r\n\r\n")
            self.send_response(200)
            for header in headers.decode().splitlines():
                name, value = header.split(":", 1)
                self.send_header(name, value.strip())
            self.end_headers()
            self.wfile.write(content)

        def log_message(self, *args):
            pass

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    try:
        git("-C", local, "remote", "add", "origin", f"http://127.0.0.1:{server.server_port}/remote.git")
        git("-C", local, "config", "push.default", "current")
        for key in ("GIT_CONFIG_GLOBAL", "GIT_CONFIG_NOSYSTEM"):
            monkeypatch.setenv(key, env[key])
        delays = []
        monkeypatch.setattr(push_refresh.time, "sleep", delays.append)
        assert push_refresh.push_refresh(local) == 0
        assert delays == [5]
        assert len(attempts) == 2
        assert git("-C", remote, "rev-parse", "refs/heads/main") == git("-C", local, "rev-parse", "HEAD")
        assert git("-C", remote, "show", "main:refresh.txt") == b"refreshed data"
    finally:
        server.shutdown()
        thread.join()
        server.server_close()


def make_repos(tmp_path):
    """A bare remote, a clone holding a refresh commit, and a second writer."""
    env = dict(os.environ, GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_NOSYSTEM="1")

    def git(*args):
        return subprocess.check_output(["git", *map(str, args)], env=env, stderr=subprocess.PIPE).strip()

    remote = tmp_path / "remote.git"
    git("init", "--bare", "-b", "main", remote)
    clones = []
    for name in ("local", "bot"):
        clone = tmp_path / name
        if not clones:
            git("init", "-b", "main", clone)
        else:
            git("clone", remote, clone)
        git("-C", clone, "config", "user.name", "Fixture")
        git("-C", clone, "config", "user.email", "fixture@example.invalid")
        git("-C", clone, "config", "core.hooksPath", os.devnull)
        if not clones:
            (clone / "shared.txt").write_text("base\n")
            git("-C", clone, "add", "shared.txt")
            git("-C", clone, "commit", "-m", "Base")
            git("-C", clone, "remote", "add", "origin", remote)
            git("-C", clone, "push", "-u", "origin", "main")
        clones.append(clone)
    return env, git, remote, *clones


def commit(git, repo, name, text, message):
    (repo / name).write_text(text)
    git("-C", repo, "add", name)
    git("-C", repo, "commit", "-m", message)


def test_rejected_push_rebases_onto_concurrent_writer(tmp_path, monkeypatch):
    env, git, remote, local, bot = make_repos(tmp_path)
    commit(git, bot, "filing.json", "new filing\n", "filings: record new filing")
    git("-C", bot, "push")
    commit(git, local, "refresh.txt", "refreshed\n", "Refresh")
    (local / "shared.txt").write_text("uncommitted generated data\n")
    for key in ("GIT_CONFIG_GLOBAL", "GIT_CONFIG_NOSYSTEM"):
        monkeypatch.setenv(key, env[key])
    assert push_refresh.push_refresh(local) == 0
    assert git("-C", remote, "rev-parse", "main") == git("-C", local, "rev-parse", "HEAD")
    assert git("-C", remote, "log", "--format=%s", "main").split(b"\n") == [
        b"Refresh", b"filings: record new filing", b"Base"]
    assert (local / "shared.txt").read_text() == "uncommitted generated data\n"


def test_conflicting_rebase_is_aborted_and_fails(tmp_path, monkeypatch):
    env, git, remote, local, bot = make_repos(tmp_path)
    commit(git, bot, "shared.txt", "bot\n", "Bot edit")
    git("-C", bot, "push")
    commit(git, local, "shared.txt", "refresh\n", "Refresh")
    head = git("-C", local, "rev-parse", "HEAD")
    for key in ("GIT_CONFIG_GLOBAL", "GIT_CONFIG_NOSYSTEM"):
        monkeypatch.setenv(key, env[key])
    assert push_refresh.push_refresh(local) != 0
    assert git("-C", local, "rev-parse", "HEAD") == head
    assert not (local / ".git" / "rebase-merge").exists()
    assert git("-C", remote, "log", "-1", "--format=%s", "main") == b"Bot edit"


def test_second_rejection_is_not_rebased_again(monkeypatch):
    calls = []
    rejected = " ! [rejected]        main -> main (fetch first)\n"

    def run(command, **kwargs):
        calls.append(command[3:])
        code = 0 if command[3] == "pull" else 1
        return subprocess.CompletedProcess(command, code, "", "" if code == 0 else rejected)

    monkeypatch.setattr(push_refresh.subprocess, "run", run)
    assert push_refresh.push_refresh("/fixture") == 1
    assert calls == [["push"], ["pull", "--rebase", "--autostash"], ["push"]]


@pytest.mark.parametrize("name", ["sa-lp-refresh", "vara-refresh"])
def test_both_refresh_modes_use_shared_retry(name):
    source = (ROOT / "scripts" / f"{name}.sh").read_text()
    assert 'python3 "$STAFFORINI_REPO/scripts/push-refresh.py" "$repo"' in source
    assert "\n  git push\n" not in source
