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
    (" ! [rejected] main -> main (non-fast-forward)", 0),
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


@pytest.mark.parametrize("name", ["sa-lp-refresh", "vara-refresh"])
def test_both_refresh_modes_use_shared_retry(name):
    source = (ROOT / "scripts" / f"{name}.sh").read_text()
    assert 'python3 "$STAFFORINI_REPO/scripts/push-refresh.py" "$repo"' in source
    assert "\n  git push\n" not in source
