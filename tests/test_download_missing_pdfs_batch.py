"""Scheduler safety contracts; worker doubles do not verify acquisition or Ebib."""

import fcntl
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import signal
import sys
import time
import tomllib
from types import SimpleNamespace

import pytest


SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "download-missing-pdfs-batch.py"
_spec = importlib.util.spec_from_file_location("download_missing_pdfs_batch", SCRIPT)
batch = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(batch)
_real_adapter = batch.adapter


def test_launchd_pool_selection_uses_configured_emacs_home(tmp_path, monkeypatch):
    (tmp_path / ".codex-current-account").write_text("epoch-pool\n")
    selected = tmp_path / "configured-account"
    selected.mkdir()
    calls = []

    def emacs(command, **kwargs):
        calls.append((command, kwargs))
        return SimpleNamespace(returncode=0, stdout=json.dumps(
            {"ok": True, "result": json.dumps(str(selected)), "truncated": False}))

    monkeypatch.setattr(batch.subprocess, "run", emacs)
    env = batch.agent_environment({"HOME": str(tmp_path), "CODEX_BUFFER_NAME": "parent"})
    assert env["CODEX_HOME"] == str(selected)
    assert "CODEX_BUFFER_NAME" not in env
    expression = calls[0][0][-1]
    assert 'selection "epoch-pool"' in expression
    assert "agent-account-route" in expression
    assert "agent-account-home" in expression
    assert "CODEX_BUFFER_NAME" not in calls[0][1]["env"]


@pytest.mark.parametrize("stdout,stderr,expected", [
    (json.dumps({"ok": False, "error": "Connection refused"}), "", "Connection refused"),
    ("", "cannot start emacs-eval", "cannot start emacs-eval"),
    ("", "", "no diagnostic output"),
])
def test_account_resolution_retains_failure_cause(tmp_path, monkeypatch, stdout, stderr, expected):
    (tmp_path / ".codex-current-account").write_text("epoch-pool\n")
    monkeypatch.setattr(batch.subprocess, "run", lambda *a, **kw:
                        SimpleNamespace(returncode=1, stdout=stdout, stderr=stderr))
    with pytest.raises(ValueError, match="exit 1") as failure:
        batch.agent_environment({"HOME": str(tmp_path)})
    assert expected in str(failure.value)


def test_explicit_account_does_not_consult_marker_or_emacs(tmp_path, monkeypatch):
    (tmp_path / ".codex-current-account").write_text("epoch-pool\n")
    monkeypatch.setattr(batch.subprocess, "run", lambda *a, **kw: pytest.fail("Resolver called"))
    env = {"HOME": str(tmp_path), "CODEX_HOME": str(tmp_path / "pinned")}
    assert batch.agent_environment(env) == env


@pytest.mark.parametrize("reply", [
    {"ok": False, "result": '"/tmp"', "truncated": False},
    {"ok": True, "result": '"/tmp"', "truncated": True},
    {"ok": True, "result": "nil", "truncated": False},
    {"ok": True, "result": '"relative"', "truncated": False},
    {"ok": True, "result": "42", "truncated": False},
    {},
])
def test_account_resolution_fails_closed(tmp_path, monkeypatch, reply):
    (tmp_path / ".codex-current-account").write_text("epoch-pool\n")
    monkeypatch.setattr(batch.subprocess, "run", lambda *a, **kw:
                        SimpleNamespace(returncode=0, stdout=json.dumps(reply)))
    with pytest.raises(ValueError):
        batch.agent_environment({"HOME": str(tmp_path)})


@pytest.fixture
def job(tmp_path, monkeypatch):
    """Isolate account selection, bibliography and worker effects from real data."""
    bib = tmp_path / "fixture.bib"
    bib.write_text("Synthetic bibliography, never passed to a live worker.\n")
    state_dir = tmp_path / "state"
    state_dir.mkdir()
    books = [{"key": key, "file": ""} for key in ("Alpha", "Beta", "Gamma")]
    inspection = SimpleNamespace(
        paper_fetch=_real_adapter().paper_fetch,
        parse_bib_books=lambda path: books,
        books_missing_pdf=lambda entries, *, include_broken: [
            entry for entry in entries
            if not entry["file"] or not Path(entry["file"]).is_file()
        ],
        extract_pdf_path=lambda value: Path(value) if value else None,
    )
    monkeypatch.setattr(batch, "adapter", lambda: inspection)
    monkeypatch.setattr(batch, "agent_environment", lambda: {})
    args = SimpleNamespace(bib=bib, state_dir=state_dir, limit=1, only="",
                           timeout=1, dry_run=False)
    return args, inspection, {"version": 1, "books": {}}


def result_for(attempt, key="Alpha", status="deferred"):
    evidence = attempt / "review.md"
    evidence.write_text("Fixture evidence: no acceptable candidate found.\n")
    return {"status": status, "key": key, "reason": "No acceptable candidate",
            "file": "", "sha256": "", "operation_id": "", "evidence": str(evidence)}


def cli_args(args):
    return ["--bib", str(args.bib), "--state-dir", str(args.state_dir),
            "--limit", str(args.limit)]


def test_repeated_batches_rotate_past_deferred_books(job, monkeypatch):
    args, inspection, state = job
    visited = []

    def worker(command, prompt, attempt, timeout, env):
        key = json.loads((attempt / "input.json").read_text())["book"]["key"]
        visited.append(key)
        batch.write_json(attempt / "result.json", result_for(attempt, key))

    monkeypatch.setattr(batch, "run_agent", worker)
    for _ in range(4):
        assert batch.execute(args, inspection, state) == 0
        state = batch.read_state(args.state_dir / "state.json")
    assert visited == ["Alpha", "Beta", "Gamma", "Alpha"]


def test_attached_history_does_not_hide_missing_disk_pdf(job, monkeypatch):
    args, inspection, state = job
    args.dry_run = True
    args.only = "Alpha"
    book = inspection.parse_bib_books(args.bib)[0]
    book["file"] = str(args.state_dir / "missing.pdf")
    state["books"]["Alpha"] = {"last_attempt": 1, "status": "attached"}
    selected = []
    real_select = batch.select_books

    def select(books, state, limit, only):
        selected.extend(real_select(books, state, limit, only))
        return selected

    monkeypatch.setattr(batch, "select_books", select)
    assert batch.execute(args, inspection, state) == 0
    assert [entry["key"] for entry in selected] == ["Alpha"]
    assert not (args.state_dir / "state.json").exists()


@pytest.mark.parametrize("payload", ["not JSON", "null", "42", "[]", "{}", '{"status":"attached"}'])
def test_incomplete_worker_output_stops_and_preserves_running_ledger(job, monkeypatch, payload):
    args, inspection, state = job
    args.limit = 3
    visited = []

    def worker(command, prompt, attempt, timeout, env):
        visited.append(attempt)
        (attempt / "result.json").write_text(payload)

    monkeypatch.setattr(batch, "run_agent", worker)
    assert batch.main(cli_args(args)) == 1
    assert len(visited) == 1
    saved = batch.read_state(args.state_dir / "state.json")
    assert saved["books"]["Alpha"]["status"] == "running"
    assert (visited[0] / "result.json").read_text() == payload
    assert (visited[0] / "input.json").exists()
    assert (visited[0] / "prompt.md").exists()
    # A second invocation must not race a possibly pending attachment operation.
    assert batch.main(cli_args(args)) == 1
    assert len(visited) == 1


IDLE_FAILURE = [
    {"type": "thread.started", "thread_id": "fixture"},
    {"type": "turn.started"},
    {"type": "error", "message": "Reconnecting... 5/5 (workspace routing discovery failed)"},
    {"type": "item.completed", "item": {"id": "item_0", "type": "error", "message": "Falling back"}},
    {"type": "turn.failed", "error": {"message": "workspace routing discovery failed"}},
]


def stranded_attempt(args, events, text=None):
    attempt = args.state_dir / "attempt-stranded"
    attempt.mkdir()
    lines = "".join(json.dumps(event) + "\n" for event in events)
    (attempt / "events.jsonl").write_text(lines if text is None else text)
    record = {"last_attempt": 1, "status": "running", "attempt": str(attempt)}
    batch.write_json(args.state_dir / "state.json", {"version": 1, "books": {"Alpha": record}})
    return attempt


def test_turn_that_failed_before_acting_does_not_block_the_next_batch(job, monkeypatch):
    args, inspection, state = job
    stranded_attempt(args, IDLE_FAILURE)
    visited = []

    def worker(command, prompt, attempt, timeout, env):
        key = json.loads((attempt / "input.json").read_text())["book"]["key"]
        visited.append(key)
        batch.write_json(attempt / "result.json", result_for(attempt, key))

    monkeypatch.setattr(batch, "run_agent", worker)
    assert batch.main(cli_args(args)) == 0
    assert visited == ["Alpha"]
    saved = batch.read_state(args.state_dir / "state.json")
    assert saved["books"]["Alpha"]["status"] == "deferred"


def test_dry_run_reports_a_released_attempt_without_writing_state(job, capsys):
    args, inspection, state = job
    stranded_attempt(args, IDLE_FAILURE)
    before = (args.state_dir / "state.json").read_text()
    assert batch.main([*cli_args(args), "--dry-run"]) == 0
    assert '"Alpha"' in capsys.readouterr().out
    assert (args.state_dir / "state.json").read_text() == before


@pytest.mark.parametrize("events,text", [
    ([], None),
    (IDLE_FAILURE[:3], None),
    ([*IDLE_FAILURE[:2], {"type": "item.started", "item": {"type": "command_execution"}},
      IDLE_FAILURE[-1]], None),
    ([*IDLE_FAILURE[:2], {"type": "item.completed", "item": {"type": "agent_message"}},
      IDLE_FAILURE[-1]], None),
    ([*IDLE_FAILURE[:2], {"type": "turn.completed"}, IDLE_FAILURE[-1]], None),
    ([], json.dumps(IDLE_FAILURE[-1]) + "\n{truncated"),
    ([], "[]\n" + json.dumps(IDLE_FAILURE[-1]) + "\n"),
])
def test_attempt_that_may_have_acted_still_blocks_retry(job, monkeypatch, events, text):
    args, inspection, state = job
    stranded_attempt(args, events, text)
    monkeypatch.setattr(batch, "run_agent", lambda *a, **kw: pytest.fail("worker started"))
    assert batch.main(cli_args(args)) == 1
    assert batch.read_state(args.state_dir / "state.json")["books"]["Alpha"]["status"] == "running"


@pytest.mark.parametrize("change", ["result", "operation", "missing-events", "no-attempt"])
def test_idle_failure_with_other_signs_of_work_still_blocks_retry(job, monkeypatch, change):
    args, inspection, state = job
    attempt = stranded_attempt(args, IDLE_FAILURE)
    saved = json.loads((args.state_dir / "state.json").read_text())
    if change == "result":
        (attempt / "result.json").write_text("{}")
    elif change == "operation":
        saved["books"]["Alpha"]["operation_id"] = "bib-operation-1"
    elif change == "missing-events":
        (attempt / "events.jsonl").unlink()
    else:
        del saved["books"]["Alpha"]["attempt"]
    batch.write_json(args.state_dir / "state.json", saved)
    monkeypatch.setattr(batch, "run_agent", lambda *a, **kw: pytest.fail("worker started"))
    assert batch.main(cli_args(args)) == 1


@pytest.mark.parametrize("failure", ["nonzero", "timeout"])
def test_real_child_failure_stops_queue_and_retains_evidence(job, monkeypatch, failure):
    args, inspection, state = job
    args.limit = 3
    real_run = batch.run_agent
    attempts = []

    def worker(command, prompt, attempt, timeout, env):
        attempts.append(attempt)
        source = "import sys; print('diagnostic', file=sys.stderr, flush=True); "
        source += "raise SystemExit(7)" if failure == "nonzero" else "import time; time.sleep(30)"
        return real_run([sys.executable, "-c", source], prompt, attempt, 0.25, {})

    monkeypatch.setattr(batch, "run_agent", worker)
    assert batch.main(cli_args(args)) == 1
    assert len(attempts) == 1
    record = batch.read_state(args.state_dir / "state.json")["books"]["Alpha"]
    assert record["status"] == "running"
    assert Path(record["attempt"]) == attempts[0]
    assert "diagnostic" in (attempts[0] / "stderr.log").read_text()
    assert (attempts[0] / "events.jsonl").exists()


@pytest.fixture
def attached_result(job, monkeypatch):
    args, inspection, state = job
    attempt = args.state_dir / "review"
    attempt.mkdir()
    pdf = attempt / "book.pdf"
    pdf.write_bytes(b"%PDF-1.7 synthetic fixture bytes")
    original = attempt / "original.pdf"
    original.write_bytes(pdf.read_bytes())
    book = inspection.parse_bib_books(args.bib)[0]
    book["file"] = str(pdf)
    result = result_for(attempt, status="attached")
    result.update(file=str(pdf), sha256=hashlib.sha256(pdf.read_bytes()).hexdigest(),
                  operation_id="fixture-operation")
    target = {"title": "Synthetic Book", "author": "Example, Alice", "year": "1960",
              "edition": "first", "language": "english"}
    md5 = hashlib.md5(original.read_bytes()).hexdigest()
    inventory = {"version": 1, "target": target,
                 "candidates": [{"md5": md5, "format": "pdf"}]}
    review = {"file": str(original), "sha256": result["sha256"]}
    for check in ("identity", "edition", "language", "completeness", "physical_pages"):
        review[check] = {"status": "verified", "evidence": "Synthetic fixture evidence."}
    reviews = {"version": 1, "target": target, "candidates": {md5: review}}
    selection = {"status": "ok", "selected": {
        "file": str(original), "md5": md5, "sha256": result["sha256"],
        "size_bytes": original.stat().st_size}}
    for name, value in (("inventory", inventory), ("reviews", reviews), ("selection", selection)):
        batch.write_json(attempt / f"{name}.json", value)
    batch.write_json(Path(result["evidence"]), {
        "inventory": str(attempt / "inventory.json"), "reviews": str(attempt / "reviews.json"),
        "selection": str(attempt / "selection.json"), "installed_file": str(pdf),
        "installed_sha256": result["sha256"], "operation_id": result["operation_id"]})
    monkeypatch.setattr(batch, "verify_operation", lambda *args: None)
    return result, book, args.bib, attempt, inspection


def test_attachment_requires_current_disk_bytes_and_saved_file_field(attached_result):
    result, book, bib, attempt, inspection = attached_result
    batch.validate_result(result, book, bib, attempt, inspection)
    pdf = Path(result["file"])
    pdf.write_bytes(pdf.read_bytes() + b"changed since review")
    with pytest.raises(ValueError, match="SHA-256"):
        batch.validate_result(result, book, bib, attempt, inspection)
    pdf.write_bytes((attempt / "original.pdf").read_bytes())
    other = attempt / "other.pdf"
    other.write_bytes(pdf.read_bytes())
    book["file"] = str(other)
    with pytest.raises(ValueError, match="attachment does not match"):
        batch.validate_result(result, book, bib, attempt, inspection)


@pytest.mark.parametrize("defect", ["wrong-key", "no-operation", "not-pdf", "missing-file", "no-evidence", "outside-evidence"])
def test_attachment_cannot_be_claimed_without_required_evidence(attached_result, defect):
    result, book, bib, attempt, inspection = attached_result
    if defect == "wrong-key":
        result["key"] = "Other"
    elif defect == "no-operation":
        result["operation_id"] = ""
    elif defect == "not-pdf":
        Path(result["file"]).write_bytes(b"not a PDF")
    elif defect == "missing-file":
        Path(result["file"]).unlink()
    elif defect == "no-evidence":
        Path(result["evidence"]).unlink()
    else:
        outside = attempt.parent / "outside.md"
        outside.write_text("outside evidence")
        result["evidence"] = str(outside)
    with pytest.raises(ValueError):
        batch.validate_result(result, book, bib, attempt, inspection)


def test_overlapping_invocation_cannot_start_worker(job, monkeypatch, capsys):
    args, inspection, state = job
    monkeypatch.setattr(batch, "run_agent", lambda *args: pytest.fail("Overlapping worker started"))
    with (args.state_dir / "lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        assert batch.main(cli_args(args)) == 0
    assert "already running" in capsys.readouterr().out
    assert not (args.state_dir / "state.json").exists()


def test_worker_error_stops_later_books(job, monkeypatch):
    args, inspection, state = job
    args.limit = 3
    visited = []

    def worker(command, prompt, attempt, timeout, env):
        visited.append(attempt)
        batch.write_json(attempt / "result.json", result_for(attempt, status="error"))

    monkeypatch.setattr(batch, "run_agent", worker)
    assert batch.execute(args, inspection, state) == 1
    assert len(visited) == 1
    assert batch.read_state(args.state_dir / "state.json")["books"]["Alpha"]["status"] == "error"


def test_pending_operation_on_deferred_result_blocks_retry(job, monkeypatch):
    args, inspection, state = job
    visited = []

    def worker(command, prompt, attempt, timeout, env):
        visited.append(attempt)
        result = result_for(attempt)
        result["operation_id"] = "pending-fixture-operation"
        batch.write_json(attempt / "result.json", result)

    monkeypatch.setattr(batch, "run_agent", worker)
    assert batch.main(cli_args(args)) == 1
    assert batch.main(cli_args(args)) == 1
    assert len(visited) == 1
    record = batch.read_state(args.state_dir / "state.json")["books"]["Alpha"]
    assert record["status"] == "running"
    assert record["operation_id"] == "pending-fixture-operation"


@pytest.mark.parametrize("reply", [
    {"ok": True, "result": "nil", "truncated": False},
    {"ok": False, "result": "t", "truncated": False},
    {"ok": True, "result": "t", "truncated": True},
    {},
])
def test_operation_status_must_confirm_complete_exact_target(tmp_path, monkeypatch, reply):
    monkeypatch.setattr(batch.subprocess, "run", lambda *args, **kwargs:
                        SimpleNamespace(returncode=0, stdout=json.dumps(reply)))
    with pytest.raises(ValueError, match="not complete"):
        batch.verify_operation("operation", "Alpha", tmp_path / "fixture.bib")


def test_operation_query_binds_id_key_and_bibliography(tmp_path, monkeypatch):
    calls = []

    def emacs(command, **kwargs):
        calls.append(command)
        return SimpleNamespace(returncode=0, stdout=json.dumps(
            {"ok": True, "result": "t", "truncated": False}))

    monkeypatch.setattr(batch.subprocess, "run", emacs)
    bib = tmp_path / "fixture.bib"
    batch.verify_operation('operation"with-quote', "Alpha", bib)
    expression = calls[0][-1]
    assert json.dumps('operation"with-quote') in expression
    assert json.dumps("Alpha") in expression
    assert json.dumps(str(bib.resolve())) in expression
    for field in (":key", ":bibfile", ":status", ":pending", ":errors"):
        assert field in expression


@pytest.mark.parametrize("defect", [
    "missing-semantic-check", "changed-original", "wrong-target", "missing-artifact",
    "wrong-selection", "wrong-install-binding",
])
def test_retained_review_is_revalidated_before_accepting_attachment(attached_result, defect):
    result, book, bib, attempt, inspection = attached_result
    if defect == "changed-original":
        original = attempt / "original.pdf"
        original.write_bytes(original.read_bytes() + b"unreviewed bytes")
    elif defect == "missing-artifact":
        (attempt / "inventory.json").unlink()
    else:
        name = "reviews.json" if defect in {"missing-semantic-check", "wrong-target"} else "selection.json"
        path = Path(result["evidence"]) if defect == "wrong-install-binding" else attempt / name
        value = json.loads(path.read_text())
        if defect == "missing-semantic-check":
            del next(iter(value["candidates"].values()))["completeness"]
        elif defect == "wrong-target":
            value["target"]["edition"] = "second"
        elif defect == "wrong-selection":
            value["selected"]["sha256"] = "0" * 64
        else:
            value["installed_sha256"] = "0" * 64
        batch.write_json(path, value)
    with pytest.raises(ValueError):
        batch.validate_result(result, book, bib, attempt, inspection)


def test_timeout_kills_term_ignoring_descendant_after_group_leader_exits(tmp_path):
    """A real child must not continue a delayed write after timeout releases lock."""
    ready = tmp_path / "descendant.pid"
    marker = tmp_path / "late-write"
    source = (
        "import os, signal, time\n"
        "from pathlib import Path\n"
        "if os.fork() == 0:\n"
        "    signal.signal(signal.SIGTERM, signal.SIG_IGN)\n"
        f"    Path({str(ready)!r}).write_text(str(os.getpid()))\n"
        "    time.sleep(0.8)\n"
        f"    Path({str(marker)!r}).write_text('descendant survived timeout')\n"
        "    os._exit(0)\n"
        "time.sleep(30)\n"
    )
    try:
        with pytest.raises(RuntimeError, match="timed out"):
            batch.run_agent([sys.executable, "-c", source], "", tmp_path, 0.3, {})
        assert ready.exists(), "Child never reached the TERM-ignoring state"
        # This bounded delay tests the actual delayed write, not async readiness.
        time.sleep(0.9)
        assert not marker.exists()
    finally:
        if ready.exists():
            try:
                os.kill(int(ready.read_text()), signal.SIGKILL)
            except ProcessLookupError:
                pass


def test_worker_policy_composes_every_broker_container_deny(tmp_path, monkeypatch):
    helper = tmp_path / "broker_policy.py"
    paths = [str(tmp_path / name) for name in ("one", "two", "three")]
    helper.write_text(f"def container_paths(home):\n    return {paths!r}\n")
    monkeypatch.setattr(batch, "OP_SANDBOX_HELPER", helper)
    flags = batch.worker_permission_flags()
    assert flags[::2] == ["-c"] * 3
    config = tomllib.loads("\n".join(flags[1::2]))
    assert config["default_permissions"] == "pdf_acquisition"
    policy = config["permissions"]["pdf_acquisition"]
    assert policy["network"]["enabled"] is True
    assert policy["filesystem"] == {
        ":root": "read", ":workspace_roots": "write",
        ":tmpdir": "write", ":slash_tmp": "write",
        **{path: "deny" for path in paths},
    }


def test_missing_broker_policy_stops_before_worker_or_ledger(job, monkeypatch):
    args, inspection, state = job
    monkeypatch.setattr(batch, "OP_SANDBOX_HELPER", args.state_dir / "missing.py")
    monkeypatch.setattr(batch, "run_agent", lambda *a: pytest.fail("Worker launched"))
    assert batch.main(cli_args(args)) == 1
    assert not (args.state_dir / "state.json").exists()


def test_worker_uses_runtime_and_named_policy_without_legacy_override(job, monkeypatch):
    args, inspection, state = job
    commands = []
    def worker(command, prompt, attempt, timeout, env):
        commands.append(command)
        batch.write_json(attempt / "result.json", result_for(attempt))
    monkeypatch.setattr(batch, "run_agent", worker)
    assert batch.execute(args, inspection, state) == 0
    command = commands[0]
    assert command[0] == str(Path.home() / "My Drive/dotfiles/bin/codex-runtime")
    assert 'default_permissions="pdf_acquisition"' in command
    assert "--sandbox" not in command
    assert not any("sandbox_workspace_write" in part for part in command)
    assert 'approval_policy="never"' in command
    assert command.count("--add-dir") == 3


REFUSED_ROUTE_REASONS = [
    "Browser security policy rejected Anna's Archive access. Other searched sources yielded no eligible PDF.",
    "Required Playwright browser tools are missing, blocking Anna's Archive search. Availability remains unresolved.",
    "paper-fetch book-candidates returned needs-browser; no other source had a PDF.",
    "Anna's Archive returned HTTP 403 (DDoS-Guard browser challenge).",
]
SYSTEMIC_REASONS = [
    "Reconciled: the agent-guard locale bug denied every worker tool call, so nothing ran.",
    "Anna's Archive access was denied and the CLI staging lacked its secret key.",
    "Emacs was unavailable, so the browser route could not be recorded.",
    "paper-fetch: dedicated runtime missing; Anna's search needs-browser.",
    "No acceptable candidate",
]


def run_one(job, monkeypatch, status, reason, limit=3):
    args, inspection, state = job
    args.limit = limit
    visited = []

    def worker(command, prompt, attempt, timeout, env):
        key = json.loads((attempt / "input.json").read_text())["book"]["key"]
        visited.append(key)
        result = result_for(attempt, key, status)
        result["reason"] = reason
        batch.write_json(attempt / "result.json", result)

    monkeypatch.setattr(batch, "run_agent", worker)
    code = batch.execute(args, inspection, state)
    return code, visited, batch.read_state(args.state_dir / "state.json")


@pytest.mark.parametrize("reason", REFUSED_ROUTE_REASONS)
def test_refused_route_reported_as_error_is_a_deferral_that_does_not_stop_the_queue(job, monkeypatch, reason):
    code, visited, state = run_one(job, monkeypatch, "error", reason)
    assert code == 0
    assert visited == ["Alpha", "Beta", "Gamma"]
    record = state["books"]["Alpha"]
    assert record["status"] == "deferred"
    assert record["worker_status"] == "error"
    assert record["reason"] == reason


@pytest.mark.parametrize("reason", SYSTEMIC_REASONS)
def test_systemic_worker_error_still_fails_and_stops_the_queue(job, monkeypatch, reason):
    code, visited, state = run_one(job, monkeypatch, "error", reason)
    assert code == 1
    assert visited == ["Alpha"]
    assert state["books"]["Alpha"]["status"] == "error"
    assert "worker_status" not in state["books"]["Alpha"]


@pytest.mark.parametrize("reason", [
    *REFUSED_ROUTE_REASONS,
    "No acceptable candidate",
    "Edition unresolved: the entry credits Sidney Hook as editor; no attachment began.",
    "Contradictory record for The Crash of 1929; bibliography unchanged.",
])
def test_per_book_deferral_stays_deferred(job, monkeypatch, reason):
    code, visited, state = run_one(job, monkeypatch, "deferred", reason)
    assert code == 0
    assert visited == ["Alpha", "Beta", "Gamma"]
    assert state["books"]["Alpha"]["status"] == "deferred"
    assert "worker_status" not in state["books"]["Alpha"]


@pytest.mark.parametrize("reason", [
    "Anna's Archive browser access was denied by security policy; CLI staging lacked its secret key.",
    "No verified PDF. Browser policy denied Anna's Archive access; download credentials were unavailable.",
    "Remote staging lacked a member key; independent searches found no copy.",
    "1Password authentication timed out before staging.",
    "Emacs was unavailable, so the attachment could not be prepared.",
    "The paper-fetch runtime is missing.",
    "Worker shell commands were refused by the sandbox.",
    "A security-hook import crash blocked every command.",
])
def test_deferral_naming_a_systemic_failure_fails_the_job(job, monkeypatch, reason):
    code, visited, state = run_one(job, monkeypatch, "deferred", reason)
    assert code == 1
    assert visited == ["Alpha"]
    record = state["books"]["Alpha"]
    assert record["status"] == "error"
    assert record["worker_status"] == "deferred"
    assert record["reason"] == reason


def test_refused_route_with_a_started_operation_still_requires_reconciliation():
    result = {"status": "error", "reason": REFUSED_ROUTE_REASONS[0], "operation_id": "op-1", "file": ""}
    assert batch.outcome_status(result) == "error"
