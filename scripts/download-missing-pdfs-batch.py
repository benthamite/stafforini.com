#!/usr/bin/env python3
"""Run bounded, serial agent reviews for existing books missing their PDF.

Workers acquire, review and select a PDF, then stop at `staged`: they never
touch the bibliography or Pablo's running Emacs. Emacs attaches staged PDFs
itself (`ebib-extras-pdf-queue-drain' in dotfiles), pulling work from this
job's state directory; this job never calls Emacs.
"""

from __future__ import annotations

import argparse
import fcntl
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import time

sys.dont_write_bytecode = True
SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_STATE = Path.home() / ".local/state/download-missing-pdfs"
DEFAULT_BIB = Path.home() / "My Drive/bibliography/old.bib"
CODEX = str(Path.home() / "My Drive/dotfiles/bin/codex-runtime")
OP_SANDBOX_HELPER = Path.home() / "My Drive/dotfiles/bin/op_automations_sandbox.py"
ACCOUNT_HOME = Path.home() / "My Drive/dotfiles/bin/agent-account-home"
QUEUE_LIBRARY = Path.home() / "My Drive/dotfiles/lib/python/pdf_queue_state.py"
IDENTITY_FIELDS = ("title", "edition", "date", "isbn", "language")


def adapter():
    spec = importlib.util.spec_from_file_location(
        "missing_pdf_inventory", SCRIPT_DIR / "missing_pdf_inventory.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def queue_state():
    """The ledger rules shared with the Emacs drain (dotfiles pdf_queue_state)."""
    module = sys.modules.get("pdf_queue_state")
    if module is None:
        spec = importlib.util.spec_from_file_location("pdf_queue_state", QUEUE_LIBRARY)
        if spec is None or spec.loader is None:
            raise ValueError(f"Cannot load the PDF queue ledger library: {QUEUE_LIBRARY}")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        sys.modules["pdf_queue_state"] = module
    return module


def read_state(path):
    if not path.exists():
        return {"version": 1, "books": {}}
    return queue_state().validate(json.loads(path.read_text()), str(path))


def save_record(args, state, key, record):
    """Write KEY's record (None deletes it) without touching other records.

    The drain changes staged records concurrently, so the ledger is re-read
    under its shared lock and only KEY changes. STATE is refreshed in place.
    """
    if record is None:
        fresh = queue_state().update_record(args.state_dir, key, delete=True)
    else:
        fresh = queue_state().update_record(args.state_dir, key, record=record)
    state["books"] = fresh["books"]
    if record is not None:
        state["books"][key] = record


def write_json(path, value):
    """Replace one private state file atomically on its own filesystem."""
    temporary = None
    try:
        with tempfile.NamedTemporaryFile("w", dir=path.parent, delete=False) as handle:
            temporary = Path(handle.name)
            json.dump(value, handle, indent=2)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        temporary.replace(path)
    finally:
        if temporary is not None and temporary.exists():
            temporary.unlink()


def select_books(books, state, limit, only=""):
    """Choose the least recently attempted books the drain does not own.

    Staged, claimed (attaching) and attach-error records wait for Emacs or for
    Pablo; acquiring them again would duplicate a reviewed PDF.
    """
    records = state["books"]
    owned = queue_state().DRAIN_OWNED
    selected = [book for book in books if (not only or book["key"] == only)
                and records.get(book["key"], {}).get("status") not in owned]
    return sorted(selected, key=lambda book: (
        records.get(book["key"], {}).get("last_attempt", 0), book["key"]))[:limit]


def agent_environment(environ=None):
    env = dict(os.environ if environ is None else environ)
    for name in list(env):
        if name.endswith("_BUFFER_NAME") or name in {
            "CODEX_THREAD_ID", "CODEX_SESSION_ID", "CLAUDE_SESSION_ID",
            "CLAUDECODE", "CODEX_INTERNAL_ORIGINATOR_OVERRIDE",
        }:
            env.pop(name, None)
    # The persisted selection must name a plain account: pool routing needs
    # the running Emacs, which an unattended job must not consult.
    home = Path(env.get("HOME", str(Path.home())))
    marker = home / ".codex-current-account"
    if not env.get("CODEX_HOME") and marker.exists():
        account = "".join(marker.read_text().split())
        if not re.fullmatch(r"[A-Za-z0-9_-]+", account):
            raise ValueError("Invalid Codex account selector")
        completed = subprocess.run(
            [sys.executable, str(ACCOUNT_HOME), "codex", account],
            capture_output=True, text=True, timeout=20, env=env)
        if completed.returncode:
            detail = completed.stderr.strip() or completed.stdout.strip() or "no diagnostic output"
            raise ValueError(
                f"Could not resolve Codex account {account} "
                f"(exit {completed.returncode}): {detail[:2000]}")
        lines = completed.stdout.splitlines()
        if len(lines) != 1 or not lines[0]:
            raise ValueError("Codex account has no configured home")
        selected = Path(lines[0])
        if not selected.is_absolute() or not selected.is_dir():
            raise ValueError(f"Selected Codex account directory is missing: {selected}")
        env["CODEX_HOME"] = str(selected)
    return env


def worker_permission_flags():
    """Compose the broker's app-data denial with the worker's write limits.

    macOS refuses nested Seatbelt initialization. The broker can reuse this
    sandbox only after its own policy checker confirms all container denials.
    Discover those paths through the broker's maintained implementation.
    """
    spec = importlib.util.spec_from_file_location("pdf_broker_sandbox", OP_SANDBOX_HELPER)
    if spec is None or spec.loader is None:
        raise ValueError("Cannot load the credential broker's sandbox policy")
    policy = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(policy)
    filesystem = {":root": "read", ":workspace_roots": "write",
                  ":tmpdir": "write", ":slash_tmp": "write"}
    filesystem.update({str(path): "deny" for path in policy.container_paths(Path.home())})
    rules = ",".join(f"{json.dumps(path)}={json.dumps(access)}"
                     for path, access in filesystem.items())
    return ["-c", 'default_permissions="pdf_acquisition"',
            "-c", f"permissions.pdf_acquisition.filesystem={{{rules}}}",
            "-c", "permissions.pdf_acquisition.network.enabled=true"]


def run_agent(command, prompt, attempt, timeout, env):
    """Bound the owned child group; never signal the persistent Emacs server."""
    with (attempt / "events.jsonl").open("w") as stdout, (attempt / "stderr.log").open("w") as stderr:
        process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=stdout,
                                   stderr=stderr, cwd=attempt, env=env,
                                   start_new_session=True, text=True)
        try:
            process.communicate(prompt, timeout=timeout)
        except BaseException as exc:
            # Only this process group was created by this invocation. The
            # running record stays unfinished and blocks retry until reconciled.
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                pass
            # The leader exiting does not prove its tools exited. Kill any
            # remaining members of this owned group, including TERM-ignoring
            # descendants, before releasing the batch lock.
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
            if isinstance(exc, subprocess.TimeoutExpired):
                raise RuntimeError(f"Agent timed out after {timeout}s; reconcile pending Ebib operation") from exc
            raise
    if process.returncode:
        raise RuntimeError(f"Agent exited {process.returncode}; see {attempt / 'stderr.log'}")


def artifact_path(value, attempt):
    if not isinstance(value, str) or not value:
        raise ValueError("Review artifact path is missing")
    path = Path(value)
    if (not path.is_absolute() or not path.resolve().is_relative_to(attempt.resolve())
            or not path.is_file()):
        raise ValueError("Review artifact must be a real file inside this attempt")
    return path


def identity(book):
    """The bibliographic identity a staged PDF was reviewed against.

    The drain compares it with the entry before attaching, so an entry edited
    in the meantime is not given a PDF reviewed for different metadata.
    """
    fields = {name: " ".join(str(book.get(name) or "").split()) for name in IDENTITY_FIELDS}
    return {"key": book["key"], "type": "book", **fields}


def validate_review(result, attempt, inspection, book):
    """Recheck retained byte-bound review structure, not semantic judgments."""
    evidence = json.loads(artifact_path(result["evidence"], attempt).read_text())
    if not isinstance(evidence, dict):
        raise ValueError("Review evidence must be a JSON object")
    for name, expected in (("staged_file", result["file"]),
                           ("staged_sha256", result["sha256"])):
        if evidence.get(name) != expected:
            raise ValueError(f"Review evidence disagrees with result: {name}")
    inventory_path = artifact_path(evidence.get("inventory"), attempt)
    reviews_path = artifact_path(evidence.get("reviews"), attempt)
    selection_path = artifact_path(evidence.get("selection"), attempt)
    reviews = json.loads(reviews_path.read_text())
    selection = json.loads(selection_path.read_text())
    if (not isinstance(selection, dict) or selection.get("status") != "ok"
            or not isinstance(selection.get("selected"), dict)):
        raise ValueError("Saved book selection is not successful")
    selected = selection["selected"]
    original = artifact_path(selected.get("file"), attempt)
    if original.resolve() == Path(result["file"]).resolve():
        raise ValueError("Staged copy must be separate from the reviewed original")
    if selected.get("sha256") != result["sha256"]:
        raise ValueError("Staged copy is not byte-identical to the selected PDF")
    if selection.get("identity") != identity(book):
        raise ValueError("Saved selection identity does not match the entry it was reviewed for")
    language = selection.get("language")
    if not isinstance(language, str) or not re.fullmatch(r"[a-z][a-z-]*", language):
        raise ValueError("Saved selection lacks the verified document language")
    library = inspection.paper_fetch
    try:
        inventory = library.read_book_candidates(inventory_path)
        # Shared selection validates matching targets, all required verified
        # checks, and each candidate's retained PDF against its review hashes.
        actual = library.select_book_candidate(inventory, reviews)
    except library.PaperFetchError as exc:
        raise ValueError(f"Invalid retained book review: {exc}") from exc
    if actual.get("status") != "ok" or not actual.get("selected"):
        raise ValueError("Retained reviews no longer select an eligible PDF")
    for name in ("md5", "sha256", "size_bytes", "file"):
        if name not in selected or selected[name] != actual["selected"].get(name):
            raise ValueError(f"Saved selection disagrees with retained reviewed PDF: {name}")


# The task prompt separates per-book outcomes from systemic failures, but
# workers blur them, so the driver applies the rule to the reason. A refused or
# challenged acquisition route is a deferral, so one book cannot stop the
# queue. A reason naming a systemic failure (credentials, Emacs, a crash, the
# runtime, the agent's own guard or sandbox) is an error even when the worker
# said "deferred", so a broken credential path fails the job visibly. A reason
# naming both is an error. Systemic terms are phrased narrowly enough that
# bibliographic text such as "Sidney Hook" or "The Crash of 1929" does not match.
ROUTE_REFUSAL_RE = re.compile(
    r"security policy|browser policy|access (?:was )?(?:denied|refused|rejected|blocked)"
    r"|(?:denied|refused|rejected|blocked)\b[^.]*\baccess|needs-browser|browser challenge|ddos-guard"
    r"|http 403|required approval|approval forbidden|browser (?:tools|access|route)", re.I)
SYSTEMIC_FAILURE_RE = re.compile(
    r"credential|secret key|member key|api key|authentication|1password|\bemacs\b|traceback"
    r"|\bcrashed\b|\b(?:import|worker|agent|process|helper|tool|hook) crash|runtime|agent-guard"
    r"|tool calls?\b|security-hook|\bhooks? (?:crash|fail|error|denied|blocked)|sandbox|permission profile",
    re.I)


def outcome_status(result):
    """The ledger status for a validated worker result."""
    reason = result["reason"]
    if result["status"] == "deferred" and SYSTEMIC_FAILURE_RE.search(reason):
        return "error"
    if (result["status"] == "error" and not result["operation_id"] and not result["file"]
            and ROUTE_REFUSAL_RE.search(reason) and not SYSTEMIC_FAILURE_RE.search(reason)):
        return "deferred"
    return result["status"]


def validate_result(result, book, attempt, inspection):
    fields = {"status", "key", "reason", "file", "sha256", "operation_id", "evidence"}
    if (not isinstance(result, dict) or set(result) != fields
            or any(not isinstance(result[field], str) for field in fields)):
        raise ValueError("Agent result has invalid fields")
    if result["key"] != book["key"] or result["status"] not in {"staged", "deferred", "error"}:
        raise ValueError("Agent result has wrong key or status")
    if not result["reason"].strip():
        raise ValueError("Agent result requires an explicit reason")
    evidence = Path(result["evidence"])
    if not evidence.is_absolute() or not evidence.resolve().is_relative_to(attempt.resolve()) or not evidence.is_file():
        raise ValueError("Agent evidence must be a real file inside this attempt")
    if result["status"] != "staged":
        return
    if result["operation_id"]:
        raise ValueError("A staged result must not start an Ebib operation")
    pdf = Path(result["file"])
    if (not pdf.is_absolute() or pdf.suffix.lower() != ".pdf" or not pdf.is_file()
            or pdf.stat().st_size == 0 or not pdf.resolve().is_relative_to(attempt.resolve())):
        raise ValueError("Staged copy is not a nonempty PDF inside this attempt")
    with pdf.open("rb") as handle:
        if handle.read(5) != b"%PDF-":
            raise ValueError("Staged copy lacks a PDF header")
        handle.seek(0)
        digest = hashlib.file_digest(handle, "sha256").hexdigest()
    if digest != result["sha256"]:
        raise ValueError("Staged copy SHA-256 does not match the reported hash")
    validate_review(result, attempt, inspection, book)


# Events a worker emits when its turn fails before the model acts: the session
# opens, the connection is retried and the turn fails. Any other event (a
# message, a command, a tool call, a file change) means work may have begun.
IDLE_EVENTS = {"thread.started", "turn.started", "error", "turn.failed"}


def attempt_never_acted(record):
    """Whether a running record's saved events prove its worker did nothing.

    A worker that cannot reach the model exits nonzero without running a tool,
    so no Ebib operation can be pending. Anything unreadable, unexpected or
    absent leaves the record unfinished.
    """
    if record.get("operation_id") or not isinstance(record.get("attempt"), str):
        return False
    attempt = Path(record["attempt"])
    if (attempt / "result.json").exists():
        return False
    try:
        events = [json.loads(line) for line in (attempt / "events.jsonl").read_text().splitlines()]
    except (OSError, ValueError):
        return False
    failed = False
    for event in events:
        if not isinstance(event, dict):
            return False
        kind = event.get("type")
        if kind in {"item.started", "item.completed"}:
            item = event.get("item")
            if not isinstance(item, dict) or item.get("type") != "error":
                return False
        elif kind not in IDLE_EVENTS:
            return False
        failed = failed or kind == "turn.failed"
    return failed


def execute(args, inspection, state):
    # A turn that failed before acting must not block every later batch. The
    # released book returns to the front of the queue as unattempted.
    released = [key for key, record in state["books"].items()
                if record.get("status") == "running" and attempt_never_acted(record)]
    if released:
        print("Released attempts that failed before the worker acted: " + ", ".join(released),
              file=sys.stderr)
    for key in released:
        if args.dry_run:
            del state["books"][key]
        else:
            save_record(args, state, key, None)
    unfinished = [key for key, record in state["books"].items()
                  if record.get("status") == "running"
                  or (record.get("status") in {"error", "deferred"} and record.get("operation_id"))]
    if unfinished:
        raise RuntimeError("Incomplete prior attempt; reconcile evidence and pending Ebib operations before retry: " + ", ".join(unfinished))
    books = inspection.books_missing_pdf(inspection.parse_bib_books(args.bib), include_broken=True)
    queue = select_books(books, state, args.limit, args.only)
    if args.dry_run:
        print(json.dumps({"books": queue}, indent=2))
        return 0
    env = agent_environment()
    permission_flags = worker_permission_flags()
    template = (SCRIPT_DIR / "download-missing-pdfs-task.md").read_text()
    for book in queue:
        attempt = Path(tempfile.mkdtemp(prefix="attempt-", dir=args.state_dir))
        write_json(attempt / "input.json", {"book": book, "identity": identity(book),
                                            "bibliography": str(args.bib), "attempt": str(attempt)})
        record = {"last_attempt": time.time(), "status": "running", "attempt": str(attempt)}
        save_record(args, state, book["key"], record)
        prompt = template + "\n\nRead the exact job input at: " + str(attempt / "input.json") + "\n"
        (attempt / "prompt.md").write_text(prompt)
        # The worker writes only its attempt directory: attachment, and with it
        # every bibliography and library write, belongs to the Emacs drain.
        command = [CODEX, "exec", *permission_flags, "-c", 'approval_policy="never"',
                   "--skip-git-repo-check",
                   "--cd", str(attempt), "--add-dir", str(args.state_dir),
                   "--json", "--output-schema", str(SCRIPT_DIR / "download-missing-pdfs-result.schema.json"),
                   "--output-last-message", str(attempt / "result.json"), "-"]
        run_agent(command, prompt, attempt, args.timeout, env)
        result = json.loads((attempt / "result.json").read_text())
        validate_result(result, book, attempt, inspection)
        if result["operation_id"]:
            record.update(operation_id=result["operation_id"], reason=result["reason"])
            save_record(args, state, book["key"], record)
            raise RuntimeError(f"Unexpected Ebib operation for {book['key']}; reconcile {attempt}")
        status = outcome_status(result)
        if status != result["status"]:
            record["worker_status"] = result["status"]
            result = {**result, "status": status, "worker_status": record["worker_status"]}
        record.update(status=status, reason=result["reason"], operation_id=result["operation_id"])
        save_record(args, state, book["key"], record)
        print(json.dumps(result), flush=True)
        if result["status"] == "error":
            return 1
    return 0


def preflight(args):
    """Resolve everything a run needs without starting a worker or writing.

    Imports the inventory and paper-fetch modules, reads the bibliography and
    ledger, resolves the Codex account home and runtime, and composes the
    worker's sandbox policy. Exit 0 when ready.
    """
    inspection = adapter()
    books = inspection.books_missing_pdf(inspection.parse_bib_books(args.bib), include_broken=True)
    state = read_state(args.state_dir / "state.json")
    env = agent_environment()
    if not os.access(CODEX, os.X_OK):
        raise ValueError(f"Codex runtime is not executable: {CODEX}")
    worker_permission_flags()
    for name in ("download-missing-pdfs-task.md", "download-missing-pdfs-result.schema.json"):
        if not (SCRIPT_DIR / name).is_file():
            raise ValueError(f"Missing worker input: {SCRIPT_DIR / name}")
    print(json.dumps({"ready": True, "missing_books": len(books),
                      "eligible_books": len(select_books(books, state, len(books) or 1)),
                      "codex_home": env.get("CODEX_HOME", ""),
                      "python": sys.executable}))
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bib", type=Path, default=DEFAULT_BIB)
    parser.add_argument("--state-dir", type=Path, default=DEFAULT_STATE)
    parser.add_argument("--limit", type=int, default=5)
    parser.add_argument("--only", default="")
    parser.add_argument("--timeout", type=float, default=1200)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--preflight", action="store_true",
                        help="check the runtime without starting a worker or writing state")
    args = parser.parse_args(argv)
    if args.limit < 1 or args.timeout <= 0:
        parser.error("--limit and --timeout must be positive")
    args.bib = args.bib.expanduser().resolve()
    args.state_dir = args.state_dir.expanduser().resolve()
    if args.state_dir.is_relative_to((Path.home() / "My Drive").resolve()):
        parser.error("--state-dir must be outside Google Drive")
    try:
        if args.preflight:
            return preflight(args)
        inspection = adapter()
        if args.dry_run:
            return execute(args, inspection, read_state(args.state_dir / "state.json"))
        os.umask(0o077)
        args.state_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
        with (args.state_dir / "lock").open("a") as lock:
            try:
                fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError:
                print("Another PDF acquisition batch is already running")
                return 0
            return execute(args, inspection, read_state(args.state_dir / "state.json"))
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
        print(f"PDF batch failed: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
