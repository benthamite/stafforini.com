# Book acquisition

The website has no book-acquisition command of its own. The legacy
`scripts/download-missing-pdfs.py` entry point was retired: its separate
discovery bypassed paper-fetch's own candidate routes. The scheduled batch below
uses `scripts/missing_pdf_inventory.py` only to list existing `old.bib` books
whose PDF is missing or broken, and to read attachments back.

For manual work, use dotfiles `bin/paper-fetch` for the complete candidate
workflow: `book-candidates`, `book-stage`, `book-inspect`, and `book-select`.
Its maintained command and manifest contract is in
`~/My Drive/dotfiles/docs/book-acquisition.md`. Bibliographic and attachment
decisions belong to the single shared policy at
`~/My Drive/dotfiles/agents/bibliography-policy.md`. Search observations,
filenames, scan flags and reported sizes do not approve a file. A selected file
goes through Zotra/Ebib for attachment and bibliography persistence; a
successful selection is not an attached file, a published PDF, or proof that
every provider and candidate was searched.

## Scheduled acquisition

`scripts/download-missing-pdfs-cron.sh` runs
`scripts/download-missing-pdfs-batch.py` using the pinned paper-fetch Python
environment. It retains the existing schedule and log destination. No plist
replacement or reload is needed when the wrapper changes.

The driver inventories missing/broken PDFs in the existing `old.bib` entries.
It runs five serial Codex workers per invocation, with a 20-minute limit per
entry. Unattempted entries precede the least recently attempted entries, so an
unavailable book does not monopolize subsequent nights. `--limit`, `--only` and
`--timeout` bound manual runs; `--dry-run` shows the queue without starting an
agent or changing files. `--bib` and `--state-dir` support isolated checks.

Each worker follows `scripts/download-missing-pdfs-task.md`, which points to
the shared policy and the `add-bib-entry` book procedure. The worker verifies
the existing edition, inspects rendered pages, records hash-bound reviews,
selects the smallest verified eligible copy, and stops at `staged`: it saves
`selection.json` (with the entry identity it matched and the verified
language) and a byte-identical copy of the selected PDF in its attempt
directory. It never writes the bibliography or contacts Emacs, so the job runs
whether or not Pablo's Emacs is up. Review is performed by the agent. It is not a
requirement for a person to approve every download. Ambiguous editions and
unavailable files remain explicit deferred outcomes; infrastructure failures
remain errors. This maintenance task does not rewrite the bibliography to a
different edition or rename existing citation keys.

Private queue state and per-attempt input, events, results and acquisition
evidence live under `/Users/pablostafforini/.local/state/download-missing-pdfs/`.
A process lock prevents overlapping batches. Before recording `staged`, the
driver reruns `book-select` against the retained reviews, checks the copy's
hash against the selected bytes and the saved identity against the entry, and
counts `staged` as success. Interrupted runs retain their evidence for
reconciliation. Never delete that state to hide a failed operation.

Attachment happens in Emacs: `ebib-extras-pdf-queue-drain` (dotfiles
`emacs/extras/ebib-extras.el`, run on an idle timer by
`ebib-extras-pdf-queue-mode`) claims each staged attempt, rechecks the entry's
identity, attaches the copy through the noninteractive Ebib operation, verifies
the result and records `attached` or `attach-error`. Emacs pulls this work; the
job never calls Emacs. Both writers change `state.json` one record at a time
under a shared lock (dotfiles `bin/pdf-queue-lock`). The batch does not select
`staged`, `attaching` or `attach-error` books; an `attach-error` book waits
until Pablo clears it with `ebib-extras-pdf-queue-clear`.

The one exception is a worker whose turn failed before the model acted, as when
the network is down at the scheduled time. Its saved events hold only the
session start, connection retries and the failed turn, so no Ebib operation can
be pending. The next batch releases that record and retries the book first.

Codex uses the account named in `~/.codex-current-account`, resolved through
dotfiles `bin/agent-account-home` (the account list Emacs also reads), with
normal configuration/hooks, explicit workspace-write permissions and network
access. A pool selection fails the job: pool routing needs the running Emacs.
Its only writable directory is the queue state directory. No API key or
sandbox-bypass flag is supplied by this job. `--preflight` (also accepted by the
cron wrapper, which then writes no log) resolves all of this without starting
a worker; the launchd environment check runs it.

The wrapper no longer commits the entire `old.bib` file: that could include
unrelated user edits. Reviewed attachments are persisted through Ebib. The old
downloader's progress file is preserved but is not proof of review or used as a
completion ledger.
