# Book acquisition

`scripts/download-missing-pdfs.py` is an inspection adapter to the shared
dotfiles acquisition library. It does not accept an automatically ranked book,
write `old.bib`, or replay old progress into bibliography attachments. Its
legacy mutating modes stop with `needs-review` (exit 5). The scheduled wrapper
uses the reviewed acquisition workflow below instead of those retired modes.

`--dry-run` still inventories candidates for books in `old.bib`. It preserves
unknown edition/language fields and provider errors. Search observations,
filenames, scan flags and reported sizes do not approve a file.

Use dotfiles `bin/paper-fetch` for the complete candidate workflow:
`book-candidates`, `book-stage`, `book-inspect`, and `book-select`. Its maintained
command and manifest contract is in `~/My Drive/dotfiles/docs/book-acquisition.md`.
Bibliographic and attachment decisions belong to the single shared policy at
`~/My Drive/dotfiles/agents/bibliography-policy.md`.

The website adapter accepts `--candidates CANDIDATES.json --reviews REVIEWS.json`
to report the same reviewed selection. That operation is read-only. The selected
file goes through Zotra/Ebib for attachment and bibliography persistence. A
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
the shared policy and `add-bib-entry` attachment procedure. The worker verifies
the existing edition, inspects rendered pages, records hash-bound reviews,
selects the smallest verified eligible copy, and attaches through the explicit
noninteractive Ebib operation. Review is performed by the agent. It is not a
requirement for a person to approve every download. Ambiguous editions and
unavailable files remain explicit deferred outcomes; infrastructure failures
remain errors. This maintenance task does not rewrite the bibliography to a
different edition or rename existing citation keys.

Private queue state and per-attempt input, events, results and acquisition
evidence live under `/Users/pablostafforini/.local/state/download-missing-pdfs/`.
A process lock prevents overlapping batches. The driver checks the worker's
result against the saved bibliography and installed file hash before recording
an attachment. Interrupted operations retain their evidence for reconciliation;
file existence alone cannot turn them into successes. Never delete that state
to hide a failed operation.

Codex uses the selected local account and normal configuration/hooks, with
explicit workspace-write permissions and network access. Its writable work
directory is the attempt directory, with the bibliography and PDF library added
for attachment. No API key or sandbox-bypass flag is supplied by this job.
Emacs must already be available; the job never starts or restarts it.

The wrapper no longer commits the entire `old.bib` file: that could include
unrelated user edits. Reviewed attachments are persisted through Ebib. The old
downloader's progress file is preserved but is not proof of review or used as a
completion ledger.
