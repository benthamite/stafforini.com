# Decision records (summary)

Full details with rejected alternatives and evidence are in `decisions/`. Read the relevant file before proposing changes to a covered subsystem.

| #   | Topic | Decision | Status |
|-----|-------|----------|--------|
| 001 | migration.bib rebuild source | Use the WordPress export (`scripts/wp-quotes-matched.json`) as the authoritative source; LLM only for entries with no WP coverage. | Final |
| 002 | rebuild gating | Plausibility-check the existing migration entry first; only replace when provably wrong (cite-key surname mismatch in author, no title overlap, all-caps title, placeholder location). | Final |
| 003 | short stories inside books | Story gets its own `@incollection{StoryKey, crossref = {BookKey}}`; book entry stays separate; quotes attribute to the story key, not the book key. | Final |
| 004 | cite-key character set | ASCII alphanumerics + hyphens (for compound surnames) only. No accents, periods, underscores, semicolons. Use `scripts/rename-cite-keys.py` for renames. | Final |
