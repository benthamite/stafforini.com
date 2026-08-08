# Decision records (summary)

Full details with rejected alternatives and evidence are in `decisions/`. Read the relevant file before proposing changes to a covered subsystem.

| #   | Topic | Decision | Status |
|-----|-------|----------|--------|
| 001 | migration.bib rebuild source | Use the WordPress export (`scripts/wp-quotes-matched.json`) as the authoritative source; LLM only for entries with no WP coverage. | Final |
| 002 | rebuild gating | Plausibility-check the existing migration entry first; only replace when provably wrong (cite-key surname mismatch in author, no title overlap, all-caps title, placeholder location). | Final |
| 003 | short stories inside books | Story gets its own `@incollection{StoryKey, crossref = {BookKey}}`; book entry stays separate; quotes attribute to the story key, not the book key. | Final |
| 004 | cite-key character set | ASCII alphanumerics + hyphens (for compound surnames) only. No accents, periods, underscores, semicolons. Use `scripts/rename-cite-keys.py` for renames. | Final |
| 005 | Netlify redirect rules | Source paths must be percent-encoded (sub-delimiters stay raw); `static/_redirects` is processed before `netlify.toml`, so grep it for a shadowing rule before adding one there. | Final |
| 006 | deploy cost | Dominated by total file count, not changed-file count. ~30 min for `--quick`; hashing and re-upload already ruled out as causes. Do not undo the fingerprinting removal or `.netlifyignore`. | Final |
| 007 | duplicate work pages | Byte-identical pages keep a `canonical` to the survivor and leave the sitemap; they stay crawlable and are not noindexed. Do not delete the redundant bib entry — most live in shared babel-refs and some are cited. | Final |
| 008 | Search Console validation | Validate only issues whose examples actually resolve. Never validate "Page with redirect" or "Not found (404)" — both fail by design, since their URLs are supposed to redirect or be gone. Watch newly crawled examples, not totals. | Final |
| 009 | Note attachments | Anki decks and other note binaries live in the notes repo and reach the build via Hugo `[[module.mounts]]`, not `static/`. A missing mount source is silent — the build just omits the files. If they vanish, fix the mount; do not re-import. | Final |
| 010 | Redirect-target audit | `scripts/audit-redirect-targets.py` is standalone on purpose: wiring it into `verify-site.py` today would block every deploy on two dead quote rules awaiting a content decision. Wire it in once those are resolved. | Re-evaluate |
