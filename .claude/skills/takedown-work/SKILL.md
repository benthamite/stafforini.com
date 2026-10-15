---
name: takedown-work
description: Take a work down from stafforini.com when an author, co-author or person named in it asks for removal. Use when given a /works/ URL, slug or cite key to remove, unpublish or take down. Not for redirect fixes, GSC alerts, or deleting bibliography entries.
---

# Take down a work

A takedown hides a work from the site. It does not delete anything from the
bibliography. The `.bib` entries (including the shared babel-refs bibs) and
the source PDF in `~/My Drive/library-pdf/` stay untouched. Only the site's
generated copies go away.

The mechanism is the blocklist `data/excluded-works.json`. Once a cite key is
there, the generators drop its work page, quotes, PDF and thumbnail; citations
of it are unlinked; and `scripts/upload-pdfs.sh` deletes its R2 objects.
Precedents: `git log --oneline --grep='take down'`.

## Steps

1. **Resolve the cite key.** Map the slug to its entry in `data/works.json`,
   then find the cite key in the files listed in `scripts/lib.py:BIB_FILES`.
   The slug is the kebab-case of the cite key, so grep the bibs for the title.
2. **Look for sibling entries of the same work.** Grep every bib for the
   title. Talks, forum posts, transcripts and reprints often have separate
   entries with their own pages, and one of them may host the PDF. These go
   on the list of decisions; recommend taking them down too unless the
   request is clearly limited to one listing.
3. **Ask once**, as a single numbered list: which entries to take down, the
   `reason` wording (who asked: author, co-author, or person named in the
   work), the redirect removal, and the full deploy. Approval covers the
   rest of the task.
4. **Add each cite key** to `data/excluded-works.json` (keys sorted,
   `added` = today, `reason` = "Requested by <who> on <date>").
5. **Remove redirects to the dropped pages.** Grep `static/_redirects` and
   `netlify.toml` for `/works/<slug>/` as a target and delete those rules,
   so they don't become redirect → 404 chains.
6. **Commit** only those two files:
   `works: take down <CiteKey> at <who>'s request`. Leave unrelated dirty
   `data/*.json` files alone; the deploy regenerates them.
7. **Remove the local generated copies:** run
   `python3 scripts/generate-work-pages.py` (deletes `content/works/<slug>.md`;
   expect "Takedown pages removed: N"), then `python3 scripts/process-pdfs.py`
   (deletes `static/pdfs/<slug>.pdf` and the thumbnail as stale). `deploy.sh`
   does not run the work-page generator, and its note export runs
   `verify-site.py`, which aborts the deploy while these files exist.
   `python3 scripts/verify-site.py --build dev` must print OK before step 8.
8. **Run the full deploy:** `bash scripts/deploy.sh`, never `--quick` (quick
   skips the R2 step that deletes the PDF and thumbnail). Check the exit
   code; the deploy can fail without publishing anything.
9. **Verify** each slug:
   - `curl -sI https://stafforini.com/works/<slug>/` → 404
   - `curl -sI "https://pdf.stafforini.com/<slug>.pdf?nocache=1"` and the
     `.png` thumbnail → 404. Without the query string, Cloudflare's edge can
     still serve 200 from cache; that needs no purge.
   - `npm test` passes.
