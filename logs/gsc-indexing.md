# Google Search Console indexing log

Persistent record for recurring Google Search Console Page indexing triage on `stafforini.com`.

## 2026-05-05 - GSC indexing triage

- Messages:
  - `19df97912c74760b` - Some fixes failed for Page indexing issues for site stafforini.com
  - `19df9792d4b40140` - Some fixes failed for Page indexing issues for site stafforini.com
- Issues:
  - `Not found (404)` - `https://search.google.com/search-console/index/drilldown?resource_id=sc-domain:stafforini.com&item_key=CAMYDSAC&hl=en-GB`
  - `Page with redirect` - `https://search.google.com/search-console/index/drilldown?resource_id=sc-domain:stafforini.com&item_key=CAMYCyAC&hl=en-GB`
- Examples checked after deploy:
  - `https://www.stafforini.com/quotes/?cat=1409` -> `https://stafforini.com/search/?q=Ian%20Eslick&section=quotes`, 200
  - `https://stafforini.com/tango/category/dancers/oscar-casas/feed/` -> `https://stafforini.com/tango/`, 200
  - `https://stafforini.com/tango/cd-collection/todo-tango-club/feed/` -> `https://stafforini.com/tango/`, 200
  - `https://www.stafforini.com/blog/bostrom/` -> `https://stafforini.com/notes/crucial-considerations-and-wise-philanthropy-by-nick-bostrom/`, 200
  - `https://www.stafforini.com/nino/Nino%20-%20La%20constituci%C3%B3n%20de%20la%20democracia%20deliberativa.pdf` -> `https://stafforini.com/search/?q=Nino&section=works`, 200
  - `https://www.stafforini.com/broad/Broad%20-%20Review%20of%20Marvin%27s%20A%20first%20book%20in%20metaphysics.pdf` -> `https://stafforini.com/search/?q=Broad&section=works`, 200
- Root cause:
  - Sitemap included non-indexable pages and rendered output still contained legacy internal links to old WordPress, `www`, asset, Anki, and deleted-PDF URL families.
- Changes:
  - Notes repo commit `05050f302 Fix stafforini.com broken note links`.
  - stafforini.com commit `8931a51 Fix Search Console indexing issues`.
  - Added shared indexability/canonical partials, sitemap filtering, targeted Netlify redirects, Hugo mounts for archived assets, and rendered-output verification for sitemap/canonical/noindex/broken-link checks.
- Verification:
  - `bash scripts/export-notes.sh` passed.
  - Production-profile Hugo render plus `python3 scripts/verify-site.py --dir "$tmp"` passed.
  - `npm test` passed with 190 tests.
  - `python3 -m py_compile scripts/verify-site.py` passed.
  - `git diff --check` passed.
- Deploy:
  - User deployed before browser validation.
- Browser validation:
  - Used Chrome with Search Console access as `pablo@stafforini.com` (`authuser=1`).
  - `Not found (404)`: validation started on 2026-05-05, `PENDING 375`, `FAILED 0`.
  - `Page with redirect`: validation started on 2026-05-05, `PENDING 1,556`, `FAILED 0`.
- Archived:
  - Not archived in this session.
- Follow-up:
  - Watch for completion/failure emails from Google and use `$gsc-indexing-triage` for the next cycle.
