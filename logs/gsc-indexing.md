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

## 2026-05-11 - GSC indexing triage

- Messages:
  - Current personal Gmail Search Console alerts could not be read because the Gmail API token refresh failed for `--account personal` with `invalid_grant` / token expired or revoked.
  - Default Gmail account query for recent Search Console/Page indexing messages returned no matches.
- Issues:
  - No new issue labels or Search Console issue URLs could be collected from Gmail in this session.
- Examples checked:
  - `https://www.stafforini.com/quotes/?cat=1409` -> `https://stafforini.com/search/?q=Ian%20Eslick&section=quotes`, 200.
  - `https://stafforini.com/tango/category/dancers/oscar-casas/feed/` -> `https://stafforini.com/tango/`, 200.
  - `https://www.stafforini.com/blog/bostrom/` -> `https://stafforini.com/notes/crucial-considerations-and-wise-philanthropy-by-nick-bostrom/`, 200.
  - `https://www.stafforini.com/nino/Nino%20-%20La%20constituci%C3%B3n%20de%20la%20democracia%20deliberativa.pdf` -> `https://stafforini.com/search/?q=Nino&section=works`, 200.
  - `https://stafforini.com/sitemap.xml` -> 28,035 `<url>` entries; `/search/` and `/tango/` were absent.
- Root cause:
  - Triage blocked at alert collection by expired/revoked personal Gmail OAuth token; previously logged representative live URL families still resolve successfully.
- Changes:
  - Appended this log entry only.
- Verification:
  - `python3 ".../gmail.py" query 'from:(sc-noreply@google.com) ("Page indexing" OR "Search Console") newer_than:45d' --account personal --max 20` failed with `invalid_grant`.
  - Same query against the default account returned no matches.
  - `curl -Ls https://stafforini.com/sitemap.xml | python3 -c ...` reported 28,035 URLs and no `/search/` or `/tango/`.
  - `curl -IL` checks above reached 200 final responses.
- Deploy:
  - Not authorized in this invocation; no local fix was made.
- Browser validation:
  - Not authorized in this invocation and no current issue URL was available.
- Archived:
  - Not authorized in this invocation; no messages were archived.
- Follow-up:
  - Refresh or reauthorize the personal Gmail token, then rerun `$gsc-indexing-triage` to collect current Search Console messages and issue URLs.

## 2026-05-11 - GSC indexing triage follow-up

- Messages:
  - `19e14c7ca2451b05` - Some fixes failed for Page indexing issues for site stafforini.com
  - `19e14c7bdc0c2aef` - Some fixes failed for Page indexing issues for site stafforini.com
- Issues:
  - `Page with redirect` - `https://search.google.com/search-console/index/drilldown?resource_id=sc-domain:stafforini.com&item_key=CAMYCyAC&utm_source=wnc_10031170&utm_medium=gamma&utm_campaign=wnc_10031170&utm_content=msg_100059089&hl=en-GB`
  - `Not found (404)` - `https://search.google.com/search-console/index/drilldown?resource_id=sc-domain:stafforini.com&item_key=CAMYDSAC&utm_source=wnc_10031170&utm_medium=gamma&utm_campaign=wnc_10031170&utm_content=msg_100059089&hl=en-GB`
- Examples checked:
  - Google examples were not available from the email body. No callable authenticated browser/Search Console surface was available in this session to inspect the issue-detail examples.
  - `https://www.stafforini.com/quotes/?cat=1409` -> `https://stafforini.com/search/?q=Ian%20Eslick&section=quotes`, 200.
  - `https://stafforini.com/tango/category/dancers/oscar-casas/feed/` -> `https://stafforini.com/tango/`, 200.
  - `https://www.stafforini.com/blog/bostrom/` -> `https://stafforini.com/notes/crucial-considerations-and-wise-philanthropy-by-nick-bostrom/`, 200.
  - `https://www.stafforini.com/nino/Nino%20-%20La%20constituci%C3%B3n%20de%20la%20democracia%20deliberativa.pdf` -> `https://stafforini.com/search/?q=Nino&section=works`, 200.
  - `https://stafforini.com/sitemap.xml` -> 28,035 `<url>` entries; `/search/` and `/tango/` were absent.
- Root cause:
  - Personal Gmail access was blocked by an expired/revoked `GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL`; OAuth reauthorization repaired alert access.
  - No local site-side root cause was identified from rendered-output verification or representative live checks. The remaining GSC validation failures may be historical discovered URLs that still redirect/404, but the current failed examples must be inspected in Search Console before deciding whether a site change is warranted.
- Changes:
  - Refreshed `GOOGLE_WORKSPACE_REFRESH_TOKEN_PERSONAL` in `/Users/pablostafforini/My Drive/dotfiles/shell/.zshenv-secrets` using the no-print OAuth flow.
  - Appended this log entry only.
- Verification:
  - Removed stale `/tmp/gworkspace-access-token-personal.json`; the old token still failed with `invalid_grant`.
  - OAuth regeneration completed and `gmail.py --account personal` successfully listed current Search Console emails.
  - Extracted the two issue-detail URLs above with `scripts/extract-gsc-links.py`.
  - `npm test` passed with 193 tests.
  - Production-profile Hugo render passed.
  - `python3 scripts/verify-site.py --dir <tmp>` passed with `Rendered site verification OK.`
  - Representative `curl -IL` checks above reached 200 final responses.
- Deploy:
  - Not authorized in this invocation; no local site fix was made.
- Browser validation:
  - Not authorized in this invocation, and no callable browser/Search Console example inspection surface was available.
- Archived:
  - Not authorized in this invocation; no messages were archived.
- Follow-up:
  - Open the two Search Console issue URLs in an authenticated browser, inspect failed examples, and only then decide whether to make a targeted source/redirect fix or treat the examples as intentionally non-actionable historical URLs.
