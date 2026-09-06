# stafforini.com Search Console reference

## Maintained locations

- Site: `/Users/pablostafforini/repos/stafforini.com`.
- Notes: `/Users/pablostafforini/My Drive/notes`.
- Quotes: `/Users/pablostafforini/My Drive/bibliographic-notes`.
- Bibliographies: resolve `scripts/lib.py:BIB_FILES`; don't infer paths from
  old repository layouts. Some are in shared `babel-refs`.
- Service routing: `/Users/pablostafforini/My Drive/dotfiles/claude/context/service-access.md`
  and `google-services.md`. Gmail uses the local `gmail.py --account personal`.
- Run history: `logs/gsc-indexing.md`, relative to the site. Read as dated
  evidence; append only for requested logging/closeout.
- Property: `sc-domain:stafforini.com`. Verify the live selector, including
  sitemap filters, rather than trusting an email subject or URL hint.

A previously working browser account occupied `authuser=1`/`/u/1/`.
Account order can change; use the visible account and current configured Chrome
profile. Never switch to a different mailbox/property just to obtain access.

## Standing validation policy

Decision008 (updated 2026-08-08) says **do not request validation for this
property's `Not found (404)` or `Page with redirect` buckets**. They contain
deliberately retired pages and intentional legacy redirects. Keep that policy
unless the user explicitly changes it; neither apparently clean samples nor
a filtered view overrides it.

This is an operational choice, not a claim that Google can never pass either
category. Google's process concerns the affected set and any applied sitemap
filter, not necessarily the whole property. See the current
[Page indexing documentation](https://support.google.com/webmasters/answer/7440203?hl=en).
Validation is optional and should not be restarted while a cycle is running.

Do not confuse skipping validation with skipping diagnosis:

- Intentional legacy redirects need no change. Check novel or suspicious
  examples for loops, a wrong destination or a final error.
- Correctly retired URLs can remain 404. Check new regressions and important
  internally linked/sitemapped URLs even when their recorded crawl date is old.
- `Redirect error` and `Soft 404` are different categories; neither inherits
  the no-validation rule simply because its name resembles another bucket.
- Do not manufacture redirects to make totals fall. Respect takedowns and
  source ownership; an uncertain replacement is not a guessed target.

Counts such as 2,498 redirects/217 404s were observed in August 2026, not a current
inventory. Old item keys in the log are navigation hints, not stable API values.
If an email link is missing, select the issue in the verified property's UI.

## Google evidence boundaries

The report's examples may be incomplete even below its 1,000-row limit.
A report crawl date differs from a live check. A success from the
[URL Inspection live test](https://support.google.com/webmasters/answer/9012289?hl=en)
does not establish actual indexing; inspect source redirects separately because
the live test can follow them. Preserve the distinction between a page's
current response, Google's recorded index state and validation progress.

A working redirect target is not necessarily the intended canonical target.
Check content, indexability and internal/sitemap references, not only final 200.
Robots restrictions and `noindex` are separate controls; consult current Google
documentation before changing either to address an indexing alert.

## Site integration

Read the canonical decisions before modifying the affected subsystem:

- 005: encoded redirect sources and `static/_redirects` precedence. Use the
  existing generator's encoding routine for work-slug mappings.
- 006: deploy-cost measurements and already-tested explanations.
- 007: duplicate work pages retain their canonical links; don't delete shared
  bibliography entries to tidy the site.
- 008/012: no-validation policy and action-specific authorization.
- 009/011: required mounts/sources must resolve before generation or deployment.
- 010/015: full rendered verification includes redirect-target checks; quote
  slug changes are handled by the content-keyed mapping generator.

Use `npm test` or `bash scripts/test.sh`, not bare pytest. The production
Hugo render plus `verify-site.py --dir EXACT_RENDER` checks local structure,
including redirect targets; it does not execute Netlify's edge rules.
`--quick` deploy skips export/PDF/R2 steps and publishes the full current tree.
Read `scripts/deploy.sh`/`PUBLISHING.md` before selecting a different mode.

The 2026-08-19 log records a bulk live-sitemap sweep stopped after rate limiting.
Use bounded representative live checks, not a high-volume sweep. Local structural
checks supply breadth; live checks supply evidence of the affected deployed
behavior. Neither alone proves Google has recrawled or indexed the pages.

Read `docs/pdf-hosting-policy.md` for the deliberate crawlability/duplicate
policy before changing PDF indexing. PDFs are served from a separate R2 host;
Netlify headers do not control that host. The policy's hosting choices are not
a legal assessment or proof that every reported PDF duplicate is intentional.

## Optional log shape

When logging is authorized, retain a short dated record:

```markdown
## YYYY-MM-DD - GSC indexing triage

- Scope and actual agent:
- Messages/issues (minimal identifiers, property and filter):
- Examples and observations (crawl/report/live times distinguished):
- Root cause or remaining uncertainty:
- Source changes and commits:
- Local verification:
- Deploy and live acceptance:
- GSC validation (observed state, or reason not started):
- Archive (exact handled IDs and confirmed label state):
- Open decisions/follow-up:
```

Keep historical entries intact. Do not copy private email bodies, credentials
or access-bearing shared-report URLs into this potentially public repository.
