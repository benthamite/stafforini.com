# Filing refresh contracts

Read this before changing disclosure/model data or running a full filing refresh.
These source observations were checked on 2026-09-06; recheck the named functions
if the producer has changed. They are not permission to run the model.

## SEC interpretation

For 13F, obtain the cover, summary and information table, distinguish report
types and amendment type, and retain the raw rows before aggregating positions.
The summary's entry total counts rows, not unique issuers. Options are reported
using underlying-security information; reported value is not a disclosed option
premium, and strikes/expirations are not recoverable from that field. Amendments
may restate the report or add holdings. Treating an addition as a replacement
can discard the previously reported book.
[SEC Form 13F instructions](https://www.sec.gov/files/form13f.pdf)

Keep value units consistent across XML, summary and model. The modern form uses
nearest-dollar amounts; older records may use thousands. Calculate the applicable
deadline from the reporting period and current SEC guidance, including weekends
and holidays. Count “days early” against that adjusted deadline, not an
unadjusted day-45 label.
[SEC 13F FAQ](https://www.sec.gov/rules-regulations/staff-guidance/division-investment-management-frequently-asked-questions/frequently-asked-questions-about-form-13f)

13D/13G concern beneficial ownership of an issuer's security class. Their event,
filing and acceptance dates are different facts; reporting deadlines vary by
form/filer/event. Verify reporting-person/group relationships and amendments
before deriving model positions. Do not apply the 13F table schema or deadline
to these disclosures.
[SEC beneficial-ownership interpretations](https://www.sec.gov/rules-regulations/staff-guidance/corporation-finance-interpretations/exchange-act-sections-13d-13g-regulation-13d-g-beneficial-ownership-reporting)

For authorized SEC retrieval, use identifiable, bounded requests under the
current [EDGAR access policy](https://www.sec.gov/search-filings/edgar-search-assistance/accessing-edgar-data).
An access failure is not evidence that no new filing exists. A recent-submissions
window/cache may omit older filings needed for a selected historical target.
Do not fetch new filings just to audit or test this skill.

## Current producer limitations to inspect

Canonical code is in `public/situational-awareness-lp.org`, not the generated
Python/HTML files.

- `sa-data` selects one accession per quarter, treats the latest amendment as
  a replacement and stores the original filing date as the rebalance date.
  It does not distinguish additional-holdings amendments or retain acceptance
  timestamps. Do not certify amendment completeness or hindsight-free returns
  from that representation alone.
- The data block can retain cached filings after an SEC failure, or skip a
  filing whose information table was not found. A nonempty result is therefore
  not proof of a complete fresh SEC fetch. Check the actual target/documents
  and run diagnostics rather than erasing caches or trusting exit status.
- `_add_13g_disclosures` currently layers each entry onto a full 13F rather than
  accumulating all earlier issuer updates, uses a fixed accession literal, and
  is not a general 13D processor. Before a new issuer/amendment/multiple-layer
  update, implement and test the intended representation under the requested
  model-change scope; do not merely append a row and call the timeline verified.
- Public availability and executable market time must be independently checked.
  Existing date-only/clamping/price-date logic is not an exchange-calendar or
  after-hours execution proof.
- The calculator aggregates ticker/type positions and emits `equity_only`,
  `scaled` and `full` datasets. Its representative options and unavailable
  sizing rows are modeled outputs, not the fund's actual option contracts.

These are prerequisites for a supported future refresh, not claims that the
skill audit repaired the canonical financial model. If a prerequisite is not
addressed, preserve the existing publication and report the unsupported target.

## Runtime and downstream ownership

- `scripts/sa-lp-init.el` owns the actual seven-block order; sensitivity comes
  after both charts and before delay/calculator. Its error-cell check does not
  establish that every number or data dependency is fresh.
- `sa-delay` models transitions between full 13Fs only. Its first/last dates
  and transition count need not change for an issuer-only disclosure.
- `scripts/sa-lp-refresh.sh` has mutation/API effects even with `DRY_RUN=1`.
  It may return zero after a source-lock skip. Its real completion log and
  source/artifact generation must be checked, not just process status.
- `scripts/generate-copycat-scripts.py --profile all` packages both public
  CLIs. `--check` compares source-derived output without executing embedded
  financial code. Even a single-profile generation reads both note sources.
- VARA's `vara-chart-salp` runs SALP's canonical data/library for its comparator
  (decision016). The generated VARA CLI embeds them (decision019). Neither is a
  separately maintained simplified SALP model. Check both consumers when inputs
  or model semantics change; preserve unrelated VARA edits.
- The refresh exports all notes. Inspect unintended output changes and required
  mounts before committing or deploying; do not turn a missing source into a
  smaller apparently successful site.

Relevant decisions: 013 sensitivity cadence; 014 disclosure-based forced-sale
caveat; 016 shared comparison; 017 successful-but-unfillable option exclusions;
018 all three proxy modes; 019 standalone source packaging. Read the full
applicable records rather than carrying forward old measured numbers.

## Evidence and publication boundaries

A complete refresh needs a coherent run: source and target identity, exact
dates, successful required evaluations, current artifact contents and inspection
of the rendered behavior. Record these locally without secrets. Timestamps,
substring hits and a successful export do not prove a new sensitivity sweep.

The bundled checker is a bounded current-full-13F consistency check. It cannot
independently reconstruct SEC rows discarded by the model, validate the financial
assumptions, establish market-data availability or accept arbitrary layered
portfolios. A supported zero-length first-day period is not a missing period.

`--fast-note` is for already-exported minor body/static-asset changes only.
Current publishing policy excludes filing-related date/link/citation/search
changes. `--quick` rebuilds the full generated site but skips export and PDF/R2
work; a full deploy has additional remote effects. Neither isolates publication
to a single commit. No mode may hide an unresolved artifact or input dependency.
