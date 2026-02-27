# Quote migration: WordPress to org-roam

## Overview

Migrating ~1,643 quotes from the WordPress blog (stafforini.com/quotes, aka "notatu dignum") into org-roam bibliographic note files. Each quote becomes a subheading in its associated bibliographic note, tagged with `:public:` for Hugo publication.

## Architecture

- **Source**: WordPress WXR XML export (`~/Downloads/notatudignum.WordPress.2026-02-15.xml`)
- **Target**: Org-roam bibliographic notes (`~/My Drive/bibliographic-notes/`)
- **Publication**: Hugo static site via ox-hugo export of `:public:`-tagged subheadings
- **Citations**: Org-cite with CSL, referencing biblatex entries

### Bib files

Five `.bib` files are used for matching:

1. `~/My Drive/bibliography/new.bib` (493 entries)
2. `~/My Drive/bibliography/old.bib` (14,454 entries)
3. `~/My Drive/repos/babel-refs/bib/fluid.bib` (158 entries)
4. `~/My Drive/repos/babel-refs/bib/stable.bib` (6,195 entries)
5. `~/My Drive/repos/babel-refs/bib/db.bib` (2,112 entries)

Total: ~23,412 entries, ~23,391 unique cite keys.

## Migration phases

### Phase 1: Parse WP export — DONE

**Script**: `scripts/parse-wp-quotes.py`
**Output**: `scripts/wp-quotes-parsed.json` (1,643 quotes)

Extracts from each WP post:
- Quote text (from `<blockquote>`)
- Attribution text and HTML
- Author (from WP categories, fallback to attribution parsing)
- Work title (from `<i>`/`<em>`/`<cite>` tags — the italic text)
- Article/chapter title (from quoted text in attribution — e.g., `'Article Title'`)
- Year, locator (page/chapter references)
- WP categories and tags

Stats:
- Work title extracted: 94.9%
- Article title extracted: 35.5% (583/1643)
- Year extracted: 96.6%
- Locator extracted: 80.3%

### Phase 2: Match quotes to bib entries — DONE

**Script**: `scripts/match-quotes-to-bib.py`
**Output**: `scripts/wp-quotes-matched.json` (augmented with `bib_match` field)

Uses citar-style orderless matching:
1. Builds a searchable string per bib entry (author + editor + title + shorttitle + booktitle + origtitle + journaltitle + year + CamelCase-split cite_key + entry_type)
2. Extracts search terms from each WP quote (author surname + year + up to 3 distinctive title words from both article_title and work_title)
3. Scores each candidate: fraction of terms found, with ±2 year tolerance (partial credit 0.5)
4. Pre-filters candidates by author surname (with multi-word surname indexing)

**Current results:**

| Status             | Count     | Description                                         |
|--------------------|-----------|-----------------------------------------------------|
| matched            | 916       | Confident match (803 auto + 113 manual)             |
| no_match           | 441       | Surname found, no title match — needs new bib entry |
| no_match_confirmed | 112       | Manually confirmed no match — needs new bib entry   |
| no_candidates      | 174       | Surname not in any bib file — needs new bib entry   |
| **Total**          | **1,643** |                                                     |

**Match rate: 55.8% (916/1,643)**

#### Disambiguation completed

All 71 ambiguous matches were manually resolved. Key principles applied:
- Cite the original work, not modern editions
- For multi-volume works, cite the work itself (e.g., `@mvbook`) and put volume in the locator
- For chapters/articles: cite the specific chapter entry if it exists; otherwise cite the collection

#### Weak matches reviewed

All 154 weak matches were reviewed: 44 confirmed correct, 97 confirmed no-match, 12 uncertain resolved, 1 deferred.

#### Bib cleanup done

- Deleted duplicate `Nagel1986ViewNowherea` from stable.bib (kept `Nagel1986ViewNowhere`)
- Deleted duplicate `Wright1994MoralAnimalWhy` from old.bib (kept `Wright1994MoralAnimalNew`)
- Removed `Nagel1986ViewNowherea` key from 9 babel-refs translation JSONs

### Phase 3: Create missing bib entries — DONE

**Script**: `scripts/create-bib-entries.py`
**Output**: `~/My Drive/bibliography/migration.bib` (644 entries)

Five-tier lookup strategy for 644 unique works (727 unmatched quotes):

1. **Tier 1 — Amazon ISBN via zotra**: Extract ISBN-10 from Amazon URLs in attribution HTML, look up via zotra `/search` + `/export?format=biblatex`
2. **Tier 2 — OpenLibrary ISBN via zotra**: Search OpenLibrary by author surname + title, feed ISBNs to zotra
3. **Tier 3 — CrossRef DOI via zotra**: Search CrossRef API by author + title for journal articles, feed DOI to zotra `/search`
4. **Tier 4 — Web URL via zotra**: Search DuckDuckGo HTML for article URL (newspaper/magazine articles), feed to zotra `/web`
5. **Tier 5 — Fallback from WP metadata**: Build minimal BibLaTeX entry from author, title, year, and parsed attribution

**Results:**

| Tier | Count | % | Description |
|------|-------|---|-------------|
| Tier 1 (Amazon ISBN) | 260 | 40.4% | Rich metadata via zotra ISBN lookup |
| Tier 2 (OpenLibrary) | 86 | 13.4% | Found ISBN via OpenLibrary search |
| Tier 3 (CrossRef DOI) | 47 | 7.3% | Found DOI via CrossRef API |
| Tier 4 (Web URL) | 33 | 5.1% | Found article URL via DuckDuckGo |
| Tier 5 (WP fallback) | 218 | 33.9% | Minimal entry from WP attribution |
| **Total** | **644** | **100%** | **0 errors** |

**Features:**
- Cite key generation matching `bibtex-autokey-*` conventions: `{Surname}{Year}{Word1}{Word2}{Word3}`
- Compound surname handling (e.g., "Bioy Casares" → `BioyCasares`)
- "Quoted in" detection: uses book author, not quoted person, for bib entry
- Work type detection: book, article, incollection, speech, letter, classical
- Resume support via `scripts/create-bib-progress.json`
- Collision detection against all existing bib files
- `--dry-run`, `--limit N`, and `--reprocess-fallbacks` flags for testing

**Total quote coverage: 100% (1,643/1,643 quotes now have cite keys)**
- 916 matched to existing bib entries (Phase 2)
- 727 with new bib entries (Phase 3)

**Items for manual review:** 218 Tier 5 entries (see `scripts/create-bib-report.txt`)

### Phase 4: Write quotes to org files — DONE

**Script**: `scripts/write-quotes-to-org.py`
**Output**: 1,324 org files in `~/My Drive/bibliographic-notes/`

For each of the 1,643 quotes:
1. Determined cite key (matched: from bib_match, unmatched: from new_cite_key)
2. Looked up bib entry for author/title/entry_type metadata
3. Found or created the bibliographic note file for the cite key
4. Added `:public:`-tagged level-2 subheading with ox-hugo export properties
5. Included org-cite reference with locator
6. Preserved WP metadata (post_id, link, date) as properties

**Results:**

| Metric | Count |
|--------|-------|
| Files created | 1,168 |
| Files modified | 156 |
| Subheadings added | 1,626 |
| Duplicates skipped | 17 |
| Missing bib entries | 0 |

**Features:**
- New org files match `orb-noter-template.org` format exactly: APA-abbreviated author, em dash separator, `[cite:@key]` ROAM_REFS, entry-type tags, NOTER_DOCUMENT path
- Subheading titles from first WP tag (falls back to quote words)
- Work slug from cite key (CamelCase → kebab-case) for Phase 5 work pages
- Globally unique export filenames (`{surname}-{title-slug}`)
- `#+hugo_base_dir:` added at file top for all files
- UUID `:ID:` on both level-1 and level-2 headings
- Resume via `scripts/write-quotes-progress.json`
- `--dry-run` and `--limit N` flags
- Idempotent: re-running skips already-processed quotes
- Duplicate detection: skips if same quote text already in file

### Phase 5: Hugo export pipeline — DONE

**Scripts**: `scripts/export-quotes.el`, `scripts/export-quotes.sh`, `scripts/generate-work-pages.py`
**Output**: 1,626 quote markdown files in `content/quotes/`, 1,323 work pages in `content/works/`

Three-step pipeline:

1. **Batch ox-hugo export** (`export-quotes.el` + `export-quotes.sh`): Emacs batch mode script that adds elpaca load paths, loads ox-hugo, registers a no-op cite processor (to avoid slow/broken citeproc in batch mode), then iterates over all 1,324 org files with `:public:` tags and calls `org-hugo-export-wim-to-md :all-subtrees`. Exports 1,626 subtrees to `content/quotes/*.md` with TOML front matter (`work`, `locator`, `date`, `tags`).

2. **Post-processing** (integrated into `generate-work-pages.py`): Strips any trailing content after the blockquote in each markdown file. The no-op cite processor already suppresses citation text, but this step ensures clean output.

3. **Work page generation** (`generate-work-pages.py`): Parses all 6 bib files, scans quote files for unique `work` slugs (1,323 found), generates `content/works/{slug}.md` with YAML front matter (`title`, `author` in "First Last" format, `year`). Uses `cite_key_to_slug()` to map cite keys to slugs.

**Results:**

| Metric | Count |
|--------|-------|
| Files exported | 1,324 |
| Subtrees exported | 1,626 |
| Export errors | 0 |
| Quote files post-processed | 1,626 |
| Work pages generated | 1,323 |
| Missing bib entries | 0 |

**Key decisions:**
- No-op cite processor instead of CSL: org-cite-basic tries to parse all bib files per subtree, which is extremely slow (~30+ min) and errors out with `hash-table-p, nil` in batch mode. Since Hugo templates handle attribution via work page lookup, citation rendering is unnecessary.
- Work pages use YAML front matter (matching existing conventions) while ox-hugo exports use TOML (ox-hugo default). Hugo handles both formats transparently.
- 5 prototype quote files and 5 prototype work pages from earlier phases were deleted and regenerated from org sources with correct slugs.

### Phase 6: Tag linking — TODO (deferred)

WP tags should eventually link to corresponding org notes (e.g., "emacs" tag → emacs.org). This was explicitly deferred for later.

## File inventory

| File                                  | Description                                 |
|---------------------------------------|---------------------------------------------|
| `scripts/parse-wp-quotes.py`          | WP XML → structured JSON parser             |
| `scripts/match-quotes-to-bib.py`      | Citar-style quote-to-bib matcher             |
| `scripts/create-bib-entries.py`       | Three-tier bib entry creator                 |
| `scripts/wp-quotes-parsed.json`       | 1,643 parsed quotes                         |
| `scripts/wp-quotes-matched.json`      | Quotes with match results + new cite keys    |
| `scripts/match-report.txt`            | Phase 2 items needing attention              |
| `scripts/create-bib-progress.json`    | Phase 3 progress cache for resume            |
| `scripts/create-bib-report.txt`       | Phase 3 items needing manual review          |
| `scripts/write-quotes-to-org.py`      | Phase 4: write quotes to org-roam notes      |
| `scripts/write-quotes-progress.json`  | Phase 4 progress cache for resume            |
| `scripts/write-quotes-report.txt`     | Phase 4 summary and duplicate report         |
| `scripts/export-quotes.el`            | Phase 5: Elisp batch ox-hugo export          |
| `scripts/export-quotes.sh`            | Phase 5: Shell wrapper for batch export      |
| `scripts/generate-work-pages.py`      | Phase 5: Work page generator + post-processor|
| `scripts/MIGRATION-STATUS.md`         | This file                                    |
| `~/...bibliography/migration.bib`     | 644 new BibLaTeX entries                     |

## User preferences (established during this session)

- Always cite original works, not modern editions
- Multi-volume works: cite the work itself, indicate volume as part of the locator (don't use separate entries per volume)
- Tags: handle later by linking to corresponding org notes
- Matching approach: citar-style orderless matching (build searchable string, check all terms appear)

---

# Blog notes migration: WordPress org files to Hugo

## Overview

Migrating ~119 blog posts from individual org files (originally extracted from
a monolithic `pablos-miscellany.org`) into Hugo notes via ox-hugo.

**Source**: `~/My Drive/notes/pablos-miscellany/*.org`
**Target**: `content/notes/*.md` in the Hugo site

## Architecture

The pipeline has three layers, each with a clear responsibility:

1. **Org files** (canonical source) — authored in org-mode, one file per post
2. **ox-hugo** (org → markdown) — exports org to Hugo-compatible markdown
3. **Hugo** (markdown → HTML) — builds the static site

There is **no custom Python conversion layer** between org and markdown.
All org-to-markdown conversion is handled natively by ox-hugo. Python scripts
only handle metadata preparation and work page generation.

### How citations work

Citations flow through three stages:

1. **Org source**: `[cite:@Parfit2017WhatMattersVolume]`
2. **Markdown (after ox-hugo export)**: `{{< cite "Parfit2017WhatMattersVolume" >}}`
   — the `hugo-cite` processor in `export-notes.el` converts org-cite to Hugo shortcodes
3. **HTML (after Hugo build)**: formatted citation with link to work page
   — the `layouts/shortcodes/cite.html` shortcode looks up `content/works/{slug}.md`
   and renders via `layouts/partials/citation-format.html`

This avoids running slow citeproc at export time while producing rich formatted
citations at build time, using the same work pages and citation formatting as quotes.

## Scripts

### prepare-org-notes.py — Add ox-hugo metadata

Adds the ox-hugo keywords needed for per-subtree export to each org file:

- **File level**: `#+hugo_base_dir: ~/My Drive/repos/stafforini.com/`
- **Heading level** (in the level-1 PROPERTIES drawer):
  - `:EXPORT_FILE_NAME:` — slug derived from the org filename
  - `:EXPORT_HUGO_SECTION: notes` — target Hugo section
  - `:EXPORT_DATE:` — extracted from `:POST_DATE:` (YYYY-MM-DD)
  - `:EXPORT_HUGO_DRAFT: true` — if no date (draft posts)

Idempotent: skips files that already have `:EXPORT_FILE_NAME:`.

### export-notes.el — Batch ox-hugo export

Emacs batch script (`emacs --batch -l scripts/export-notes.el`) that:

1. Loads elpaca build directories to find ox-hugo
2. Registers a `hugo-cite` export processor that converts `[cite:@Key]` to
   `{{< cite "Key" >}}` (and `[cite:@Key, pp. 3-28]` to `{{< cite "Key" "pp. 3-28" >}}`)
3. Iterates over all `.org` files with `:EXPORT_FILE_NAME:` metadata
4. Calls `org-hugo-export-wim-to-md :all-subtrees` for each file

### cite.html shortcode — Hugo citation rendering

`layouts/shortcodes/cite.html` takes a cite key, converts it to a kebab-case slug,
looks up the corresponding work page, and renders the citation using
`citation-format.html`. Falls back to a `<span class="cite-unresolved">` if
no work page exists.

## Full rebuild workflow

To rebuild the site from scratch (e.g. after editing org files):

```bash
# 1. Add ox-hugo metadata (only needed once, or after adding new org files)
python scripts/prepare-org-notes.py

# 2. Export org to Hugo markdown via ox-hugo
emacs --batch -l scripts/export-notes.el

# 3. Generate work pages for any new cite keys in notes
#    (run the inline script or extend generate-work-pages.py)

# 4. Generate backlinks from org-roam database
python scripts/generate-backlinks.py

# 5. Build the Hugo site
hugo --minify

# 6. Generate Pagefind search index
npx pagefind --site public
```

For local preview, replace steps 5-6 with `hugo server --buildDrafts`.

## Results

| Metric | Count |
|--------|-------|
| Org files prepared | 119 |
| Notes exported | 119 |
| Unique cite keys in notes | 287 |
| Work pages created for notes | 262 |
| Export errors | 0 |
| Unresolved citations | 0 |
| Draft posts | ~45 |
| Published posts | ~74 |
