# Quote migration: WordPress to org-roam

## Overview

Migrating ~1,643 quotes from the WordPress blog (stafforini.com/quotes, aka "notatu dignum") into org-roam bibliographic note files. Each quote becomes a subheading in its associated bibliographic note, tagged with `:public:` for Hugo publication.

## Architecture

- **Source**: WordPress WXR XML export (`~/Downloads/notatudignum.WordPress.2026-02-15.xml`)
- **Target**: Org-roam bibliographic notes (`~/Library/CloudStorage/Dropbox/bibliographic-notes/`)
- **Publication**: Hugo static site via ox-hugo export of `:public:`-tagged subheadings
- **Citations**: Org-cite with CSL, referencing biblatex entries

### Bib files

Five `.bib` files are used for matching:

1. `~/Library/CloudStorage/Dropbox/bibliography/new.bib` (493 entries)
2. `~/Library/CloudStorage/Dropbox/bibliography/old.bib` (14,454 entries)
3. `~/Library/CloudStorage/Dropbox/repos/babel-refs/bib/fluid.bib` (158 entries)
4. `~/Library/CloudStorage/Dropbox/repos/babel-refs/bib/stable.bib` (6,195 entries)
5. `~/Library/CloudStorage/Dropbox/repos/babel-refs/bib/db.bib` (2,112 entries)

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
**Output**: `~/Library/CloudStorage/Dropbox/bibliography/migration.bib` (644 entries)

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

### Phase 4: Write quotes to org files — TODO

For each matched quote:
1. Find or create the bibliographic note file for the cite key
2. Add a subheading with the quote text, tagged `:public:`
3. Include org-cite reference and locator
4. Preserve WP metadata (post date, link) as properties

### Phase 5: Hugo export pipeline — TODO

Build the pipeline to:
1. Find all `:public:`-tagged subheadings across bibliographic notes
2. Export to Hugo-compatible markdown via ox-hugo
3. Integrate with the existing Hugo site structure (`content/quotes/`)

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
| `scripts/MIGRATION-STATUS.md`         | This file                                    |
| `~/...bibliography/migration.bib`     | 644 new BibLaTeX entries                     |

## User preferences (established during this session)

- Always cite original works, not modern editions
- Multi-volume works: cite the work itself, indicate volume as part of the locator (don't use separate entries per volume)
- Tags: handle later by linking to corresponding org notes
- Matching approach: citar-style orderless matching (build searchable string, check all terms appear)
