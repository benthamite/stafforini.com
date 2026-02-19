# Publishing workflow

How to get changes in org source files or BibTeX files reflected on the Hugo site.

## Prerequisites

The following must be installed (all available via Homebrew):

- **Hugo** (extended edition, v0.151.2+) — `brew install hugo`
- **Node.js** (v20+) — managed via nvm; `nvm install 20` if needed
- **Python 3** — for the helper scripts
- **Emacs** — for ox-hugo export (both interactive and batch)

## Starting the dev server

From Emacs:

- `M-x stafforini-start-server` — starts the Hugo dev server (`npm run dev`) in a dedicated `*hugo-server*` buffer. If a server is already running, it is killed first to ensure a fresh build (Hugo's incremental rebuild does not track cross-page shortcode dependencies, so reusing a running server after content changes causes stale data).
- `M-x stafforini-stop-server` — stops the server.

Alternatively, from the project root (`~/Library/CloudStorage/Dropbox/repos/stafforini.com/`):

```bash
npm install     # first time only, or after dependency changes
npm run dev     # kills old servers, regenerates data, starts hugo server
```

The site will be available at `http://localhost:1313/`.

**Important**: Hugo watches `content/` for changes and does live reload, but this only works reliably for changes to the file you're editing. Changes that affect other pages (e.g. updating a BibTeX entry that changes how citations render on note pages) require restarting the server. Always call `stafforini-start-server` after running `stafforini-update-works` or `stafforini-update-backlinks`.

To also include draft content:

```bash
hugo server --buildDrafts --navigateToChanged
```

## Source files

| Content type | Source directory                                                                 | Hugo section      |
|--------------|----------------------------------------------------------------------------------|-------------------|
| Notes        | `~/Library/CloudStorage/Dropbox/websites/pablos-miscellany/*.org`                | `content/notes/`  |
| Quotes       | `~/Library/CloudStorage/Dropbox/bibliographic-notes/*.org` (`:public:` subtrees) | `content/quotes/` |
| Works        | `.bib` files (see SPEC.md for paths)                                             | `content/works/`  |

## Pipeline overview

Org files are the canonical source. They are exported to Hugo-compatible markdown via ox-hugo, either interactively from Emacs or in batch mode. Hugo then builds the static site from that markdown. There is no manual markdown editing step.

```
org file  ──►  ox-hugo export  ──►  content/*.md  ──►  hugo build  ──►  public/
```

### How citations work

Citations go through three stages:

1. **Org source**: `[cite:@Parfit2017WhatMattersVolume]`
2. **Markdown** (after ox-hugo export): `{{< cite "Parfit2017WhatMattersVolume" >}}`
3. **HTML** (after Hugo build): formatted citation with link to work page

The `hugo-cite` processor (registered in `config.org`, ox-hugo section) converts org-cite references to Hugo shortcodes at export time. Hugo's `cite.html` shortcode then renders them at build time by looking up work pages in `content/works/`.

## Interactive workflow (recommended)

The normal day-to-day workflow uses ox-hugo's interactive export from within Emacs.

### Editing a note

1. Open the org file in Emacs (e.g. `derek-parfit-a-bibliography.org`)
2. Edit the content
3. Export with `C-c C-e H H` (current subtree) or `C-c C-e H A` (all subtrees)
4. If `hugo server` is running, the site updates automatically

The `.dir-locals.el` in `pablos-miscellany/` activates the `hugo-cite` processor so citations are correctly converted to Hugo shortcodes. It also sets `org-export-with-broken-links` to handle any dead links.

### Editing a quote

1. Open the bibliographic note in Emacs
2. Edit the `:public:` subtree
3. Export with `C-c C-e H A`
4. If `hugo server` is running, the site updates automatically

The `.dir-locals.el` in `bibliographic-notes/` activates the `hugo-cite-noop` processor, which suppresses citation text (quotes get their attribution from work pages instead).

### Editing a BibTeX entry

1. Edit the `.bib` file
2. Run `M-x stafforini-update-works`
3. Run `M-x stafforini-start-server` to restart the server with fresh data

Hugo's live reload does **not** propagate BibTeX changes to note pages that cite the updated work (shortcode cross-references are not tracked). You must restart the server.

The script reads all `.bib` files listed in `scripts/lib.py`, regenerates work pages for every cited key, and updates any that have changed. It is safe to re-run at any time.

### When you need extra steps

- **New org file**: run `M-x stafforini-prepare-notes` to add ox-hugo metadata (`:EXPORT_FILE_NAME:`, `:EXPORT_HUGO_SECTION:`, etc.)
- **Changed links**: run `M-x stafforini-update-backlinks` to regenerate backlink data from the org-roam database

## Emacs commands

All build commands are provided by the `stafforini` package. They run asynchronously via `compile`, so output appears in a compilation buffer with proper error highlighting.

| Command | What it does |
|---|---|
| `M-x stafforini-prepare-notes` | Add ox-hugo metadata to new org note files |
| `M-x stafforini-export-all-notes` | Export all notes to Hugo markdown (batch mode) |
| `M-x stafforini-export-all-quotes` | Export all quotes to Hugo markdown (batch mode) |
| `M-x stafforini-update-works` | Generate/update work pages from BibTeX data |
| `M-x stafforini-update-backlinks` | Regenerate backlink data from org-roam |
| `M-x stafforini-start-server` | Start Hugo dev server (always restarts for fresh data) |
| `M-x stafforini-stop-server` | Stop the Hugo dev server |
| `M-x stafforini-full-rebuild` | Run the full pipeline (see below) |

## Batch workflow

For bulk operations (e.g. after a migration or mass edit), use the batch Emacs commands:

- `M-x stafforini-export-all-notes` — exports all notes
- `M-x stafforini-export-all-quotes` — exports all quotes

Or from the shell:

```bash
# Export all notes
emacs --batch -l scripts/export-notes.el

# Export all quotes
bash scripts/export-quotes.sh
```

## Full rebuild

Run `M-x stafforini-full-rebuild` to execute the full pipeline sequentially:

1. Prepare notes (add ox-hugo metadata)
2. Export all notes
3. Export all quotes
4. Generate/update work pages
5. Regenerate backlinks
6. Generate citing-notes data
7. Inject lastmod dates
8. Process PDFs
9. Clean `public/` (remove stale files)
10. Hugo build (`hugo --minify`)
11. Pagefind index (`npx pagefind --site public`)

Or from the shell:

```bash
python scripts/prepare-org-notes.py          # add ox-hugo metadata to new files
emacs --batch -l scripts/export-notes.el     # export all notes
bash scripts/export-quotes.sh                # export all quotes
python scripts/generate-work-pages.py        # generate/update work pages
python scripts/generate-backlinks.py         # regenerate backlinks
python scripts/generate-citing-notes.py      # generate citing-notes data
python scripts/inject-lastmod.py             # inject lastmod dates
python scripts/process-pdfs.py               # strip annotations, generate thumbnails
rm -rf public                                # clean stale build output
hugo --minify                                # build the site
npx pagefind --site public                   # generate search index
```

## Deployment

Run `scripts/deploy.sh` from the project root. There is no CI/CD from git pushes — all deploys are triggered manually by running the script.

The deploy script:

1. Injects lastmod dates from org file modification times
2. Processes PDFs (strip annotations, generate thumbnails)
3. Generates citing-notes data
4. Cleans `public/` (removes stale files from previous builds)
5. Builds the site with `hugo --minify`
6. Generates the Pagefind search index
7. Deploys to Netlify via CLI

Note: The org-to-markdown export must be run locally before deploying. The org source files and `content/` directory are not tracked in this repository.

## Configuration

The `hugo-cite` and `hugo-cite-noop` processors are registered in the ox-hugo section of `config.org`. Per-directory activation is handled by `.dir-locals.el`:

| Directory                     | Processor        | Effect                                               |
|-------------------------------|------------------|------------------------------------------------------|
| `websites/pablos-miscellany/` | `hugo-cite`      | `[cite:@Key]` → `{{< cite "Key" >}}`                 |
| `bibliographic-notes/`        | `hugo-cite-noop` | Citations suppressed (work pages handle attribution) |
