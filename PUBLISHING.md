# Publishing workflow

How to get changes in org source files reflected on the Hugo site.

## Source files

| Content type | Source directory | Hugo section |
|---|---|---|
| Notes | `~/Library/CloudStorage/Dropbox/websites/pablos-miscellany/*.org` | `content/notes/` |
| Quotes | `~/Library/CloudStorage/Dropbox/bibliographic-notes/*.org` (`:public:` subtrees) | `content/quotes/` |

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

### When you need extra steps

- **New org file**: run `python scripts/prepare-org-notes.py` first to add ox-hugo metadata (`:EXPORT_FILE_NAME:`, `:EXPORT_HUGO_SECTION:`, etc.)
- **New cite keys**: run `python scripts/generate-work-pages.py` to create `content/works/` pages for any new citations
- **Changed links**: run `python scripts/generate-backlinks.py` to regenerate backlink data from the org-roam database

## Batch workflow

For bulk operations (e.g. after a migration or mass edit), batch scripts process all files at once:

```bash
# Export all notes
emacs --batch -l scripts/export-notes.el

# Export all quotes
bash scripts/export-quotes.sh
```

## Full rebuild

```bash
python scripts/prepare-org-notes.py          # add ox-hugo metadata to new files
emacs --batch -l scripts/export-notes.el     # export all notes
bash scripts/export-quotes.sh                # export all quotes
python scripts/generate-work-pages.py        # generate/update work pages
python scripts/generate-backlinks.py         # regenerate backlinks
hugo --minify                                # build the site
npx pagefind --site public                   # generate search index
```

## Local preview

```bash
hugo server --buildDrafts
```

## Deployment

Pushing to the `main` branch triggers a Netlify build. The Netlify build command (`netlify.toml`) runs:

```bash
hugo --minify && npx pagefind --site public
```

Note: Netlify only builds from the committed markdown in `content/`. The org-to-markdown export must be run locally before committing. The org source files are not part of this repository.

## Configuration

The `hugo-cite` and `hugo-cite-noop` processors are registered in the ox-hugo section of `config.org`. Per-directory activation is handled by `.dir-locals.el`:

| Directory | Processor | Effect |
|---|---|---|
| `websites/pablos-miscellany/` | `hugo-cite` | `[cite:@Key]` → `{{< cite "Key" >}}` |
| `bibliographic-notes/` | `hugo-cite-noop` | Citations suppressed (work pages handle attribution) |
