# Publishing workflow

How to get changes in org source files reflected on the Hugo site.

## Source files

| Content type | Source directory | Hugo section |
|---|---|---|
| Notes | `~/Library/CloudStorage/Dropbox/websites/pablos-miscellany/*.org` | `content/notes/` |
| Quotes | `~/Library/CloudStorage/Dropbox/bibliographic-notes/*.org` (`:public:` subtrees) | `content/quotes/` |

## Pipeline overview

Org files are the canonical source. They are exported to Hugo-compatible markdown via ox-hugo running in Emacs batch mode. Hugo then builds the static site from that markdown. There is no manual markdown editing step.

```
org file  ──►  ox-hugo export  ──►  content/*.md  ──►  hugo build  ──►  public/
```

## Editing a note

After modifying an org file in `pablos-miscellany/` (e.g. `derek-parfit-a-bibliography.org`):

### 1. (Once only) Add ox-hugo metadata to new files

```bash
python scripts/prepare-org-notes.py
```

Adds `:EXPORT_FILE_NAME:`, `:EXPORT_HUGO_SECTION: notes`, and `:EXPORT_DATE:` to any org file that doesn't already have them. Safe to re-run — it skips prepared files.

### 2. Export org to markdown

```bash
emacs --batch -l scripts/export-notes.el
```

Processes all prepared org files and writes markdown to `content/notes/`. Citations (`[cite:@Key]`) are converted to `{{< cite "Key" >}}` shortcodes.

### 3. (If new cite keys were added) Generate work pages

```bash
python scripts/generate-work-pages.py
```

Creates `content/works/{slug}.md` for any cite keys referenced in notes or quotes that don't already have a work page.

### 4. (If links to other notes changed) Regenerate backlinks

```bash
python scripts/generate-backlinks.py
```

Reads the org-roam SQLite database and writes `data/backlinks.json`, which Hugo templates use to render backlink sections.

### 5. Preview locally

```bash
hugo server --buildDrafts
```

## Editing a quote

After modifying a `:public:` subtree in a bibliographic note in `bibliographic-notes/`:

### 1. Export quotes to markdown

```bash
bash scripts/export-quotes.sh
```

Exports all `:public:`-tagged subtrees to `content/quotes/`.

### 2. (If needed) Regenerate work pages and post-process

```bash
python scripts/generate-work-pages.py
```

This also strips any trailing content after blockquotes in exported quote files.

### 3. Preview locally

```bash
hugo server --buildDrafts
```

## Quick reference

For the common case of editing an existing note with no new citations or links:

```bash
emacs --batch -l scripts/export-notes.el && hugo server --buildDrafts
```

For a full rebuild from scratch:

```bash
python scripts/prepare-org-notes.py
emacs --batch -l scripts/export-notes.el
bash scripts/export-quotes.sh
python scripts/generate-work-pages.py
python scripts/generate-backlinks.py
hugo --minify
npx pagefind --site public
```

## Deployment

Pushing to the `main` branch triggers a Netlify build. The Netlify build command (`netlify.toml`) runs:

```bash
hugo --minify && npx pagefind --site public
```

Note: Netlify only builds from the committed markdown in `content/`. The org-to-markdown export (steps 1-4 above) must be run locally before committing. The org source files are not part of this repository.
