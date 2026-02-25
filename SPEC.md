# stafforini.com

Hugo reimplementation of the current WordPress version of stafforini.com (http://stafforini.com/).

## Background

Currently, the site has four main sections:

- Home
- Blog
- Tango
- Quotes

Each of these is a different WordPress installation. (The 'Contact' page is just part of the 'Home' installation.)

The new Hugo website should have a related but different structure, using a single Hugo installation. WordPress content migration is out of scope for this project.

## Site structure

### Sections

The site has two content sections:

1. **Notes** — A digital garden collecting miscellaneous notes, organized systematically like a wiki. Content from the current 'Blog' and 'Tango' sections will eventually live here.
2. **Quotes** — A diary of quotes, organized as a reverse-chronological feed (newest first).

### Home page

The home page combines:

- A brief personal introduction paragraph with links to the Notes and Quotes sections.
- An activity feed showing recently updated notes and recently added quotes.

### Navigation

Top-level navigation links: Home, Notes, Quotes.

## Notes section

### Index page

A full list of all notes, sortable by:

- Alphabetical order (default)
- Date of last update

No tags or categories. The organizational principle is **backlinks-as-taxonomy**: instead of tagging a note with "emacs", the note links to the "emacs" note. The set of all emacs-related pages is then the backlinks list on the "emacs" note page. This keeps the site's ontology flat and emergent.

### Individual note page

Each note page includes:

- **Title**
- **Metadata header**: last-updated date and word count
- **Auto-generated table of contents** (for longer notes; hide for very short ones)
- **Body content**
- **Per-note reference list** (rendered from org-cite citations using CSL)
- **Backlinks section** at the bottom, listing all notes that link to this note

### Search

Client-side search using [Pagefind](https://pagefind.app/). The search index is built at Hugo build time and runs entirely in the browser — no server or third-party service needed. Pagefind is lightweight, fast, and well-suited to static sites.

### Link previews

Wikipedia-style popup previews on hover for internal links. Shows the first paragraph of the target note. Simple and clean — no metadata in the preview popup.

## Quotes section

### Index page

A reverse-chronological scrollable feed with pagination. Each entry shows:

- The quote text
- Attribution via org-cite (rendered with CSL from the shared `.bib` file), including locator (e.g. page number)
- Date added

### Individual quote page

Each quote gets its own permalink page with the same elements as the feed entry, plus the full CSL-rendered reference.

## Authoring workflow

### Org-mode + ox-hugo

All source content is authored in org-mode files and exported to Hugo-compatible Markdown via [ox-hugo](https://github.com/kaushalmodi/ox-hugo).

**Export strategy**: One post per org file. Each `.org` file is its own note or quote, with `#+hugo_base_dir`, `#+hugo_section`, and other front matter set via ox-hugo keywords.

### Org-roam integration

[Org-roam](https://www.orgroam.com/) manages the note graph. Backlinks are derived from org-roam's database. The Hugo build needs a mechanism to translate org-roam backlinks into Hugo data (e.g. a pre-build script that queries the org-roam SQLite database and generates backlink data files).

### Org-cite + CSL

[Org-cite](https://orgmode.org/manual/Citations.html) handles all citations, for both Notes and Quotes. A set of shared `.bib` files provide the citation sources. CSL styles control the rendered output. The ox-hugo export must preserve citations and render them correctly on the site. 

The `.bib` files are

"~/Library/CloudStorage/Dropbox/bibliography/new.bib"
"~/Library/CloudStorage/Dropbox/bibliography/old.bib"
"~/Library/CloudStorage/Dropbox/repos/babel-refs/bib/fluid.bib"
"~/Library/CloudStorage/Dropbox/repos/babel-refs/bib/stable.bib"

## Design

### Visual direction

Minimalist and academic. Inspired by [Gwern.net](https://gwern.net/) (dense, long-form, sidenotes, metadata-rich) and [Andy Matuschak's working notes](https://notes.andymatuschak.org/) (evergreen structure, densely interlinked).

### Typography

- **Body text**: Serif font (e.g. ET Book, Literata, or Source Serif Pro) for a classic academic feel.
- **Headings**: Can be the same serif or a contrasting sans-serif for hierarchy.
- **Code**: Monospace font for inline code and code blocks.
- Generous line height and comfortable reading width (max ~65–75 characters).

### Color palette

Muted, restrained palette. Light background, dark text. Accent color used sparingly for links and interactive elements. Exact colors to be determined during implementation, but the feel should be calm and scholarly.

### Layout

- Single-column centered content for readability.
- No sidebar. Metadata and TOC are integrated into the page flow (e.g. TOC at the top, metadata below the title).
- Responsive: works well on mobile, tablet, and desktop.

### Dark mode

Supports both light and dark modes:

- User toggle (button in the header/nav).
- Preference is persisted in `localStorage`.
- Respects `prefers-color-scheme` as the default on first visit.

## Content types

The site must support:

- **Prose text** with rich formatting (headings, lists, blockquotes, footnotes)
- **LaTeX math** rendering (via KaTeX for performance, or MathJax if needed)
- **Code blocks** with syntax highlighting (Hugo's built-in Chroma highlighter)
- **Embedded media**: images, audio, and video

## RSS/Atom feeds

Two separate feeds:

1. **Notes feed** (`/notes/index.xml`) — new and updated notes
2. **Quotes feed** (`/quotes/index.xml`) — newly added quotes

Both should include full content in the feed (not just summaries).

## Deployment

### Hosting

[Netlify](https://www.netlify.com/) with continuous deployment from the git repository.

### Domain

Continue using `stafforini.com`. DNS will be pointed to Netlify.

### Build

Hugo build triggered on push to the main branch. The build pipeline should:

1. Run any pre-build scripts (e.g. backlink data generation from org-roam).
2. Build the Hugo site.
3. Run Pagefind to generate the search index.
4. Deploy to Netlify.

## Accessibility and performance

- Target **WCAG AA** compliance: sufficient color contrast, keyboard navigation, proper heading hierarchy, alt text for images, focus indicators.
- Target **Lighthouse performance score of 90+**: minimal JavaScript, optimized images, fast load times.
- Semantic HTML throughout.
- Progressive enhancement: the site should be fully functional without JavaScript (search and link previews gracefully degrade).

## Technical summary

| Aspect                | Choice                                         |
|-----------------------|------------------------------------------------|
| Static site generator | Hugo                                           |
| Content authoring     | Org-mode files                                 |
| Org-to-Hugo export    | ox-hugo (one post per file)                    |
| Backlinks             | Org-roam (pre-build script generates data)     |
| Citations             | Org-cite + shared `.bib` file + CSL            |
| Theme                 | Custom, built from scratch                     |
| Search                | Pagefind (client-side)                         |
| Link previews         | Wikipedia-style hover popups (first paragraph) |
| Dark mode             | Toggle + system preference + localStorage      |
| Math rendering        | KaTeX                                          |
| Syntax highlighting   | Hugo/Chroma                                    |
| RSS                   | Separate feeds for Notes and Quotes            |
| Hosting               | Netlify (continuous deployment)                |
| Domain                | stafforini.com                                 |
| Accessibility         | WCAG AA                                        |
| Performance           | Lighthouse 90+                                 |
|                       |                                                |
