# PDF hosting policy

Guidelines for hosting PDFs on stafforini.com, based on Gwern's approach (documented on his [Internet Search Tips](https://gwern.net/search) and [Archiving URLs](https://gwern.net/archiving) pages).

## Rules for books

1. **Only host books published before 2000.**
2. **Check that the book has no Kindle edition** or other sign of active commercial exploitation (i.e., the book is effectively an "orphan work").

Both conditions must be met.

## Papers vs. books

Academic papers carry much lower copyright risk than books. Gwern hosts many post-2000 papers without issue. The pre-2000 rule applies specifically to books.

## Technical measures

- ~~Block search engine indexing of hosted PDFs.~~ Gwern does *not* actually block PDF indexing — his `robots.txt` only blocks source files, metadata, and private directories. PDFs are fully crawlable and appear in Google results. We follow the same approach.
- The pre-2000 book exclusion is enforced automatically by `scripts/process-pdfs.py`, which filters out book-like entry types (`book`, `collection`, `reference` and their multi-volume variants) published in 2000 or later. This also blocks crossref inheritance from excluded parents.

### Expected Search Console side effect

Because the PDFs stay crawlable, most of them duplicate the text of their own work page. Google resolves that by picking the work page and filing the PDF under **"Duplicate without user-selected canonical"** in the Page indexing report. As of 2026-07-27 that bucket held 373 URLs, the majority of them `pdf.stafforini.com/*.pdf`.

This is the intended consequence of the policy above, not a defect, and it should not be "fixed" by disallowing the PDF host. `pdf.stafforini.com/robots.txt` is Cloudflare Managed Content and currently sends `User-agent: * / Allow: /`.

If you ever want the duplicate signals consolidated onto the work page while keeping the PDFs crawlable, the way to do it is a Cloudflare Transform Rule on the `pdf.stafforini.com` hostname that adds a response header:

```
Link: <https://stafforini.com/works/{slug}/>; rel="canonical"
```

where `{slug}` is the request path minus its `.pdf` suffix, which matches the bucket layout (all objects live at the bucket root as `<slug>.pdf`). This cannot be done from this repo: R2 serves only a fixed set of object headers, and `scripts/upload-pdfs.sh` holds S3 credentials that cannot alter edge configuration. It needs the Cloudflare dashboard.

## Gwern's track record

Over ~9 years hosting ~4,090 files, Gwern received only 4 takedown orders:

1. A behavioral genetics textbook (2013) — violated his own rule
2. *The Handbook of Psychopathy* (2005) — violated his own rule
3. A meta-analysis paper (Roberts et al 2016)
4. A Cambridge University Press order covering 27 files

The two book takedowns were for books that broke his own guidelines, which he acknowledged as "my mistake."

## Caveats

This is Gwern's personal risk assessment, not legal advice. His low takedown rate may partly reflect his site's specific context (academic audience, niche topics). Our risk profile may differ.

## Hosting architecture

PDFs and thumbnails are served from **Cloudflare R2** (S3-compatible object storage with zero egress fees).  The Netlify deploy never carries the PDF tree.

Components:

- `static/pdfs/` and `static/pdf-thumbnails/`: local source of truth, populated by `scripts/process-pdfs.py`.
- R2 bucket (default: `stafforini-pdfs`) with two top-level prefixes: `pdfs/` and `pdf-thumbnails/`.
- `scripts/upload-pdfs.sh`: incremental `aws s3 sync` from static/ to R2.  Runs automatically on full deploys.
- `hugo.toml` / `hugo.deploy.toml`: `params.pdfBaseURL` and `params.thumbBaseURL` point local builds at `/pdfs` and production at the R2 URL.

### One-time R2 setup

1. **Create the bucket.** Cloudflare dashboard → R2 → *Create bucket* → name `stafforini-pdfs` (or whatever you prefer).  Pick a location hint closest to your readers.
2. **Expose it publicly.** The bucket's *Settings* tab has two options:
   - **Public dev URL** (quickest): enable "R2.dev subdomain" to get `https://pub-<hash>.r2.dev`.  Cloudflare throttles and brands these URLs; fine for testing but not production.
   - **Custom domain** (recommended): click *Connect Domain* and add something like `pdfs.stafforini.com`.  Requires the apex domain to be on a Cloudflare-managed DNS zone or a CNAME from wherever DNS lives.
3. **Create an API token.**  R2 → *Manage R2 API Tokens* → *Create API token* → *Object Read & Write* scoped to the bucket.  Copy the access key id and secret -- they are shown only once.
4. **Fill in config.** Copy `scripts/r2.env.sh.example` → `scripts/r2.env.sh` and either paste the values or use 1Password `op://` references + `op run`.  Update `params.pdfBaseURL` and `params.thumbBaseURL` in `hugo.deploy.toml` with the public URL from step 2, pointed at the `pdfs/` and `pdf-thumbnails/` prefixes respectively.
5. **Initial upload.**  Source the env file and run `bash scripts/upload-pdfs.sh` once.  Expect a long upload on first run (~53 GB); subsequent runs are near-instant because `aws s3 sync` compares hashes.
6. **Deploy.**  `stafforini-deploy` (or `bash scripts/deploy.sh`) from here on pushes HTML-only to Netlify and deltas to R2.
