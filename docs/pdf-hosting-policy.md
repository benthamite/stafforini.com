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

## Gwern's track record

Over ~9 years hosting ~4,090 files, Gwern received only 4 takedown orders:

1. A behavioral genetics textbook (2013) — violated his own rule
2. *The Handbook of Psychopathy* (2005) — violated his own rule
3. A meta-analysis paper (Roberts et al 2016)
4. A Cambridge University Press order covering 27 files

The two book takedowns were for books that broke his own guidelines, which he acknowledged as "my mistake."

## Caveats

This is Gwern's personal risk assessment, not legal advice. His low takedown rate may partly reflect his site's specific context (academic audience, niche topics). Our risk profile may differ.
