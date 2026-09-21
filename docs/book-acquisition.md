# Book acquisition

`scripts/download-missing-pdfs.py` is now an inspection adapter to the shared
dotfiles acquisition library. It no longer downloads an automatically chosen
book, changes `old.bib`, or replays old progress into bibliography attachments.
Existing scheduled runs stop with `needs-review` (exit 5) before accessing a
service or reading a credential. The old progress file remains untouched.

`--dry-run` still inventories candidates for books in `old.bib`. It preserves
unknown edition/language fields and provider errors. Search observations,
filenames, scan flags and reported sizes do not approve a file.

Use dotfiles `bin/paper-fetch` for the complete candidate workflow:
`book-candidates`, `book-stage`, `book-inspect`, and `book-select`. Its maintained
command and manifest contract is in `~/My Drive/dotfiles/docs/book-acquisition.md`.
Bibliographic and attachment decisions belong to the single shared policy at
`~/My Drive/dotfiles/agents/bibliography-policy.md`.

The website adapter accepts `--candidates CANDIDATES.json --reviews REVIEWS.json`
to report the same reviewed selection. That operation is read-only. The selected
file goes through Zotra/Ebib for attachment and bibliography persistence. A
successful selection is not an attached file, a published PDF, or proof that
every provider and candidate was searched.
