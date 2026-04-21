#!/usr/bin/env python3
"""Print the kebab-case slugs of every cite key on the takedown blocklist.

Used by scripts/upload-pdfs.sh to explicitly remove the corresponding PDFs
and thumbnails from the R2 bucket, since `aws s3 sync` without --delete
leaves remote objects in place even when their local source is gone.

Outputs one slug per line to stdout.  Exits 0 with no output when the
blocklist is empty.
"""

from lib import excluded_work_slugs


def main() -> None:
    for slug in sorted(excluded_work_slugs()):
        print(slug)


if __name__ == "__main__":
    main()
