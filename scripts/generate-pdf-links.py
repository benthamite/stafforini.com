#!/usr/bin/env python3
"""Generate Hugo's PDF link inventory from current bibliography attachments."""

from lib import REPO_ROOT, atomic_write_json
from pdf_inventory import available_pdf_slugs


def generate_pdf_links(repo_root=REPO_ROOT):
    """Refresh the link map atomically without deleting generated assets."""
    slugs = available_pdf_slugs(repo_root)
    generated = {path.stem for path in (repo_root / "static" / "pdfs").glob("*.pdf")}
    output = repo_root / "data" / "pdf-links.json"
    output.parent.mkdir(parents=True, exist_ok=True)
    atomic_write_json(output, {slug: True for slug in sorted(slugs)})
    print(f"PDF links: {len(slugs)} current attachments")
    print(f"Generated PDFs without current matching attachments: {len(generated - slugs)} "
          "(including removed attachments; files retained)")
    return slugs


if __name__ == "__main__":
    generate_pdf_links()
