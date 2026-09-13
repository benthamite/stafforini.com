"""Render the real work template as bibliography attachments change."""

import importlib.util
import json
from pathlib import Path
import shutil
import subprocess

import pytest

import pdf_inventory
from lib import cite_key_to_slug


REPO_ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "generate_pdf_links_rendering", REPO_ROOT / "scripts/generate-pdf-links.py"
)
generator = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(generator)


@pytest.fixture
def work_site(tmp_path, monkeypatch):
    """Small Hugo site with real PDF inventory generation and work rendering."""
    if not shutil.which("hugo"):
        pytest.skip("Hugo is required to verify rendered PDF links")
    for directory in (
        "layouts/_default", "layouts/works", "layouts/partials", "content/works",
        "static/pdfs", "static/pdf-thumbnails", "data", "sources",
    ):
        (tmp_path / directory).mkdir(parents=True)
    (tmp_path / "hugo.toml").write_text(
        'baseURL = "https://example.org/"\n'
        '[params]\npdfBaseURL = "https://pdf.example.org"\n'
        'thumbBaseURL = "https://thumb.example.org"\n'
    )
    (tmp_path / "layouts/_default/baseof.html").write_text(
        '<!doctype html><html><body>{{ block "main" . }}{{ end }}</body></html>'
    )
    shutil.copyfile(
        REPO_ROOT / "layouts/works/single.html",
        tmp_path / "layouts/works/single.html",
    )
    for name in (
        "pagefind-work-meta", "canonical-link-url", "editor-abbrev",
        "note-url", "quote-entry",
    ):
        (tmp_path / f"layouts/partials/{name}.html").write_text("")
    (tmp_path / "layouts/partials/standalone-type.html").write_text(
        "{{ return false }}"
    )
    keys = ("Sinhababu2024PleasureGoodnessMorality", "Parfit2006KantArgumentsHis")
    sources = {}
    manifest = {}
    for key in keys:
        slug = cite_key_to_slug(key)
        source = tmp_path / "sources" / f"{slug}.pdf"
        source.write_bytes(b"fixture source PDF")
        sources[key] = source
        manifest[slug] = {"src_path": str(source)}
        (tmp_path / f"static/pdfs/{slug}.pdf").write_bytes(b"old generated PDF")
        (tmp_path / f"static/pdf-thumbnails/{slug}.png").write_bytes(b"old thumbnail")
        (tmp_path / f"content/works/{slug}.md").write_text(
            f'+++\ntitle = "{key}"\nentry_type = "article"\n+++\n'
        )
    (tmp_path / "static/pdfs/.manifest.json").write_text(json.dumps(manifest))
    bibliography = tmp_path / "references.bib"
    monkeypatch.setattr(pdf_inventory, "BIB_FILES", [bibliography])
    monkeypatch.setattr(pdf_inventory, "load_excluded_works", lambda: {})

    def update_bibliography(attachments):
        bibliography.write_text("\n".join(
            f"@article{{{key},\n  title = {{{key}}},\n  year = {{2024}},\n"
            + (f"  file = {{{attachments[key]}}},\n" if key in attachments else "")
            + "}\n"
            for key in keys
        ))
        generator.generate_pdf_links(tmp_path)

    def render():
        return subprocess.run(
            ["hugo", "--source", str(tmp_path), "--destination", str(tmp_path / "public"),
             "--noBuildLock"],
            capture_output=True, text=True, check=False,
        )

    def html(key):
        return (tmp_path / "public/works" / cite_key_to_slug(key) / "index.html").read_text()

    return tmp_path, sources, update_bibliography, render, html


def test_removing_attachment_removes_rendered_link_despite_retained_assets(work_site):
    root, sources, update, render, html = work_site
    removed, retained = sources
    update(sources)
    result = render()
    assert result.returncode == 0, result.stdout + result.stderr
    for key in sources:
        assert f'href="https://pdf.example.org/{cite_key_to_slug(key)}.pdf"' in html(key)
        assert 'class="pdf-preview"' in html(key)

    # Exactly the reported transition: only the bibliography attachment changes.
    assets = {path: path.read_bytes() for path in (root / "static").rglob("*") if path.is_file()}
    update({retained: sources[retained]})
    result = render()
    assert result.returncode == 0, result.stdout + result.stderr
    assert 'class="pdf-preview"' not in html(removed)
    assert f"{cite_key_to_slug(removed)}.pdf" not in html(removed)
    assert f"{cite_key_to_slug(removed)}.png" not in html(removed)
    assert f'href="https://pdf.example.org/{cite_key_to_slug(retained)}.pdf"' in html(retained)
    assert all(path.read_bytes() == original for path, original in assets.items())


def test_changing_attachment_hides_previous_pdf_until_reprocessed(work_site):
    root, sources, update, render, html = work_site
    changed, retained = sources
    replacement = root / "sources/replacement.pdf"
    replacement.write_bytes(b"replacement PDF")
    update({**sources, changed: replacement})
    result = render()
    assert result.returncode == 0, result.stdout + result.stderr
    assert 'class="pdf-preview"' not in html(changed)
    assert 'class="pdf-preview"' in html(retained)


def test_missing_inventory_aborts_build_instead_of_hiding_every_pdf(work_site):
    _, _, _, render, _ = work_site
    result = render()
    assert result.returncode != 0
    assert "Missing PDF link inventory" in result.stdout + result.stderr
