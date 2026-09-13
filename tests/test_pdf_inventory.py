"""PDF links must follow current attachments, even when old assets remain."""

import importlib.util
import json
from pathlib import Path

import pytest

import pdf_inventory


@pytest.fixture
def inventory(tmp_path, monkeypatch):
    bib = tmp_path / 'works.bib'
    source = tmp_path / 'source.pdf'
    source.write_bytes(b'%PDF source')
    slug = 'sinhababu-2024-pleasure-goodness-morality'
    pdfs = tmp_path / 'static/pdfs'
    thumbs = tmp_path / 'static/pdf-thumbnails'
    pdfs.mkdir(parents=True)
    thumbs.mkdir(parents=True)
    (pdfs / f'{slug}.pdf').write_bytes(b'%PDF generated')
    (thumbs / f'{slug}.png').write_bytes(b'thumbnail')
    (pdfs / '.manifest.json').write_text(json.dumps({slug: {'src_path': str(source)}}))
    monkeypatch.setattr(pdf_inventory, 'BIB_FILES', [bib])
    monkeypatch.setattr(pdf_inventory, 'load_excluded_works', lambda: {})

    def write_entry(attachment=source):
        file_field = f'file = {{{attachment}}},' if attachment else ''
        bib.write_text('@article{Sinhababu2024PleasureGoodnessMorality,\n'
                       'title = {Pleasure, Goodness, and Morality},\n'
                       f'year = {{2024}},\n{file_field}\n}}\n')

    write_entry()
    return tmp_path, slug, bib, source, write_entry


def test_removed_attachment_hides_retained_generated_files(inventory):
    root, slug, _, _, write_entry = inventory
    assert pdf_inventory.available_pdf_slugs(root) == {slug}
    write_entry(None)
    assert pdf_inventory.available_pdf_slugs(root) == set()
    assert (root / f'static/pdfs/{slug}.pdf').is_file()
    assert (root / f'static/pdf-thumbnails/{slug}.png').is_file()


def test_changed_attachment_does_not_link_previous_pdf(inventory):
    root, _, _, _, write_entry = inventory
    replacement = root / 'replacement.pdf'
    replacement.write_bytes(b'%PDF replacement')
    write_entry(replacement)
    assert pdf_inventory.available_pdf_slugs(root) == set()


@pytest.mark.parametrize('missing', ['static/pdfs', 'static/pdf-thumbnails',
                                      'static/pdfs/.manifest.json', 'works.bib'])
def test_missing_required_input_aborts(inventory, missing):
    root, *_ = inventory
    target = root / missing
    target.rename(target.with_name(target.name + '-unavailable'))
    with pytest.raises(SystemExit, match='missing'):
        pdf_inventory.available_pdf_slugs(root)


@pytest.mark.parametrize('missing', [
    'static/pdfs/sinhababu-2024-pleasure-goodness-morality.pdf',
    'static/pdf-thumbnails/sinhababu-2024-pleasure-goodness-morality.png'])
def test_missing_individual_asset_hides_link(inventory, missing):
    root, *_ = inventory
    (root / missing).unlink()
    assert pdf_inventory.available_pdf_slugs(root) == set()


def test_generator_replaces_map_after_attachment_removal(inventory):
    root, slug, _, _, write_entry = inventory
    spec = importlib.util.spec_from_file_location(
        'generate_pdf_links', Path(__file__).parents[1] / 'scripts/generate-pdf-links.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.generate_pdf_links(root)
    output = root / 'data/pdf-links.json'
    assert json.loads(output.read_text()) == {slug: True}
    write_entry(None)
    module.generate_pdf_links(root)
    assert json.loads(output.read_text()) == {}


def test_unavailable_bibliography_preserves_previous_map(inventory):
    root, _, bib, _, _ = inventory
    spec = importlib.util.spec_from_file_location(
        'generate_pdf_links', Path(__file__).parents[1] / 'scripts/generate-pdf-links.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.generate_pdf_links(root)
    output = root / 'data/pdf-links.json'
    previous = output.read_bytes()
    bib.unlink()
    with pytest.raises(SystemExit):
        module.generate_pdf_links(root)
    assert output.read_bytes() == previous


def test_processing_rebuilds_changed_source_even_with_same_mtime(inventory, monkeypatch):
    from process_pdfs_helpers import _mod as processor
    root, slug, _, source, _ = inventory
    monkeypatch.setattr(processor, 'PDFS_DIR', root / 'static/pdfs')
    monkeypatch.setattr(processor, 'THUMBS_DIR', root / 'static/pdf-thumbnails')
    record = {slug: {'src_path': str(source), 'src_mtime': source.stat().st_mtime}}
    assert not processor.needs_processing(slug, source, record, False)
    record[slug]['src_path'] = str(root / 'old-attachment.pdf')
    assert processor.needs_processing(slug, source, record, False)


@pytest.mark.parametrize('asset', ['pdfs', 'pdf-thumbnails'])
def test_processing_rebuilds_missing_generated_asset(inventory, monkeypatch, asset):
    from process_pdfs_helpers import _mod as processor
    root, slug, _, source, _ = inventory
    monkeypatch.setattr(processor, 'PDFS_DIR', root / 'static/pdfs')
    monkeypatch.setattr(processor, 'THUMBS_DIR', root / 'static/pdf-thumbnails')
    record = {slug: {'src_path': str(source), 'src_mtime': source.stat().st_mtime}}
    suffix = 'pdf' if asset == 'pdfs' else 'png'
    (root / 'static' / asset / f'{slug}.{suffix}').unlink()
    assert processor.needs_processing(slug, source, record, False)


def test_offline_source_preserves_current_processed_attachment(inventory):
    root, slug, _, source, _ = inventory
    source.unlink()
    assert pdf_inventory.available_pdf_slugs(root) == {slug}
