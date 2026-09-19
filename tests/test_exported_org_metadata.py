"""Keep exported subtree titles, links and URL overrides paired together."""

import importlib.util
import sqlite3
import sys
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))

from lib import exported_org_pages


def load_script(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


SOURCE = """:PROPERTIES:
:ID: FILE
:END:
* First
:PROPERTIES:
:ID: FIRST
:EXPORT_FILE_NAME: first
:EXPORT_HUGO_URL: /first-custom/
:END:
** Detail
:PROPERTIES:
:ID: FIRST-CHILD
:END:
** Hidden :noexport:
:PROPERTIES:
:ID: HIDDEN
:END:
* Second =code= :note:
:PROPERTIES:
:ID: SECOND
:EXPORT_FILE_NAME: second
:END:
** Detail
:PROPERTIES:
:ID: SECOND-CHILD
:END:
"""


def test_both_discovery_paths_map_ids_to_their_exported_subtree(tmp_path, monkeypatch):
    module = load_script("generate-id-slug-map")
    source = tmp_path / "bundle.org"
    source.write_text(SOURCE)
    database = tmp_path / "roam.db"
    with sqlite3.connect(database) as conn:
        conn.execute("CREATE TABLE nodes (id TEXT, file TEXT, level INTEGER)")
        for node_id in ("FILE", "FIRST", "FIRST-CHILD", "HIDDEN", "SECOND", "SECOND-CHILD"):
            conn.execute("INSERT INTO nodes VALUES (?, ?, ?)",
                         (f'"{node_id}"', f'"{source}"', 1))
    monkeypatch.setattr(module, "NOTES_DIR", tmp_path)
    monkeypatch.setattr(module, "ORGROAM_DB_PATH", database)
    expected = ({"FIRST": "first", "FIRST-CHILD": "first",
                 "SECOND": "second", "SECOND-CHILD": "second"},
                {"FIRST": "/first-custom/", "FIRST-CHILD": "/first-custom/"})
    assert module.scan_notes_filesystem() == expected
    assert module.scan_published_notes() == expected


def test_title_fixup_selects_matching_export_and_preserves_body(tmp_path):
    module = load_script("inject-lastmod")
    source = tmp_path / "bundle.org"
    source.write_text(SOURCE)
    target = tmp_path / "second.md"
    target.write_text('+++\ntitle = "Second code"\n+++\nSecond body.\n')
    module.apply_front_matter_fixups(target, {"second.md": source})
    assert target.read_text() == '+++\ntitle = "Second `code`"\n+++\nSecond body.\n'


def test_nested_export_resets_ownership_and_honors_export_title():
    pages = exported_org_pages(SOURCE.replace("* Second =code= :note:", "** Second"))
    assert pages[1]["ids"] == ["SECOND"]
    assert pages[0]["ids"] == ["FIRST", "FIRST-CHILD", "SECOND-CHILD"]
    pages = exported_org_pages(SOURCE.replace(":EXPORT_FILE_NAME: second",
                                             ":EXPORT_FILE_NAME: second\n:EXPORT_TITLE: Custom"))
    assert pages[1]["title"] == "Custom"


def test_single_export_keeps_file_id_but_excluded_export_is_ignored():
    pages = exported_org_pages(SOURCE.replace("* Second =code= :note:", "* Second :ARCHIVE:"))
    assert len(pages) == 1
    assert pages[0]["ids"] == ["FIRST", "FIRST-CHILD", "FILE"]
