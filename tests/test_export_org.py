"""Tests for scripts/export-org.py and shared export discovery helpers."""

import importlib.util
import sys
from pathlib import Path
from unittest.mock import patch

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

from lib import extract_export_file_names_from_text

_SCRIPT = Path(__file__).parent.parent / "scripts" / "export-org.py"
_spec = importlib.util.spec_from_file_location("export_org", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)


class TestExportFileDiscovery:
    def test_extracts_export_file_names(self):
        text = """* Published
:PROPERTIES:
:EXPORT_FILE_NAME: published
:END:
"""
        assert extract_export_file_names_from_text(text) == ["published.md"]

    def test_honors_inherited_noexport(self):
        text = """* Private :noexport:
** Child
:PROPERTIES:
:EXPORT_FILE_NAME: child
:END:
* Public
:PROPERTIES:
:EXPORT_FILE_NAME: public
:END:
"""
        assert extract_export_file_names_from_text(text) == ["public.md"]

    def test_allows_sibling_after_noexport(self):
        text = """* Private :noexport:
:PROPERTIES:
:EXPORT_FILE_NAME: private
:END:
* Public
:PROPERTIES:
:EXPORT_FILE_NAME: public
:END:
"""
        assert extract_export_file_names_from_text(text) == ["public.md"]


class TestFullExport:
    @pytest.mark.parametrize("failure", ["dataless", "unreadable", "directory"])
    def test_incomplete_scan_aborts_without_exporting_or_pruning(self, tmp_path, failure):
        src, out = tmp_path / "src", tmp_path / "out"
        src.mkdir()
        out.mkdir()
        for name in ("available", "unavailable"):
            (src / f"{name}.org").write_text(
                f"* Note\n:PROPERTIES:\n:EXPORT_FILE_NAME: {name}\n:END:\n")
            (out / f"{name}.md").write_text(f"previous {name}")
        cfg = {
            "source_dirs": [src], "output_dir": out,
            "elisp": tmp_path / "export.el", "skip_files": set(),
            "preserve_output": lambda _path: False,
        }
        extract = _mod.extract_export_file_names

        def read(path):
            if failure == "unreadable" and path.stem == "unavailable":
                raise PermissionError(f"Cannot read {path}")
            return extract(path)

        walk = _mod.os.walk

        def traverse(path, *, onerror):
            yield from walk(path, onerror=onerror)
            if failure == "directory":
                onerror(PermissionError(f"Cannot scan {path}/private"))

        with patch.dict(_mod.SECTIONS, {"notes": cfg}), \
             patch.object(_mod, "is_dataless", side_effect=lambda p:
                          failure == "dataless" and p.stem == "unavailable"), \
             patch.object(_mod, "extract_export_file_names", side_effect=read), \
             patch.object(_mod.os, "walk", side_effect=traverse), \
             patch.object(_mod, "run_emacs") as export, \
             patch.object(_mod, "safe_remove") as remove:
            with pytest.raises(SystemExit) as error:
                _mod.run_export("notes")
        assert error.value.code == 1
        export.assert_not_called()
        remove.assert_not_called()
        assert {p.name: p.read_text() for p in out.iterdir()} == {
            "available.md": "previous available", "unavailable.md": "previous unavailable",
        }

    def test_failure_leaves_outputs_untouched(self, tmp_path):
        src = tmp_path / "src"
        out = tmp_path / "out"
        src.mkdir()
        out.mkdir()

        org_file = src / "note.org"
        org_file.write_text("* Note\n:PROPERTIES:\n:EXPORT_FILE_NAME: note\n:END:\n")
        stale = out / "stale.md"
        stale.write_text("old")

        cfg = {
            "source_dirs": [src],
            "output_dir": out,
            "elisp": tmp_path / "export.el",
            "skip_files": set(),
            "preserve_output": lambda _path: False,
        }

        with patch.dict(_mod.SECTIONS, {"notes": cfg}), \
             patch.object(_mod, "run_emacs", return_value=1), \
             patch.object(_mod, "delete_md_files") as delete_mock:
            try:
                _mod.run_export("notes")
            except SystemExit as exc:
                assert exc.code == 1

        delete_mock.assert_not_called()
        assert stale.exists()

    def test_success_removes_stale_outputs_after_export(self, tmp_path):
        src = tmp_path / "src"
        out = tmp_path / "out"
        src.mkdir()
        out.mkdir()

        org_file = src / "note.org"
        org_file.write_text("* Note\n:PROPERTIES:\n:EXPORT_FILE_NAME: note\n:END:\n")
        stale = out / "stale.md"
        stale.write_text("old")

        cfg = {
            "source_dirs": [src],
            "output_dir": out,
            "elisp": tmp_path / "export.el",
            "skip_files": set(),
            "preserve_output": lambda _path: False,
        }

        with patch.dict(_mod.SECTIONS, {"notes": cfg}), \
             patch.object(_mod, "run_emacs", return_value=0), \
             patch.object(_mod, "delete_md_files") as delete_mock:
            _mod.run_export("notes")

        delete_mock.assert_called_once_with(out, ["stale.md"])

    def test_preserves_non_diary_quote_outputs(self, tmp_path):
        out = tmp_path / "out"
        out.mkdir()
        generated = out / "work-q-abcdef12.md"
        generated.write_text("generated separately")

        removed = _mod.remove_stale_outputs(
            out,
            valid_outputs=set(),
            preserve_output=lambda path: "-q-" in path.stem,
        )

        assert removed == 0
        assert generated.exists()
