"""Tests for scripts/export-org.py and shared export discovery helpers."""

import importlib.util
import sys
from pathlib import Path
from unittest.mock import patch

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
