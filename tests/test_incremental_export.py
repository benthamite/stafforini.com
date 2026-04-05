"""Tests for scripts/incremental-export.py — failure handling and cleanup."""

import sys
from pathlib import Path
from unittest.mock import patch

import importlib.util

sys.path.insert(0, str(Path(__file__).parent.parent / "scripts"))

_SCRIPT = Path(__file__).parent.parent / "scripts" / "incremental-export.py"
_spec = importlib.util.spec_from_file_location("incremental_export", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

run_full_export = _mod.run_full_export
run_incremental_export = _mod.run_incremental_export


class TestRunIncrementalExport:
    def test_failure_leaves_outputs_and_manifest_unchanged(self, tmp_path):
        src = tmp_path / "src"
        out = tmp_path / "out"
        src.mkdir()
        out.mkdir()

        org_file = src / "note.org"
        org_file.write_text(":EXPORT_FILE_NAME: note\n")
        stale = out / "old-note.md"
        stale.write_text("old")

        manifest = {"files": {"note.org": {"mtime": 1.0, "outputs": ["old-note.md"]}}}
        current = {"note.org": 2.0}
        cfg = {"source_dirs": [src], "output_dir": out, "elisp": tmp_path / "export.el"}

        with patch.object(_mod, "resolve_relpath", return_value=org_file), \
             patch.object(_mod, "extract_export_file_names", return_value=["note.md"]), \
             patch.object(_mod, "run_emacs", return_value=1), \
             patch.object(_mod, "delete_md_files") as delete_mock, \
             patch.object(_mod, "save_manifest") as save_mock:
            ok = run_incremental_export("notes", cfg, manifest, current)

        assert ok is False
        delete_mock.assert_not_called()
        save_mock.assert_not_called()
        assert stale.exists()

    def test_deleted_sources_cleanup_without_export(self, tmp_path):
        src = tmp_path / "src"
        out = tmp_path / "out"
        src.mkdir()
        out.mkdir()

        stale = out / "old-note.md"
        stale.write_text("old")

        manifest = {"files": {"deleted.org": {"mtime": 1.0, "outputs": ["old-note.md"]}}}
        current = {}
        cfg = {"source_dirs": [src], "output_dir": out, "elisp": tmp_path / "export.el"}

        with patch.object(_mod, "delete_md_files") as delete_mock, \
             patch.object(_mod, "save_manifest") as save_mock:
            ok = run_incremental_export("notes", cfg, manifest, current)

        assert ok is True
        delete_mock.assert_called_once_with(out, ["old-note.md"])
        save_mock.assert_called_once_with("notes", {"files": {}})


class TestRunFullExport:
    def test_failure_leaves_outputs_and_manifest_unchanged(self, tmp_path):
        src = tmp_path / "src"
        out = tmp_path / "out"
        src.mkdir()
        out.mkdir()

        org_file = src / "note.org"
        org_file.write_text(":EXPORT_FILE_NAME: note\n")
        stale = out / "stale.md"
        stale.write_text("old")

        current = {"note.org": 1.0}
        cfg = {"source_dirs": [src], "output_dir": out, "elisp": tmp_path / "export.el"}

        with patch.object(_mod, "resolve_relpath", return_value=org_file), \
             patch.object(_mod, "extract_export_file_names", return_value=["note.md"]), \
             patch.object(_mod, "run_emacs", return_value=1), \
             patch.object(_mod, "delete_md_files") as delete_mock, \
             patch.object(_mod, "save_manifest") as save_mock:
            ok = run_full_export("notes", cfg, current)

        assert ok is False
        delete_mock.assert_not_called()
        save_mock.assert_not_called()
        assert stale.exists()

    def test_success_removes_stale_outputs_after_export(self, tmp_path):
        src = tmp_path / "src"
        out = tmp_path / "out"
        src.mkdir()
        out.mkdir()

        org_file = src / "note.org"
        org_file.write_text(":EXPORT_FILE_NAME: note\n")
        stale = out / "stale.md"
        stale.write_text("old")

        current = {"note.org": 1.0}
        cfg = {"source_dirs": [src], "output_dir": out, "elisp": tmp_path / "export.el"}

        with patch.object(_mod, "resolve_relpath", return_value=org_file), \
             patch.object(_mod, "extract_export_file_names", return_value=["note.md"]), \
             patch.object(_mod, "run_emacs", return_value=0), \
             patch.object(_mod, "delete_md_files") as delete_mock, \
             patch.object(_mod, "save_manifest") as save_mock:
            ok = run_full_export("notes", cfg, current)

        assert ok is True
        delete_mock.assert_called_once_with(out, ["stale.md"])
        save_mock.assert_called_once()
