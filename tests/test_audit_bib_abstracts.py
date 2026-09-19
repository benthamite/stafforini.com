"""Clearing flagged abstracts must preserve valid bibliography entries."""

import importlib.util
from pathlib import Path
import shutil
import subprocess

import pytest


_SCRIPT = Path(__file__).parent.parent / "scripts" / "audit-bib-abstracts.py"
_spec = importlib.util.spec_from_file_location("audit_bib_abstracts", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)


@pytest.mark.parametrize("position,final_comma", [
    (0, True), (1, True), (4, True), (4, False),
])
def test_apply_removes_the_complete_field(tmp_path, monkeypatch, position, final_comma):
    fields = ["author = {Example, A.}", "title = {A title}",
              "publisher = {A publisher}", "year = {2020}"]
    fields.insert(position, "abstract = {Please provide the {text} you would like me to summarize}")
    original = "@book{Example,\n " + ",\n ".join(fields) + ("," if final_comma else "") + "\n}\n"
    bib = tmp_path / "check.bib"
    bib.write_text(original)
    monkeypatch.setattr(_mod, "BIB_DIR", tmp_path)
    monkeypatch.setattr(_mod.sys, "argv", [str(_SCRIPT), "--apply"])

    assert _mod.main() == 0
    rewritten = bib.read_text()
    assert "abstract" not in rewritten
    assert ",," not in rewritten
    for field in fields:
        if not field.startswith("abstract"):
            assert field in rewritten
    assert bib.with_suffix(".bib.bak").read_text() == original

    # A real parser detects the original doubled-comma corruption, which the
    # site's permissive field extractor would silently accept.
    bibtex = shutil.which("bibtex")
    if bibtex:
        (tmp_path / "check.aux").write_text(
            "\\relax\n\\citation{Example}\n\\bibdata{check}\n\\bibstyle{plain}\n")
        result = subprocess.run([bibtex, "check"], cwd=tmp_path, text=True,
                                capture_output=True)
        assert result.returncode == 0, result.stdout + result.stderr


def test_report_only_leaves_bibliography_unchanged(tmp_path, monkeypatch):
    bib = tmp_path / "check.bib"
    original = "@book{Example,\n abstract = {Please provide the text},\n title = {Keep}\n}\n"
    bib.write_text(original)
    monkeypatch.setattr(_mod, "BIB_DIR", tmp_path)
    monkeypatch.setattr(_mod.sys, "argv", [str(_SCRIPT)])
    assert _mod.main() == 0
    assert bib.read_text() == original
    assert not bib.with_suffix(".bib.bak").exists()
