"""Import helpers from process-pdfs.py for testing.

process-pdfs.py has a hyphen in its name and heavy top-level imports
(pikepdf, PIL), so we import only the pure-logic functions we need.
"""

import importlib.util
import sys
from pathlib import Path
from unittest.mock import MagicMock

# Add scripts/ to path for lib imports
SCRIPTS_DIR = Path(__file__).parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS_DIR))

# Stub out pikepdf and PIL so we don't need them for pure-logic tests
sys.modules.setdefault("pikepdf", MagicMock())
sys.modules.setdefault("PIL", MagicMock())
sys.modules.setdefault("PIL.Image", MagicMock())

_SCRIPT = SCRIPTS_DIR / "process-pdfs.py"
_spec = importlib.util.spec_from_file_location("process_pdfs", _SCRIPT)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

# Re-export the functions and constants tests need
_is_excluded_book = _mod._is_excluded_book
resolve_crossref_pdfs = _mod.resolve_crossref_pdfs
BOOK_LIKE_TYPES = _mod.BOOK_LIKE_TYPES
BOOK_YEAR_CUTOFF = _mod.BOOK_YEAR_CUTOFF
