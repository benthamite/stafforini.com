#!/usr/bin/env python3
"""Compatibility wrapper for the old incremental exporter.

The publishing pipeline now uses scripts/export-org.py, which always does a
full source scan and does not read or write export manifests.  This wrapper
keeps old shell/Emacs habits from breaking while routing them to the new
stateless exporter.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path


def main() -> None:
    script = Path(__file__).with_name("export-org.py")
    os.execv(sys.executable, [sys.executable, str(script), *sys.argv[1:]])


if __name__ == "__main__":
    main()
