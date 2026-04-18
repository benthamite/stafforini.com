#!/usr/bin/env bash
# Run the Python test suite with a working pytest invocation.
#
# The Homebrew pytest console script on this machine points at a removed
# Python 3.9 interpreter.  Prefer the selected Python's own pytest module, but
# fall back to the still-installed Homebrew Python 3.9 site-packages when the
# module is available there.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

PYTHON_BIN="${PYTHON:-python3}"

if "$PYTHON_BIN" -m pytest --version >/dev/null 2>&1; then
  exec "$PYTHON_BIN" -m pytest "$@"
fi

HOMEBREW_PY39_SITE_PACKAGES="/opt/homebrew/lib/python3.9/site-packages"
if [ -d "$HOMEBREW_PY39_SITE_PACKAGES/pytest" ]; then
  export PYTHONPATH="$HOMEBREW_PY39_SITE_PACKAGES${PYTHONPATH:+:$PYTHONPATH}"
  # The fallback pytest/pluggy install was built for older Python and emits
  # harmless SyntaxWarnings under Python 3.14.
  export PYTHONWARNINGS="${PYTHONWARNINGS:-ignore::SyntaxWarning}"
  exec "$PYTHON_BIN" -m pytest "$@"
fi

cat >&2 <<EOF
Error: pytest is not importable by $PYTHON_BIN.

Install pytest for the selected interpreter, or set PYTHON to an interpreter
that has pytest installed. Example:

  python3 -m pip install pytest
  bash scripts/test.sh
EOF
exit 1
