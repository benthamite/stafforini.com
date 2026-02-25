#!/usr/bin/env bash
# Batch export blog org files to Hugo markdown via ox-hugo.
# Uses incremental export by default (only changed files).
# Pass --full to force a complete re-export.
# Usage: bash scripts/export-notes.sh [--full]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

python3 "$SCRIPT_DIR/incremental-export.py" notes "$@"

echo "--- Injecting lastmod dates ---"
python3 "$SCRIPT_DIR/inject-lastmod.py"

echo "--- Generating backlinks ---"
python3 "$SCRIPT_DIR/generate-backlinks.py"

echo "--- Generating citing-notes index ---"
python3 "$SCRIPT_DIR/generate-citing-notes.py"

echo "--- Validating exported files ---"
python3 -c "
import re, sys
from pathlib import Path
content_dir = Path('$SCRIPT_DIR').parent / 'content' / 'notes'
if not content_dir.exists():
    sys.exit(0)
missing = []
for md in sorted(content_dir.glob('*.md')):
    if md.name == '_index.md':
        continue
    text = md.read_text()
    match = re.match(r'(\+\+\+\n)(.*?)(\n\+\+\+)', text, re.DOTALL)
    if match and not re.search(r'^title\s*=', match.group(2), re.MULTILINE):
        missing.append(md.name)
if missing:
    print(f'ERROR: {len(missing)} file(s) missing title in frontmatter:', file=sys.stderr)
    for name in missing:
        print(f'  {name}', file=sys.stderr)
    sys.exit(1)
print(f'Validation OK: all exported files have titles.')
"
