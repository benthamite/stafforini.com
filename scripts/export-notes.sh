#!/usr/bin/env bash
# Batch export blog org files to Hugo markdown via ox-hugo.
# Always does a full source scan/export.  The optional --full flag is accepted
# for compatibility with older Emacs commands and shell habits.
# Usage: bash scripts/export-notes.sh [--full]
source "$(dirname "$0")/common.sh"

# Pre-export: ensure the ID-slug map is fresh (the export itself needs it)
run_step "Generating ID-slug map" python3 "$SCRIPT_DIR/generate-id-slug-map.py"

python3 "$SCRIPT_DIR/export-org.py" notes "$@"

bash "$SCRIPT_DIR/regenerate-data.sh" --notes

echo "--- Validating exported files ---"
python3 -c "
import re, sys
from pathlib import Path
import os
content_dir = Path(os.environ.get('REPO_ROOT', '$REPO_ROOT')) / 'content' / 'notes'
if not content_dir.exists():
    print('Warning: content/notes/ not found — skipping validation.', file=sys.stderr)
    sys.exit(0)
missing = []
for md in sorted(content_dir.glob('*.md')):
    if md.name == '_index.md':
        continue
    text = md.read_text()
    # ox-hugo wraps frontmatter in +++ delimiters (TOML format).
    # Check that the title key exists inside the frontmatter block.
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

run_step "Verifying dev site" python3 "$SCRIPT_DIR/verify-site.py" --build dev

stop_hugo_servers_after_content_change "notes export"
