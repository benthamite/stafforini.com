#!/usr/bin/env python3
"""Generate standalone public Python scripts from the two copycat Org notes."""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_NOTES_DIR = Path.home() / "My Drive" / "notes" / "public"
COMMANDS = (
    "data",
    "performance",
    "chart",
    "comparison",
    "sensitivity",
    "delay",
    "calculator",
)


@dataclass(frozen=True)
class Profile:
    slug: str
    org_name: str
    prefix: str
    comparison_block: str
    comparison_label: str
    option_cache_literal: str

    @property
    def output_name(self) -> str:
        return f"{self.slug}.py"

    @property
    def blocks(self) -> dict[str, str]:
        return {
            "performance": f"{self.prefix}-perf",
            "chart": f"{self.prefix}-chart",
            "comparison": self.comparison_block,
            "sensitivity": f"{self.prefix}-sensitivity",
            "delay": f"{self.prefix}-delay",
            "calculator": f"{self.prefix}-calc",
        }


PROFILES = (
    Profile(
        slug="situational-awareness-lp",
        org_name="situational-awareness-lp.org",
        prefix="sa",
        comparison_block="sa-chart-ais",
        comparison_label="AIS",
        option_cache_literal=(
            "OPTION_CACHE_DIR = os.path.expanduser("
            "'~/My Drive/notes/.sa-lp-option-cache')"
        ),
    ),
    Profile(
        slug="value-aligned-research-advisors",
        org_name="value-aligned-research-advisors.org",
        prefix="vara",
        comparison_block="vara-chart-salp",
        comparison_label="SALP",
        option_cache_literal=(
            "OPTION_CACHE_DIR = os.path.expanduser("
            "'~/My Drive/notes/.value-aligned-research-advisors-option-cache')"
        ),
    ),
)


class GenerationError(RuntimeError):
    """Raised when the Org source no longer matches the packaging contract."""


def extract_python_blocks(org_text: str) -> dict[str, str]:
    """Return named Python block bodies, rejecting ambiguous source."""
    name_re = re.compile(r"^#\+name:\s*(\S+)\s*$", re.MULTILINE | re.IGNORECASE)
    blocks: dict[str, str] = {}
    for match in name_re.finditer(org_text):
        name = match.group(1)
        line_end = org_text.find("\n", match.end())
        if line_end == -1:
            continue
        begin_end = org_text.find("\n", line_end + 1)
        begin_line = org_text[line_end + 1 : begin_end if begin_end != -1 else None]
        if not re.match(r"^#\+begin_src\s+python(?:\s|$)", begin_line, re.I):
            continue
        if begin_end == -1:
            raise GenerationError(f"Unclosed Python block {name!r}")
        end_match = re.search(
            r"^#\+end_src\s*$", org_text[begin_end + 1 :], re.MULTILINE | re.I
        )
        if not end_match:
            raise GenerationError(f"Unclosed Python block {name!r}")
        body_start = begin_end + 1
        body_end = body_start + end_match.start()
        if name in blocks:
            raise GenerationError(f"Duplicate named Python block {name!r}")
        blocks[name] = org_text[body_start:body_end].rstrip("\n")
    return blocks


def _require_blocks(blocks: dict[str, str], names: list[str], org_name: str) -> None:
    missing = [name for name in names if name not in blocks]
    if missing:
        raise GenerationError(f"{org_name}: missing blocks: {', '.join(missing)}")


def _indent(text: str, spaces: int) -> str:
    prefix = " " * spaces
    return "\n".join(prefix + line if line else "" for line in text.splitlines())


def _replace_once(text: str, old: str, new: str, context: str) -> str:
    count = text.count(old)
    if count != 1:
        raise GenerationError(
            f"{context}: expected exactly one occurrence of {old!r}, found {count}"
        )
    return text.replace(old, new, 1)


def _portable_library(body: str, profile: Profile, cache_expression: str) -> str:
    body = _replace_once(
        body,
        profile.option_cache_literal,
        f"OPTION_CACHE_DIR = str({cache_expression})",
        f"{profile.org_name} shared library",
    )
    start_marker = "def _marketdata_key():"
    end_marker = "def _marketdata_get(path, params, api_key):"
    start = body.find(start_marker)
    end = body.find(end_marker)
    if start == -1 or end == -1 or end < start:
        raise GenerationError(
            f"{profile.org_name} shared library: MarketData key resolver changed"
        )
    replacement = '''def _marketdata_key():
    """Return a MarketData key supplied explicitly through the environment."""
    key = (os.environ.get('MARKETDATA_KEY', '')
           or os.environ.get('MARKETDATA_API_KEY', ''))
    return key or None


'''
    return body[:start] + replacement + body[end:]


def _portable_data(body: str, profile: Profile) -> str:
    local_identity = """SEC_UA = os.environ.get(
    'SEC_USER_AGENT',
    'Pablo Stafforini stafforini.com; contact@stafforini.com')"""
    public_identity = """SEC_UA = os.environ.get('SEC_USER_AGENT')
if not SEC_UA:
    raise RuntimeError(
        'SEC_USER_AGENT must identify you and provide a contact address')"""
    return _replace_once(
        body,
        local_identity,
        public_identity,
        f"{profile.org_name} data block",
    )


def _portable_command(body: str, profile: Profile, command: str) -> str:
    marker = f"<<{profile.prefix}-lib>>"
    lines = body.splitlines()
    marker_lines = [index for index, line in enumerate(lines) if line.strip() == marker]
    if marker_lines != [0]:
        raise GenerationError(
            f"{profile.org_name} {command}: expected {marker!r} as the first line"
        )
    body = "\n".join(lines[1:]).lstrip("\n")
    local_root = "HUGO_BASE = os.path.expanduser('~/repos/stafforini.com')"
    if local_root in body:
        body = _replace_once(
            body,
            local_root,
            "HUGO_BASE = str(args.output_root)",
            f"{profile.org_name} {command}",
        )

    if profile.prefix == "vara" and command == "comparison":
        start_marker = "# ── SALP series: run the Situational Awareness LP note's own model ──"
        end_marker = "exec(_extract_salp_block(_salp_org, 'sa-lib'), salp_ns)"
        start = body.find(start_marker)
        end = body.find(end_marker)
        if start == -1 or end == -1 or end < start:
            raise GenerationError("VARA comparison: SALP Org-loading prelude changed")
        end += len(end_marker)
        replacement = (
            "# ── SALP series: run the embedded SALP model ────────────────\n"
            "# The generator packages SALP's current data and shared-library blocks\n"
            "# into this file so the comparison remains standalone.\n"
            "salp_ns = _build_salp_context(args)"
        )
        body = body[:start] + replacement + body[end:]

    return body


def _data_function(name: str, body: str, indent: int = 0) -> str:
    prefix = " " * indent
    return f"{prefix}def {name}():\n{_indent(body, indent + 4)}"


def _embedded_salp_context(salp_blocks: dict[str, str], salp: Profile) -> str:
    data_name = f"{salp.prefix}-data"
    lib_name = f"{salp.prefix}-lib"
    _require_blocks(salp_blocks, [data_name, lib_name], salp.org_name)
    library = _portable_library(
        salp_blocks[lib_name], salp, "args.salp_cache_dir"
    )
    parts = [
        "def _build_salp_context(args):",
        _data_function(
            "_load_salp_data",
            _portable_data(salp_blocks[data_name], salp),
            indent=4,
        ),
        "    data = _load_salp_data()",
        _indent(library, 4),
        "    return locals()",
    ]
    return "\n".join(parts)


def render_script(
    profile: Profile,
    org_text: str,
    *,
    salp_org_text: str | None = None,
) -> str:
    """Render one deterministic standalone script."""
    blocks = extract_python_blocks(org_text)
    data_name = f"{profile.prefix}-data"
    lib_name = f"{profile.prefix}-lib"
    required = [data_name, lib_name, *profile.blocks.values()]
    _require_blocks(blocks, required, profile.org_name)

    library = _portable_library(blocks[lib_name], profile, "args.cache_dir")
    commands = {
        command: _portable_command(blocks[block_name], profile, command)
        for command, block_name in profile.blocks.items()
    }

    title = (
        "Situational Awareness LP copycat analysis"
        if profile.prefix == "sa"
        else "Value-Aligned Research Advisors copycat analysis"
    )
    header = f'''#!/usr/bin/env python3
"""{title}.

Generated from the code blocks in the corresponding stafforini.com note.
Do not edit this file directly.

Dependencies:
    python -m pip install yfinance pandas numpy requests scipy plotly

Historical option data comes from MarketData. Set MARKETDATA_KEY when the
selected command needs quotes that are not already present in --cache-dir.
Set SEC_USER_AGENT to your name, application, and contact address as required
by the SEC's automated-access policy.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path


COMMANDS = {COMMANDS!r}


'''
    parts = [
        header,
        _data_function("_load_data", _portable_data(blocks[data_name], profile)),
        "",
    ]

    if profile.prefix == "vara":
        if salp_org_text is None:
            raise GenerationError("VARA generation requires the SALP Org source")
        salp = PROFILES[0]
        salp_blocks = extract_python_blocks(salp_org_text)
        parts.extend([_embedded_salp_context(salp_blocks, salp), ""])

    run_lines = [
        "def _run(command, args):",
        "    data = _load_data()",
        "    if command == 'data':",
        "        print(data)",
        "        return",
        _indent(library, 4),
        "    (args.output_root / 'static' / 'images').mkdir(parents=True, exist_ok=True)",
    ]
    for command in COMMANDS:
        if command == "data":
            continue
        run_lines.extend(
            [
                f"    if command == {command!r}:",
                _indent(commands[command], 8),
                "        return",
            ]
        )
    run_lines.append("    raise ValueError(f'Unknown command: {command}')")
    parts.extend(["\n".join(run_lines), ""])

    cache_default = f"Path.home() / '.cache' / 'stafforini-copycat' / '{profile.slug}'"
    parser_lines = [
        "def _parser():",
        f"    parser = argparse.ArgumentParser(description={title!r})",
        "    parser.add_argument('command', nargs='?', default='performance',",
        "                        choices=(*COMMANDS, 'all'))",
        "    parser.add_argument('--output-root', type=Path, default=Path.cwd(),",
        "                        help='Root containing static/images (default: cwd)')",
        f"    parser.add_argument('--cache-dir', type=Path, default={cache_default},",
        "                        help='Historical option-price cache directory')",
    ]
    if profile.prefix == "vara":
        parser_lines.extend(
            [
                "    parser.add_argument('--salp-cache-dir', type=Path,",
                "                        default=Path.home() / '.cache' /",
                "                        'stafforini-copycat' / 'situational-awareness-lp',",
                "                        help='SALP option cache used by the comparison')",
            ]
        )
    parser_lines.extend(
        [
            "    parser.add_argument('--list-commands', action='store_true',",
            "                        help='List commands without fetching any data')",
            "    return parser",
            "",
            "",
            "def main():",
            "    args = _parser().parse_args()",
            "    if args.list_commands:",
            "        print('\\n'.join((*COMMANDS, 'all')))",
            "        return",
            "    if args.command == 'all':",
            "        for command in COMMANDS[1:]:",
            "            _run(command, args)",
            "    else:",
            "        _run(args.command, args)",
            "",
            "",
            "if __name__ == '__main__':",
            "    main()",
        ]
    )
    parts.append("\n".join(parser_lines))
    rendered = "\n".join(parts).rstrip() + "\n"

    forbidden = ("<<", "~/repos/stafforini.com", "My Drive", "SALP_ORG")
    leftovers = [token for token in forbidden if token in rendered]
    if leftovers:
        raise GenerationError(
            f"{profile.output_name}: non-portable source remains: {leftovers}"
        )
    return rendered


def generate_all(
    notes_dir: Path, selected: tuple[str, ...] = ("sa", "vara")
) -> dict[Profile, str]:
    sources = {profile: (notes_dir / profile.org_name).read_text() for profile in PROFILES}
    salp_text = sources[PROFILES[0]]
    return {
        profile: render_script(
            profile,
            sources[profile],
            salp_org_text=salp_text if profile.prefix == "vara" else None,
        )
        for profile in PROFILES
        if profile.prefix in selected
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--notes-dir", type=Path, default=DEFAULT_NOTES_DIR)
    parser.add_argument(
        "--output-dir", type=Path, default=REPO_ROOT / "static" / "code"
    )
    parser.add_argument(
        "--check", action="store_true", help="Fail if checked-in files are stale"
    )
    parser.add_argument(
        "--profile",
        choices=("all", "sa", "vara"),
        default="all",
        help="Generate both scripts or only the selected profile",
    )
    args = parser.parse_args(argv)

    selected = ("sa", "vara") if args.profile == "all" else (args.profile,)
    rendered = generate_all(args.notes_dir, selected)
    stale: list[Path] = []
    for profile, text in rendered.items():
        destination = args.output_dir / profile.output_name
        if args.check:
            if not destination.exists() or destination.read_text() != text:
                stale.append(destination)
            continue
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(text)
        print(destination)

    if stale:
        for path in stale:
            print(f"Stale generated script: {path}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
