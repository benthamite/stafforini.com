import ast
import importlib.util
import os
import subprocess
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
GENERATOR_PATH = ROOT / "scripts/generate-copycat-scripts.py"
NOTES_DIR = Path.home() / "My Drive/notes/public"
OUTPUT_DIR = ROOT / "static/code"

spec = importlib.util.spec_from_file_location("copycat_script_generator", GENERATOR_PATH)
generator = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = generator
spec.loader.exec_module(generator)


@pytest.mark.parametrize("profile", generator.PROFILES, ids=lambda item: item.prefix)
def test_checked_in_script_matches_org_source(profile):
    sources = {
        item: (NOTES_DIR / item.org_name).read_text()
        for item in generator.PROFILES
    }
    rendered = generator.render_script(
        profile,
        sources[profile],
        salp_org_text=sources[generator.PROFILES[0]]
        if profile.prefix == "vara"
        else None,
    )

    assert (OUTPUT_DIR / profile.output_name).read_text() == rendered


@pytest.mark.parametrize("profile", generator.PROFILES, ids=lambda item: item.prefix)
def test_generated_script_is_valid_and_portable(profile):
    text = (OUTPUT_DIR / profile.output_name).read_text()
    tree = ast.parse(text)

    assert not any(isinstance(node, ast.Return) for node in tree.body)
    for forbidden in ("<<", "#+", "/Users/", "My Drive", "~/repos", "SALP_ORG"):
        assert forbidden not in text
    assert "Pablo Stafforini" not in text
    assert "import subprocess" not in text
    assert "['pass', 'show'" not in text
    assert "SEC_USER_AGENT must identify you" in text
    assert text.count("def _marketdata_key") == (
        2 if profile.prefix == "vara" else 1
    )
    if profile.prefix == "vara":
        assert "def _build_salp_context(args):" in text


@pytest.mark.parametrize("profile", generator.PROFILES, ids=lambda item: item.prefix)
def test_generated_cli_starts_without_network_or_credentials(profile):
    script = OUTPUT_DIR / profile.output_name
    env = os.environ.copy()
    env.pop("MARKETDATA_KEY", None)
    env.pop("MARKETDATA_API_KEY", None)
    env.pop("SEC_USER_AGENT", None)
    env["HTTPS_PROXY"] = "http://127.0.0.1:9"
    env["HTTP_PROXY"] = "http://127.0.0.1:9"

    help_result = subprocess.run(
        [sys.executable, script, "--help"],
        capture_output=True,
        text=True,
        env=env,
        timeout=5,
    )
    list_result = subprocess.run(
        [sys.executable, script, "--list-commands"],
        capture_output=True,
        text=True,
        env=env,
        timeout=5,
    )
    data_result = subprocess.run(
        [sys.executable, script, "data"],
        capture_output=True,
        text=True,
        env=env,
        timeout=5,
    )

    assert help_result.returncode == 0
    assert list_result.returncode == 0
    assert list_result.stdout.splitlines() == [*generator.COMMANDS, "all"]
    assert data_result.returncode != 0
    assert "SEC_USER_AGENT must identify you" in data_result.stderr


def test_block_extractor_rejects_ambiguous_or_incomplete_source():
    duplicate = """#+name: demo
#+begin_src python
one = 1
#+end_src
#+name: demo
#+begin_src python
two = 2
#+end_src
"""
    unclosed = """#+name: demo
#+begin_src python
one = 1
"""

    with pytest.raises(generator.GenerationError, match="Duplicate"):
        generator.extract_python_blocks(duplicate)
    with pytest.raises(generator.GenerationError, match="Unclosed"):
        generator.extract_python_blocks(unclosed)


@pytest.mark.parametrize(
    "refresh_name,profile,staged_scripts",
    (
        (
            "sa-lp-refresh.sh",
            "all",
            (
                "static/code/situational-awareness-lp.py",
                "static/code/value-aligned-research-advisors.py",
            ),
        ),
        (
            "vara-refresh.sh",
            "vara",
            ("static/code/value-aligned-research-advisors.py",),
        ),
    ),
)
def test_refresh_regenerates_only_safe_script_scope(
    refresh_name, profile, staged_scripts
):
    text = (ROOT / "scripts" / refresh_name).read_text()
    generation = text.index("generate-copycat-scripts.py")
    export = text.index("scripts/export-notes.sh")

    assert generation < export
    assert f"--profile {profile}" in text
    assert "dependency has uncommitted changes" in text
    for script in staged_scripts:
        assert f'"{script}"' in text
    if refresh_name == "vara-refresh.sh":
        assert '"static/code/situational-awareness-lp.py"' not in text


def test_production_build_mounts_public_code_directory():
    deploy_config = (ROOT / "hugo.deploy.toml").read_text()

    assert 'source = "static/code"\n    target = "static/code"' in deploy_config


@pytest.mark.parametrize("profile", generator.PROFILES, ids=lambda item: item.prefix)
def test_note_has_only_the_github_source_link(profile):
    text = (NOTES_DIR / profile.org_name).read_text()
    link = (
        "[[https://github.com/benthamite/stafforini.com/blob/main/static/code/"
        f"{profile.output_name}][View on GitHub]]"
    )

    assert text.count(link) == 1
    assert "Download complete script" not in text
    assert "Download the complete script" not in text


def _fake_org(profile, value):
    data = f"""import json, os
SEC_UA = os.environ.get(
    'SEC_USER_AGENT',
    'Pablo Stafforini stafforini.com; contact@stafforini.com')
return json.dumps({{'value': {value}}})"""
    library = f"""import json, os
parsed = json.loads(data)
value = parsed['value']
{profile.option_cache_literal}

def _marketdata_key():
    return None

def _marketdata_get(path, params, api_key):
    return []"""

    commands = {}
    for command, block_name in profile.blocks.items():
        if profile.prefix == "vara" and command == "comparison":
            body = f"""<<{profile.prefix}-lib>>
# ── SALP series: run the Situational Awareness LP note's own model ──
SALP_ORG = 'private source'
exec(_extract_salp_block(_salp_org, 'sa-lib'), salp_ns)
print(f"COMPARISON {{value}} {{salp_ns['value']}}")"""
        elif command == "performance":
            body = f"<<{profile.prefix}-lib>>\nprint(f'PERFORMANCE {{value}}')"
        else:
            body = f"<<{profile.prefix}-lib>>\npass"
        commands[block_name] = body

    blocks = {
        f"{profile.prefix}-data": data,
        f"{profile.prefix}-lib": library,
        **commands,
    }
    return "\n".join(
        f"#+name: {name}\n#+begin_src python\n{body}\n#+end_src\n"
        for name, body in blocks.items()
    )


def test_generated_runtime_dispatches_performance_and_embedded_comparison(tmp_path):
    salp, vara = generator.PROFILES
    salp_source = _fake_org(salp, 2)
    vara_source = _fake_org(vara, 3)
    salp_script = tmp_path / salp.output_name
    vara_script = tmp_path / vara.output_name
    salp_script.write_text(generator.render_script(salp, salp_source))
    vara_script.write_text(
        generator.render_script(vara, vara_source, salp_org_text=salp_source)
    )
    env = {**os.environ, "SEC_USER_AGENT": "Test runner test@example.com"}

    performance = subprocess.run(
        [sys.executable, salp_script, "performance", "--output-root", tmp_path],
        capture_output=True,
        text=True,
        env=env,
        check=True,
    )
    comparison = subprocess.run(
        [sys.executable, vara_script, "comparison", "--output-root", tmp_path],
        capture_output=True,
        text=True,
        env=env,
        check=True,
    )

    assert performance.stdout.strip() == "PERFORMANCE 2"
    assert comparison.stdout.strip() == "COMPARISON 3 2"
