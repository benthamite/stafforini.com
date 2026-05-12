import importlib.util
import subprocess
from pathlib import Path


SCRIPT_PATH = Path(__file__).resolve().parents[1] / "scripts" / "netlify_deploy.py"


def load_module():
    spec = importlib.util.spec_from_file_location("netlify_deploy", SCRIPT_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def completed(args, returncode=0, stdout="", stderr=""):
    return subprocess.CompletedProcess(args, returncode, stdout, stderr)


def test_extracts_deploy_id_from_cancel_warning():
    module = load_module()
    output = "Warning: Failed canceling deploy with id 6a033aa2999e3de7f215f62d: Too Many Requests"

    assert module.extract_deploy_id(output) == "6a033aa2999e3de7f215f62d"


def test_confirms_rate_limited_post_upload_deploy_that_later_becomes_ready(tmp_path):
    module = load_module()
    state_file = tmp_path / ".netlify" / "state.json"
    state_file.parent.mkdir()
    state_file.write_text('{"siteId":"site-123"}')
    calls = []
    sleeps = []

    def runner(args, **kwargs):
        calls.append(args)
        if args[1] == "deploy":
            return completed(
                args,
                returncode=1,
                stdout=(
                    "Finished uploading blobs to deploy store\n"
                    "Finished uploading 5 assets\n"
                    "Waiting for deploy to go live...\n"
                ),
                stderr=(
                    "Warning: Failed canceling deploy with id 6a033aa2999e3de7f215f62d: Too Many Requests\n"
                    "JSONHTTPError: API Request rate limit surpassed for application 429\n"
                ),
            )
        return completed(args, stdout='{"state":"ready","ssl_url":"https://stafforini.com"}')

    exit_code = module.main(
        [
            "--repo-root",
            str(tmp_path),
            "--status-delays",
            "0",
            "--netlify-command",
            "netlify",
            "--",
            "deploy",
            "--prod",
        ],
        runner=runner,
        sleeper=sleeps.append,
    )

    assert exit_code == 0
    assert sleeps == [0]
    assert calls[1][:3] == ["netlify", "api", "getSiteDeploy"]


def test_non_rate_limit_deploy_failure_is_not_reinterpreted(tmp_path):
    module = load_module()
    calls = []

    def runner(args, **kwargs):
        calls.append(args)
        return completed(args, returncode=2, stderr="Deploy failed for a real reason")

    exit_code = module.main(
        ["--repo-root", str(tmp_path), "--netlify-command", "netlify", "--", "deploy", "--prod"],
        runner=runner,
        sleeper=lambda delay: None,
    )

    assert exit_code == 2
    assert len(calls) == 1


def test_default_command_uses_pinned_local_netlify_cli_for_success(tmp_path):
    module = load_module()
    calls = []

    def runner(args, **kwargs):
        calls.append(args)
        return completed(args, stdout="netlify/26.0.1")

    exit_code = module.main(
        ["--repo-root", str(tmp_path), "--", "--version"],
        runner=runner,
        sleeper=lambda delay: None,
    )

    assert exit_code == 0
    assert calls == [["npx", "--no-install", "netlify", "--version"]]
