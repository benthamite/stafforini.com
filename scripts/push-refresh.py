#!/usr/bin/env python3
"""Push a refresh commit, retrying only recognized transient HTTPS failures.

A push the remote rejects as non-fast-forward is expected: the 13F filings
watcher (.github/workflows/sa-lp-13f-check.yml) commits to main on its own.
The helper then rebases the refresh commits onto the remote once and pushes
again; if that rebase does not apply cleanly it aborts it and fails.
"""

import argparse
import os
import re
import subprocess
import sys
import time


TRANSIENT_ERROR = re.compile(
    r"^fatal: unable to access '[^'\n]+': "
    r"(?:Could not resolve (?:host|proxy):|Failed to connect to |"
    r".*(?:Connection timed out|Connection reset by peer)|"
    r"The requested URL returned error: (?:408|429|500|502|503|504)\b)",
    re.MULTILINE,
)
NON_FAST_FORWARD = re.compile(
    r"^ ! \[rejected\] .* \((?:fetch first|non-fast-forward)\)$", re.MULTILINE)
MAX_ATTEMPTS = 4


def git(repo, *args):
    result = subprocess.run(
        ["git", "-C", str(repo), *args],
        env=dict(os.environ, LC_ALL="C", GIT_TERMINAL_PROMPT="0"),
        capture_output=True, text=True,
    )
    print(result.stdout, end="", flush=True)
    print(result.stderr, end="", file=sys.stderr, flush=True)
    return result


def integrate_remote(repo):
    """Rebase local commits onto the remote branch; return True on success."""
    print("Remote has commits this clone lacks; rebasing onto them once",
          file=sys.stderr, flush=True)
    if git(repo, "pull", "--rebase", "--autostash").returncode == 0:
        return True
    git(repo, "rebase", "--abort")
    print("Rebase onto the remote failed and was aborted", file=sys.stderr, flush=True)
    return False


def push_refresh(repo):
    integrated = False
    for attempt in range(1, MAX_ATTEMPTS + 1):
        result = git(repo, "push")
        if (result.returncode != 0 and not integrated
                and NON_FAST_FORWARD.search(result.stderr)):
            integrated = True
            if not integrate_remote(repo):
                return result.returncode
            result = git(repo, "push")
        if (result.returncode == 0 or attempt == MAX_ATTEMPTS
                or not TRANSIENT_ERROR.search(result.stderr)):
            return result.returncode
        delay = 5 * 2 ** (attempt - 1)
        print(f"Transient Git transport failure; retry {attempt}/{MAX_ATTEMPTS - 1} "
              f"in {delay} seconds", file=sys.stderr, flush=True)
        time.sleep(delay)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("repo", help="Repository whose normal configured push should run")
    raise SystemExit(push_refresh(parser.parse_args().repo))
