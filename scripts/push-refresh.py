#!/usr/bin/env python3
"""Push a refresh commit, retrying only recognized transient HTTPS failures."""

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
MAX_ATTEMPTS = 4


def push_refresh(repo):
    for attempt in range(1, MAX_ATTEMPTS + 1):
        result = subprocess.run(
            ["git", "-C", str(repo), "push"],
            env=dict(os.environ, LC_ALL="C", GIT_TERMINAL_PROMPT="0"),
            capture_output=True, text=True,
        )
        print(result.stdout, end="", flush=True)
        print(result.stderr, end="", file=sys.stderr, flush=True)
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
