#!/usr/bin/env python3
"""Delete superseded compiler caches on main, keeping the newest per key.

hendrikmuhs/ccache-action appends a timestamp to every saved key, so each
save on main adds a new copy and the older copies stay until GitHub evicts
them. They are never restored again (restore picks the newest), but they
count towards the 10 GB repository cache limit.
"""

import json
import os
import re
import subprocess
import sys

TIMESTAMP = re.compile(r"-\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?Z$")


def superseded(caches):
    """Return the caches that have a newer cache with the same base key."""
    newest = {}
    for cache in caches:
        match = TIMESTAMP.search(cache["key"])
        if not match:
            continue
        base = cache["key"][:match.start()]
        if base not in newest or cache["created_at"] > newest[base]["created_at"]:
            newest[base] = cache
    keep = {cache["id"] for cache in newest.values()}
    return [cache for cache in caches
            if cache["id"] not in keep and TIMESTAMP.search(cache["key"])]


def gh(*args):
    return subprocess.run(["gh", *args], check=True, capture_output=True,
                          text=True).stdout


def main():
    repo = os.environ["GH_REPO"]
    output = gh("api", "--paginate",
                f"repos/{repo}/actions/caches?ref=refs/heads/main&per_page=100",
                "--jq", ".actions_caches[]")
    caches = [json.loads(line) for line in output.splitlines() if line]
    for cache in superseded(caches):
        print(f"Deleting {cache['key']} ({cache['size_in_bytes']} bytes)")
        try:
            gh("api", "-X", "DELETE", f"repos/{repo}/actions/caches/{cache['id']}")
        except subprocess.CalledProcessError as error:
            # Another run may have deleted or evicted it already.
            print(error.stderr, file=sys.stderr)


if __name__ == "__main__":
    main()
