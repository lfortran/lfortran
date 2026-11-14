#!/usr/bin/env python3
"""Record only webhook data, without fetching or executing the PR's code."""

import json
from pathlib import Path
import sys


def snapshot(event):
    pr = event["pull_request"]
    action = event["action"]
    if event["repository"]["full_name"] != "lfortran/lfortran":
        raise ValueError("unexpected repository")
    if pr["base"]["ref"] != "main" or action not in ("labeled", "unlabeled", "synchronize"):
        raise ValueError("unexpected pull request event")
    if action != "synchronize" and event["label"]["name"] != "auto-merge":
        raise ValueError("unexpected label")
    return {
        "number": event["number"],
        "action": action,
        "head": pr["head"]["sha"],
        "before": event.get("before"),
        "actor": event["sender"]["login"],
        "time": pr["updated_at"],
    }


if __name__ == "__main__":
    Path(sys.argv[2]).write_text(json.dumps(snapshot(
        json.loads(Path(sys.argv[1]).read_text()))) + "\n")
