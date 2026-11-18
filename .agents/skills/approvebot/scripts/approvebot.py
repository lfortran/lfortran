#!/usr/bin/env python3
"""Mechanical commands for the approvebot skill.

The agent judges each PR against ../references/policy.md. This script does the
bookkeeping that must not depend on judgment: which PRs need a verdict,
whether the required checks passed on the exact head, which tests a PR adds,
running those tests against a build of main, keeping exactly one verdict label
per classified head, and journaling every verdict and every human reaction to
it. See ../SKILL.md for the workflow.

All state lives in `.approvebot/` next to the main checkout, including the
worktree and build of main. Comments are never state.
"""

import argparse
from datetime import datetime, timezone
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys

REPO = "lfortran/lfortran"
REPO_URL = f"https://github.com/{REPO}.git"
# PR authors are members of this org team who can merge the repository.
# authors() resolves the set each run; it is not a fixed login list.
PUSH_ACCESS_TEAM = "push-access"
APPROVED = "bot-approved"
REVIEW = "needs-review"
VERDICTS = {"approved": APPROVED, "review": REVIEW}
AUTO_MERGE = "auto-merge"
POLICY_VERSION = 4
SIGNATURE = "-- approvebot (automated; runs on {login}'s account)"
BUILD_FLAGS = ["-DCMAKE_CXX_FLAGS_DEBUG=-g -O2", "-DCMAKE_C_FLAGS_DEBUG=-g -O2"]
# Changes to these require `pixi run build` (regenerates sources) instead of
# `rebuild`; see AGENTS.md. CMakeLists.txt and cmake/ are not listed: `rebuild`
# reconfigures by itself when a file of the build changes, and most commits
# touch only integration_tests/CMakeLists.txt, which is not part of the build.
GENERATOR_INPUTS = re.compile(
    r"(^|/)(ASR\.asdl|AST\.asdl|parser\.yy|tokenizer\.re|preprocessor\.re"
    r"|wasm_instructions\.txt|asdl_cpp\.py|wasm_instructions_visitor\.py"
    r"|intrinsic_func_registry_util_gen\.py|build0\.sh|build1\.sh"
    r"|pixi\.toml|pixi\.lock)$|^src/server/generator/")
CONTROL = re.compile(
    r"^(\.agents/|\.claude/|\.github/|ci/|cmake/|AGENTS\.md$|CLAUDE\.md$|pixi\.|"
    r"build\d?\.sh$|environment.*\.yml$|.*CMakeLists\.txt$|.*\.cmake$|.*\.py$)")
REGISTRATION = re.compile(r"^\s*(RUN|COMPILE)\s*\(\s*NAME\s+([^\s)]+)(.*)$")
# Keywords of the RUN macro in integration_tests/CMakeLists.txt.
RUN_OPTIONS = {"FAIL", "NO_FAST", "NO_FAST_MATH", "NO_DETECT_LEAKS", "NO_STD_F23",
               "NO_LLVM_GOC", "HOST_STATIC_LIBRARY"}
RUN_ARGUMENTS = {"FILE", "INCLUDE_PATH", "COPY_TO_BIN", "NUM_IMAGES", "LABELS",
                 "EXTRAFILES", "EXTRA_ARGS", "GFORTRAN_ARGS", "SHARED_LIBRARY_FILES",
                 "STATIC_LIBRARY_FILES", "COMPILERS"}

_spec = importlib.util.spec_from_file_location(
    "mergebot", Path(__file__).resolve().parents[2] / "mergebot/scripts/mergebot.py")
mergebot = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(mergebot)
run, gh, gh_json, gh_items, git = (mergebot.run, mergebot.gh, mergebot.gh_json,
                                   mergebot.gh_items, mergebot.git)
read_json, write_json = mergebot.read_json, mergebot.write_json


# --- Pure decision logic (unit-tested in ci/test_ci.py) ----------------------

def select_authors(members, can_merge):
    """Push Access members whose repository role can merge.

    `members` is the team. `can_merge(login)` is true for the same roles
    mergebot accepts on an `auto-merge` label (`admin` and `write`).
    """
    return {login for login in members if can_merge(login)}


def area(path):
    """Classify a changed path for the policy's size budgets."""
    if path.startswith("tests/reference/"):
        return "reference"
    if path == "integration_tests/CMakeLists.txt" or path == "tests/tests.toml":
        return "registration"
    if path.startswith(("integration_tests/", "tests/", "src/lfortran/tests/")):
        return "test"
    if CONTROL.match(path):
        return "control"
    if path.startswith("doc/") or path.endswith((".md", ".rst", ".txt")):
        return "docs"
    if path.startswith("src/"):
        return "production"
    return "other"


def summarize_files(files):
    """Per-area line and file counts (added plus deleted lines)."""
    summary = {}
    for f in files:
        entry = summary.setdefault(area(f["path"]), {"files": 0, "lines": 0, "paths": []})
        entry["files"] += 1
        entry["lines"] += f["additions"] + f["deletions"]
        entry["paths"].append(f["path"])
    return summary


def registrations(diff):
    """Registrations a unified diff of integration_tests/CMakeLists.txt adds.

    Returns (entries, warnings). Each entry has the test name, the full line,
    its labels and its options (FAIL, NO_FAST, NO_STD_F23, ...).
    """
    entries, warnings = [], []
    for line in diff.splitlines():
        if not line.startswith("+") or line.startswith("+++"):
            continue
        text = line[1:]
        match = REGISTRATION.match(text)
        if not match:
            if re.match(r"^\s*(RUN|COMPILE)\s*\(", text):
                warnings.append(f"unparsed registration: {text.strip()}")
            continue
        if text.count("(") != text.count(")"):
            warnings.append(f"multi-line registration: {text.strip()}")
            continue
        words = re.sub(r"#.*$", "", match.group(3)).replace(")", " ").split()
        labels, options, key = [], [], None
        for word in words:
            if word in RUN_ARGUMENTS:
                key = word
                if word != "LABELS":
                    options.append(word)
            elif word in RUN_OPTIONS:
                options.append(word)
                key = None
            elif key == "LABELS":
                labels.append(word)
        entries.append({"kind": match.group(1), "name": match.group(2),
                        "line": text.strip(), "labels": labels, "options": options})
    return entries, warnings


def added_lines(diff):
    return [line[1:] for line in diff.splitlines()
            if line.startswith("+") and not line.startswith("+++")]


def removed_lines(diff):
    return [line[1:] for line in diff.splitlines()
            if line.startswith("-") and not line.startswith("---")]


def human_action(entry, labels, state):
    """What a human did to a classified head, or None if nothing yet.

    `entry` is the journal's current verdict; `labels` the PR's labels now.
    """
    verdict, other = entry["label"], (REVIEW if entry["label"] == APPROVED else APPROVED)
    if AUTO_MERGE in labels:
        return "auto-merge"  # also when mergebot has merged it since
    if state == "MERGED":
        return "merged"
    if state == "CLOSED":
        return "closed"
    if other in labels:
        return f"relabeled to {other}"
    if verdict not in labels:
        return f"removed {verdict}"
    return None


def outcome(entry, action):
    """Default agreement reading of a human action; the agent may refine it."""
    approved = entry["label"] == APPROVED
    if action == "auto-merge":
        return "agree" if approved else "disagree"
    if action == "merged":
        return "agree"  # merged by hand after the review the label asked for
    if action == "closed":
        return "disagree" if approved else "agree"
    return "disagree"


def next_action(pr, entry, ci):
    """The step `status` recommends for one open PR."""
    labels = pr["labels"]
    current = entry if entry and not entry.get("retracted") else None
    if current and current["head"] == pr["head"]:
        if current.get("acked"):
            return "done"
        if human_action(current, labels, "OPEN"):
            return "ack"
        # The verdict assumed the head would pass CI; a failure withdraws it.
        if ci == "failure":
            return "retract"
        # A newer policy may approve what an older one sent to review. An
        # older `bot-approved` stays: the maintainer is already looking at it.
        if current["label"] == REVIEW and current.get("policy", 0) < POLICY_VERSION:
            return "retract"
        return "done"
    if current and AUTO_MERGE in labels:
        # A maintainer took the PR over; mergebot's rebases keep the label.
        return "hands-off: labeled auto-merge" if current.get("acked") else "ack"
    if current:
        # New head: the old verdict no longer applies.
        return "retract"
    if AUTO_MERGE in labels:
        return "hands-off: labeled auto-merge"
    if APPROVED in labels or REVIEW in labels:
        return "hands-off: verdict label without a journal entry"
    if pr["draft"]:
        return "skip: draft"
    if ci in ("success", "pending"):
        # Mergebot waits for Quick before merging, so pending checks are fine.
        return "classify"
    return "skip: CI failed"


# --- Helpers -------------------------------------------------------------------

def now():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def state_dir():
    common = Path(git("rev-parse", "--path-format=absolute", "--git-common-dir"))
    path = common.parent / ".approvebot"
    (path / "work").mkdir(parents=True, exist_ok=True)
    return path


def main_worktree():
    return state_dir() / "main"


def journal_path(number):
    return state_dir() / "prs" / f"{number}.json"


def journal(number):
    return read_json(journal_path(number), {"history": []})


def current_entry(number):
    history = journal(number)["history"]
    return history[-1] if history else None


def save_entry(number, entry, replace=False):
    data = journal(number)
    if replace and data["history"]:
        data["history"][-1] = entry
    else:
        data["history"].append(entry)
    write_json(journal_path(number), data)


def ledger_path():
    return state_dir() / "feedback.jsonl"


def ledger():
    path = ledger_path()
    if not path.exists():
        return []
    return [json.loads(line) for line in path.read_text().splitlines() if line]


def save_ledger(entries):
    path = ledger_path()
    temporary = path.with_suffix(".tmp")
    temporary.write_text("".join(json.dumps(e) + "\n" for e in entries))
    os.replace(temporary, path)


def ci(head):
    checks = gh_items(f"repos/{REPO}/commits/{head}/check-runs?per_page=100",
                      "check_runs")
    return mergebot.ci_state(mergebot.required_checks(), checks)


def authors():
    """Logins whose PRs this process classifies, cached for the process.

    Members of the Push Access team with write or admin permission on the
    repository. An empty result is a lookup failure, not an empty team the
    bot should quietly skip.
    """
    def compute():
        output = gh("api", "--paginate",
                    f"orgs/{REPO.split('/')[0]}/teams/{PUSH_ACCESS_TEAM}/members",
                    "--jq", ".[].login")
        members = [line.strip() for line in output.splitlines() if line.strip()]
        if not members:
            fail(f"the {PUSH_ACCESS_TEAM} team returned no members")
        selected = select_authors(members, mergebot.can_write)
        if not selected:
            fail(f"no member of {PUSH_ACCESS_TEAM} has write access")
        return selected
    return mergebot.cached("approvebot-authors", compute)


def open_prs():
    prs = []
    for author in sorted(authors()):
        prs += gh_json("pr", "list", "--repo", REPO, "--author", author,
                       "--base", "main", "--state", "open", "--limit", "500",
                       "--json", "number,title,isDraft,headRefOid,labels")
    return [{"number": p["number"], "title": p["title"], "draft": p["isDraft"],
             "head": p["headRefOid"], "labels": [l["name"] for l in p["labels"]]}
            for p in prs]


def pr_view(number):
    p = gh_json("pr", "view", str(number), "--repo", REPO, "--json",
                "number,title,state,isDraft,headRefOid,baseRefName,author,labels")
    return {"number": p["number"], "title": p["title"], "state": p["state"],
            "draft": p["isDraft"], "head": p["headRefOid"], "base": p["baseRefName"],
            "author": p["author"]["login"], "labels": [l["name"] for l in p["labels"]]}


def fetch(number=None):
    refs = ["+refs/heads/main:refs/approvebot/main"]
    if number:
        refs.append(f"+refs/pull/{number}/head:refs/approvebot/pr/{number}")
    git("fetch", "-q", REPO_URL, *refs)
    return git("rev-parse", "refs/approvebot/main")


def comment(number, body):
    text = f"{body.rstrip()}\n\n{SIGNATURE.format(login=mergebot.login())}\n"
    out = gh("pr", "comment", str(number), "--repo", REPO, "--body-file", "-", input=text)
    return out.strip().splitlines()[-1] if out.strip() else None


def set_labels(number, add=(), remove=()):
    for label in remove:
        gh("pr", "edit", str(number), "--repo", REPO, "--remove-label", label)
    for label in add:
        gh("pr", "edit", str(number), "--repo", REPO, "--add-label", label)


def fail(message):
    sys.exit(f"approvebot: {message}")


# --- Commands ------------------------------------------------------------------

def cmd_status(args):
    prs = open_prs()
    if args.numbers:
        prs = [p for p in prs if p["number"] in args.numbers]
    open_numbers = {p["number"] for p in prs}
    result = []
    for pr in sorted(prs, key=lambda p: p["number"]):
        entry = current_entry(pr["number"])
        state = None
        current = entry if entry and not entry.get("retracted") else None
        if not current or current["head"] != pr["head"] or not current.get("acked"):
            if not pr["draft"] and AUTO_MERGE not in pr["labels"]:
                state, _ = ci(pr["head"])
        action = next_action(pr, entry, state)
        row = {**pr, "ci": state, "action": action}
        if current:
            row["verdict"] = {k: current.get(k) for k in ("head", "label", "scope", "kind")}
            human = human_action(current, pr["labels"], "OPEN")
            if human and not current.get("acked"):
                row["human"] = human
                row["suggested_outcome"] = outcome(current, human)
        result.append(row)
    # Classified PRs that were merged or closed since: record what happened.
    closed = []
    directory = state_dir() / "prs"
    if not args.numbers and directory.exists():
        for path in sorted(directory.glob("*.json")):
            number = int(path.stem)
            entry = current_entry(number)
            if number in open_numbers or not entry or entry.get("acked") \
                    or entry.get("retracted"):
                continue
            pr = pr_view(number)
            if pr["state"] == "OPEN":
                continue  # e.g. no longer by a selected author
            action = human_action(entry, pr["labels"], pr["state"])
            closed.append({"number": number, "title": pr["title"], "state": pr["state"],
                           "verdict": {k: entry.get(k) for k in ("head", "label", "scope", "kind")},
                           "human": action, "suggested_outcome": outcome(entry, action),
                           "action": "ack"})
    main = read_json(state_dir() / "main.json", {})
    print(json.dumps({"login": mergebot.login(), "policy": POLICY_VERSION,
                      "authors": sorted(authors()),
                      "main_build": main, "prs": result, "closed": closed}, indent=2))


FACTS_QUERY = """
query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      number title body url state isDraft headRefOid baseRefOid baseRefName
      changedFiles additions deletions author { login }
      labels(first: 50) { nodes { name } }
      closingIssuesReferences(first: 20) {
        nodes { number title state labels(first: 30) { nodes { name } } }
      }
      reviews(last: 50) { nodes { state author { login } submittedAt body commit { oid } } }
      reviewThreads(first: 100) {
        nodes { isResolved isOutdated
                comments(first: 3) { nodes { author { login } body } } }
      }
      comments(last: 100) { nodes { author { login } body createdAt url } }
      files(first: 100) { nodes { path additions deletions changeType } }
    }
  }
}
"""


def cmd_facts(args):
    owner, name = REPO.split("/")
    data = gh_json("api", "graphql", "-f", f"query={FACTS_QUERY}", "-F", f"owner={owner}",
                   "-F", f"name={name}", "-F", f"number={args.number}")
    if data.get("errors"):
        fail(f"GraphQL errors: {data['errors']}")
    pr = data["data"]["repository"]["pullRequest"]
    files = pr["files"]["nodes"]
    signature = SIGNATURE.split("(")[0].strip()
    comments = pr["comments"]["nodes"]
    state, failed = ci(pr["headRefOid"])
    main_sha = fetch(args.number)
    head_ref = f"refs/approvebot/pr/{args.number}"
    if git("rev-parse", head_ref) != pr["headRefOid"]:
        fail("fetched PR head differs from GitHub's head; retry")
    cmake_diff = git("diff", f"{main_sha}...{head_ref}", "--",
                     "integration_tests/CMakeLists.txt")
    regs, warnings = registrations(cmake_diff)
    behind = int(git("rev-list", "--count", f"{head_ref}..{main_sha}"))
    print(json.dumps({
        "number": pr["number"], "title": pr["title"], "url": pr["url"],
        "author": pr["author"]["login"], "state": pr["state"], "draft": pr["isDraft"],
        "head": pr["headRefOid"], "base": pr["baseRefOid"], "main": main_sha,
        "commits_behind_main": behind,
        "labels": [l["name"] for l in pr["labels"]["nodes"]],
        "ci": state, "failed_checks": failed,
        "changed_files": pr["changedFiles"], "files_complete": len(files) == pr["changedFiles"],
        "areas": summarize_files(files),
        "registrations": regs, "registration_warnings": warnings,
        "issues": [{"number": i["number"], "title": i["title"], "state": i["state"],
                    "labels": [l["name"] for l in i["labels"]["nodes"]]}
                   for i in pr["closingIssuesReferences"]["nodes"]],
        "reviews": [{"state": r["state"], "author": (r["author"] or {}).get("login"),
                     "commit": (r["commit"] or {}).get("oid"), "body": r["body"]}
                    for r in pr["reviews"]["nodes"]],
        "unresolved_threads": [t["comments"]["nodes"] for t in pr["reviewThreads"]["nodes"]
                               if not t["isResolved"]],
        "human_comments": [c for c in comments if signature not in (c["body"] or "")],
        "bot_comments": [c["url"] for c in comments if signature in (c["body"] or "")],
        "body": pr["body"],
    }, indent=2))


def require_clean(worktree):
    dirty = git("-C", str(worktree), "status", "--porcelain")
    if dirty:
        fail(f"{worktree} is not clean; run `restore` first:\n{dirty}")


def cmd_main(args):
    worktree = main_worktree()
    sha = fetch()
    record = read_json(state_dir() / "main.json", {})
    if not (worktree / ".git").exists():
        # --force: if `.approvebot/` was deleted, git still registers the old
        # worktree there and refuses to add it again.
        git("worktree", "add", "-q", "--force", "--detach", str(worktree), sha)
        mode = "build"
    else:
        require_clean(worktree)
        old = git("-C", str(worktree), "rev-parse", "HEAD")
        git("-C", str(worktree), "checkout", "-q", "--detach", sha)
        changed = git("diff", "--name-only", old, sha).splitlines()
        if not (worktree / "build/llvm11/src/bin/lfortran").exists() \
                or any(GENERATOR_INPUTS.search(p) for p in changed):
            mode = "build"
        elif record.get("sha") != sha or not record.get("ok"):
            mode = "rebuild"
        else:
            mode = None
    if args.no_build or mode is None:
        print(json.dumps({"sha": sha, "mode": mode, "built": record}, indent=2))
        return
    log = state_dir() / "main-build.log"
    command = ["pixi", "run", "-e", "llvm11", mode] + (BUILD_FLAGS if mode == "build" else [])
    env = {**os.environ, "CMAKE_BUILD_PARALLEL_LEVEL": str(args.jobs)}
    with open(log, "w") as stream:
        status = subprocess.run(command, cwd=worktree, stdout=stream,
                                stderr=subprocess.STDOUT, env=env).returncode
    record = {"sha": sha, "mode": mode, "ok": status == 0, "time": now(), "log": str(log)}
    write_json(state_dir() / "main.json", record)
    print(json.dumps(record, indent=2))
    if status:
        fail(f"main build failed; see {log}")


def overlay(number, worktree):
    """Write the PR's test changes onto the main worktree.

    Test inputs and references are taken from the PR head as whole files;
    registrations the PR adds are appended (or replace main's line for the
    same test name). Production files are never copied.
    """
    head_ref = f"refs/approvebot/pr/{number}"
    main_sha = git("-C", str(worktree), "rev-parse", "HEAD")
    base = git("merge-base", main_sha, head_ref)
    changes = git("diff", "--name-status", "--no-renames", f"{main_sha}...{head_ref}")
    written, warnings, integration, reference_inputs, unit = [], [], [], [], []
    for line in changes.splitlines():
        status, path = line.split("\t", 1)
        if path == "integration_tests/CMakeLists.txt":
            diff = git("diff", f"{main_sha}...{head_ref}", "--", path)
            regs, more = registrations(diff)
            warnings += more
            target = worktree / path
            lines = target.read_text().splitlines()
            for reg in regs:
                pattern = re.compile(rf"^\s*{reg['kind']}\s*\(\s*NAME\s+{re.escape(reg['name'])}\s")
                index = next((i for i, l in enumerate(lines) if pattern.match(l)), None)
                if index is None:
                    lines.append(reg["line"])
                else:
                    lines[index] = reg["line"]
                integration.append(reg)
            target.write_text("\n".join(lines) + "\n")
        elif path == "tests/tests.toml":
            diff = git("diff", f"{main_sha}...{head_ref}", "--", path)
            if removed_lines(diff):
                warnings.append("tests.toml: the PR changes existing entries; "
                                "reference registrations were not overlaid")
                continue
            target = worktree / path
            target.write_text(target.read_text().rstrip("\n") + "\n\n"
                              + "\n".join(added_lines(diff)) + "\n")
            reference_inputs += re.findall(r'^\s*filename\s*=\s*"([^"]+)"',
                                           "\n".join(added_lines(diff)), re.M)
        elif path.startswith(("integration_tests/", "tests/")):
            if status == "D":
                warnings.append(f"{path}: deleted by the PR; left in place")
                continue
            target = worktree / path
            existed = target.exists()
            if existed and git("diff", "--name-only", base, main_sha, "--", path):
                warnings.append(f"{path}: main changed it since the PR's base {base[:10]}; "
                                "the PR's whole file replaced main's, so differences "
                                "outside the PR's own changes are not evidence")
            target.parent.mkdir(parents=True, exist_ok=True)
            content = subprocess.run(["git", "show", f"{head_ref}:{path}"],
                                     capture_output=True, check=True).stdout
            target.write_bytes(content)
            written.append({"path": path, "existed": existed})
            if path.startswith("tests/") and not path.startswith("tests/reference/"):
                reference_inputs.append(path[len("tests/"):])
        elif path.startswith("src/lfortran/tests/"):
            unit.append(path)
    write_json(state_dir() / "overlay.json", {"pr": number, "written": written})
    return {"integration": integration, "reference_inputs": sorted(set(reference_inputs)),
            "unit_tests": unit, "warnings": warnings}


def restore(worktree):
    manifest = read_json(state_dir() / "overlay.json", {"written": []})
    for item in manifest["written"]:
        if not item["existed"]:
            (worktree / item["path"]).unlink(missing_ok=True)
    git("-C", str(worktree), "checkout", "-q", "--", ".")
    (state_dir() / "overlay.json").unlink(missing_ok=True)
    require_clean(worktree)


def diff_lines(path, limit=300):
    """The reference comparison lines of a `tests` log: what main printed
    (`>`) against the PR's references (`<`), with diff headers."""
    text = re.sub(r"\x1b\[[0-9;]*m", "", Path(path).read_text(errors="replace"))
    found = [l for l in text.splitlines()
             if re.match(r"^([<>] |\d+(,\d+)?[acd]\d+(,\d+)?$|---$)", l)]
    return "\n".join(found[:limit]) + ("\n[truncated]" if len(found) > limit else "")


def tail(path, lines=40):
    text = re.sub(r"\x1b\[[0-9;]*m", "", Path(path).read_text(errors="replace"))
    return "\n".join(text.splitlines()[-lines:])


def cmd_check_main(args):
    worktree = main_worktree()
    record = read_json(state_dir() / "main.json", {})
    if not record.get("ok") or record.get("sha") != git("-C", str(worktree), "rev-parse", "HEAD"):
        fail("the main build is missing, failed, or stale; run `main` first")
    require_clean(worktree)
    fetch(args.number)
    head = git("rev-parse", f"refs/approvebot/pr/{args.number}")
    logs = state_dir() / "checks" / f"{args.number}-{head[:10]}"
    logs.mkdir(parents=True, exist_ok=True)
    found = overlay(args.number, worktree)
    results = []
    try:
        jobs = str(args.jobs)
        for reg in found["integration"]:
            if "llvm" not in reg["labels"]:
                results.append({"test": reg["name"], "result": "unverifiable",
                                "why": "not registered for the llvm backend"})
                continue
            log = logs / f"integration-{reg['name']}.log"
            command = ["pixi", "run", "-e", "llvm11", "integration_tests",
                       "-t", f"^{re.escape(reg['name'])}$", "-j", jobs]
            with open(log, "w") as stream:
                code = subprocess.run(command, cwd=worktree, stdout=stream,
                                      stderr=subprocess.STDOUT).returncode
            results.append({"test": reg["name"], "kind": "integration",
                            "options": reg["options"], "exit": code,
                            "result": "fails on main" if code else "passes on main",
                            "log": str(log), "tail": tail(log)})
        for path in found["reference_inputs"]:
            log = logs / f"reference-{path.replace('/', '_')}.log"
            command = ["pixi", "run", "-e", "llvm11", "tests", "-t", re.escape(path)]
            with open(log, "w") as stream:
                code = subprocess.run(command, cwd=worktree, stdout=stream,
                                      stderr=subprocess.STDOUT).returncode
            results.append({"test": path, "kind": "reference", "exit": code,
                            "result": "fails on main" if code else "passes on main",
                            "log": str(log), "diff": diff_lines(log), "tail": tail(log, 15)})
        for path in found["unit_tests"]:
            results.append({"test": path, "kind": "unit", "result": "unverifiable",
                            "why": "C++ unit tests need a compiler rebuild"})
    finally:
        restore(worktree)
    print(json.dumps({"pr": args.number, "head": head, "main": record["sha"],
                      "mode": "default only; CI covers --fast and --std=f23",
                      "warnings": found["warnings"], "tests": results}, indent=2))


def cmd_apply(args):
    label = VERDICTS[args.verdict]
    pr = pr_view(args.number)
    if pr["state"] != "OPEN" or pr["base"] != "main" or pr["draft"]:
        fail("only open, non-draft PRs against main get a verdict")
    if pr["author"] not in authors():
        fail(f"author {pr['author']} is not a Push Access member with write access")
    if pr["head"] != args.head:
        fail(f"the head moved to {pr['head']}; classify the new head")
    if AUTO_MERGE in pr["labels"]:
        fail("a maintainer labeled the PR auto-merge; hands off")
    state, failed = ci(pr["head"])
    if state not in ("success", "pending"):
        fail(f"required checks are {state} on the head {failed}")
    entry = current_entry(args.number)
    if entry and not entry.get("retracted"):
        if entry["head"] == pr["head"]:
            fail("this head already has a verdict")
        fail("a verdict for an older head is still current; `retract` it first")
    if not entry or entry.get("retracted"):
        for other in (APPROVED, REVIEW):
            if other in pr["labels"]:
                fail(f"`{other}` is on the PR without a current journal entry; "
                     "a human applied it, hands off")
    body = Path(args.comment_file).read_text()
    if pr["head"][:10] not in body:
        fail("the comment must name the classified head")
    remove = [l for l in (APPROVED, REVIEW) if l != label and l in pr["labels"]]
    set_labels(args.number, add=[] if label in pr["labels"] else [label], remove=remove)
    after = pr_view(args.number)
    if after["head"] != args.head:
        set_labels(args.number, remove=[label])
        fail("the head moved while labeling; label removed, classify again")
    if {APPROVED, REVIEW} & set(after["labels"]) != {label}:
        fail(f"labels did not settle: {after['labels']}")
    entry = {"head": args.head, "label": label, "scope": args.scope,
             "kind": args.kind, "policy": POLICY_VERSION, "ci": state,
             "main": read_json(state_dir() / "main.json", {}).get("sha"),
             "time": now(), "comment": None}
    save_entry(args.number, entry)
    entry["comment"] = comment(args.number, body)
    save_entry(args.number, entry, replace=True)
    print(json.dumps({"pr": args.number, "label": label, "comment": entry["comment"]}))


def cmd_retract(args):
    entry = current_entry(args.number)
    if not entry or entry.get("retracted"):
        fail("no current verdict to retract")
    pr = pr_view(args.number)
    if AUTO_MERGE in pr["labels"] and pr["head"] != entry["head"]:
        fail("labeled auto-merge after a new head: `ack` the maintainer's decision instead")
    if entry["label"] in pr["labels"]:
        set_labels(args.number, remove=[entry["label"]])
    entry.update(retracted=now(), retract_reason=args.reason)
    save_entry(args.number, entry, replace=True)
    print(json.dumps({"pr": args.number, "removed": entry["label"]}))


def cmd_ack(args):
    entry = current_entry(args.number)
    if not entry or entry.get("acked"):
        fail("no unacknowledged verdict")
    entries = ledger()
    entries.append({"pr": args.number, "head": entry["head"], "label": entry["label"],
                    "scope": entry.get("scope"), "kind": entry.get("kind"),
                    "policy": entry.get("policy"), "human": args.human,
                    "outcome": args.outcome, "reason": args.reason, "time": now(),
                    "captured": None})
    save_ledger(entries)
    entry["acked"] = now()
    save_entry(args.number, entry, replace=True)
    print(json.dumps(entries[-1]))


def cmd_feedback(args):
    entries = ledger()
    if args.capture:
        for e in entries:
            if e["captured"] is None and e["pr"] in args.prs:
                e["captured"] = args.capture
        save_ledger(entries)
    print(json.dumps([e for e in entries if e["captured"] is None], indent=2))


def cmd_stats(args):
    stats = {}
    for path in sorted((state_dir() / "prs").glob("*.json")):
        for entry in read_json(path, {"history": []})["history"]:
            key = f"{entry['label']} / {entry.get('kind')}"
            stats.setdefault(key, {"verdicts": 0, "agree": 0, "disagree": 0, "unknown": 0})
            stats[key]["verdicts"] += 1
    for e in ledger():
        key = f"{e['label']} / {e.get('kind')}"
        stats.setdefault(key, {"verdicts": 0, "agree": 0, "disagree": 0, "unknown": 0})
        stats[key][e["outcome"]] += 1
    print(json.dumps(stats, indent=2))


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = parser.add_subparsers(dest="command", required=True)

    def command(name, function, help, number=True):
        p = sub.add_parser(name, help=help)
        if number:
            p.add_argument("number", type=int)
        p.set_defaults(function=function)
        return p

    p = command("status", cmd_status, "PRs needing a verdict, retraction or ack",
                number=False)
    p.add_argument("numbers", type=int, nargs="*")
    command("facts", cmd_facts, "evidence for classifying one PR")
    p = command("main", cmd_main, "update and build the main worktree", number=False)
    p.add_argument("--jobs", type=int, default=8)
    p.add_argument("--no-build", action="store_true")
    p = command("check-main", cmd_check_main, "run the PR's tests against main")
    p.add_argument("--jobs", type=int, default=8)
    p = command("restore", lambda a: restore(main_worktree()),
                "undo an interrupted overlay", number=False)
    p = command("apply", cmd_apply, "label a head and post the verdict comment")
    p.add_argument("--verdict", choices=sorted(VERDICTS), required=True)
    p.add_argument("--head", required=True)
    p.add_argument("--scope", required=True)
    p.add_argument("--kind", required=True)
    p.add_argument("--comment-file", required=True)
    p = command("retract", cmd_retract, "remove a stale verdict label")
    p.add_argument("--reason", required=True)
    p = command("ack", cmd_ack, "record a human reaction to a verdict")
    p.add_argument("--human", required=True, help="what the human did")
    p.add_argument("--outcome", choices=("agree", "disagree", "unknown"), required=True)
    p.add_argument("--reason", required=True)
    p = command("feedback", cmd_feedback, "uncaptured ledger entries", number=False)
    p.add_argument("--capture", help="calibration PR URL that captured --prs")
    p.add_argument("--prs", type=int, nargs="*", default=[])
    command("stats", cmd_stats, "verdicts and agreement per category", number=False)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
