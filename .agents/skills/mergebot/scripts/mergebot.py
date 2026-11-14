#!/usr/bin/env python3
"""Safety-critical commands for the mergebot skill.

The agent makes the judgment calls (merge method, rebases, CI failure
classification); this script enforces the rules that must never be broken.
The label-event workflow records the exact approved head independently of CI.
Rebases remain pending until the worker explicitly completes validation.
Semantic equivalence is the worker's judgment, not something git can prove.
See ../SKILL.md for the workflow.

Approval snapshots live in immutable GitHub Actions artifacts. Local control
state lives in `.mergebot/` next to the main checkout. Comments are never state.
"""

import argparse
from datetime import datetime, timedelta, timezone
import importlib.util
import io
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
from urllib.parse import urlencode
import zipfile

REPO = "lfortran/lfortran"
REPO_URL = f"https://github.com/{REPO}.git"
LABEL = "auto-merge"
QUICK_FILE = "Quick-Checks-CI.yml"
QUICK_NAME = "Quick checks"
EVENT_FILE = "Mergebot-Events.yml"
MAX_IN_FLIGHT = 4
LOCK_HOURS = 12
MAX_RUN_ATTEMPTS = 2  # rerun a flaky Quick run at most once

SIGNATURE = "-- mergebot (automated; runs on {login}'s account)"
CONVENTIONAL_PREFIX = re.compile(r"^[a-z]+(\([^)]*\))?!?:\s*")
VAGUE_DESCRIPTION = re.compile(
    r"^(fix(es|ed)?( (it|this|that|tests?|ci|build|typos?|bug|more))?"
    r"|updates?|updated|wip|tmp|temp|clean ?up|cleanups?|refactor(ing)?"
    r"|format(ting)?|lint|minor( changes| fixes)?|typos?|changes|tweaks?"
    r"|try again|retry|oops|more( tests| fixes| changes)?|tests?|ci"
    r"|address(ed)? (the )?(review|comments?|feedback)( comments)?"
    r"|review( comments| feedback)?|apply suggestions?( from code review)?"
    r"|update (the )?(reference|ref)s?( tests)?)\.?$",
    re.IGNORECASE)
TRAILER = re.compile(
    r"^(co-authored-by|co-author|generated-by|generated-with|assisted-by)\s*:",
    re.IGNORECASE | re.MULTILINE)


# --- Pure decision logic (unit-tested in ci/test_ci.py) ----------------------

def parse_time(text):
    return datetime.fromisoformat(text.replace("Z", "+00:00"))


def approval_snapshot(labeled, events):
    """Bind the webhook snapshot to one unambiguous label application."""
    current = labeled[-1]
    same_time = [n for n in labeled if n["createdAt"] == current["createdAt"]]
    if len(same_time) != 1:
        return None, "ambiguous label events in the same second; remove and reapply the label"
    matches = [e for e in events if e["action"] == "labeled"
               and e["time"] == current["createdAt"]
               and e["actor"] == (current["actor"] or {}).get("login")]
    if len({e["head"] for e in matches}) > 1:
        return None, "ambiguous approval snapshots; remove and reapply the label"
    return (matches[0] if matches else None), None


def approval_problem(head, snapshot, state, events, me):
    approved = state.get("approved_head", snapshot["head"])
    pending = state.get("rebase")
    expected = {approved}
    if pending:
        expected.add(pending["head"])
    if head not in expected:
        return f"the head changed from approved {approved[:10]} to {head[:10]}"
    pushes = {(p["from"], p["head"]) for p in state.get("pushes", [])}
    for event in events:
        if event["action"] != "synchronize" or event["time"] < snapshot["time"]:
            continue
        if event["time"] == snapshot["time"] and event["head"] == snapshot["head"]:
            continue  # The label may have followed this push in the same second.
        if event["actor"] != me or (event["before"], event["head"]) not in pushes:
            return f"the branch was pushed to {event['head'][:10]} after approval"
    return None


def ci_state(required, check_runs):
    """Return ("success" | "pending" | "failure", failed required check names)."""
    latest = {}
    for run in check_runs:
        name = run["name"]
        if name in required and (name not in latest or run["id"] > latest[name]["id"]):
            latest[name] = run
    failed = sorted(name for name, run in latest.items()
                    if run["status"] == "completed"
                    and run["conclusion"] not in ("success", "skipped", "neutral"))
    if failed:
        return "failure", failed
    if len(latest) < len(required) or any(
            run["status"] != "completed" for run in latest.values()):
        return "pending", []
    return "success", []


def vague_subject(subject):
    if re.match(r"^(fixup|squash|amend)!", subject):
        return True
    description = CONVENTIONAL_PREFIX.sub("", subject).strip()
    return len(description) < 8 or bool(VAGUE_DESCRIPTION.match(description))


def _content(lines):
    return {line.strip() for line in lines if len(line.strip()) > 2}


def history_facts(commits):
    """Collect signals for choosing between merge and squash.

    commits: oldest first; each has sha, subject, body, parents, and
    added/removed {path: [lines]} relative to its first parent. The agent
    makes the final call; these are hints, not a verdict.
    """
    facts = {"commits": len(commits), "merges": [], "reverts": [],
             "vague_subjects": [], "churn": [], "references_rewritten": []}
    for commit in commits:
        short = commit["sha"][:10]
        if len(commit["parents"]) > 1:
            facts["merges"].append(short)
        if commit["subject"].startswith('Revert "') or \
                "This reverts commit" in commit["body"]:
            facts["reverts"].append(short)
        if len(commit["parents"]) <= 1 and vague_subject(commit["subject"]):
            facts["vague_subjects"].append(f"{short} {commit['subject']}")
    for i, older in enumerate(commits):
        for newer in commits[i + 1:]:
            lines = sum(len(_content(older["added"].get(path, ())) & _content(removed))
                        for path, removed in newer["removed"].items())
            if lines:
                facts["churn"].append({"added_in": older["sha"][:10],
                                       "removed_in": newer["sha"][:10],
                                       "lines": lines})
    touched = {}
    for commit in commits:
        for path in set(commit["added"]) | set(commit["removed"]):
            if path.startswith("tests/reference/"):
                touched[path] = touched.get(path, 0) + 1
    facts["references_rewritten"] = sorted(p for p, n in touched.items() if n > 1)
    return facts


def squash_requests(comments):
    """Comments that mention squashing, excluding mergebot's own."""
    return [{"author": c["author"], "time": c["createdAt"], "url": c["url"],
             "body": c["body"][:500]}
            for c in comments
            if re.search(r"\bsquash", c["body"], re.IGNORECASE)
            and "<!-- mergebot:" not in c["body"]
            and "-- mergebot (" not in c["body"]]


def squash_subject(subject, number):
    subject = subject.strip()
    suffix = f"(#{number})"
    return subject if subject.endswith(suffix) else f"{subject} {suffix}"


# --- GitHub and git access ----------------------------------------------------

def run(command, cwd=None, input=None, check=True):
    result = subprocess.run(command, cwd=cwd, input=input, capture_output=True,
                            text=True)
    if check and result.returncode != 0:
        sys.exit(f"mergebot: {' '.join(command[:4])} failed:\n{result.stderr.strip()}")
    return result


def gh(*args, input=None):
    return run(["gh", *args], input=input).stdout


def gh_json(*args):
    return json.loads(gh(*args))


def gh_items(path, key):
    output = gh("api", "--paginate", path, "--jq", f".{key}[]")
    return [json.loads(line) for line in output.splitlines() if line]


def git(*args, cwd=None):
    return run(["git", *args], cwd=cwd).stdout.strip()


_cache = {}


def cached(key, compute):
    if key not in _cache:
        _cache[key] = compute()
    return _cache[key]


def login():
    return cached("login", lambda: gh("api", "user", "--jq", ".login").strip())


def can_write(user):
    def compute():
        result = run(["gh", "api", f"repos/{REPO}/collaborators/{user}/permission",
                      "--jq", ".permission"], check=False)
        return result.returncode == 0 and result.stdout.strip() in ("admin", "write")
    return cached(("write", user), compute)


def required_checks():
    def compute():
        rules = gh_json("api", f"repos/{REPO}/rules/branches/main")
        return {check["context"] for rule in rules
                if rule["type"] == "required_status_checks"
                for check in rule["parameters"]["required_status_checks"]}
    return cached("required", compute)


def state_dir():
    common = Path(git("rev-parse", "--path-format=absolute", "--git-common-dir"))
    path = common.parent / ".mergebot"
    (path / "locks").mkdir(parents=True, exist_ok=True)
    return path


def read_json(path, default):
    return json.loads(path.read_text()) if path.exists() else default


def write_json(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(mode="w", dir=path.parent, delete=False) as stream:
        temporary = Path(stream.name)
        json.dump(data, stream)
        stream.write("\n")
    try:
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def pr_state(number, label_id):
    state = read_json(state_dir() / "prs" / f"{number}.json", {})
    return state if state.get("label_id") == label_id else {"label_id": label_id}


def save_pr_state(number, state):
    write_json(state_dir() / "prs" / f"{number}.json", state)


def event_artifact(run_id):
    path = state_dir() / "events" / f"{run_id}.json"
    if path.exists():
        return read_json(path, {})
    artifacts = gh_items(f"repos/{REPO}/actions/runs/{run_id}/artifacts?per_page=100",
                         "artifacts")
    artifact = next((a for a in artifacts if a["name"] == "mergebot-event"
                     and not a["expired"]), None)
    if artifact is None:
        return None
    result = subprocess.run(["gh", "api",
                             f"repos/{REPO}/actions/artifacts/{artifact['id']}/zip"],
                            capture_output=True)
    if result.returncode:
        sys.exit(f"mergebot: cannot download event artifact: {result.stderr.decode()}")
    with zipfile.ZipFile(io.BytesIO(result.stdout)) as archive:
        event = json.loads(archive.read("mergebot-event.json"))
    write_json(path, event)
    return event


def label_events(number, since):
    query = urlencode({"event": "pull_request_target", "created": f">={since}",
                       "per_page": 100})
    runs = gh_items(f"repos/{REPO}/actions/workflows/{EVENT_FILE}/runs?{query}",
                    "workflow_runs")
    events = []
    for run in runs:
        if (run["event"] != "pull_request_target"
                or run["path"] != f".github/workflows/{EVENT_FILE}"
                or not run["display_title"].startswith(f"mergebot {number} ")
                or run["conclusion"] != "success"):
            continue
        event = event_artifact(run["id"])
        if event is not None:
            expected = f"mergebot {number} {event['action']} {event['head']}"
            if event["number"] != number or run["display_title"] != expected:
                sys.exit("mergebot: event artifact does not match its trusted workflow run")
            events.append(event)
    return events


PR_QUERY = """
query($owner: String!, $name: String!, $number: Int!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      number title state isDraft baseRefName headRefName headRefOid mergeable
      maintainerCanModify author { login } headRepository { nameWithOwner }
      labels(first: 100) { nodes { name } }
      timelineItems(first: 100, after: $cursor, itemTypes: [LABELED_EVENT, ISSUE_COMMENT]) {
        pageInfo { hasNextPage endCursor }
        nodes {
          __typename
          ... on LabeledEvent { id createdAt actor { login } label { name } }
          ... on IssueComment { createdAt author { login } body url }
        }
      }
    }
  }
}
"""


def load_pr(number):
    owner, name = REPO.split("/")
    cursor, nodes, pr = None, [], None
    while True:
        args = ["api", "graphql", "-f", f"query={PR_QUERY}", "-F", f"owner={owner}",
                "-F", f"name={name}", "-F", f"number={number}"]
        if cursor:
            args += ["-f", f"cursor={cursor}"]
        page = gh_json(*args)["data"]["repository"]["pullRequest"]
        if page is None:
            sys.exit(f"mergebot: PR #{number} not found")
        pr = pr or page
        nodes += page["timelineItems"]["nodes"]
        info = page["timelineItems"]["pageInfo"]
        if not info["hasNextPage"]:
            break
        cursor = info["endCursor"]
    return pr, nodes


def fetch(number):
    """Fetch main and the PR head; return the main SHA."""
    git("fetch", "-q", REPO_URL, "+refs/heads/main:refs/mergebot/main",
        f"+refs/pull/{number}/head:refs/mergebot/pr/{number}")
    return git("rev-parse", "refs/mergebot/main")


def conflicts(main, head):
    result = run(["git", "merge-tree", "--write-tree", "--name-only", "--no-messages",
                  main, head], check=False)
    if result.returncode not in (0, 1):
        sys.exit(f"mergebot: git merge-tree failed:\n{result.stderr}")
    return [line for line in result.stdout.splitlines()[1:] if line]


def lock_info(number):
    path = state_dir() / "locks" / f"{number}.json"
    if not path.exists():
        return None
    data = json.loads(path.read_text())
    age = datetime.now(timezone.utc) - parse_time(data["time"])
    data["stale"] = age > timedelta(hours=LOCK_HOURS)
    return data


def paused():
    return (state_dir() / "PAUSE").exists()


def facts(number, with_conflicts=True):
    pr, nodes = load_pr(number)
    me = login()
    comments = [{"author": (n["author"] or {}).get("login"), "body": n["body"],
                 "createdAt": n["createdAt"], "url": n["url"]}
                for n in nodes if n["__typename"] == "IssueComment"]
    labeled = [n for n in nodes if n["__typename"] == "LabeledEvent"
               and n["label"]["name"] == LABEL]
    is_labeled = LABEL in {label["name"] for label in pr["labels"]["nodes"]}
    label_id = labeled[-1]["id"] if labeled and is_labeled else None
    labeler = (labeled[-1]["actor"] or {}).get("login") if labeled else None
    state = pr_state(number, label_id)
    head = pr["headRefOid"]
    head_repo = (pr["headRepository"] or {}).get("nameWithOwner")
    runs = [r for r in gh_items(f"repos/{REPO}/actions/runs?head_sha={head}&per_page=100",
                                "workflow_runs")
            if r["event"] == "pull_request" and r["head_branch"] == pr["headRefName"]
            and (r["head_repository"] or {}).get("full_name") == head_repo]
    problem, pending, snapshot = None, None, None
    if not is_labeled:
        problem = "not labeled"
    elif pr["state"] != "OPEN" or pr["baseRefName"] != "main":
        problem = "not an open PR against main"
    elif not labeled:
        problem = "the label has no labeled event"
    elif not labeler or not can_write(labeler):
        problem = "the label was applied by an account without write access"
    else:
        events = label_events(number, labeled[-1]["createdAt"])
        snapshot, problem = approval_snapshot(labeled, events)
        if not problem and snapshot is None:
            pending = ("waiting for the label-event artifact; labels applied before "
                       "Mergebot events was installed, or with expired artifacts, "
                       "must be removed and reapplied")
        elif not problem:
            problem = approval_problem(head, snapshot, state, events, me)
            if not problem and state.get("rebase"):
                pending = "rebase validation is unfinished; complete or reject it"
    checks = gh_items(f"repos/{REPO}/commits/{head}/check-runs?per_page=100",
                      "check_runs")
    ci, failed = ci_state(required_checks(), checks)
    quick = max((r for r in runs if r["name"] == QUICK_NAME),
                key=lambda r: r["id"], default=None)
    result = {
        "number": number, "title": pr["title"], "open": pr["state"] == "OPEN", "author": (pr["author"] or {}).get("login"),
        "draft": pr["isDraft"], "head": head, "head_repo": head_repo,
        "head_branch": pr["headRefName"], "labeled": is_labeled, "labeler": labeler,
        "approval_problem": problem, "approval_pending": pending,
        "approval_captured": snapshot is not None,
        "label_id": label_id, "approved_head": state.get("approved_head",
                                                       snapshot and snapshot["head"]),
        "method": state.get("method"), "rebase": state.get("rebase"),
        "head_bot_pushed": any(p["head"] == head for p in state.get("pushes", [])),
        "ci": ci, "failed_checks": failed,
        "quick_run": quick and {"id": quick["id"], "attempt": quick["run_attempt"],
                                "status": quick["status"],
                                "conclusion": quick["conclusion"],
                                "url": quick["html_url"]},
        "mergeable": pr["mergeable"], "maintainer_can_modify": pr["maintainerCanModify"],
        "squash_requests": squash_requests(comments), "lock": lock_info(number),
    }
    if with_conflicts:
        result["conflicts"] = conflicts(fetch(number), f"refs/mergebot/pr/{number}")
    return result


def main_state():
    runs, page = [], 1
    while True:
        batch = gh_json("api", f"repos/{REPO}/actions/workflows/{QUICK_FILE}/runs"
                        f"?branch=main&event=push&per_page=100&page={page}")["workflow_runs"]
        runs.extend(batch)
        if len(batch) < 100 or any(r["status"] == "completed"
                                  and r["conclusion"] == "success" for r in batch):
            break
        page += 1
    sha = gh("api", f"repos/{REPO}/commits/main", "--jq", ".sha").strip()
    failure_dir = state_dir() / "main-failures"
    suspicion_dir = state_dir() / "main-suspicions"
    successful = [r for r in runs if r["status"] == "completed"
                  and r["conclusion"] == "success"]
    green_id = max((r["id"] for r in successful), default=0)
    for run in runs:
        if run["id"] <= green_id:
            continue
        failed = run["conclusion"] in ("failure", "timed_out", "action_required")
        if run["status"] != "completed" or run["conclusion"] == "cancelled":
            jobs = gh_items(f"repos/{REPO}/actions/runs/{run['id']}/jobs"
                            "?filter=latest&per_page=100", "jobs")
            failed |= any(j["conclusion"] in ("failure", "timed_out", "action_required")
                          for j in jobs)
        # Recover known red state even when starting a new session mid-rerun.
        failure_path = failure_dir / f"{run['id']}.json"
        if run["run_attempt"] > 1 and not failure_path.exists() and not failed:
            for attempt in range(1, run["run_attempt"]):
                previous = gh_json("api", f"repos/{REPO}/actions/runs/{run['id']}"
                                   f"/attempts/{attempt}")
                if previous["conclusion"] in ("failure", "timed_out", "action_required"):
                    failed = True
                    break
                if previous["conclusion"] == "cancelled":
                    jobs = gh_items(f"repos/{REPO}/actions/runs/{run['id']}"
                                    f"/attempts/{attempt}/jobs?per_page=100", "jobs")
                    if any(j["conclusion"] in ("failure", "timed_out", "action_required")
                           for j in jobs):
                        failed = True
                        break
        if failed:
            write_json(failure_path, {"sha": run["head_sha"], "url": run["html_url"]})
    failures = [read_json(path, {}) for path in failure_dir.glob("*.json")
                if int(path.stem) > green_id]
    # Read workers' independent records after the network requests. A status
    # update must not overwrite a suspicion posted while those requests ran.
    suspicions = {}
    for path in suspicion_dir.glob("*.json"):
        suspicion = read_json(path, {})
        if suspicion["run_floor"] > green_id:
            suspicions[path.stem] = suspicion
    current = next((r for r in runs if r["head_sha"] == sha), None)
    return {
        "green": bool(current and current["status"] == "completed"
                      and current["conclusion"] == "success"),
        "blocked": bool(failures or suspicions),
        "sha": sha, "url": current and current["html_url"],
        "run_floor": current["id"] if current else max((r["id"] for r in runs), default=0) + 1,
        "failures": failures, "suspicions": suspicions,
    }


def comment(number, body):
    text = f"{body.rstrip()}\n\n{SIGNATURE.format(login=login())}\n"
    gh("pr", "comment", str(number), "--repo", REPO, "--body-file", "-", input=text)


def require_approved(info, pending_rebase=False):
    # A maintainer may merge, close, or unlabel a PR by hand at any time.
    if not info["open"] or not info["labeled"]:
        sys.exit(f"mergebot: PR #{info['number']} is no longer open and labeled; "
                 "skip it")
    if info["approval_problem"]:
        sys.exit(f"mergebot: PR #{info['number']}: {info['approval_problem']}; "
                 "run `reject` instead")
    if info["approval_pending"] and not (
            pending_rebase and info["rebase"] and info["approval_captured"]):
        sys.exit(f"mergebot: PR #{info['number']}: {info['approval_pending']}")


def require_picked_up(info, pending_rebase=False):
    require_approved(info, pending_rebase=pending_rebase)
    if not info["method"]:
        sys.exit(f"mergebot: PR #{info['number']} has not been picked up; run `pickup`")


def text_arg(args):
    if args.reason_file:
        return Path(args.reason_file).read_text().strip()
    return args.reason.strip()


# --- Commands -------------------------------------------------------------------

def cmd_status(args):
    numbers = args.numbers or [p["number"] for p in gh_json(
        "pr", "list", "--repo", REPO, "--label", LABEL, "--state", "open",
        "--limit", "500", "--json", "number")]
    prs = [facts(n) for n in sorted(numbers)]
    in_flight = sum(1 for p in prs if p["lock"] or p["rebase"]
                    or (p["quick_run"] and p["quick_run"]["status"] != "completed"))
    print(json.dumps({"main": main_state(), "paused": paused(), "login": login(),
                      "in_flight": in_flight, "max_in_flight": MAX_IN_FLIGHT,
                      "prs": prs}, indent=2, default=str))


def cmd_pickup(args):
    info = facts(args.number, with_conflicts=False)
    require_approved(info)
    if info["method"]:
        sys.exit(f"mergebot: PR #{args.number} was already picked up ({info['method']})")
    state = pr_state(args.number, info["label_id"])
    state.update(approved_head=info["head"], method=args.method)
    save_pr_state(args.number, state)
    if info["draft"]:
        gh("pr", "ready", str(args.number), "--repo", REPO)
    comment(args.number,
            f"Picked up approved head `{info['head'][:12]}` for auto-merge. "
            f"Planned method: **{args.method}**. {text_arg(args)}\n\n"
            "Any push to this branch other than mergebot's own rebases removes "
            f"the `{LABEL}` label; to approve a new head, review it and apply "
            "the label again.")


def cmd_squash(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info)
    if info["method"] == "squash":
        return
    if info["lock"]:
        sys.exit("mergebot: wait for the rebase worker before changing the merge method")
    state = pr_state(args.number, info["label_id"])
    state["method"] = "squash"
    save_pr_state(args.number, state)
    comment(args.number, f"Switching to **squash** merge. {text_arg(args)}")


def commit_diff(sha, parent):
    added, removed, path = {}, {}, None
    diff = git("diff", "-U0", "--no-color", "--no-renames", parent, sha) if parent else \
        git("show", "-U0", "--no-color", "--format=", sha)
    for line in diff.splitlines():
        if line.startswith("+++ "):
            path = line[6:] if line.startswith("+++ b/") else path
        elif line.startswith("--- "):
            path = line[6:] if line.startswith("--- a/") else path
        elif line.startswith("+") and path:
            added.setdefault(path, []).append(line[1:])
        elif line.startswith("-") and path:
            removed.setdefault(path, []).append(line[1:])
    return added, removed


def cmd_history(args):
    main = fetch(args.number)
    head = f"refs/mergebot/pr/{args.number}"
    commits = []
    for sha in git("rev-list", "--reverse", "--topo-order", f"{main}..{head}").split():
        parents = git("show", "-s", "--format=%P", sha).split()
        added, removed = commit_diff(sha, parents[0] if parents else None)
        commits.append({"sha": sha, "parents": parents,
                        "subject": git("show", "-s", "--format=%s", sha),
                        "body": git("show", "-s", "--format=%b", sha),
                        "added": added, "removed": removed})
    result = history_facts(commits)
    result["log"] = [f"{c['sha'][:10]} {c['subject']}" for c in commits]
    info = facts(args.number, with_conflicts=False)
    result["squash_requests"] = info["squash_requests"]
    print(json.dumps(result, indent=2))


def check_commits(worktree, base):
    """Mergebot pushes only commits committed by the configured human identity,
    with no AI attribution, and adds no co-author lines to commits it authors.
    The contributors' own commits keep their authors and messages."""
    email = git("config", "user.email", cwd=worktree)
    spec = importlib.util.spec_from_file_location(
        "check_ai_commit_authorship", Path(worktree) / "check_ai_commit_authorship.py")
    checker = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(checker)
    problems = []
    previous = os.getcwd()
    os.chdir(worktree)
    try:
        for sha in git("rev-list", f"{base}..HEAD").split():
            if git("show", "-s", "--format=%ce", sha) != email:
                problems.append(f"{sha[:10]}: committer is not {email}")
            if git("show", "-s", "--format=%ae", sha) == email and \
                    TRAILER.search(git("show", "-s", "--format=%B", sha)):
                problems.append(f"{sha[:10]}: has a co-author or AI trailer")
            problems += [f"{sha[:10]}: {r}" for r in checker.check_commit(sha)]
    finally:
        os.chdir(previous)
    if problems:
        sys.exit("mergebot: refusing to push:\n" + "\n".join(problems))


def require_lock(number, owner):
    lock = lock_info(number)
    if not lock or lock["owner"] != owner:
        sys.exit(f"mergebot: PR #{number} is not locked by {owner}")


def clean_head(worktree):
    if git("status", "--porcelain", "--untracked-files=no", cwd=worktree):
        sys.exit("mergebot: commit all tracked changes before publishing or completing")
    return git("rev-parse", "HEAD", cwd=worktree)


def cmd_push(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info, pending_rebase=True)
    require_lock(args.number, args.owner)
    if info["head"] != args.expect:
        sys.exit(f"mergebot: PR head moved: expected {args.expect[:10]}, "
                 f"found {info['head'][:10]}; rerun status before rebasing again")
    worktree = str(Path(args.worktree).resolve())
    new = clean_head(worktree)
    if new == args.expect:
        sys.exit("mergebot: no new head to push; complete validation or rerun CI instead")
    main = fetch(args.number)
    for ancestor, descendant, what in ((args.base, main, "a commit on main"),
                                       (args.base, new, "an ancestor of HEAD")):
        if run(["git", "merge-base", "--is-ancestor", ancestor, descendant],
               cwd=worktree, check=False).returncode != 0:
            sys.exit(f"mergebot: --base {args.base[:10]} is not {what}")
    check_commits(worktree, args.base)
    reason = text_arg(args)
    state = pr_state(args.number, info["label_id"])
    transition = {"from": args.expect, "head": new, "base": args.base,
                  "owner": args.owner, "status": "prepared"}
    state.setdefault("pushes", []).append(transition)
    state["rebase"] = transition.copy()
    save_pr_state(args.number, state)
    if args.needs_review:
        # Revoke approval before the changed head can become visible.
        cmd_reject(argparse.Namespace(number=args.number, reason=reason,
                                     reason_file=None, draft=False))
    run(["git", "push", f"--force-with-lease=refs/heads/{info['head_branch']}:{args.expect}",
         f"https://github.com/{info['head_repo']}.git",
         f"{new}:refs/heads/{info['head_branch']}"], cwd=worktree)
    if not args.needs_review:
        transition["status"] = "pushed"
        state["rebase"] = transition.copy()
        save_pr_state(args.number, state)
    comment(args.number, f"{reason}\n\nPushed `{new[:12]}`; "
            + ("human re-review is required." if args.needs_review else
               "rebase validation is still pending."))
    print(new)


def cmd_complete(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info, pending_rebase=True)
    require_lock(args.number, args.owner)
    pending = info["rebase"]
    if not pending or pending["owner"] != args.owner:
        sys.exit("mergebot: no pending rebase owned by this worker")
    if info["head"] != args.expect or pending["head"] != args.expect:
        sys.exit("mergebot: the head changed since validation")
    if clean_head(str(Path(args.worktree).resolve())) != args.expect:
        sys.exit("mergebot: the validated worktree does not match the PR head")
    if args.verdict != "same":
        cmd_reject(argparse.Namespace(number=args.number, reason=text_arg(args),
                                     reason_file=None, draft=args.verdict == "pr-bug"))
        return
    state = pr_state(args.number, info["label_id"])
    state.update(approved_head=args.expect, rebase=None)
    save_pr_state(args.number, state)
    comment(args.number, f"Completed validation of `{args.expect[:12]}`: {text_arg(args)}")


def cmd_merge(args):
    if paused():
        sys.exit("mergebot: merging is paused (run `resume` to continue)")
    info = facts(args.number)
    require_picked_up(info)
    blockers = []
    if info["draft"]:
        blockers.append("PR is a draft")
    if info["ci"] != "success":
        blockers.append(f"CI is {info['ci']}")
    quick = info["quick_run"]
    if not quick or quick["status"] != "completed" or quick["conclusion"] != "success":
        blockers.append("the current Quick run has not succeeded")
    if info["lock"]:
        blockers.append("PR has a rebase lock (including an expired lock)")
    if info["rebase"]:
        blockers.append("rebase validation is unfinished")
    if info["conflicts"] or info["mergeable"] == "CONFLICTING":
        blockers.append("PR conflicts with main")
    if not args.allow_red_main and main_state()["blocked"]:
        blockers.append("main has a confirmed or suspected CI failure")
    method = info["method"]
    if method == "squash" and not (args.subject and args.body_file):
        blockers.append("a squash merge needs --subject and --body-file")
    if method == "merge" and (args.subject or args.body_file):
        blockers.append("a merge uses GitHub's default message")
    body = Path(args.body_file).read_text() if args.body_file else ""
    if TRAILER.search(body):
        blockers.append("the squash message has a co-author or AI trailer")
    if blockers:
        sys.exit(f"mergebot: not merging #{args.number}: " + "; ".join(blockers))
    command = ["pr", "merge", str(args.number), "--repo", REPO,
               "--match-head-commit", info["head"]]
    if method == "squash":
        command += ["--squash", "--subject", squash_subject(args.subject, args.number),
                    "--body-file", args.body_file]
    else:
        command += ["--merge"]
    gh(*command)
    how = "Squash-merged" if method == "squash" else "Merged (merge commit)"
    comment(args.number, f"{how}: {text_arg(args)}")


def cmd_ready(args):
    info = facts(args.number, with_conflicts=False)
    require_approved(info)
    if info["draft"]:
        gh("pr", "ready", str(args.number), "--repo", REPO)


def cmd_reject(args):
    pr = gh_json("pr", "view", str(args.number), "--repo", REPO, "--json", "state,labels")
    if pr["state"] != "OPEN" or LABEL not in {l["name"] for l in pr["labels"]}:
        print(f"PR #{args.number} is no longer open and labeled; nothing to do")
        return
    gh("pr", "edit", str(args.number), "--repo", REPO, "--remove-label", LABEL)
    if args.draft:
        gh("pr", "ready", str(args.number), "--repo", REPO, "--undo")
    path = state_dir() / "prs" / f"{args.number}.json"
    state = read_json(path, {})
    state["rebase"] = None
    write_json(path, state)
    comment(args.number, f"Removed the `{LABEL}` label"
            f"{' and converted the PR to a draft' if args.draft else ''}: "
            f"{text_arg(args)}")


def cmd_rerun(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info)
    quick = info["quick_run"]
    if quick and quick["status"] != "completed":
        print("Quick is still running; wait before rerunning failed jobs")
        return
    if not quick or quick["conclusion"] != "failure":
        sys.exit("mergebot: the head's Quick run has not failed")
    if quick["attempt"] >= MAX_RUN_ATTEMPTS:
        sys.exit("mergebot: the failed jobs were already rerun once")
    gh("run", "rerun", str(quick["id"]), "--repo", REPO, "--failed")


def cmd_comment(args):
    require_approved(facts(args.number, with_conflicts=False))
    comment(args.number, text_arg(args))


def cmd_suspect_main(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info, pending_rebase=True)
    main = main_state()
    if main["green"]:
        sys.exit("mergebot: the current main head already passed Quick; reclassify the PR failure")
    path = state_dir() / "main-suspicions" / f"{args.number}.json"
    suspicion = {
        "head": info["head"], "main_sha": main["sha"],
        "run_floor": main["run_floor"], "reason": text_arg(args),
    }
    if read_json(path, None) == suspicion:
        return
    write_json(path, suspicion)
    comment(args.number, f"Pausing merges while main CI confirms or rules out: {text_arg(args)}")


def cmd_lock(args):
    path = state_dir() / "locks" / f"{args.number}.json"
    current = lock_info(args.number)
    if current:
        sys.exit(f"mergebot: PR #{args.number} is locked by {current['owner']}")
    try:
        with path.open("x") as stream:
            json.dump({"owner": args.owner, "time": datetime.now(timezone.utc).isoformat()},
                      stream)
    except FileExistsError:
        sys.exit(f"mergebot: PR #{args.number} was locked by another worker")


def cmd_unlock(args):
    require_lock(args.number, args.owner)
    state = read_json(state_dir() / "prs" / f"{args.number}.json", {})
    if state.get("rebase"):
        info = facts(args.number, with_conflicts=False)
        if info["open"] and info["labeled"] and state["label_id"] == info["label_id"]:
            sys.exit("mergebot: unfinished rebase; recover and complete it or reject before unlocking")
        state["rebase"] = None
        save_pr_state(args.number, state)
    (state_dir() / "locks" / f"{args.number}.json").unlink(missing_ok=True)


def cmd_pause(args):
    (state_dir() / "PAUSE").write_text(datetime.now(timezone.utc).isoformat())


def cmd_resume(args):
    (state_dir() / "PAUSE").unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = parser.add_subparsers(dest="command", required=True)

    def command(name, function, help, number=True, reason=False):
        p = sub.add_parser(name, help=help)
        if number:
            p.add_argument("number", type=int)
        if reason:
            group = p.add_mutually_exclusive_group(required=True)
            group.add_argument("--reason", help="comment text")
            group.add_argument("--reason-file", help="file with the comment text")
        p.set_defaults(function=function)
        return p

    p = command("status", cmd_status, "JSON state of main and labeled PRs", number=False)
    p.add_argument("numbers", type=int, nargs="*")
    p = command("pickup", cmd_pickup, "record the approved head and merge method",
                reason=True)
    p.add_argument("--method", choices=("merge", "squash"), required=True)
    command("ready", cmd_ready, "mark an approved draft PR ready for review")
    command("squash", cmd_squash, "switch a picked-up PR to squash", reason=True)
    command("history", cmd_history, "signals for choosing merge or squash")
    p = command("push", cmd_push, "push a rebased worktree to the PR branch",
                reason=True)
    p.add_argument("--worktree", required=True)
    p.add_argument("--expect", required=True, help="PR head SHA the rebase started from")
    p.add_argument("--base", required=True, help="main SHA the PR was rebased onto")
    p.add_argument("--owner", required=True, help="owner of the rebase lock")
    p.add_argument("--needs-review", action="store_true",
                   help="remove approval before publishing changed behavior")
    p = command("complete", cmd_complete, "finish validation of the exact rebased head",
                reason=True)
    p.add_argument("--worktree", required=True)
    p.add_argument("--expect", required=True)
    p.add_argument("--owner", required=True)
    p.add_argument("--verdict", choices=("same", "changed", "pr-bug"), required=True)
    p = command("merge", cmd_merge, "merge a green, approved PR", reason=True)
    p.add_argument("--subject", help="squash commit subject")
    p.add_argument("--body-file", help="squash commit body")
    p.add_argument("--allow-red-main", action="store_true",
                   help="only when the user explicitly asks")
    p = command("reject", cmd_reject, "remove the label", reason=True)
    p.add_argument("--draft", action="store_true", help="also convert to draft")
    command("rerun", cmd_rerun, "rerun failed Quick jobs once")
    command("comment", cmd_comment, "post a signed comment", reason=True)
    command("suspect-main", cmd_suspect_main, "pause merges for a suspected main failure",
            reason=True)
    p = command("lock", cmd_lock, "claim a PR for a rebase")
    p.add_argument("--owner", required=True)
    p = command("unlock", cmd_unlock, "release a PR after completion or rejection")
    p.add_argument("--owner", required=True)
    command("pause", cmd_pause, "stop merging", number=False)
    command("resume", cmd_resume, "resume merging", number=False)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
