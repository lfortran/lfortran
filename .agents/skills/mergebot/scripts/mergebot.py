#!/usr/bin/env python3
"""Safety-critical commands for the mergebot skill.

The agent makes the judgment calls (merge method, rebases, CI failure
classification); this script enforces the rules that must never be broken.
Every command that changes GitHub first re-checks that the PR's current head is
the head a maintainer approved with the `auto-merge` label, or a mergebot
rebase of it. A confused or manipulated agent therefore cannot merge or push
unapproved code. See ../SKILL.md for the workflow.

All state lives on GitHub (labels, the PR timeline, and marker comments that
mergebot posts), except the pause flag and per-PR locks, which live in
`.mergebot/` next to the main checkout.
"""

import argparse
from datetime import datetime, timedelta, timezone
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys

REPO = "lfortran/lfortran"
REPO_URL = f"https://github.com/{REPO}.git"
LABEL = "auto-merge"
QUICK_FILE = "Quick-Checks-CI.yml"
QUICK_NAME = "Quick checks"
MAX_IN_FLIGHT = 4
LOCK_HOURS = 12
MAX_RUN_ATTEMPTS = 2  # rerun a flaky Quick run at most once

MARKER = re.compile(r"<!-- mergebot:(\w+)((?: [\w-]+=\S+)*) -->")
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


def markers(comments, login):
    """Return (kind, fields, time) for each marker in mergebot's own comments."""
    found = []
    for comment in comments:
        if comment["author"] != login:
            continue
        for match in MARKER.finditer(comment["body"]):
            fields = dict(f.split("=", 1) for f in match.group(2).split())
            found.append((match.group(1), fields, parse_time(comment["createdAt"])))
    return found


def approval_problem(head, label_time, labeler_can_write, force_pushes,
                     head_run_times, bot_pushed):
    """Return None if `head` may be merged under the current label, else why not.

    Approved means: `head` was the PR head when the label was applied, or it is
    a mergebot rebase announced after that. Only server-side timestamps are
    trusted; commit dates are chosen by the author and are ignored.

    force_pushes: [(time, after_sha)] from the PR timeline.
    head_run_times: creation times of pull_request workflow runs for `head` on
        the PR's own branch; the earliest one is when `head` was pushed there.
    bot_pushed: SHAs that mergebot announced after `label_time`.
    """
    if label_time is None:
        return "the label has no labeled event"
    if not labeler_can_write:
        return "the label was applied by an account without write access"
    for time, after in force_pushes:
        if time > label_time and after not in bot_pushed:
            return f"the branch was force-pushed to {after[:10]} after approval"
    if head in bot_pushed:
        return None
    if not head_run_times:
        return f"no CI run shows when {head[:10]} was pushed"
    pushed = min(head_run_times)
    if pushed > label_time:
        return f"{head[:10]} was pushed after approval"
    # A force-push between `pushed` and the label must have left `head` in place;
    # otherwise the approved head was something else and `head` came back later.
    between = [after for time, after in sorted(force_pushes)
               if pushed < time <= label_time]
    if between and between[-1] != head:
        return (f"the head at approval time was {between[-1][:10]}, "
                f"not {head[:10]}")
    return None


def merge_method(found, label_time):
    """Return the recorded merge method, or None before pickup.

    Only a switch to squash can follow the method chosen at pickup.
    """
    recorded = [(fields.get("method"), time) for kind, fields, time in found
                if kind in ("approved", "method") and time > label_time]
    if not recorded:
        return None
    if any(method == "squash" for method, _ in recorded):
        return "squash"
    return "merge"


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


PR_QUERY = """
query($owner: String!, $name: String!, $number: Int!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      number title state isDraft baseRefName headRefName headRefOid mergeable
      maintainerCanModify author { login } headRepository { nameWithOwner }
      labels(first: 100) { nodes { name } }
      timelineItems(first: 100, after: $cursor, itemTypes: [LABELED_EVENT,
          HEAD_REF_FORCE_PUSHED_EVENT, ISSUE_COMMENT]) {
        pageInfo { hasNextPage endCursor }
        nodes {
          __typename
          ... on LabeledEvent { createdAt actor { login } label { name } }
          ... on HeadRefForcePushedEvent { createdAt afterCommit { oid } }
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
    label_time = parse_time(labeled[-1]["createdAt"]) if labeled and is_labeled else None
    labeler = (labeled[-1]["actor"] or {}).get("login") if labeled else None
    force_pushes = [(parse_time(n["createdAt"]), (n["afterCommit"] or {}).get("oid", "?"))
                    for n in nodes if n["__typename"] == "HeadRefForcePushedEvent"]
    found = markers(comments, me)
    bot_pushed = {fields["sha"] for kind, fields, time in found
                  if kind == "pushed" and label_time and time > label_time}
    head = pr["headRefOid"]
    head_repo = (pr["headRepository"] or {}).get("nameWithOwner")
    runs = [r for r in gh_items(f"repos/{REPO}/actions/runs?head_sha={head}&per_page=100",
                                "workflow_runs")
            if r["event"] == "pull_request" and r["head_branch"] == pr["headRefName"]
            and (r["head_repository"] or {}).get("full_name") == head_repo]
    problem = None
    if not is_labeled:
        problem = "not labeled"
    elif pr["state"] != "OPEN" or pr["baseRefName"] != "main":
        problem = "not an open PR against main"
    else:
        problem = approval_problem(
            head, label_time, bool(labeler) and can_write(labeler), force_pushes,
            [parse_time(r["created_at"]) for r in runs], bot_pushed)
    checks = gh_items(f"repos/{REPO}/commits/{head}/check-runs?per_page=100",
                      "check_runs")
    state, failed = ci_state(required_checks(), checks)
    quick = max((r for r in runs if r["name"] == QUICK_NAME),
                key=lambda r: r["id"], default=None)
    result = {
        "number": number, "title": pr["title"], "open": pr["state"] == "OPEN", "author": (pr["author"] or {}).get("login"),
        "draft": pr["isDraft"], "head": head, "head_repo": head_repo,
        "head_branch": pr["headRefName"], "labeled": is_labeled, "labeler": labeler,
        "approval_problem": problem, "method": merge_method(found, label_time)
        if label_time else None, "head_bot_pushed": head in bot_pushed,
        "ci": state, "failed_checks": failed,
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
    runs = gh_json("api", f"repos/{REPO}/actions/workflows/{QUICK_FILE}/runs"
                   "?branch=main&event=push&per_page=20")["workflow_runs"]
    done = [r for r in runs if r["status"] == "completed" and r["conclusion"] != "cancelled"]
    if not done:
        return {"green": False, "sha": None, "url": None}
    return {"green": done[0]["conclusion"] == "success", "sha": done[0]["head_sha"],
            "url": done[0]["html_url"]}


def comment(number, body):
    text = f"{body.rstrip()}\n\n{SIGNATURE.format(login=login())}\n"
    gh("pr", "comment", str(number), "--repo", REPO, "--body-file", "-", input=text)


def require_approved(info):
    # A maintainer may merge, close, or unlabel a PR by hand at any time.
    if not info["open"] or not info["labeled"]:
        sys.exit(f"mergebot: PR #{info['number']} is no longer open and labeled; "
                 "skip it")
    if info["approval_problem"]:
        sys.exit(f"mergebot: PR #{info['number']}: {info['approval_problem']}; "
                 "run `reject` instead")


def require_picked_up(info):
    require_approved(info)
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
    in_flight = sum(1 for p in prs if (p["lock"] and not p["lock"]["stale"])
                    or (p["head_bot_pushed"] and p["ci"] == "pending"))
    print(json.dumps({"main": main_state(), "paused": paused(), "login": login(),
                      "in_flight": in_flight, "max_in_flight": MAX_IN_FLIGHT,
                      "prs": prs}, indent=2, default=str))


def cmd_pickup(args):
    info = facts(args.number, with_conflicts=False)
    require_approved(info)
    if info["method"]:
        sys.exit(f"mergebot: PR #{args.number} was already picked up ({info['method']})")
    if info["draft"]:
        gh("pr", "ready", str(args.number), "--repo", REPO)
    comment(args.number,
            f"Picked up approved head `{info['head'][:12]}` for auto-merge. "
            f"Planned method: **{args.method}**. {text_arg(args)}\n\n"
            "Any push to this branch other than mergebot's own rebases removes "
            f"the `{LABEL}` label; to approve a new head, review it and apply "
            "the label again.\n"
            f"<!-- mergebot:approved sha={info['head']} method={args.method} -->")


def cmd_squash(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info)
    if info["method"] == "squash":
        return
    comment(args.number, f"Switching to **squash** merge. {text_arg(args)}\n"
            "<!-- mergebot:method method=squash -->")


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


def cmd_push(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info)
    if info["head"] != args.expect:
        sys.exit(f"mergebot: PR head moved: expected {args.expect[:10]}, "
                 f"found {info['head'][:10]}; rerun status before rebasing again")
    worktree = str(Path(args.worktree).resolve())
    new = git("rev-parse", "HEAD", cwd=worktree)
    main = fetch(args.number)
    for ancestor, descendant, what in ((args.base, main, "a commit on main"),
                                       (args.base, new, "an ancestor of HEAD")):
        if run(["git", "merge-base", "--is-ancestor", ancestor, descendant],
               cwd=worktree, check=False).returncode != 0:
            sys.exit(f"mergebot: --base {args.base[:10]} is not {what}")
    check_commits(worktree, args.base)
    # Announce first: if the push fails, a stray SHA in the record is harmless.
    comment(args.number, f"{text_arg(args)}\n"
            f"<!-- mergebot:pushed sha={new} from={args.expect} -->")
    run(["git", "push", f"--force-with-lease=refs/heads/{info['head_branch']}:{args.expect}",
         f"https://github.com/{info['head_repo']}.git",
         f"{new}:refs/heads/{info['head_branch']}"], cwd=worktree)
    print(new)


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
    if info["conflicts"] or info["mergeable"] == "CONFLICTING":
        blockers.append("PR conflicts with main")
    if not args.allow_red_main and not main_state()["green"]:
        blockers.append("Quick on main is red")
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
    comment(args.number, f"Removed the `{LABEL}` label"
            f"{' and converted the PR to a draft' if args.draft else ''}: "
            f"{text_arg(args)}")


def cmd_rerun(args):
    info = facts(args.number, with_conflicts=False)
    require_picked_up(info)
    quick = info["quick_run"]
    if not quick or quick["conclusion"] != "failure":
        sys.exit("mergebot: the head's Quick run has not failed")
    if quick["attempt"] >= MAX_RUN_ATTEMPTS:
        sys.exit("mergebot: the failed jobs were already rerun once")
    gh("run", "rerun", str(quick["id"]), "--repo", REPO, "--failed")


def cmd_comment(args):
    comment(args.number, text_arg(args))


def cmd_lock(args):
    path = state_dir() / "locks" / f"{args.number}.json"
    current = lock_info(args.number)
    if current and not current["stale"]:
        sys.exit(f"mergebot: PR #{args.number} is locked by {current['owner']}")
    path.write_text(json.dumps({"owner": args.owner,
                                "time": datetime.now(timezone.utc).isoformat()}))


def cmd_unlock(args):
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
    p = command("merge", cmd_merge, "merge a green, approved PR", reason=True)
    p.add_argument("--subject", help="squash commit subject")
    p.add_argument("--body-file", help="squash commit body")
    p.add_argument("--allow-red-main", action="store_true",
                   help="only when the user explicitly asks")
    p = command("reject", cmd_reject, "remove the label", reason=True)
    p.add_argument("--draft", action="store_true", help="also convert to draft")
    command("rerun", cmd_rerun, "rerun failed Quick jobs once")
    command("comment", cmd_comment, "post a signed comment", reason=True)
    p = command("lock", cmd_lock, "claim a PR for a rebase")
    p.add_argument("--owner", required=True)
    command("unlock", cmd_unlock, "release a PR")
    command("pause", cmd_pause, "stop merging", number=False)
    command("resume", cmd_resume, "resume merging", number=False)
    args = parser.parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
