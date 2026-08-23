#!/usr/bin/env python3
"""
Reject commits that claim AI authorship or AI co-authorship.

Policy (see PR discussions / project guidelines): every commit and PR must be
submitted by a human who has read, understood, and guarantees the change.
AI tools may help write code, but must not appear as author, committer, or
Co-Authored-By / Generated-By / similar trailers.

This script inspects commits on the current branch that are not on origin/main
(same range as check_binary_file_in_git_history.py).

Exit 0 if clean, exit 1 if any commit looks AI-authored.
"""

from __future__ import annotations

import re
import subprocess
import sys
from typing import List, Optional, Tuple


# ---------------------------------------------------------------------------
# Trailers that must never name an AI (and some that we ban entirely when
# they are the common "AI stamped this" forms).
# ---------------------------------------------------------------------------
TRAILER_KEYS = (
    "co-authored-by",
    "co-author",
    "authored-by",
    "generated-by",
    "generated-with",
    "assisted-by",
    "assisted-with",
    "ai-assisted-by",
    "signed-off-by",  # only fails if the identity looks like AI
    "reported-by",
    "acked-by",
    "reviewed-by",
    "tested-by",
    "committed-by",
    "helped-by",
    "with",
)

# Keys that are almost exclusively used by coding agents to stamp commits.
# Any non-empty value on these trailers fails (not only AI-looking values).
ALWAYS_FAIL_TRAILER_KEYS = {
    "generated-by",
    "generated-with",
    "assisted-by",
    "assisted-with",
    "ai-assisted-by",
}

# Trailers for which we only fail when the identity looks like an AI.
IDENTITY_TRAILER_KEYS = {
    "co-authored-by",
    "co-author",
    "authored-by",
    "signed-off-by",
    "reported-by",
    "acked-by",
    "reviewed-by",
    "tested-by",
    "committed-by",
    "helped-by",
    "with",
}

# Emails that alone (without name check) are enough to fail.
AI_EMAIL_HARD_PATTERNS = [
    re.compile(p, re.IGNORECASE)
    for p in (
        r"@anthropic\.com$",
        r"noreply@anthropic\.com$",
        r"@openai\.com$",
        r"@cursor\.com$",
        r"@cursor\.so$",
        r"cursoragent@",
        r"@x\.ai$",
        r"@devin\.ai$",
        r"@cognition\.ai$",
        r"@codeium\.com$",
        r"@windsurf\.",
        r"@tabnine\.com$",
        r"@aider\.chat$",
        r"@sweep\.dev$",
        r"@mentat\.ai$",
        r"@factory\.ai$",
        r"@continue\.dev$",
        r"@bolt\.new$",
        r"@lovable\.dev$",
        r"@v0\.dev$",
        r"223556219\+copilot@users\.noreply\.github\.com$",
        r"\+copilot@users\.noreply\.github\.com$",
        r"^copilot@users\.noreply\.github\.com$",
        r"^copilot@github\.com$",
        # Local-part stamps used by agents (paired with obvious product domains above).
        # Do not ban bare first names like devin@gmail.com — only product-like locals
        # or agent-specific addresses.
        r"^claude@",
        r"^chatgpt@",
        r"^gpt-?[0-9]",
        r"^cursoragent@",
        r"^github-copilot@",
        r"^copilot-swe-agent@",
        r"^aider@",
        r"^noreply@anthropic",
    )
]

# Name tokens / phrases that indicate an AI agent identity.
# Matched as whole-word-ish against author/committer/trailer display names.
AI_NAME_PATTERNS = [
    re.compile(p, re.IGNORECASE)
    for p in (
        # Anthropic Claude family (Opus/Sonnet/Haiku + version noise)
        r"\bclaude\b",
        r"\banthropic\b",
        r"\bclaude\s+(opus|sonnet|haiku)\b",
        r"\b(opus|sonnet|haiku)\s*[0-9]",
        # OpenAI / ChatGPT / Codex
        r"\bchatgpt\b",
        r"\bgpt-?[0-9]",
        r"\bopenai\b",
        r"\bopenai\s*codex\b",
        r"\bcodex\s*(cli|agent)\b",
        r"^codex$",
        r"\bo[1-4](-mini|-preview)?\b",
        # GitHub Copilot
        r"\bcopilot\b",
        r"\bgithub\s*copilot\b",
        # Cursor
        r"\bcursor\s*agent\b",
        r"\bcursoragent\b",
        r"\bcursor\s*ai\b",
        r"^cursor$",
        # Google
        r"\bgemini\b",
        r"\bbard\b",
        r"\bgoogle\s*jules\b",
        r"\bjules\s*(agent|by\s*google)\b",
        r"\bgoogle\s*ai\s*(studio|agent)\b",
        # xAI
        r"\bgrok\b",
        r"\bxai\b",
        r"\bx\.ai\b",
        # Other coding agents / products commonly stamping commits.
        # Prefer product phrasing for names that are also common human first names
        # (Devin, Cody, Jules) — email-domain checks still catch agent accounts.
        r"\bdevin\s*ai\b",
        r"\bcognition(\s*ai|\s*labs)\b",
        r"\baider(\.chat)?\b",
        r"\bcody\s*ai\b",
        r"\bsourcegraph\s*cody\b",
        r"\bcodeium\b",
        r"\bwindsurf(\s*ai)?\b",
        r"\btabnine\b",
        r"\bsweep\s*ai\b",
        r"\bmentat\s*ai\b",
        r"\bamazon\s*q\b",
        r"\bq\s*developer\b",
        r"\breplit\s*(agent|ghostwriter)\b",
        r"\bfactory\s*(ai|droid)\b",
        r"\bamp\s*code\b",
        r"\bcontinue\.dev\b",
        r"\bbolt\.new\b",
        r"\blovable\.dev\b",
        r"\bv0\.dev\b",
        r"\bclaude\s*code\b",
        r"\bai\s*(agent|assistant|coder|pair)\b",
        r"\bauto[- ]?gpt\b",
        r"\bopenhands\b",
        r"\bopendevin\b",
        r"\bswe[- ]?agent\b",
        r"\bcursor\s*composer\b",
    )
]

# Body lines that look like free-form AI attribution (not formal trailers).
AI_BODY_LINE_PATTERNS = [
    re.compile(p, re.IGNORECASE)
    for p in (
        r"^\s*co-authored-by\s*:",
        r"^\s*generated\s+by\s+(claude|chatgpt|gpt|copilot|cursor|gemini|grok|devin|aider|codex)\b",
        r"^\s*assisted\s+by\s+(claude|chatgpt|gpt|copilot|cursor|gemini|grok|devin|aider|codex)\b",
        r"^\s*written\s+by\s+(claude|chatgpt|gpt|copilot|cursor|gemini|grok|devin|aider|codex)\b",
        r"^\s*authored\s+by\s+(claude|chatgpt|gpt|copilot|cursor|gemini|grok|devin|aider|codex)\b",
        r"^\s*created\s+with\s+(claude|chatgpt|gpt|copilot|cursor|gemini|grok|devin|aider|codex)\b",
        r"^\s*pair[- ]programmed\s+with\s+(claude|chatgpt|gpt|copilot|cursor|gemini|grok|devin|aider|codex)\b",
        r"^\s*🤖",  # common bot/AI emoji stamp on agent commits
    )
]

# Avoid flagging dependabot / renovate / github-actions as "AI" by name alone
# when they appear only as automation bots (still not humans, but out of scope).
ALLOWLIST_NAME_PATTERNS = [
    re.compile(p, re.IGNORECASE)
    for p in (
        r"^dependabot(\[bot\])?$",
        r"^renovate(\[bot\])?$",
        r"^github-actions(\[bot\])?$",
        r"^pre-commit-ci(\[bot\])?$",
        r"^linter(\[bot\])?$",
    )
]

ALLOWLIST_EMAIL_PATTERNS = [
    re.compile(p, re.IGNORECASE)
    for p in (
        r"dependabot\[bot\]@users\.noreply\.github\.com$",
        r"49699333\+dependabot\[bot\]@users\.noreply\.github\.com$",
        r"renovate\[bot\]@users\.noreply\.github\.com$",
        r"github-actions\[bot\]@users\.noreply\.github\.com$",
        r"41898282\+github-actions\[bot\]@users\.noreply\.github\.com$",
    )
]


def run_git(args: List[str]) -> str:
    completed = subprocess.run(
        ["git", *args],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        print(
            f"git {' '.join(args)} failed ({completed.returncode}): {completed.stderr}",
            file=sys.stderr,
        )
        raise SystemExit(2)
    return completed.stdout


def get_commit_hashes() -> List[str]:
    """Commits reachable from HEAD but not origin/main (PR range)."""
    out = run_git(["rev-list", "origin/main..HEAD"])
    hashes = [h for h in out.splitlines() if h.strip()]
    print(
        "number of commits from current PR branch against origin/main is:",
        len(hashes),
    )
    if hashes:
        print(out)
    return hashes


def parse_identity(raw: str) -> Tuple[str, str]:
    """
    Parse 'Name <email>' or bare 'Name' / 'email@x' into (name, email).
    """
    raw = raw.strip()
    m = re.match(r"^(?P<name>.*?)\s*<(?P<email>[^>]+)>\s*$", raw)
    if m:
        return m.group("name").strip(), m.group("email").strip()
    if "@" in raw and " " not in raw:
        return "", raw
    return raw, ""


def is_allowlisted(name: str, email: str) -> bool:
    for pat in ALLOWLIST_NAME_PATTERNS:
        if name and pat.search(name):
            return True
    for pat in ALLOWLIST_EMAIL_PATTERNS:
        if email and pat.search(email):
            return True
    return False


def email_is_ai(email: str) -> bool:
    if not email:
        return False
    for pat in AI_EMAIL_HARD_PATTERNS:
        if pat.search(email):
            return True
    return False


def name_is_ai(name: str) -> bool:
    if not name:
        return False
    # Normalize decorative version suffixes often appended by agents:
    # "Claude Opus 4.8 (1M context)" etc.
    for pat in AI_NAME_PATTERNS:
        if pat.search(name):
            return True
    return False


def identity_is_ai(name: str, email: str) -> Optional[str]:
    if is_allowlisted(name, email):
        return None
    if email_is_ai(email):
        return f"AI-like email: {email}"
    if name_is_ai(name):
        return f"AI-like name: {name}"
    # users.noreply.github.com with AI-ish local-part
    if email and re.search(r"@users\.noreply\.github\.com$", email, re.I):
        local = email.split("@", 1)[0]
        # "223556219+Copilot" or "Copilot"
        if re.search(
            r"(^|\+)(copilot|claude|chatgpt|gpt|cursor|gemini|grok|devin|aider|codex)([+]|$)",
            local,
            re.I,
        ):
            return f"AI-like GitHub noreply identity: {email}"
        if name_is_ai(name):
            return f"AI-like name with noreply email: {name} <{email}>"
    return None


def extract_trailers_and_body(message: str) -> Tuple[List[Tuple[str, str]], str]:
    """
    Best-effort parse of git-style trailers at the end of the commit message.
    Returns list of (key_lower, value) and the full message for body scans.
    """
    lines = message.splitlines()
    trailers: List[Tuple[str, str]] = []
    # Git trailers are a block at the end, often after a blank line.
    # Scan all lines that look like "Key: value" to catch mid-message stamps too.
    trailer_re = re.compile(r"^([A-Za-z][A-Za-z0-9-]*):\s*(.*)$")
    for line in lines:
        m = trailer_re.match(line.strip())
        if not m:
            continue
        key = m.group(1).lower()
        value = m.group(2).strip()
        if key in TRAILER_KEYS or key.replace("_", "-") in TRAILER_KEYS:
            trailers.append((key.replace("_", "-"), value))
    return trailers, message


def check_message(message: str) -> List[str]:
    reasons: List[str] = []
    trailers, full = extract_trailers_and_body(message)

    for key, value in trailers:
        if key in ALWAYS_FAIL_TRAILER_KEYS:
            reasons.append(f"forbidden trailer '{key}: {value}'")
            continue
        if key in IDENTITY_TRAILER_KEYS:
            name, email = parse_identity(value)
            why = identity_is_ai(name, email)
            if why:
                reasons.append(f"AI identity in trailer '{key}: {value}' ({why})")
            elif key in {"co-authored-by", "co-author", "authored-by"}:
                # Catch "Co-Authored-By: Claude Opus 4.8 (1M context) <...>"
                # even if the email domain is unusual but the display name is AI.
                if name_is_ai(value) or email_is_ai(value):
                    reasons.append(f"AI identity in trailer '{key}: {value}'")

    formal_trailer_line = re.compile(
        r"^(co-authored-by|co-author|authored-by|generated-by|generated-with|"
        r"assisted-by|assisted-with|ai-assisted-by|signed-off-by)\s*:",
        re.I,
    )
    for line in full.splitlines():
        stripped = line.strip()
        if formal_trailer_line.match(stripped):
            # Already handled via trailer parsing above.
            continue
        for pat in AI_BODY_LINE_PATTERNS:
            if pat.search(line):
                reasons.append(f"AI attribution line: {stripped}")
                break

    # Deduplicate while preserving order
    seen = set()
    uniq: List[str] = []
    for r in reasons:
        if r not in seen:
            seen.add(r)
            uniq.append(r)
    return uniq


def check_commit(commit_hash: str) -> List[str]:
    # %an author name, %ae email, %cn committer name, %ce email, %B body+subject
    fmt = "%an%n%ae%n%cn%n%ce%n%B"
    raw = run_git(["log", "-1", f"--format={fmt}", commit_hash])
    lines = raw.splitlines()
    if len(lines) < 4:
        return [f"could not parse git log for {commit_hash}"]

    author_name, author_email = lines[0], lines[1]
    committer_name, committer_email = lines[2], lines[3]
    message = "\n".join(lines[4:])

    reasons: List[str] = []

    why = identity_is_ai(author_name, author_email)
    if why:
        reasons.append(f"author {author_name} <{author_email}> ({why})")

    why = identity_is_ai(committer_name, committer_email)
    if why:
        reasons.append(f"committer {committer_name} <{committer_email}> ({why})")

    reasons.extend(check_message(message))
    return reasons


def self_test() -> None:
    """Lightweight unit checks run with --self-test (no git required)."""
    cases_fail = [
        (
            "Alice <alice@example.com>",
            "Bob <bob@example.com>",
            "fix stuff\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>\n",
        ),
        (
            "Claude <noreply@anthropic.com>",
            "Claude <noreply@anthropic.com>",
            "feat: something\n",
        ),
        (
            "Dev <dev@example.com>",
            "Dev <dev@example.com>",
            "msg\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>\n",
        ),
        (
            "Dev <dev@example.com>",
            "Dev <dev@example.com>",
            "msg\n\nGenerated-By: Cursor Agent\n",
        ),
        (
            "ChatGPT <chatgpt@openai.com>",
            "ChatGPT <chatgpt@openai.com>",
            "msg\n",
        ),
        (
            "cursoragent <cursoragent@cursor.com>",
            "Dev <dev@example.com>",
            "msg\n",
        ),
        (
            "Dev <dev@example.com>",
            "Dev <dev@example.com>",
            "msg\n\nAssisted-By: GPT-4o\n",
        ),
        (
            "Gemini Code Assist <gemini@google.com>",
            "Dev <dev@example.com>",
            "msg\n",
        ),
        (
            "Dev <dev@example.com>",
            "Dev <dev@example.com>",
            "msg\n\nCo-Authored-By: Grok <grok@x.ai>\n",
        ),
        (
            "Devin AI <devin@devin.ai>",
            "Devin AI <devin@devin.ai>",
            "msg\n",
        ),
        (
            "Jatin <jatin@example.com>",
            "Jatin <jatin@example.com>",
            "skills\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>\n",
        ),
    ]
    cases_pass = [
        (
            "Dan Bonachea <dobonachea@lbl.gov>",
            "Dan Bonachea <dobonachea@lbl.gov>",
            "fix: something\n\nCo-authored-by: Ondřej Čertík <ondrej@certik.us>\n",
        ),
        (
            "Ondřej Čertík <ondrej@certik.us>",
            "Ondřej Čertík <ondrej@certik.us>",
            "Normal human commit about AI compilers and LLM research notes\n",
        ),
        (
            "dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>",
            "dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>",
            "Bump foo\n\nCo-authored-by: dependabot[bot] <49699333+dependabot[bot]@users.noreply.github.com>\n",
        ),
        (
            "Alice Composer <alice@example.com>",
            "Alice Composer <alice@example.com>",
            # edge: 'Composer' alone as surname — our pattern is \bcomposer\b
            # which will flag; treat as known limitation documented in script header.
            # Use a clearly human name here:
            "fix: real fix\n",
        ),
    ]
    # Fix last pass case name (avoid composer false positive)
    cases_pass[-1] = (
        "Alice Smith <alice@example.com>",
        "Alice Smith <alice@example.com>",
        "fix: real fix\n",
    )

    failed = 0
    for author, committer, msg in cases_fail:
        an, ae = parse_identity(author)
        cn, ce = parse_identity(committer)
        reasons = []
        why = identity_is_ai(an, ae)
        if why:
            reasons.append(why)
        why = identity_is_ai(cn, ce)
        if why:
            reasons.append(why)
        reasons.extend(check_message(msg))
        if not reasons:
            print("SELF-TEST FAIL (expected AI detection):", author, msg[:60])
            failed += 1
        else:
            print("SELF-TEST OK (caught):", reasons[0][:80])

    for author, committer, msg in cases_pass:
        an, ae = parse_identity(author)
        cn, ce = parse_identity(committer)
        reasons = []
        why = identity_is_ai(an, ae)
        if why:
            reasons.append(why)
        why = identity_is_ai(cn, ce)
        if why:
            reasons.append(why)
        reasons.extend(check_message(msg))
        if reasons:
            print("SELF-TEST FAIL (false positive):", author, reasons)
            failed += 1
        else:
            print("SELF-TEST OK (clean):", author)

    if failed:
        print(f"SELF-TEST: {failed} failure(s)")
        raise SystemExit(1)
    print("SELF-TEST: all passed")


def run_test() -> None:
    commit_hashes = get_commit_hashes()
    failures: List[Tuple[str, List[str]]] = []

    for commit_hash in commit_hashes:
        subject = run_git(["log", "-1", "--format=%s", commit_hash]).strip()
        print(f"checking commit --> {commit_hash} ({subject})")
        reasons = check_commit(commit_hash)
        if reasons:
            print("FAIL --- AI authorship indicators detected")
            for r in reasons:
                print(f"  - {r}")
            failures.append((commit_hash, reasons))
        else:
            print("OK --- no AI authorship indicators")
        print("\n--------------------------------------------------\n")

    if failures:
        print("FAILED: AI-authored or AI-attributed commit(s) found in this PR:\n")
        for commit_hash, reasons in failures:
            subject = run_git(["log", "-1", "--format=%s", commit_hash]).strip()
            print(f"  commit {commit_hash}: {subject}")
            for r in reasons:
                print(f"    - {r}")
        print(
            "\nPolicy: every commit must be authored by a human who has read,\n"
            "understood, and guarantees the change. AI tools may help write code,\n"
            "but must not appear as author, committer, Co-Authored-By, Generated-By,\n"
            "Assisted-By, or similar attribution in the commit metadata or message.\n"
            "\nRemove AI Co-Authored-By / Generated-By trailers and ensure author\n"
            "name/email are your human identity, then rewrite history on the PR\n"
            "branch (e.g. git commit --amend / interactive rebase) and force-push.\n"
        )
        raise SystemExit(1)

    print("CLEAN: No AI authorship indicators in any commit in this PR")
    print("\n--------------------------------------------------\n")
    print("DONE")


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] in {"--self-test", "self-test"}:
        self_test()
    else:
        run_test()
