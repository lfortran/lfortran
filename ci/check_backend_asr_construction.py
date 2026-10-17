#!/usr/bin/env python3
"""Forbid new ASR construction calls in the backends.

Backends lower ASR; they must not create it. Everything a backend lowers has
to be built before codegen, where asr_verify checks it (see "Architecture &
Scope" in AGENTS.md). The existing construction sites in src/libasr/codegen/
are being removed, so no backend file may gain one:

- Every backend source file must have at most as many calls as the same file
  has at the base commit (--base). A file that does not exist at the base,
  including a renamed one, must have none.
- Fewer calls always pass. The base moves with main, so a removal is locked
  in as soon as it merges; there is no baseline file to update.

The current side is the working tree: tracked and untracked files under
src/libasr/codegen/, except ignored ones. The base side is read from git
without a checkout. The default base is the merge base of HEAD and
origin/main; if the base cannot be determined, the check fails. --list prints
every counted call in the working tree.

Counted calls are ASR::make_<Name>_t(...) and ASRUtils::make_<Name>(...) (any
ASRUtils maker, such as make_<Name>_t_util or make_Cast_t_value), outside
comments and string and character literals. Known limitations: macros that
construct nodes (such as make_ConstantWithType in asr_builder.h, unused in
codegen) and indirect helpers (such as ASRUtils::duplicate_type or
ASRBuilder) are not counted, and calls in #if 0 blocks are counted.
"""

import argparse
from pathlib import Path
import re
import subprocess
import sys


ROOT = Path(__file__).resolve().parent.parent
BACKEND_DIR = "src/libasr/codegen"
SOURCE_SUFFIXES = (".cpp", ".h")
SCRIPT = "python3 ci/check_backend_asr_construction.py"
CONSTRUCTION = re.compile(
    r"\b(?:ASR\s*::\s*make_\w+_t|ASRUtils\s*::\s*make_\w+)\s*\(")
RAW_STRING = re.compile(r'(?:u8|[uUL])?R"([^()\\\s]{0,16})\(')


class BaseError(Exception):
    pass


def calls(count):
    return f"{count} ASR construction call{'' if count == 1 else 's'}"


def files(count):
    return f"{count} backend file{'' if count == 1 else 's'}"


def is_digit_separator(text, i):
    """True if the quote at text[i] separates digits, as in 1'000 or 0xFF'FF."""
    if i == 0 or i + 1 == len(text) or not text[i + 1].isalnum():
        return False
    start = i
    while start > 0 and (text[start - 1].isalnum() or text[start - 1] in "_.'"):
        start -= 1
    return start < i and text[start].isdigit()


def strip_comments_and_strings(text):
    """Blank out comments and string/character literals, keeping newlines."""
    out = []
    i = 0
    n = len(text)

    def blank(segment):
        out.append("".join(c if c == "\n" else " " for c in segment))

    while i < n:
        c = text[i]
        if text.startswith("//", i):
            end = text.find("\n", i)
            end = n if end == -1 else end
            blank(text[i:end])
            i = end
        elif text.startswith("/*", i):
            end = text.find("*/", i + 2)
            end = n if end == -1 else end + 2
            blank(text[i:end])
            i = end
        elif c in "uULR" and (raw := RAW_STRING.match(text, i)) and (
                i == 0 or not (text[i - 1].isalnum() or text[i - 1] == "_")):
            close = ")" + raw.group(1) + '"'
            end = text.find(close, raw.end())
            end = n if end == -1 else end + len(close)
            blank(text[i:end])
            i = end
        elif c == "'" and is_digit_separator(text, i):
            out.append(c)
            i += 1
        elif c in "\"'":
            j = i + 1
            while j < n and text[j] != c and text[j] != "\n":
                j += 2 if text[j] == "\\" else 1
            end = min(j + 1, n)
            blank(text[i:end])
            i = end
        else:
            out.append(c)
            i += 1
    return "".join(out)


def construction_sites(text):
    """Return (line, maker) for every construction call in a source text."""
    code = strip_comments_and_strings(text)
    return [(code.count("\n", 0, m.start()) + 1,
             re.sub(r"\s+", "", m.group(0)).rstrip("("))
            for m in CONSTRUCTION.finditer(code)]


def git(root, *args):
    result = subprocess.run(["git", *args], cwd=root, capture_output=True, text=True)
    if result.returncode != 0:
        raise BaseError(f"git {' '.join(args)} failed: {result.stderr.strip()}")
    return result.stdout


def working_tree_sources(root):
    names = git(root, "ls-files", "-z", "--cached", "--others", "--exclude-standard",
                "--", BACKEND_DIR).split("\0")
    for name in sorted(set(names)):
        path = Path(root) / name
        if name.endswith(SOURCE_SUFFIXES) and path.is_file():
            yield name, path.read_text(encoding="utf-8")


def base_sources(root, commit):
    names = git(root, "ls-tree", "-r", "-z", "--name-only", commit, "--",
                BACKEND_DIR).split("\0")
    for name in sorted(names):
        if name.endswith(SOURCE_SUFFIXES):
            yield name, git(root, "show", f"{commit}:{name}")


def count_calls(sources):
    counts = {}
    for name, text in sources:
        count = len(construction_sites(text))
        if count:
            counts[name] = count
    return counts


def resolve_base(root, base):
    """Return (commit, description) of --base, or of the merge base with origin/main."""
    if base is None:
        try:
            commit = git(root, "merge-base", "HEAD", "origin/main").strip()
        except BaseError as error:
            raise BaseError("cannot determine the base commit: no merge base of HEAD "
                            f"and origin/main ({error}). Fetch origin/main or pass "
                            "--base <rev>.") from None
        return commit, f"merge base {commit[:10]} with origin/main"
    try:
        commit = git(root, "rev-parse", "--verify", "--quiet", f"{base}^{{commit}}").strip()
    except BaseError:
        raise BaseError(f"cannot determine the base commit: {base!r} is not a commit "
                        "here. Fetch it (git fetch --depth=1 origin <sha>) or pass "
                        "another --base.") from None
    return commit, f"base {commit[:10]}"


def check(head, base):
    """Return an error for every file with more calls than at the base."""
    return [f"{name}: {base.get(name, 0)} at the base, {count} now"
            for name, count in sorted(head.items()) if count > base.get(name, 0)]


def main(argv=None, root=ROOT):
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--base", default=None,
                        help="git revision to compare with "
                             "(default: the merge base of HEAD and origin/main)")
    parser.add_argument("--list", action="store_true",
                        help="print every counted call in the working tree "
                             "as file:line: maker")
    args = parser.parse_args(argv)

    try:
        head_sources = list(working_tree_sources(root))
        if args.list:
            for name, text in head_sources:
                for line, maker in construction_sites(text):
                    print(f"{name}:{line}: {maker}")
        commit, described = resolve_base(root, args.base)
        base = count_calls(base_sources(root, commit))
    except BaseError as error:
        print(f"FAIL --- {error}", file=sys.stderr)
        return 1
    head = count_calls(head_sources)

    errors = check(head, base)
    if errors:
        print("FAIL --- new ASR construction calls in backends, compared with the "
              f"{described}:", file=sys.stderr)
        for error in errors:
            print(f"  {error}", file=sys.stderr)
        print("Backends must not create ASR nodes; create them before codegen (in "
              "semantics or an ASR pass) so asr_verify checks them. Find the new "
              f"calls with {SCRIPT} --list and git diff {commit[:10]}.",
              file=sys.stderr)
        return 1
    print(f"OK --- {calls(sum(head.values()))} in {files(len(head))}, "
          f"{sum(base.values())} at the {described}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
