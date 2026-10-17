#!/usr/bin/env python3
"""Ratchet the number of ASR construction calls in the backends.

Backends lower ASR; they must not create it. Everything a backend lowers has
to be built before codegen, where asr_verify checks it (see "Architecture &
Scope" in AGENTS.md). The existing construction sites in src/libasr/codegen/
are being removed, so the per-file counts in the baseline file are upper
bounds that may only go down:

- A file with more calls than its baseline entry fails, and so does a file
  with calls that is not in the baseline (it is allowed none).
- A file with fewer calls than its baseline entry passes, with a notice to
  lower the baseline. Parallel PRs that each remove a site therefore cannot
  turn main red when their baseline edits merge.
- --update rewrites the baseline from the current sources, but only lowers
  counts (and drops files that reached zero). It refuses, and writes nothing,
  if any count would rise or a file would be added. Raising a limit means
  editing the baseline by hand, where reviewers see it.

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
import sys


ROOT = Path(__file__).resolve().parent.parent
BACKEND_DIR = Path("src/libasr/codegen")
SOURCE_SUFFIXES = (".cpp", ".h")
BASELINE = Path("ci/backend_asr_construction_baseline.txt")
SCRIPT = "python3 ci/check_backend_asr_construction.py"
CONSTRUCTION = re.compile(
    r"\b(?:ASR\s*::\s*make_\w+_t|ASRUtils\s*::\s*make_\w+)\s*\(")
RAW_STRING = re.compile(r'(?:u8|[uUL])?R"([^()\\\s]{0,16})\(')

BASELINE_HEADER = """\
# Allowed ASR construction calls per backend source file (upper bounds).
#
# Backends lower ASR; they must not create it. Everything a backend lowers has
# to be built before codegen, where asr_verify checks it (see "Architecture &
# Scope" in AGENTS.md). ci/check_backend_asr_construction.py fails if a file
# has more calls than listed here, or has calls and is not listed. Fewer calls
# pass with a notice: when you remove a construction site, lower its count
# (run the script with --update, which only ever lowers counts).
# Never raise a count or add a file; create the node before codegen instead.
"""


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
    """Return (line, call) for every construction call in a source text."""
    code = strip_comments_and_strings(text)
    return [(code.count("\n", 0, m.start()) + 1,
             re.sub(r"\s+", "", m.group(0)).rstrip("("))
            for m in CONSTRUCTION.finditer(code)]


def backend_sources(root):
    for path in sorted((root / BACKEND_DIR).rglob("*")):
        if path.suffix in SOURCE_SUFFIXES and path.is_file():
            yield path.relative_to(root).as_posix(), path


def current_counts(root):
    counts = {}
    for name, path in backend_sources(root):
        count = len(construction_sites(path.read_text(encoding="utf-8")))
        if count:
            counts[name] = count
    return counts


def read_baseline(path):
    baseline = {}
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        fields = line.split()
        if len(fields) != 2 or not fields[1].isdigit():
            raise SystemExit(f"{path}:{number}: expected '<file> <count>', got {line!r}")
        baseline[fields[0]] = int(fields[1])
    return baseline


def write_baseline(path, counts):
    lines = [BASELINE_HEADER.rstrip("\n"), ""]
    lines += [f"{name} {count}" for name, count in sorted(counts.items())]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def display(path, root):
    path = path.resolve()
    root = root.resolve()
    return path.relative_to(root).as_posix() if path.is_relative_to(root) else str(path)


def check(counts, baseline):
    """Return (errors, notices): counts above and below their baseline."""
    errors = []
    notices = []
    for name in sorted(set(counts) | set(baseline)):
        allowed = baseline.get(name, 0)
        actual = counts.get(name, 0)
        if actual > allowed:
            errors.append(
                f"{name}: {calls(actual)}, baseline allows {allowed}. "
                "Backends must not create ASR nodes; create them before codegen "
                "(in semantics or an ASR pass) so asr_verify checks them. "
                f"Find the new call with {SCRIPT} --list (compare with "
                "git diff origin/main).")
        elif actual < allowed:
            notices.append(
                f"{name}: {calls(actual)}, baseline allows {allowed}; "
                f"lower the baseline with {SCRIPT} --update")
    return errors, notices


def main(argv=None, root=ROOT):
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--update", action="store_true",
                        help="lower the baseline to the current counts "
                             "(refuses to raise a count or add a file)")
    parser.add_argument("--list", action="store_true",
                        help="print every counted call as file:line: call")
    parser.add_argument("--baseline", type=Path, default=None,
                        help=f"baseline file (default: {BASELINE})")
    args = parser.parse_args(argv)
    baseline_path = args.baseline if args.baseline is not None else root / BASELINE

    if args.list:
        for name, path in backend_sources(root):
            for line, call in construction_sites(path.read_text(encoding="utf-8")):
                print(f"{name}:{line}: {call}")

    counts = current_counts(root)
    errors, notices = check(counts, read_baseline(baseline_path))
    shown = display(baseline_path, root)
    if errors:
        if args.update:
            print(f"FAIL --- refusing to update {shown}: --update only lowers "
                  "counts, and these would rise:", file=sys.stderr)
        else:
            print("FAIL --- ASR construction in backends exceeds the baseline "
                  f"({shown}):", file=sys.stderr)
        for error in errors:
            print(f"  {error}", file=sys.stderr)
        return 1
    if args.update:
        write_baseline(baseline_path, counts)
        print(f"wrote {shown} ({calls(sum(counts.values()))} in "
              f"{files(len(counts))})")
        return 0
    for notice in notices:
        print(f"NOTICE --- {notice}")
    print(f"OK --- {calls(sum(counts.values()))} in {files(len(counts))}, "
          f"within the baseline ({shown})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
