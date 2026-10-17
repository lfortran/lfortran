#!/usr/bin/env python3
"""Ratchet the number of ASR construction calls in the backends.

Backends lower ASR; they must not create it. Everything a backend lowers has
to exist before codegen, where asr_verify checks it (see "Architecture &
Scope" in AGENTS.md). The existing construction sites in src/libasr/codegen/
are being removed, so this check only lets their number go down: a file fails
if it has more calls than its entry in the baseline file (a file missing from
the baseline is allowed none), and also if it has fewer, so that the PR which
removes a site lowers the baseline too. Run with --update to rewrite the
baseline from the current sources.

Counted calls are ASR::make_<Name>_t(...) and ASRUtils::make_<Name>_util(...),
which includes ASRUtils::make_<Name>_t_util(...), outside comments and string
literals.
"""

import argparse
from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parent.parent
BACKEND_DIR = Path("src/libasr/codegen")
SOURCE_SUFFIXES = (".cpp", ".h")
BASELINE = ROOT / "ci" / "backend_asr_construction_baseline.txt"
CONSTRUCTION = re.compile(r"\b(?:ASR::make_\w+_t|ASRUtils::make_\w+_util)\s*\(")
RAW_STRING = re.compile(r'(?:u8|[uUL])?R"([^()\\\s]{0,16})\(')

BASELINE_HEADER = """\
# Allowed ASR construction calls per backend source file.
#
# Backends lower ASR; they must not create it. Everything a backend lowers has
# to exist before codegen, where asr_verify checks it (see "Architecture &
# Scope" in AGENTS.md). ci/check_backend_asr_construction.py fails if a file
# has more calls than listed here, or fewer: when you remove a construction
# site, lower its count in the same PR (or run the script with --update).
# Never raise a count or add a file; create the node before codegen instead.
"""


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
    return [(code.count("\n", 0, m.start()) + 1, m.group(0).rstrip("( \t"))
            for m in CONSTRUCTION.finditer(code)]


def current_counts(root):
    counts = {}
    for path in sorted((root / BACKEND_DIR).rglob("*")):
        if path.suffix in SOURCE_SUFFIXES and path.is_file():
            count = len(construction_sites(path.read_text(encoding="utf-8")))
            if count:
                counts[path.relative_to(root).as_posix()] = count
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


def display(path):
    path = path.resolve()
    return path.relative_to(ROOT).as_posix() if path.is_relative_to(ROOT) else str(path)


def check(counts, baseline):
    errors = []
    for name in sorted(set(counts) | set(baseline)):
        allowed = baseline.get(name, 0)
        actual = counts.get(name, 0)
        if actual > allowed:
            errors.append(
                f"{name}: {actual} ASR construction calls, baseline allows {allowed}. "
                "Backends must not create ASR nodes; create them before codegen "
                "(in semantics or an ASR pass) so asr_verify checks them.")
        elif actual < allowed:
            errors.append(
                f"{name}: {actual} ASR construction calls, baseline allows {allowed}. "
                "Thank you for removing some; lower the baseline in the same PR "
                "(python3 ci/check_backend_asr_construction.py --update).")
    return errors


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--update", action="store_true",
                        help="rewrite the baseline from the current counts")
    parser.add_argument("--list", action="store_true",
                        help="print every counted call as file:line: call")
    parser.add_argument("--baseline", type=Path, default=BASELINE,
                        help="baseline file (default: %(default)s)")
    args = parser.parse_args()

    if args.list:
        for path in sorted((ROOT / BACKEND_DIR).rglob("*")):
            if path.suffix in SOURCE_SUFFIXES and path.is_file():
                name = path.relative_to(ROOT).as_posix()
                for line, call in construction_sites(path.read_text(encoding="utf-8")):
                    print(f"{name}:{line}: {call}")

    counts = current_counts(ROOT)
    if args.update:
        write_baseline(args.baseline, counts)
        print(f"wrote {display(args.baseline)} ({sum(counts.values())} calls in {len(counts)} files)")
        return 0

    errors = check(counts, read_baseline(args.baseline))
    if errors:
        print("FAIL --- ASR construction in backends does not match the baseline "
              f"({display(args.baseline)}):", file=sys.stderr)
        for error in errors:
            print(f"  {error}", file=sys.stderr)
        return 1
    print(f"OK --- {sum(counts.values())} ASR construction calls in "
          f"{len(counts)} backend files, matching the baseline")
    return 0


if __name__ == "__main__":
    sys.exit(main())
