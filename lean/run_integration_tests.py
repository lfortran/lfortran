#!/usr/bin/env python3
"""Run integration tests by translating them to Lean and executing them.

`lfortran --backend=lean` writes the program as Lean templates over
`K : Kinds` with a `main` that instantiates them at `LFortran.machine`. This script
compiles the small runtime once, then for every selected test translates the
Fortran and runs `lean --run`. A test passes when the program's exit code
agrees with the RUN() entry: zero, unless the entry is marked FAIL.

By default only tests labeled `lean` are run. `--all` tries every integration
test and prints why the others are rejected. `--apply` adds the `lean` label
to tests that pass.
"""

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CMAKE = ROOT / "integration_tests" / "CMakeLists.txt"
TESTS = ROOT / "integration_tests"
TOOLCHAIN = (ROOT / "lean" / "lean-toolchain").read_text().strip()

KEYWORDS = {
    "NAME", "FILE", "LABELS", "EXTRA_ARGS", "EXTRAFILES", "GFORTRAN_ARGS",
    "INCLUDE_PATH", "COPY_TO_BIN", "NUM_IMAGES", "SHARED_LIBRARY_FILES",
    "STATIC_LIBRARY_FILES", "FAIL", "NO_FAST", "NO_FAST_MATH",
    "NO_DETECT_LEAKS", "NO_STD_F23", "NO_LLVM_GOC", "HOST_STATIC_LIBRARY",
}


def parse_runs(text):
    runs = []
    i = 0
    while True:
        j = text.find("RUN(", i)
        if j < 0:
            break
        depth = 0
        k = j + 3
        while k < len(text):
            if text[k] == "(":
                depth += 1
            elif text[k] == ")":
                depth -= 1
                if depth == 0:
                    k += 1
                    break
            k += 1
        body = text[j + 4:k - 1]
        tokens = body.split()
        name = None
        filename = None
        labels = []
        extra = []
        fail = False
        mode = None
        for tok in tokens:
            if tok in KEYWORDS:
                mode = tok
                if tok == "FAIL":
                    fail = True
                    mode = None
                continue
            if mode == "NAME":
                name = tok
                mode = None
            elif mode == "FILE":
                filename = tok
                mode = None
            elif mode == "LABELS":
                labels.append(tok)
            elif mode == "EXTRA_ARGS":
                extra.append(tok)
            elif mode is None:
                continue
        if name:
            runs.append({
                "name": name,
                "file": filename or name,
                "labels": labels,
                "extra": extra,
                "fail": fail,
                "start": j,
                "end": k,
            })
        i = k
    return runs


def lean_compile_error(text):
    for line in text.splitlines():
        s = line.strip()
        if s.startswith("error:") or s.startswith("error(") or "PANIC" in s:
            return True
        if "unknown identifier" in s or "failed to compile" in s:
            return True
    return False


def first_reason(stderr):
    for line in stderr.splitlines():
        line = line.strip()
        if "not supported by the Lean backend" in line:
            return line
        if line.startswith("error:") or line.startswith("semantic error:"):
            return line
    for line in stderr.splitlines():
        if line.strip():
            return line.strip()[:200]
    return "failed"


def run_one(task):
    name = task["name"]
    src = TESTS / (task["file"] + ".f90")
    if not src.is_file():
        src = TESTS / (task["file"] + ".f")
    if not src.is_file():
        return name, "missing", "source file is missing", task["fail"]
    lean_file = Path(task["workdir"]) / (name + ".lean")
    cmd = [task["lfortran"], "--backend=lean", str(src), "-o", str(lean_file)]
    cmd.extend(task["extra"])
    try:
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=task["timeout"])
    except subprocess.TimeoutExpired:
        return name, "timeout", "lfortran timed out", task["fail"]
    if proc.returncode != 0 or not lean_file.is_file():
        return name, "translate", first_reason(proc.stderr + proc.stdout), task["fail"]
    env = os.environ.copy()
    env["LEAN_PATH"] = task["lean_path"]
    try:
        run = subprocess.run(
            ["elan", "run", task["toolchain"], "lean", "--run", str(lean_file)],
            capture_output=True, text=True, timeout=task["timeout"], env=env)
    except subprocess.TimeoutExpired:
        return name, "timeout", "lean timed out", task["fail"]
    output = run.stderr + "\n" + run.stdout
    if lean_compile_error(output):
        reason = first_reason(output)
        return name, "lean", reason, task["fail"]
    ok = (run.returncode != 0) if task["fail"] else (run.returncode == 0)
    if ok:
        return name, "pass", "", task["fail"]
    return name, "run", f"exit {run.returncode}", task["fail"]


RUNTIME = ["Arith", "Arr", "Runtime"]


def prepare_runtime(toolchain):
    """Compile the mathlib-free runtime (`LFortran.Runtime` and what it
    imports) once, in dependency order."""
    cache = Path(tempfile.mkdtemp(prefix="lfortran-lean-"))
    dest = cache / "LFortran"
    dest.mkdir()
    env = os.environ.copy()
    env["LEAN_PATH"] = str(cache)
    for mod in RUNTIME:
        shutil.copy(ROOT / "lean" / "LFortran" / (mod + ".lean"), dest / (mod + ".lean"))
        subprocess.run(
            ["elan", "run", toolchain, "lean", "-o", str(dest / (mod + ".olean")),
             "LFortran/" + mod + ".lean"],
            cwd=cache, check=True, env=env)
    return cache


def apply_labels(text, runs, passed, attempted):
    """Add `lean` to tests that passed and drop it from attempted tests that
    did not. Tests that this run did not attempt keep their label."""
    passed = set(passed)
    attempted = set(attempted)
    added = 0
    removed = 0
    pieces = []
    cursor = 0
    for run in runs:
        pieces.append(text[cursor:run["start"]])
        block = text[run["start"]:run["end"]]
        if run["name"] in passed and "lean" not in run["labels"]:
            if "LABELS " in block:
                block = block.replace("LABELS ", "LABELS lean ", 1)
                added += 1
        elif (run["name"] in attempted and run["name"] not in passed
                and "lean" in run["labels"]):
            block = re.sub(r"(?<!\S)lean(?!\S)", "", block, count=1)
            removed += 1
        pieces.append(block)
        cursor = run["end"]
    pieces.append(text[cursor:])
    return "".join(pieces), added, removed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--all", action="store_true",
                        help="try every integration test, not only lean-labeled ones")
    parser.add_argument("--apply", action="store_true",
                        help="add the lean label to tests that pass and "
                             "remove it from attempted tests that fail")
    parser.add_argument("--jobs", type=int, default=os.cpu_count() or 4)
    parser.add_argument("--timeout", type=int, default=30)
    parser.add_argument("--only", action="append", default=[],
                        help="run only these test names")
    parser.add_argument("--limit", type=int, default=0)
    args = parser.parse_args()

    lfortran = shutil.which("lfortran")
    if not lfortran:
        sys.exit("lfortran is not on PATH; run via `pixi run -e llvm11`")
    text = CMAKE.read_text()
    runs = parse_runs(text)
    if args.only:
        wanted = set(args.only)
        runs = [r for r in runs if r["name"] in wanted]
    elif not args.all and not args.apply:
        runs = [r for r in runs if "lean" in r["labels"]]
    if args.limit:
        runs = runs[:args.limit]
    if not runs:
        print("no tests selected")
        return

    print(f"preparing Lean runtime ({TOOLCHAIN})")
    cache = prepare_runtime(TOOLCHAIN)
    work = Path(tempfile.mkdtemp(prefix="lfortran-lean-tests-"))
    tasks = []
    for run in runs:
        tasks.append({
            "name": run["name"],
            "file": run["file"],
            "extra": run["extra"],
            "fail": run["fail"],
            "lfortran": lfortran,
            "toolchain": TOOLCHAIN,
            "lean_path": str(cache),
            "workdir": str(work),
            "timeout": args.timeout,
        })
    print(f"running {len(tasks)} tests with {args.jobs} jobs")
    counts = {}
    reasons = {}
    passed = []
    done = 0
    with ProcessPoolExecutor(max_workers=args.jobs) as pool:
        futures = [pool.submit(run_one, task) for task in tasks]
        for fut in as_completed(futures):
            name, kind, reason, _fail = fut.result()
            counts[kind] = counts.get(kind, 0) + 1
            done += 1
            if kind == "pass":
                passed.append(name)
            else:
                reasons.setdefault(reason, []).append(name)
                print(f"  {kind}: {name}: {reason}", flush=True)
            if done % 200 == 0 or done == len(tasks):
                print(f"  {done}/{len(tasks)}  pass {counts.get('pass', 0)}",
                      flush=True)
    print("results:", ", ".join(f"{k} {v}" for k, v in sorted(counts.items())),
          flush=True)
    if args.all or args.apply:
        ranked = sorted(reasons.items(), key=lambda kv: -len(kv[1]))
        print("top rejections:")
        for reason, names in ranked[:25]:
            print(f"  {len(names):5d}  {reason}")
    if args.apply:
        # Re-parse so offsets match the file we write. Tests this run did
        # not attempt (a filtered run) keep whatever label they had.
        full = parse_runs(text)
        updated, added, removed = apply_labels(
            text, full, passed, [r["name"] for r in runs])
        CMAKE.write_text(updated)
        print(f"labeled {len(passed)} passing tests in {CMAKE} "
              f"(added {added}, removed {removed})")
    shutil.rmtree(cache, ignore_errors=True)
    shutil.rmtree(work, ignore_errors=True)
    if counts.get("pass", 0) != len(tasks) and not args.all and not args.apply:
        sys.exit(1)


if __name__ == "__main__":
    main()
