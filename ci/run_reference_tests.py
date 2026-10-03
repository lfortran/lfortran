#!/usr/bin/env python3

import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys


ROOT_DIR = Path(__file__).resolve().parent.parent


def prepare_workspace(source_dir, build_dir):
    workspace = build_dir / "reference-tests"
    if workspace.exists():
        shutil.rmtree(workspace)
    workspace.mkdir(parents=True)
    files = subprocess.check_output(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard",
         "--", "tests", "integration_tests"],
        cwd=source_dir,
    )
    for filename in set(os.fsdecode(files).split("\0")) - {""}:
        if filename.startswith("tests/reference/"):
            continue
        source = source_dir / filename
        if not source.exists():
            continue  # Tracked inputs may have been deleted by the current change.
        destination = workspace / filename
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)
    return workspace


def main():
    parser = argparse.ArgumentParser(
        description="Run LLVM 11 reference tests in an isolated build workspace.")
    parser.add_argument("build_dir", type=Path)
    args, test_args = parser.parse_known_args()
    build_dir = args.build_dir.resolve()
    compiler_dir = build_dir / "src" / "bin"
    compiler = compiler_dir / ("lfortran.exe" if os.name == "nt" else "lfortran")
    if not compiler.is_file():
        parser.error(f"{compiler} is missing; build this configuration first")
    version = subprocess.check_output([str(compiler), "--version"], text=True)
    if not re.search(r"^LLVM:\s+11\.", version, re.MULTILINE):
        parser.error("reference tests require an LFortran build using LLVM 11")
    workspace = prepare_workspace(ROOT_DIR, build_dir)
    print(f"Reference test workspace: {workspace}", flush=True)
    return subprocess.call(
        [sys.executable, str(ROOT_DIR / "run_tests.py"),
         "--compiler-dir", str(compiler_dir), *test_args],
        cwd=workspace,
    )


if __name__ == "__main__":
    sys.exit(main())
