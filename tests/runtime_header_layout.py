#!/usr/bin/env python3

import argparse
import os
import pathlib
import shlex
import shutil
import subprocess
import sys
import tempfile


HEADERS = ("lfortran_intrinsics.h", "ISO_Fortran_binding.h")
ERROR_MARKER = "LFORTRAN_RUNTIME_HEADER_LAYOUT_TEST"


def run(command, cwd, env, expected_error=None):
    result = subprocess.run(
        [str(argument) for argument in command],
        cwd=cwd, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        text=True, timeout=60,
    )
    if expected_error is None:
        passed = result.returncode == 0
    else:
        passed = result.returncode != 0 and expected_error in result.stdout
    if not passed:
        raise RuntimeError(
            f"{shlex.join(str(argument) for argument in command)}\n"
            f"expected {'success' if expected_error is None else expected_error}, "
            f"got exit {result.returncode}:\n{result.stdout}"
        )
    return result.stdout.strip()


def check_headers(binary, expected_dir, source_runtime, cwd, env):
    include_dir = pathlib.Path(
        run([binary, "--print-c-include-dir"], cwd, env)
    ).resolve()
    expected_dir = expected_dir.resolve()
    if include_dir != expected_dir:
        raise RuntimeError(
            f"--print-c-include-dir returned {include_dir}, expected {expected_dir}"
        )
    for name in HEADERS:
        header = include_dir / name
        if not header.is_file():
            raise RuntimeError(f"missing runtime header: {header}")
        if header.read_bytes() != (source_runtime / name).read_bytes():
            raise RuntimeError(f"runtime header differs from LFortran source: {header}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=pathlib.Path)
    parser.add_argument("--source-runtime-dir", required=True, type=pathlib.Path)
    parser.add_argument("--fixture", required=True, type=pathlib.Path)
    parser.add_argument("--work-dir", required=True, type=pathlib.Path)
    parser.add_argument(
        "--backends", nargs="+", choices=("c", "cpp"), default=["c"],
        help="include cpp when a working Kokkos installation is available",
    )
    args = parser.parse_args()
    binary = args.lfortran.resolve()
    source_runtime = args.source_runtime_dir.resolve()
    build_runtime = binary.parent.parent / "libasr" / "runtime"
    env = os.environ.copy()
    env.pop("LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR", None)
    env.pop("LFORTRAN_RUNTIME_LIBRARY_DIR", None)

    with tempfile.TemporaryDirectory(
        prefix="runtime-header-layout-", dir=args.work_dir
    ) as directory:
        work_dir = pathlib.Path(directory).resolve()
        check_headers(binary, build_runtime, source_runtime, work_dir, env)
        fixture = work_dir / "check.f90"
        shutil.copy2(args.fixture, fixture)
        layout = work_dir / "relocated" / "src"
        runtime = layout / "libasr" / "runtime"
        runtime.mkdir(parents=True)
        for name in HEADERS:
            shutil.copy2(build_runtime / name, runtime / name)

        for relative in ("bin/lfortran", "lfortran/tests/lfortran"):
            relocated = layout / relative
            relocated.parent.mkdir(parents=True)
            shutil.copy2(binary, relocated)
            check_headers(relocated, runtime, source_runtime, work_dir, env)
            for backend in args.backends:
                command = [
                    relocated, f"--backend={backend}", "--no-color", "-c",
                    fixture, "-o", work_dir / "check.o",
                ]
                run(command, work_dir, env)
                # Prove the compiler reads the copied header, not the old source.
                header = runtime / "lfortran_intrinsics.h"
                header.write_text(f"#error {ERROR_MARKER}\n", encoding="utf-8")
                try:
                    run(command, work_dir, env, expected_error=ERROR_MARKER)
                finally:
                    shutil.copy2(build_runtime / header.name, header)

    print("runtime header layout checks passed for development and CTest paths")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except RuntimeError as error:
        print("error: {}".format(error), file=sys.stderr)
        sys.exit(1)
