"""Check the real C++ driver's standard selection without requiring Kokkos."""

import argparse
import os
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--workdir", required=True)
    args = parser.parse_args()
    workdir = Path(args.workdir).resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    source = workdir / "main.f90"
    source.write_text("end\n")
    failures = []
    cases = [
        ("3", "c++17"),
        ("4", "c++17"),
        ("5", "c++20"),
        ("6", "c++20"),
        ("invalid", "c++17"),
        ("5junk", "c++17"),
        ("999999999999999999999999", "c++17"),
        ("-5", "c++17"),
    ]
    for header in ("Kokkos_Macros.hpp", "KokkosCore_config.h"):
        for macro in ("KOKKOS_VERSION_MAJOR", "KOKKOS_VERSION"):
            for value, expected in cases:
                encoded = value
                if macro == "KOKKOS_VERSION" and value.isdigit() and len(value) == 1:
                    encoded = str(int(value) * 10000 + 202)
                case = workdir / f"{header}-{macro}-{value}"
                include = case / "include"
                include.mkdir(parents=True, exist_ok=True)
                (include / header).write_text(f"#define {macro} {encoded}\n")
                check(args.lfortran, source, case, expected, failures)
    for name, content in (
        ("missing", None),
        ("empty", ""),
        ("unrelated", "#define OTHER_VERSION 50000\n"),
        ("recover", "#define KOKKOS_VERSION_MAJOR bad\n#define KOKKOS_VERSION 50202\n"),
        ("whitespace", "  # define KOKKOS_VERSION_MAJOR 5 // supported\n"),
    ):
        case = workdir / name
        include = case / "include"
        include.mkdir(parents=True, exist_ok=True)
        if content is not None:
            (include / "KokkosCore_config.h").write_text(content)
        expected = "c++20" if name in ("recover", "whitespace") else "c++17"
        check(args.lfortran, source, case, expected, failures)
    assert not failures, "\n".join(failures)
    print("37 real-driver standard-selection cases passed")


def check(lfortran, source, case, expected, failures):
    env = dict(os.environ, LFORTRAN_KOKKOS_DIR=str(case), TMPDIR=str(case))
    result = subprocess.run(
        [lfortran, "--backend=cpp", "--verbose", "-c", str(source),
         "-o", str(case / "main.o")],
        env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        check=False,
    )
    (case / "driver.log").write_text(result.stdout)
    # No Kokkos_Core.hpp is supplied: compilation must fail, but the actual
    # driver command must already have selected the right language standard.
    if result.returncode == 0 or f"-std={expected} " not in result.stdout:
        failures.append(f"{case.name}: expected {expected}\n{result.stdout}")


if __name__ == "__main__":
    main()
