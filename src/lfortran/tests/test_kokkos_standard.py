"""Check standard selection and compile/run against Kokkos when installed."""

import argparse
import os
from pathlib import Path
import re
import shlex
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
    check_installed_kokkos(args.lfortran, workdir / "installed")


def check_installed_kokkos(lfortran, workdir):
    prefix = os.environ.get("LFORTRAN_KOKKOS_DIR") or os.environ.get("CONDA_PREFIX")
    if not prefix or not (Path(prefix) / "include/Kokkos_Core.hpp").is_file():
        assert not os.environ.get("LFORTRAN_KOKKOS_DIR"), "invalid Kokkos prefix"
        print("Installed-Kokkos execution not tested: no package found")
        return
    prefix = Path(prefix).resolve()
    headers = "\n".join(
        header.read_text()
        for header in (prefix / "include/KokkosCore_config.h",
                       prefix / "include/Kokkos_Macros.hpp")
        if header.is_file()
    )
    version = re.search(r"^\s*#\s*define\s+KOKKOS_VERSION\s+(\d+)\b", headers, re.M)
    assert version, "installed Kokkos has no numeric version"
    standard = "c++20" if int(version[1]) >= 50000 else "c++17"
    flags = ["--backend=cpp", "-v"]
    if re.search(r"^\s*#\s*define\s+KOKKOS_ENABLE_OPENMP\b", headers, re.M):
        flags.append("--openmp")
    workdir.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, LFORTRAN_KOKKOS_DIR=str(prefix), TMPDIR=str(workdir))
    env.setdefault("OMP_NUM_THREADS", "3")
    runtime = Path(lfortran).resolve().parent.parent / "libasr/runtime"
    if (runtime / "lfortran_intrinsics.h").is_file():
        env.setdefault("LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR", str(runtime))
    source = Path(__file__).resolve().parents[3] / "integration_tests/kokkos_cpp_standard.f90"

    def run(name, command):
        result = subprocess.run(command, cwd=workdir, env=env, text=True,
                                capture_output=True, check=False)
        log = workdir / f"{name}.log"
        log.write_text(shlex.join(command) + "\n" + result.stdout + result.stderr)
        assert result.returncode == 0, f"{log}:\n{result.stdout}{result.stderr}"
        return result.stdout

    print(f"Installed Kokkos {version[1]} at {prefix}: expecting {standard}")
    for mode, options in (("normal", []), ("fast", ["--fast"]), ("f23", ["--std=f23"])):
        exe = workdir / mode
        output = run(f"{mode}-compile", [lfortran, *flags, *options, str(source),
                                       "-o", str(exe)])
        assert f"-std={standard} " in output, output
        assert run(f"{mode}-run", [str(exe)]).split() == ["42"]
        obj = workdir / f"{mode}.o"
        output = run(f"{mode}-object", [lfortran, *flags, *options, "-c", str(source),
                                      "-o", str(obj)])
        assert f"-std={standard} " in output, output
        linked = workdir / f"{mode}-linked"
        run(f"{mode}-link", [lfortran, *flags, str(obj), "-o", str(linked)])
        assert run(f"{mode}-linked-run", [str(linked)]).split() == ["42"]
    print("Installed-Kokkos executable and object/link/run passed in normal/fast/f23")


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
