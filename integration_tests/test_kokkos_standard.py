"""Compile and execute the real Kokkos regression in the C++ backend suite."""

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
    lfortran = str(Path(args.lfortran).resolve())
    workdir = Path(args.workdir).resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    prefix = os.environ.get("LFORTRAN_KOKKOS_DIR") or os.environ.get("CONDA_PREFIX")
    assert prefix, "the C++ backend suite requires an installed Kokkos"
    prefixes = [Path(prefix).resolve()]
    # Quick also has its packaged Kokkos alongside the C++ suite's source build.
    packaged = os.environ.get("CONDA_PREFIX")
    if packaged:
        packaged = Path(packaged).resolve()
        if packaged not in prefixes and (packaged / "include/Kokkos_Core.hpp").is_file():
            prefixes.append(packaged)
    for index, prefix in enumerate(prefixes):
        check_installed_kokkos(lfortran, prefix, workdir / str(index))


def check_installed_kokkos(lfortran, prefix, workdir):
    workdir.mkdir(parents=True, exist_ok=True)
    assert (prefix / "include/Kokkos_Core.hpp").is_file(), f"invalid Kokkos prefix: {prefix}"
    headers = "\n".join(
        header.read_text()
        for header in (prefix / "include/KokkosCore_config.h",
                       prefix / "include/Kokkos_Macros.hpp")
        if header.is_file()
    )
    version = re.search(r"^\s*#\s*define\s+KOKKOS_VERSION\s+(\d+)\b", headers, re.M)
    assert version, "installed Kokkos has no numeric version"
    # C++20 is the default; Kokkos 5 and later no longer build as C++17.
    standards = [("c++20", [])]
    if int(version[1]) < 50000:
        standards.append(("c++17", ["--cpp-std=c++17"]))
    flags = ["--backend=cpp", "-v"]
    if re.search(r"^\s*#\s*define\s+KOKKOS_ENABLE_OPENMP\b", headers, re.M):
        flags.append("--openmp")
    env = dict(os.environ, LFORTRAN_KOKKOS_DIR=str(prefix), TMPDIR=str(workdir))
    env.setdefault("OMP_NUM_THREADS", "3")
    # Kokkos warns on stdout when OpenMP binding is unspecified.
    env.setdefault("OMP_PROC_BIND", "false")
    runtime = Path(lfortran).parent.parent / "libasr/runtime"
    if (runtime / "lfortran_intrinsics.h").is_file():
        env.setdefault("LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR", str(runtime))
    source = Path(__file__).resolve().with_name("kokkos_cpp_standard.f90")

    def run(name, command):
        result = subprocess.run(command, cwd=workdir, env=env, text=True,
                                capture_output=True, check=False)
        log = workdir / f"{name}.log"
        log.write_text(shlex.join(command) + "\n" + result.stdout + result.stderr)
        assert result.returncode == 0, f"{log}:\n{result.stdout}{result.stderr}"
        return result.stdout

    print(f"Installed Kokkos {version[1]} at {prefix}", flush=True)
    for (standard, std_options), (mode, options) in (
            (s, m) for s in standards
            for m in (("normal", []), ("fast", ["--fast"]), ("f23", ["--std=f23"]))):
        options = [*std_options, *options]
        mode = f"{standard}-{mode}"
        exe = workdir / mode
        output = run(f"{mode}-compile", [lfortran, *flags, *options, str(source),
                                       "-o", str(exe)])
        assert f"-std={standard} " in output, output
        output = run(f"{mode}-run", [str(exe)])
        assert output.split() == ["42"], f"{workdir / f'{mode}-run.log'}:\n{output}"
        obj = workdir / f"{mode}.o"
        output = run(f"{mode}-object", [lfortran, *flags, *options, "-c", str(source),
                                      "-o", str(obj)])
        assert f"-std={standard} " in output, output
        linked = workdir / f"{mode}-linked"
        run(f"{mode}-link", [lfortran, *flags, str(obj), "-o", str(linked)])
        output = run(f"{mode}-linked-run", [str(linked)])
        assert output.split() == ["42"], f"{workdir / f'{mode}-linked-run.log'}:\n{output}"
    print("Installed-Kokkos executable and object/link/run passed in normal/fast/f23 for "
          + ", ".join(standard for standard, _ in standards))


if __name__ == "__main__":
    main()
