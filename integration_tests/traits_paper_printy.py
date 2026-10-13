"""Check the explicitly imported paper example and independently compiled providers."""

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import re
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=Path)
    parser.add_argument("--work-dir", type=Path, default=Path.cwd())
    parser.add_argument("--fast", action="store_true")
    args = parser.parse_args()
    source = Path(__file__).resolve().parent
    compiler = args.lfortran.resolve()
    work = args.work_dir.resolve() / f"printy-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--verify-all-passes", "--no-color"]
    if args.fast:
        flags.append("--fast")
    commands = []

    def run(name, command, cwd, expected_error=None):
        command = list(map(str, command))
        log = work / f"{name}.log"
        with log.open("w") as output:
            result = subprocess.run(command, cwd=cwd, env=environment, text=True,
                                    stdout=output, stderr=subprocess.STDOUT, timeout=120)
        commands.append({"command": command, "cwd": str(cwd),
                         "returncode": result.returncode, "log": str(log),
                         "expected_error": expected_error})
        (work / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        text = log.read_text()
        if expected_error:
            if (result.returncode == 0 or expected_error not in text or
                    any(marker in text for marker in ("ASR verify", "Traceback", "Assertion"))):
                raise RuntimeError(f"{name} did not produce the expected diagnostic:\n{text}")
        elif result.returncode:
            raise RuntimeError(f"{name} failed ({result.returncode}):\n{text}")
        return text

    paper = source / "traits_paper_printy.f90"
    content = paper.read_bytes()
    addition = b"   use real64_module\n"
    if content.count(addition) != 1 or hashlib.sha256(
            content.replace(addition, b"", 1)).hexdigest() != (
            "416c16b58fb0bd38f891eb78485827543cdfad9415240798bfd37207ee5de484"):
        raise RuntimeError("printy must preserve the original except for one provider USE")
    executable = work / "printy"
    run("paper-compile", [compiler, *flags, paper, "-o", executable], work)
    output = run("paper-run", [executable], work)
    match = re.fullmatch(r"\s*I am\s+([-+0-9.eEdD]+)\s*", output)
    if not match or not math.isclose(
            float(match[1].replace("D", "E").replace("d", "e")), 4.9,
            rel_tol=0, abs_tol=1e-12):
        raise RuntimeError(f"unexpected printy output: {output!r}")
    print("paper printy: exact source plus provider USE; output 4.9", flush=True)

    paper_modules = work / "paper-modules"
    paper_modules.mkdir()
    provider, separator, client = content.partition(b"\nprogram printy\n")
    if not separator:
        raise RuntimeError("the original paper program boundary was changed")
    provider_path = paper_modules / "provider.f90"
    client_path = paper_modules / "client.f90"
    missing_use_path = paper_modules / "missing_use.f90"
    provider_path.write_bytes(provider + b"\n")
    client_path.write_bytes(b"program printy\n" + client)
    missing_use_path.write_bytes(b"program printy\n" + client.replace(addition, b"", 1))
    for name, path in (("provider", provider_path), ("client", client_path)):
        run("paper-" + name, [compiler, *flags, "--separate-compilation", "-c",
                              path, "-J", paper_modules, "-I", paper_modules,
                              "-o", paper_modules / (name + ".o")], paper_modules)
    executable = paper_modules / "printy"
    run("paper-link", [compiler, *flags, paper_modules / "provider.o",
                       paper_modules / "client.o", "-o", executable], paper_modules)
    if run("paper-separate-run", [executable], paper_modules).strip() != output.strip():
        raise RuntimeError("separate compilation changed the paper output")
    run("paper-missing-use", [compiler, *flags, "--semantics-only",
                             "-I", paper_modules, missing_use_path], paper_modules,
        expected_error="no visible intrinsic trait method 'output'")
    print("paper provider/client: separate compilation; unused provider remains invisible",
          flush=True)

    private = work / "separate"
    modules = private / "modules"
    modules.mkdir(parents=True)
    objects = []
    for part in ("contracts", "provider", "facade", ""):
        name = "traits_intrinsic_02" + ("_" + part if part else "")
        obj = private / (name + ".o")
        run(name, [compiler, *flags, "--separate-compilation", "-c",
                   source / (name + ".f90"), "-I", modules, "-J", modules, "-o", obj],
            private)
        objects.append(obj)
    executable = private / "client"
    run("separate-link", [compiler, *flags, *objects, "-o", executable], private)
    run("separate-run", [executable], private)
    print("intrinsic kind identity and generic forwarding through provider/facade modules: pass",
          flush=True)


if __name__ == "__main__":
    main()
