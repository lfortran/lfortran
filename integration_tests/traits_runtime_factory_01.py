"""Freeze providers before compiling contract-only factory consumers."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--work-dir", required=True)
    parser.add_argument("--fast", action="store_true")
    parser.add_argument("--slots", action="store_true")
    parser.add_argument("--detect-leaks", action="store_true")
    args = parser.parse_args()
    compiler = Path(args.lfortran).resolve()
    sources = Path(__file__).resolve().parent
    prefix = "traits_runtime_07" if args.slots else "traits_runtime_factory_01"
    work = Path(args.work_dir).resolve() / f"trait-factory-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes", "--separate-compilation"]
    if args.fast:
        flags.append("--fast")
    if args.detect_leaks:
        flags.append("--detect-leaks")
    records = []

    def digest(path):
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def run(name, command, cwd=work):
        command = list(map(str, command))
        print("+", " ".join(command), flush=True)
        log = work / (name + ".log")
        with log.open("w") as output:
            process = subprocess.run(command, cwd=cwd, env=environment,
                                     stdout=output, stderr=subprocess.STDOUT)
        records.append({"name": name, "command": command, "cwd": str(cwd),
                        "status": process.returncode, "log": str(log)})
        (work / "commands.json").write_text(json.dumps(records, indent=2) + "\n")
        text = log.read_text()
        assert process.returncode == 0, (command, process.returncode, text)
        return text

    def compile_part(part, imports):
        directory = work / part
        directory.mkdir()
        modules = directory / "modules"
        modules.mkdir()
        suffix = "" if part == "driver" else "_" + part
        original = sources / (prefix + suffix + ".f90")
        source = directory / original.name
        shutil.copyfile(original, source)
        assert digest(source) == digest(original)
        includes = []
        for imported in imports:
            includes += ["-I", work / imported / "modules"]
        obj = directory / (part + ".o")
        run(part, [compiler, *flags, *includes, "-J", modules, "-c", source, "-o", obj], directory)
        return obj, source, includes

    contracts, _, _ = compile_part("contracts", [])
    providers = ["impl_a", "impl_b"] if args.slots else ["provider"]
    objects = [compile_part(part, ["contracts"])[0] for part in providers]
    archive = work / "providers.a"
    run("archive", ["ar", "rcs", archive, *objects])
    frozen = digest(archive)
    archive_checks = {"before_clients": frozen}
    if not args.slots:
        hidden = {}
        for module in (work / "provider").rglob("*.mod"):
            hidden[str(module.relative_to(work))] = digest(module)
            module.rename(module.with_suffix(".mod.hidden"))
        assert hidden, "the private provider must have compiled its module"
        (work / "hidden-provider-modules.json").write_text(json.dumps(hidden, indent=2) + "\n")

    def check_archive(stage):
        archive_checks[stage] = digest(archive)
        assert archive_checks[stage] == frozen
        (work / "archive.json").write_text(json.dumps(archive_checks, indent=2) + "\n")

    consumer, source, includes = compile_part("consumer", ["contracts"])
    check_archive("after_consumer")
    semantic = run("consumer-asr", [compiler, "--no-color", *includes, "--show-asr", source],
                   work / "consumer")
    llvm = run("consumer-llvm", [compiler, *flags, *includes, "--show-llvm", source],
               work / "consumer")
    assert "TraitFunctionCall" in semantic
    assert "TraitWitness" not in semantic and "TraitImplementation" not in semantic
    assert "TraitPack" not in semantic
    assert re.search(r"call i32 %", llvm), "dispatch must use the carried witness"
    if not args.slots:
        assert "TraitBorrow" in semantic and "TraitAssignment" in semantic
        assert "ReturnVar" in semantic and "make_value" in semantic
        assert re.search(r"call (?:i8\*|ptr) %", llvm), "copy must use the private lifecycle"
        assert "hiddena" not in semantic.lower() and "hiddenb" not in semantic.lower()
    driver_imports = ["contracts", "consumer", *providers] if args.slots else ["contracts", "consumer"]
    driver, source, includes = compile_part("driver", driver_imports)
    check_archive("after_driver")
    if not args.slots:
        for part in ["contracts", "consumer", "driver"]:
            assert not list((work / part).rglob("*provider*.mod"))
        semantic = run("driver-asr", [compiler, "--no-color", *includes, "--show-asr", source],
                       work / "driver")
        assert "TraitImplementation" not in semantic and "TraitWitness" not in semantic
        assert "hiddena" not in semantic.lower() and "hiddenb" not in semantic.lower()
    executable = work / "program"
    run("link", [compiler, *flags, driver, consumer, archive, contracts, "-o", executable])
    check_archive("after_link")
    for name, arguments in [("first-a", []), ("first-b", ["select-second-first"])]:
        output = run(name, [executable, *arguments])
        if not args.slots:
            assert re.search(r"factory results:\s+16\s+16\s+272\s+464\s+10", output), output
        if args.detect_leaks:
            assert "NO LEAKS FOUND" in output, output
        check_archive("after_" + name)
    print(f"{prefix}: fresh clients, both selection orders, unchanged archive {frozen}")


if __name__ == "__main__":
    main()
