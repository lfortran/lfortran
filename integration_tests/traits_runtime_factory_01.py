"""Freeze providers before compiling contract-only factory/projection consumers."""

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
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--slots", action="store_true")
    mode.add_argument("--projections", action="store_true")
    mode.add_argument("--combinations", action="store_true")
    mode.add_argument("--inspection", action="store_true")
    parser.add_argument("--detect-leaks", action="store_true")
    args = parser.parse_args()
    compiler = Path(args.lfortran).resolve()
    sources = Path(__file__).resolve().parent
    prefix = ("traits_runtime_inspection_separate_01" if args.inspection else
              "traits_runtime_combination_01" if args.combinations else
              "traits_runtime_05" if args.projections else
              "traits_runtime_07" if args.slots else "traits_runtime_factory_01")
    work = Path(args.work_dir).resolve() / f"trait-factory-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes", "--separate-compilation"]
    if args.fast:
        flags.append("--fast")
    if args.detect_leaks:
        flags.append("--detect-leaks")
    records = []
    source_hashes = {}

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
        source_hashes[str(original)] = digest(source)
        (work / "sources.json").write_text(json.dumps(source_hashes, indent=2) + "\n")
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
    frozen_files = {str(obj): digest(obj) for obj in objects}
    if args.combinations or args.inspection:
        for path in [contracts, *(work / "contracts").rglob("*.mod")]:
            frozen_files[str(path)] = digest(path)
    if not args.slots:
        hidden = {}
        for module in (work / "provider").rglob("*.mod"):
            hidden[str(module.relative_to(work))] = digest(module)
            module = module.rename(module.with_suffix(".mod.hidden"))
            frozen_files[str(module)] = digest(module)
        assert hidden, "the private provider must have compiled its module"
        (work / "hidden-provider-modules.json").write_text(json.dumps(hidden, indent=2) + "\n")
        if args.combinations or args.inspection:
            hidden_sources = {}
            for source in (work / "provider").glob("*.f90"):
                hidden_sources[str(source.relative_to(work))] = digest(source)
                source = source.rename(source.with_suffix(".f90.hidden"))
                frozen_files[str(source)] = digest(source)
            assert hidden_sources, "provider source must be withheld before late clients"
            (work / "hidden-provider-sources.json").write_text(json.dumps(hidden_sources, indent=2) + "\n")

    def check_archive(stage):
        archive_checks[stage] = digest(archive)
        assert archive_checks[stage] == frozen
        assert all(digest(Path(path)) == value for path, value in frozen_files.items())
        (work / "archive.json").write_text(json.dumps(archive_checks, indent=2) + "\n")
        (work / "provider-files.json").write_text(json.dumps(frozen_files, indent=2) + "\n")

    check_archive("before_clients")
    extra_objects = []
    consumer_imports = ["contracts"]
    if args.combinations:
        extra_objects.append(compile_part("reordered", ["contracts"])[0])
        check_archive("after_reordered")
        consumer_imports.append("reordered")
    consumer, source, includes = compile_part("consumer", consumer_imports)
    check_archive("after_consumer")
    semantic = run("consumer-asr", [compiler, "--no-color", *includes, "--show-asr", source],
                   work / "consumer")
    llvm = run("consumer-llvm", [compiler, *flags, *includes, "--show-llvm", source],
               work / "consumer")
    assert "TraitFunctionCall" in semantic
    assert "TraitWitness" not in semantic and "TraitImplementation" not in semantic
    assert "TraitPack" not in semantic
    assert re.search(r"call i32 %", llvm), "dispatch must use the carried witness"
    if args.projections or args.combinations or args.inspection:
        assert "TraitProject" in semantic and "TraitAssociate" in semantic
        assert "TraitBorrow" in semantic
        if args.combinations:
            assert "warning: traits are an experimental LFortran extension" in (work / "consumer.log").read_text()
            assert "TraitAssignment" in semantic and "TraitAllocate" in semantic
            assert re.search(r"call (?:i8\*|ptr) %", llvm), "copy must use the retained concrete lifecycle"
            assert "hiddenbox" not in semantic.lower()
        if args.inspection:
            assert "TraitInspect" in semantic and "SelectType" in semantic
            assert "Association" in semantic and "ClassToStruct" in semantic
            assert "ClassToClass" in semantic and "hiddenleaf" not in semantic.lower()
    elif not args.slots:
        assert "TraitBorrow" in semantic and "TraitAssignment" in semantic
        assert "ReturnVar" in semantic and "make_value" in semantic
        assert re.search(r"call (?:i8\*|ptr) %", llvm), "copy must use the private lifecycle"
        assert "hiddena" not in semantic.lower() and "hiddenb" not in semantic.lower()
    if args.projections or args.combinations or args.inspection:
        extra_objects.append(compile_part("alternative_impl", ["contracts"])[0])
        extra_objects.append(compile_part("alternative", ["contracts", "alternative_impl"])[0])
        check_archive("after_alternative")
    driver_imports = (["contracts", "consumer", "reordered"] if args.combinations else
                      ["contracts"] if args.projections else
                      ["contracts", "consumer", *providers] if args.slots else
                      ["contracts", "consumer"])
    driver, source, includes = compile_part("driver", driver_imports)
    check_archive("after_driver")
    if not args.slots:
        for part in ["contracts", "consumer", "driver"]:
            assert not list((work / part).rglob("*provider*.mod"))
        semantic = run("driver-asr", [compiler, "--no-color", *includes, "--show-asr", source],
                       work / "driver")
        assert "TraitImplementation" not in semantic and "TraitWitness" not in semantic
        assert "hiddena" not in semantic.lower() and "hiddenb" not in semantic.lower()
        if args.combinations:
            assert "hiddenbox" not in semantic.lower()
        if args.inspection:
            assert "hiddenleaf" not in semantic.lower()
    executable = work / "program"
    run("link", [compiler, *flags, driver, consumer, *extra_objects, archive, contracts, "-o", executable])
    check_archive("after_link")
    for name, arguments in [("first-a", []), ("first-b", ["select-second-first"])]:
        output = run(name, [executable, *arguments])
        if args.inspection:
            assert "concrete inspection: frozen providers, selected methods, identity and finalization" in output
        elif args.combinations:
            assert "late combinations: selected procedures, addresses, slots and finalization" in output
        elif not args.slots and not args.projections:
            assert re.search(r"factory results:\s+16\s+16\s+272\s+464\s+10", output), output
        if args.detect_leaks:
            assert "NO LEAKS FOUND" in output, output
        check_archive("after_" + name)
    print(f"{prefix}: fresh clients, both selection orders, unchanged archive {frozen}")


if __name__ == "__main__":
    main()
