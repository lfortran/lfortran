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
    mode.add_argument("--generic", action="store_true")
    parser.add_argument("--detect-leaks", action="store_true")
    args = parser.parse_args()
    compiler = Path(args.lfortran).resolve()
    sources = Path(__file__).resolve().parent
    prefix = ("traits_runtime_generic_01" if args.generic else
              "traits_runtime_inspection_separate_01" if args.inspection else
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

    def compile_part(part, imports, source_name=None):
        directory = work / part
        directory.mkdir()
        modules = directory / "modules"
        modules.mkdir()
        suffix = "" if part == "driver" else "_" + part
        original = sources / (source_name or prefix + suffix + ".f90")
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
    if args.generic:
        inspection = work / "provider" / "inspection"
        inspection.mkdir()
        provider_source = work / "provider" / (prefix + "_provider.f90")
        inspect_flags = [*flags, "-I", work / "contracts" / "modules", "-J", inspection]
        semantic = run("provider-asr", [compiler, *inspect_flags, "--show-asr", provider_source],
                       work / "provider")
        llvm = run("provider-llvm", [compiler, *inspect_flags, "--show-llvm", provider_source],
                   work / "provider")
        symbols = run("provider-symbols", ["nm", objects[0]])
        assert "traits are an experimental LFortran extension" in semantic
        assert semantic.count("(TraitErasure") == 2
        for name in ["offset_apply", "scaled_apply"]:
            entry = rf"__trait_erasure_{name}.*_entry"
            assert len(re.findall(rf"(?m)^.*\b[Tt] .*{entry}$", symbols)) == 1, symbols
            assert re.search(rf"define i32 @.*{entry}\(", llvm), llvm
            assert "__instantiated_" + name not in semantic
        assert re.search(r"call i32 %", llvm), "provider operations must use supplied evidence"
        for late_type in ["latevalue", "paddedvalue", "alternatevalue"]:
            assert late_type not in semantic.lower() and late_type not in llvm.lower()
    archive = work / "providers.a"
    run("archive", ["ar", "rcs", archive, *objects])
    frozen = digest(archive)
    archive_checks = {"before_clients": frozen}
    frozen_files = {str(obj): digest(obj) for obj in objects}
    if args.combinations or args.inspection or args.generic:
        for path in [contracts, *(work / "contracts").rglob("*.mod")]:
            frozen_files[str(path)] = digest(path)
    if args.generic:
        for path in (work / "contracts").glob("*.f90"):
            frozen_files[str(path)] = digest(path)
    if not args.slots:
        hidden = {}
        for module in (work / "provider").rglob("*.mod"):
            hidden[str(module.relative_to(work))] = digest(module)
            module = module.rename(module.with_suffix(".mod.hidden"))
            frozen_files[str(module)] = digest(module)
        assert hidden, "the private provider must have compiled its module"
        (work / "hidden-provider-modules.json").write_text(json.dumps(hidden, indent=2) + "\n")
        if args.combinations or args.inspection or args.generic:
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
        if args.generic:
            assert not list((work / "provider").rglob("*.mod"))
            assert not list((work / "provider").glob("*.f90"))
        (work / "archive.json").write_text(json.dumps(archive_checks, indent=2) + "\n")
        (work / "provider-files.json").write_text(json.dumps(frozen_files, indent=2) + "\n")

    check_archive("before_clients")
    if args.generic:
        def inspect_client(part, source, includes, deferred=False):
            directory = work / part
            inspection = directory / "inspection"
            inspection.mkdir()
            semantic = run(part + "-asr", [compiler, *flags, *includes, "-J", inspection,
                                           "--show-asr", source], directory)
            check_archive("after_" + part + "_asr")
            llvm = run(part + "-llvm", [compiler, *flags, *includes, "-J", inspection,
                                       "--show-llvm", source], directory)
            check_archive("after_" + part + "_llvm")
            for forbidden in ["offset_apply", "scaled_apply", "offsetalgorithm",
                              "scaledalgorithm", "traits_runtime_generic_01_provider_m"]:
                assert forbidden not in semantic.lower()
                assert forbidden not in llvm.lower()
            assert "TraitErasure" not in semantic
            assert "TraitFunctionCall" in semantic
            if deferred:
                assert "TraitDeferredPack" in semantic and "TraitPack" not in semantic
            else:
                assert "TraitPack" in semantic
                assert re.search(r"call i32 %", llvm), "the client must dynamically select the provider"

        def late_part(part, imports, source_name=None):
            result = compile_part(part, imports, source_name)
            check_archive("after_" + part)
            return result

        def execute(name, objects):
            executable = work / ("program-" + name)
            run("link-" + name, [compiler, *flags, *objects, archive, contracts, "-o", executable])
            check_archive("after_link_" + name)
            for order, arguments in [("first-a", []), ("first-b", ["select-second-first"])]:
                output = run(name + "-" + order, [executable, *arguments])
                if args.detect_leaks:
                    assert "NO LEAKS FOUND" in output, output
                check_archive("after_" + name + "_" + order)

        # The checked forwarding template, too, must predate all client types.
        assert not (work / "late_client").exists() and not (work / "matrix_client").exists()
        forwarding, source, includes = late_part("forwarding", ["contracts"])
        inspect_client("forwarding", source, includes, deferred=True)
        for path in [forwarding, *(work / "forwarding" / "modules").glob("*.mod")]:
            frozen_files[str(path)] = digest(path)
        check_archive("before_late_types")
        late, _, _ = late_part("late_client", ["contracts"])
        consumer, source, includes = late_part("consumer", ["contracts", "late_client"])
        inspect_client("consumer", source, includes)
        explicit, _, _ = late_part("explicit_syntax", ["contracts", "late_client"])
        driver, _, _ = late_part("driver", ["contracts", "late_client", "consumer"])
        execute("original", [driver, consumer, late, explicit])
        matrix, _, _ = late_part("matrix_client", ["contracts"])
        consumer, source, includes = late_part("matrix_consumer", ["contracts", "matrix_client"])
        inspect_client("matrix_consumer", source, includes)
        driver, _, _ = late_part("matrix", ["contracts", "matrix_client", "matrix_consumer"])
        execute("matrix", [driver, consumer, matrix])
        driver, source, includes = late_part("forward", ["contracts", "matrix_client", "forwarding"])
        inspect_client("forward", source, includes)
        execute("forward", [driver, forwarding, matrix])
        print(f"{prefix}: frozen open-world 47/174, layout/nominal 84/248, "
              f"generic forwarding, both selection orders, unchanged archive {frozen}")
        return
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
