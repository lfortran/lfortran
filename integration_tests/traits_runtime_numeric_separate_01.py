"""Freeze a finite-numeric generic provider before compiling its clients.

The provider of ISum/IAverager generic messages over integer | real(real64)
is compiled, inspected, archived and physically hidden first. A
contract-only consumer and a late driver must then reach every type-set
member through the declared runtime views, without provider names, bodies,
witnesses or erasures, and the archive must stay byte-identical.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time

PREFIX = "traits_runtime_numeric_separate_01"
PROVIDER_GENERICS = ["hidden_simple_sum", "hidden_pairwise_sum",
                     "hidden_scaled_sum", "hidden_average"]
PROVIDER_NAMES = PROVIDER_GENERICS + [
    "hiddensimple", "hiddenpairwise", "hiddenscaled", "hiddenaverager",
    "traits_runtime_numeric_separate_01_provider_m"]
EXPECTED_SUMMARY = "numeric separate: frozen provider, contract-only consumer, 123 checks passed"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--work-dir", required=True)
    parser.add_argument("--fast", action="store_true")
    parser.add_argument("--detect-leaks", action="store_true")
    args = parser.parse_args()
    compiler = Path(args.lfortran).resolve()
    sources = Path(__file__).resolve().parent
    work = Path(args.work_dir).resolve() / f"numeric-runtime-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes", "--separate-compilation"]
    if args.fast:
        flags.append("--fast")
    if args.detect_leaks:
        flags.append("--detect-leaks")
    records = []
    frozen = {}
    archive_checks = {}

    def digest(path):
        return hashlib.sha256(Path(path).read_bytes()).hexdigest()

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
        original = sources / (PREFIX + suffix + ".f90")
        source = directory / original.name
        shutil.copyfile(original, source)
        assert digest(source) == digest(original)
        includes = []
        for imported in imports:
            includes += ["-I", work / imported / "modules"]
        obj = directory / (part + ".o")
        run(part, [compiler, *flags, *includes, "-J", modules, "-c", source, "-o", obj],
            directory)
        return obj, source, includes

    def inspect(part, source, includes):
        directory = work / part
        inspection = directory / "inspection"
        inspection.mkdir()
        semantic = run(part + "-asr", [compiler, *flags, *includes, "-J", inspection,
                                       "--show-asr", source], directory)
        check_frozen("after_" + part + "_asr")
        llvm = run(part + "-llvm", [compiler, *flags, *includes, "-J", inspection,
                                    "--show-llvm", source], directory)
        check_frozen("after_" + part + "_llvm")
        return semantic, llvm

    def check_frozen(stage):
        if not frozen:
            return
        archive_checks[stage] = digest(archive)
        assert all(digest(path) == value for path, value in frozen.items()), stage
        assert not list((work / "provider").rglob("*.mod")), stage
        assert not list((work / "provider").glob("*.f90")), stage
        (work / "archive.json").write_text(json.dumps(archive_checks, indent=2) + "\n")

    def check_client(part, semantic, llvm, obj, deferred):
        lowered = semantic.lower() + llvm.lower()
        for name in PROVIDER_NAMES:
            assert name not in lowered, (part, name)
        for node in ["(TraitErasure", "(TraitWitness", "(TraitImplementation", "(TraitPack"]:
            assert node not in semantic, (part, node)
        assert "(TraitFunctionCall" in semantic, part
        assert "__trait_contract_isum" in semantic.lower(), part
        assert "__trait_contract_iaverager" in semantic.lower(), part
        if deferred:
            assert "(TraitDeferredCall" in semantic, "the generic consumer keeps member selection open"
        # Optimized calls may carry fast-math flags before the result type.
        assert re.search(r"call (?:[a-z]+ )*i32 %", llvm), (part, "integer member slot must be loaded and called")
        assert re.search(r"call (?:[a-z]+ )*double %", llvm), (part, "real64 member slot must be loaded and called")
        symbols = run(part + "-symbols", ["nm", obj])
        assert not re.search(r"provider_m|hidden", symbols, re.IGNORECASE), symbols

    contracts, _, _ = compile_part("contracts", [])
    provider, provider_source, provider_includes = compile_part("provider", ["contracts"])
    semantic, llvm = inspect("provider", provider_source, provider_includes)
    assert "traits are an experimental LFortran extension" in semantic
    assert semantic.count("(TraitWitness") == 4, "three ISum and one IAverager conformance"
    assert semantic.count("(TraitErasure") == 8, "one provider entry per generic and member"
    symbols = run("provider-symbols", ["nm", provider])
    for name in PROVIDER_GENERICS:
        entries = re.findall(rf"(?m)^define [^@\n]*?(i32|double) @[^(\n]*__trait_erasure_"
                             rf"{name}[^(\n]*_entry\(", llvm)
        assert sorted(entries) == ["double", "i32"], (name, entries)
        assert len(re.findall(rf"(?m)^\S* *[Tt] \S*__trait_erasure_{name}\S*_entry$",
                              symbols)) == 2, (name, symbols)
        assert "__instantiated_" + name not in semantic, "the provider owns its member entries"
    assert len(re.findall(r"(?m)^\S* *[DdSsRr] \S*__trait_witness_", symbols)) == 4, symbols

    archive = work / "providers.a"
    run("archive", ["ar", "rcs", archive, provider])
    frozen[archive] = digest(archive)
    frozen[provider] = digest(provider)
    for path in [contracts, *(work / "contracts").rglob("*.mod"),
                 *(work / "contracts").glob("*.f90")]:
        frozen[path] = digest(path)
    hidden = {}
    for path in [*(work / "provider").rglob("*.mod"), *(work / "provider").glob("*.f90")]:
        hidden[str(path.relative_to(work))] = digest(path)
        moved = path.with_name(path.name + ".hidden")
        path.rename(moved)
        frozen[moved] = digest(moved)
    assert any(name.endswith(".mod") for name in hidden), "the provider module must be hidden"
    assert any(name.endswith(".f90") for name in hidden), "the provider source must be hidden"
    (work / "hidden-provider-files.json").write_text(json.dumps(hidden, indent=2) + "\n")
    check_frozen("before_clients")

    consumer, consumer_source, consumer_includes = compile_part("consumer", ["contracts"])
    check_frozen("after_consumer")
    semantic, llvm = inspect("consumer", consumer_source, consumer_includes)
    check_client("consumer", semantic, llvm, consumer, deferred=True)
    for path in [consumer, *(work / "consumer" / "modules").glob("*.mod")]:
        frozen[path] = digest(path)

    driver, driver_source, driver_includes = compile_part("driver", ["contracts", "consumer"])
    check_frozen("after_driver")
    semantic, llvm = inspect("driver", driver_source, driver_includes)
    check_client("driver", semantic, llvm, driver, deferred=False)

    executable = work / "program"
    run("link", [compiler, *flags, driver, consumer, archive, contracts, "-o", executable])
    check_frozen("after_link")
    for order, arguments in [("first-a", []), ("first-b", ["select-second-first"])]:
        output = run("run-" + order, [executable, *arguments])
        assert EXPECTED_SUMMARY in output, output
        if args.detect_leaks:
            assert "NO LEAKS FOUND" in output, output
        check_frozen("after_" + order)
    print(f"{PREFIX}: frozen provider {frozen[archive]}, contract-only consumer and "
          f"driver, integer and real64 member slots, both selection orders")
    shutil.rmtree(work)


if __name__ == "__main__":
    main()
