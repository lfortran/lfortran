"""Freeze a numeric generic provider before compiling its clients.

numeric: the provider of ISum/IAverager generic messages over the closed
integer | real(real64) type set is compiled, inspected, archived and
physically hidden first. A contract-only consumer and a late driver must then
reach every type-set member through the declared runtime views, without
provider names, bodies, witnesses or erasures, and the archive must stay
byte-identical.

protocol: the provider of generic messages over the open INumeric protocol
is frozen the same way before any derived or real(real32) conformance
exists. A late module then adds Rational and real(real32) conformances, each
with its own dictionary, and the driver reaches the frozen open entries with
them, never with provider names, bodies or per-type entries.
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

NUMERIC_GENERICS = ["hidden_simple_sum", "hidden_pairwise_sum",
                    "hidden_scaled_sum", "hidden_average"]
PROTOCOL_GENERICS = ["open_simple_sum", "open_reverse_sum", "open_pairwise_sum",
                     "open_average"]
CASES = {
    "numeric": {
        "prefix": "traits_runtime_numeric_separate_01",
        "generics": NUMERIC_GENERICS,
        "names": NUMERIC_GENERICS + [
            "hiddensimple", "hiddenpairwise", "hiddenscaled", "hiddenaverager",
            "traits_runtime_numeric_separate_01_provider_m"],
        "summary": "numeric separate: frozen provider, contract-only consumer, 123 checks passed",
    },
    "protocol": {
        "prefix": "traits_protocol_separate_01",
        "generics": PROTOCOL_GENERICS,
        "names": PROTOCOL_GENERICS + [
            "opensimple", "openreverse", "openpairwise", "openaverager",
            "traits_protocol_separate_01_provider_m"],
        "summary": "traits_protocol_separate_01 checks: 75",
    },
}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--work-dir", required=True)
    parser.add_argument("--fast", action="store_true")
    parser.add_argument("--detect-leaks", action="store_true")
    parser.add_argument("--case", choices=sorted(CASES), default="numeric")
    args = parser.parse_args()
    case = CASES[args.case]
    prefix = case["prefix"]
    provider_names = case["names"]
    compiler = Path(args.lfortran).resolve()
    sources = Path(__file__).resolve().parent
    work = Path(args.work_dir).resolve() / f"{args.case}-runtime-{os.getpid()}-{time.time_ns()}"
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
    archive = work / "providers.a"

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
        original = sources / (prefix + suffix + ".f90")
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
        for name in provider_names:
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

    def freeze(paths, hidden_part):
        for path in paths:
            frozen[path] = digest(path)
        hidden = {}
        for path in [*(work / hidden_part).rglob("*.mod"), *(work / hidden_part).glob("*.f90")]:
            hidden[str(path.relative_to(work))] = digest(path)
            moved = path.with_name(path.name + ".hidden")
            path.rename(moved)
            frozen[moved] = digest(moved)
        assert any(name.endswith(".mod") for name in hidden), "the provider module must be hidden"
        assert any(name.endswith(".f90") for name in hidden), "the provider source must be hidden"
        (work / "hidden-provider-files.json").write_text(json.dumps(hidden, indent=2) + "\n")
        check_frozen("before_clients")

    def protocol_flow():
        # No numeric conformance is visible to the provider: it sees only the
        # open contract, so each generic message has one erased entry taking
        # the caller's INumeric dictionary.
        contracts, _, _ = compile_part("contracts", [])
        intrinsics, _, _ = compile_part("intrinsics", ["contracts"])
        provider, provider_source, provider_includes = compile_part("provider", ["contracts"])
        semantic, llvm = inspect("provider", provider_source, provider_includes)
        assert "traits are an experimental LFortran extension" in semantic
        assert semantic.count("(TraitWitness") == 4, "three ISum and one IAverager conformance"
        assert semantic.count("(TraitErasure") == 4, "one open entry per generic message"
        assert len(re.findall(r"(?m)\(TraitDictionary$", semantic)) == 0, \
            "the provider declares no conformance dictionary"
        assert "(TraitDictionaryContract" in semantic, "the contract lays out INumeric dictionaries"
        symbols = run("provider-symbols", ["nm", provider])
        for name in case["generics"]:
            assert "__instantiated_" + name not in semantic, "no specialized provider body"
            entries = re.findall(rf"(?m)^define [^@\n]*?@[^(\n]*__trait_erasure_"
                                 rf"{name}[^(\n]*_entry\(([^)]*)\)", llvm)
            assert len(entries) == 1, (name, entries)
            # Typed (i8*) or opaque (ptr) pointers, depending on LLVM.
            parameters = [p.strip() for p in entries[0].split(",")]
            assert re.match(r"(i8\*|ptr) ", parameters[0]), (name, "erased result storage",
                                                             parameters)
            assert re.match(r"(i8\*|ptr) %__dictionary_", parameters[-1]), (name, "dictionary",
                                                                            parameters)
            assert len(re.findall(rf"(?m)^\S* *[Tt] \S*__trait_erasure_{name}\S*_entry$",
                                  symbols)) == 1, (name, symbols)
        assert not re.search(r"(?m)^define [^@\n]*?(i32|i64|float|double) @[^(\n]*__trait_erasure_",
                             llvm), "no per-type provider entry"
        assert len(re.findall(r"(?m)^\S* *[DdSsRr] \S*__trait_witness_", symbols)) == 4, symbols
        assert not re.search(r"__trait_dictionary_", symbols), symbols

        run("archive", ["ar", "rcs", archive, provider])
        paths = [archive, provider, contracts, intrinsics]
        for part in ["contracts", "intrinsics"]:
            paths += [*(work / part).rglob("*.mod"), *(work / part).glob("*.f90")]
        freeze(paths, "provider")

        # Rational and real(real32) join INumeric after the freeze.
        late, late_source, late_includes = compile_part("late", ["contracts"])
        check_frozen("after_late")
        semantic, llvm = inspect("late", late_source, late_includes)
        lowered = semantic.lower() + llvm.lower()
        for name in provider_names:
            assert name not in lowered, ("late", name)
        assert len(re.findall(r"(?m)\(TraitDictionary$", semantic)) == 2, \
            "Rational and real(real32) dictionaries live in the late module"
        late_symbols = run("late-symbols", ["nm", late])
        assert len(re.findall(r"(?m)^\S* *[DdSsRr] \S*__trait_dictionary_\S*$",
                              late_symbols)) == 2, late_symbols
        assert not re.search(r"provider_m|open_", late_symbols, re.IGNORECASE), late_symbols
        for path in [late, *(work / "late" / "modules").glob("*.mod")]:
            frozen[path] = digest(path)

        driver, driver_source, driver_includes = compile_part(
            "driver", ["contracts", "intrinsics", "late"])
        check_frozen("after_driver")
        semantic, llvm = inspect("driver", driver_source, driver_includes)
        lowered = semantic.lower() + llvm.lower()
        for name in provider_names:
            assert name not in lowered, ("driver", name)
        for node in ["(TraitErasure", "(TraitWitness"]:
            assert node not in semantic, ("driver", node)
        assert "(TraitFunctionCall" in semantic and "(TraitDictionaryValue" in semantic
        driver_symbols = run("driver-symbols", ["nm", driver])
        assert not re.search(r"provider_m|open_|__trait_erasure_", driver_symbols,
                             re.IGNORECASE), driver_symbols
        dictionaries = set(re.findall(r"(?m)^ +U (\S*__trait_dictionary_\S*)$", driver_symbols))
        assert len(dictionaries) == 4, ("integer, real64, real32 and Rational", dictionaries)

        executable = work / "program"
        run("link", [compiler, *flags, driver, late, intrinsics, archive, contracts,
                     "-o", executable])
        check_frozen("after_link")
        output = run("run", [executable])
        assert case["summary"] in output, output
        if args.detect_leaks:
            assert "NO LEAKS FOUND" in output, output
        check_frozen("after_run")
        print(f"{prefix}: frozen open provider {frozen[archive]}, late Rational and "
              f"real(real32) dictionaries, 75 checks")

    def numeric_flow():
        contracts, _, _ = compile_part("contracts", [])
        provider, provider_source, provider_includes = compile_part("provider", ["contracts"])
        semantic, llvm = inspect("provider", provider_source, provider_includes)
        assert "traits are an experimental LFortran extension" in semantic
        assert semantic.count("(TraitWitness") == 4, "three ISum and one IAverager conformance"
        assert semantic.count("(TraitErasure") == 8, "one provider entry per generic and member"
        symbols = run("provider-symbols", ["nm", provider])
        for name in case["generics"]:
            entries = re.findall(rf"(?m)^define [^@\n]*?(i32|double) @[^(\n]*__trait_erasure_"
                                 rf"{name}[^(\n]*_entry\(", llvm)
            assert sorted(entries) == ["double", "i32"], (name, entries)
            assert len(re.findall(rf"(?m)^\S* *[Tt] \S*__trait_erasure_{name}\S*_entry$",
                                  symbols)) == 2, (name, symbols)
            assert "__instantiated_" + name not in semantic, "the provider owns its member entries"
        assert len(re.findall(r"(?m)^\S* *[DdSsRr] \S*__trait_witness_", symbols)) == 4, symbols

        run("archive", ["ar", "rcs", archive, provider])
        freeze([archive, provider, contracts, *(work / "contracts").rglob("*.mod"),
                *(work / "contracts").glob("*.f90")], "provider")

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
            assert case["summary"] in output, output
            if args.detect_leaks:
                assert "NO LEAKS FOUND" in output, output
            check_frozen("after_" + order)
        print(f"{prefix}: frozen provider {frozen[archive]}, contract-only consumer and "
              f"driver, integer and real64 member slots, both selection orders")

    if args.case == "protocol":
        protocol_flow()
    else:
        numeric_flow()
    shutil.rmtree(work)


if __name__ == "__main__":
    main()
