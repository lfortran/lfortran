"""Freeze a numeric generic provider before compiling its clients.

numeric: the provider of ISum/IAverager generic messages over the closed
integer | real(real64) type set is compiled, inspected, archived and
physically hidden first. A contract-only consumer and a late driver must then
reach every type-set member through the declared runtime views, without
provider names, bodies, witnesses or erasures, and the archive must stay
byte-identical.

complex: the same flow for the closed integer | complex(real32) |
complex(real64) type set and an ordinary message with complex results, so
that results of all three return conventions cross the frozen boundary.

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
# The LLVM result types of the member entries and slot calls: complex(real32)
# is <2 x float> or, on Apple arm64, the complex struct type.
I32 = r"i32"
DOUBLE = r"double"
COMPLEX32 = r"(?:<2 x float>|%complex_4|\{ float, float \})"
COMPLEX64 = r"(?:%complex_8|\{ double, double \})"
CASES = {
    "numeric": {
        "prefix": "traits_runtime_numeric_separate_01",
        "generics": NUMERIC_GENERICS,
        "names": NUMERIC_GENERICS + [
            "hiddensimple", "hiddenpairwise", "hiddenscaled", "hiddenaverager",
            "traits_runtime_numeric_separate_01_provider_m"],
        "witnesses": 4,
        "members": {"i32": I32, "double": DOUBLE},
        "contracts": ["isum", "iaverager"],
        "summary": "numeric separate: frozen provider, contract-only consumer, 123 checks passed",
    },
    "complex": {
        "prefix": "traits_runtime_numeric_separate_02",
        "generics": NUMERIC_GENERICS,
        "names": NUMERIC_GENERICS + [
            "hidden_turn", "hidden_mirror", "hiddensimple", "hiddenpairwise",
            "hiddenscaled", "hiddenaverager", "hiddenturn",
            "traits_runtime_numeric_separate_02_provider_m"],
        "witnesses": 5,
        "members": {"i32": I32, "complex32": COMPLEX32, "complex64": COMPLEX64},
        "contracts": ["isum", "iaverager", "iphase"],
        "summary": "complex separate: frozen provider, contract-only consumer, 162 checks passed",
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
# An LLVM name, quoted or not; LLVM spells a quote inside a quoted name \22.
LLVM_NAME = r'(?:"[^"]*"|[-\w.$]+)'
# What an optimizer may infer about how a function uses a pointer parameter,
# such as nocapture or captures(none), readonly, nofree, align 8 or
# dereferenceable(8); unlike byval or sret, none of it changes how the
# pointer is passed.
POINTER_ATTRIBUTE = (r"(?:align \d+|noalias|nocapture|nofree|nonnull|noundef|readnone|readonly|"
                     r"writeonly|(?:captures|dereferenceable(?:_or_null)?|initializes)"
                     r"\((?:[^()]|\([^()]*\))*\))")
# A typed (i8*) or opaque (ptr) pointer parameter, depending on LLVM, and its name.
POINTER_PARAMETER = re.compile(rf"(?:i8\*|ptr)(?: {POINTER_ATTRIBUTE})* %({LLVM_NAME})")
BRACKETS = {"(": ")", "[": "]", "{": "}"}
# The open_simple_sum entry with the parameters LLVM 11 (--fast), 19 and 20
# (nocapture), 21 and 22 (captures(none)) and 23 (nofree) give it, with what
# an optimizer may infer about its result and dictionary pointers, and with
# nested commas and quoted names: check_open_entry accepts each of them.
ENTRY = ("define void @__module_traits_protocol_separate_01_provider_m___trait_erasure_"
         "open_simple_sum_kvCAxFyWx57YNac6PXATJp_entry")
DICTIONARY = "%__dictionary_t_kvCAxFyWx57YNac6PXATJp"
OPEN_ENTRIES = [
    f"{ENTRY}(i8* %_trait_result, %array.1* nocapture readonly %x, i8* {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr nocapture readonly %x, ptr {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr readonly captures(none) %x, ptr {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr nofree readonly captures(none) %x, ptr {DICTIONARY}) {{",
    f"{ENTRY}(ptr noalias writeonly align 8 captures(ret: address, provenance) "
    f"initializes((0, 8), (16, 24)) %_trait_result, {{ i32, [2 x i64] }} %pair, ptr nofree "
    f"noundef nonnull readonly align 8 captures(none) dereferenceable(24) {DICTIONARY}) {{",
    f"{ENTRY}(i8* nocapture writeonly %_trait_result, void (i8*, i32)* %f, "
    f"i8* nocapture readonly dereferenceable_or_null(16) {DICTIONARY}) {{",
    'define void @"m(1)__trait_erasure_open_simple_sum_k_entry"(ptr %_trait_result, '
    'ptr %"x, (y)", ptr %"__dictionary_t,1") {',
]
# A malformed or missing parameter; the result or the dictionary out of place,
# of another type, passed otherwise or misnamed; no or two entries:
# check_open_entry rejects each of them.
WRONG_OPEN_ENTRIES = [
    f"{ENTRY}(ptr %_trait_result, ptr readonly captures(none %x, ptr {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr readonly captures(none) %x) {{",
    f"{ENTRY}(ptr %x, ptr %_trait_result, ptr {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr {DICTIONARY}, ptr %x) {{",
    f"{ENTRY}(i8** %_trait_result, %array.1* %x, i8* {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr %x, i64 {DICTIONARY}) {{",
    f"{ENTRY}(ptr sret(i8) %_trait_result, ptr %x, ptr {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr %x, ptr byval(i8) {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr %x, ptr %x__dictionary_t) {{",
    f'{ENTRY}(ptr %_trait_result, i64 %"ptr {DICTIONARY}") {{',
    f"{ENTRY}_1(ptr %_trait_result, ptr %x, ptr {DICTIONARY}) {{",
    f"{ENTRY}(ptr %_trait_result, ptr %x, ptr {DICTIONARY}) {{\n" * 2,
]


def llvm_name(spelling):
    """A name from its LLVM spelling, quoted or not."""
    if not spelling.startswith('"'):
        return spelling
    return re.sub(r"\\([0-9A-Fa-f]{2})", lambda match: chr(int(match[1], 16)), spelling[1:-1])


def llvm_parameters(llvm, function):
    """The parameters of each function that LLVM IR defines with a name fully
    matching the pattern `function`."""
    return [parameter_list(llvm, head.end() - 1)
            for head in re.finditer(rf"(?m)^define [^@\n]*@({LLVM_NAME})\(", llvm)
            if re.fullmatch(function, llvm_name(head[1]))]


def parameter_list(text, start):
    """The parameters of the LLVM parameter list opened by text[start]. The
    list ends at its balanced ')' on the same line and splits at its
    top-level commas only, so neither attributes such as captures(none) or
    initializes((0, 8), (16, 24)), function or aggregate types nor quoted
    names end or split a parameter."""
    closers, quoted, cuts = [], False, [start]
    for index in range(start, len(text)):
        char = text[index]
        if quoted:
            quoted = char != '"'
        elif char == '"':
            quoted = True
        elif char in BRACKETS:
            closers.append(BRACKETS[char])
        elif char == "," and len(closers) == 1:
            cuts.append(index)
        elif char == "\n":
            break
        elif char in ")]}":
            if closers.pop() != char:
                break
            if not closers:
                cuts.append(index)
                return [text[a + 1:b].strip() for a, b in zip(cuts, cuts[1:])]
    raise AssertionError(("unbalanced parameter list", text[start:index + 1]))


def pointer_named(parameter, name):
    """Whether an LLVM parameter is a pointer whose name fully matches the
    pattern `name`, whatever an optimizer inferred about it."""
    match = POINTER_PARAMETER.fullmatch(parameter)
    return match is not None and re.fullmatch(name, llvm_name(match[1])) is not None


def check_open_entry(llvm, name):
    """The provider defines one open entry of the generic message `name`; it
    takes the caller's storage for the erased result first and the caller's
    dictionary last, each as a pointer."""
    entries = llvm_parameters(llvm, rf".*__trait_erasure_{name}.*_entry")
    assert len(entries) == 1, (name, entries)
    parameters = entries[0]
    assert pointer_named(parameters[0], "_trait_result"), (name, "erased result storage",
                                                           parameters)
    assert pointer_named(parameters[-1], "__dictionary_.+"), (name, "dictionary", parameters)


def check_open_entry_controls():
    """check_open_entry accepts OPEN_ENTRIES and rejects WRONG_OPEN_ENTRIES,
    whichever LLVM compiles the provider."""
    for llvm in OPEN_ENTRIES:
        check_open_entry(llvm, "open_simple_sum")
    for llvm in WRONG_OPEN_ENTRIES:
        try:
            check_open_entry(llvm, "open_simple_sum")
        except AssertionError:
            continue
        raise AssertionError(("accepted a wrong open entry", llvm))


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
        for contract in case["contracts"]:
            assert "__trait_contract_" + contract in semantic.lower(), (part, contract)
        if deferred:
            assert "(TraitDeferredCall" in semantic, "the generic consumer keeps member selection open"
        # Optimized calls may carry fast-math flags before the result type.
        for member, result in case["members"].items():
            assert re.search(rf"call (?:[a-z]+ )*{result} %", llvm), \
                (part, member + " member slot must be loaded and called")
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
        check_open_entry_controls()
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
            check_open_entry(llvm, name)
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
        members = case["members"]
        contracts, _, _ = compile_part("contracts", [])
        provider, provider_source, provider_includes = compile_part("provider", ["contracts"])
        semantic, llvm = inspect("provider", provider_source, provider_includes)
        assert "traits are an experimental LFortran extension" in semantic
        assert semantic.count("(TraitWitness") == case["witnesses"], \
            "one witness per conformance"
        assert semantic.count("(TraitErasure") == len(members) * len(case["generics"]), \
            "one provider entry per generic and member"
        symbols = run("provider-symbols", ["nm", provider])
        for name in case["generics"]:
            entries = []
            for head in re.finditer(rf"(?m)^define [^@\n]*?@[^(\n]*__trait_erasure_"
                                    rf"{name}[^(\n]*_entry\(", llvm):
                entries += [member for member, result in members.items()
                            if re.match(rf"define [^@\n]*?{result} @", head[0])]
            assert sorted(entries) == sorted(members), (name, entries)
            assert len(re.findall(rf"(?m)^\S* *[Tt] \S*__trait_erasure_{name}\S*_entry$",
                                  symbols)) == len(members), (name, symbols)
            assert "__instantiated_" + name not in semantic, "the provider owns its member entries"
        assert len(re.findall(r"(?m)^\S* *[DdSsRr] \S*__trait_witness_", symbols)) == \
            case["witnesses"], symbols

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
              f"driver, {', '.join(members)} member slots, both selection orders")

    if args.case == "protocol":
        protocol_flow()
    else:
        numeric_flow()
    shutil.rmtree(work)


if __name__ == "__main__":
    main()
