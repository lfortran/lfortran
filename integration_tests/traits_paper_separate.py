"""Compile one paper program against frozen libraries it cannot recompile.

The fixture is cut at the program-unit boundaries recorded in its split map
after the fixture, its paper original, every piece and the rejoined text are
verified. Library units are compiled in dependency order, each into its own
module directory and seeing only the module directories of the units it uses
directly or indirectly. Their objects are archived; every piece, object,
module file and the archive is hashed, and the library sources are hidden.
The main program is then compiled against the module directories alone,
inspected, linked with the archive and run through the input and output
checks of traits_paper_functional.py. Every frozen file must stay
byte-identical after each later step.

A library is either a generic package, which defines only generic types, so
the client may create and own a genuinely new instance of it, or a provider,
which owns its traits, conformances and runtime entries; the client must not
create those again. The structure each example requires of the main
program's ASR and LLVM and of the objects is asserted from the inspection
logs (STRUCTURE).
"""

import argparse
from collections import namedtuple
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import traits_paper_functional as paper  # noqa: E402

# The split maps beside the fixtures are pinned like the fixtures themselves.
SPLIT_HASHES = {
    "static": "80415aaa04a5a4dc36d1ad0464bc628a29459d2d3b0afa46e855ffd3870aef62",
    "overloading": "169bf3b284a278ff59bfe5bbcb5e0cadb1fa725f20935978f4b5bb09e1b15c42",
}
# Every library unit of a split has a role; see the module docstring.
ROLES = {
    "static": {"interfaces": "provider", "simple_library": "provider",
               "pairwise_library": "package", "averager_library": "package"},
    "overloading": {"basic_interfaces": "provider", "my_type": "provider",
                    "real_type": "provider"},
}
# The units, symbols, symbol references and imports of an ASR dump, and the
# definitions of LLVM IR. Compiler-made names, such as those of generic type
# instances, may contain any character but whitespace and ':' (LLVM quotes
# them), so the patterns do not assume Fortran identifiers.
ASR_UNIT = re.compile(r"^ {12}(\w+):$", re.MULTILINE)
ASR_SYMBOL = re.compile(r"\s*,?\s*([^\s:]+):\s*\(")
ASR_REFERENCE = re.compile(r"\s*(?:\(\)|\d+ (\S+))")
ASR_EXTERNAL = re.compile(r"\(ExternalSymbol\s+\d+\s+\S+\s+\d+ \S+\s+(\S+)\s+\[[^\]]*\]\s+(\S+)")
LLVM_NAME = r'(?:"(?:[^"\\]|\\.)*"|[\w.$]+)'
LLVM_DEFINITION = re.compile(rf"^(?:define\b[^@]*@({LLVM_NAME})\(|@({LLVM_NAME}) = (?!external\b))",
                             re.MULTILINE)
HEADER = re.compile(r"(module|program)\s+(\w+)\s*", re.IGNORECASE)
END = re.compile(r"\s*end\s+(module|program)\s+(\w+)\s*", re.IGNORECASE)
USE = re.compile(r"\s*use\s*(?:,\s*(intrinsic|non_intrinsic)\s*::\s*|::\s*|\s+)(\w+)",
                 re.IGNORECASE)
FORTRAN_SUFFIXES = {".f", ".for", ".fpp", ".f90", ".f95", ".f03", ".f08"}
Unit = namedtuple("Unit", "name kind piece imports file record")


def digest(data):
    return hashlib.sha256(data).hexdigest()


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def asr_units(dump):
    """The text of each top-level unit of an ASR dump, by name."""
    starts = list(ASR_UNIT.finditer(dump))
    return {match[1]: dump[match.start():starts[i + 1].start() if i + 1 < len(starts)
                          else len(dump)] for i, match in enumerate(starts)}


def llvm_name(text):
    """A global's name from its LLVM IR spelling, quoted or not."""
    if not text.startswith('"'):
        return text
    return re.sub(r"\\([0-9A-Fa-f]{2})", lambda match: chr(int(match[1], 16)), text[1:-1])


def nm_references(listing):
    """The undefined symbols an nm listing of one object references."""
    return [fields[-1] for fields in map(str.split, listing.splitlines())
            if len(fields) >= 2 and fields[-2] == "U"]


def nm_definitions(listing):
    """The external symbols each object of an nm listing defines, by object."""
    objects, current = {}, None
    for line in listing.splitlines():
        header = re.fullmatch(r"(?:.*\()?([\w.-]+\.o)\)?:", line.strip())
        if header:
            current = objects.setdefault(header[1], [])
            continue
        fields = line.split()
        if len(fields) >= 2 and fields[-2].isupper() and fields[-2] != "U":
            if current is None:
                current = objects.setdefault("", [])
            current.append(fields[-1])
    return objects


def node_end(text, start):
    """The end of the ASR node whose opening parenthesis is at `start`."""
    depth, quoted, escaped = 0, False, False
    for index in range(start, len(text)):
        char = text[index]
        if escaped:
            escaped = False
        elif quoted:
            escaped, quoted = char == "\\", char != '"'
        elif char == '"':
            quoted = True
        elif char in "()":
            depth += 1 if char == "(" else -1
            if depth == 0:
                return index + 1
    raise RuntimeError(f"the ASR node at offset {start} is not closed")


def asr_symbols(node):
    """The node of each symbol of the outermost symbol table of an ASR node."""
    table = re.search(r"\(SymbolTable\s+\d+\s+\{", node)
    require(table is not None, "the ASR node has no symbol table")
    symbols, position = {}, table.end()
    while entry := ASR_SYMBOL.match(node, position):
        start = entry.end() - 1
        position = node_end(node, start)
        symbols[entry[1]] = node[start:position]
    return symbols


def generic_instance(node):
    """The definition and the type argument declarations that a generic type
    instance records, and the symbols of the instance's own scope, from
    which those references are made, or None for any other node."""
    start = node.find("(GenericTypeInstance")
    if start < 0:
        return None
    block = node[start:node_end(node, start)]
    head = re.match(r"\(GenericTypeInstance\s+\d+ (\S+)\s+\[", block)
    require(head is not None, "a generic type instance names no definition")
    arguments, position = [], head.end()
    # Each argument is (type declaration [evidence]).
    while argument := re.compile(r"\s*,?\s*\(").match(block, position):
        declaration = ASR_REFERENCE.match(block, node_end(block, argument.end()))
        require(declaration is not None, "a generic type argument has no declaration")
        arguments.append(declaration[1])
        position = node_end(block, argument.end() - 1)
    return head[1], arguments, asr_symbols(node)


def spelling(scopes, name):
    """`module::name` for an import, `definition{arguments}` for an instance,
    looking `name` up in `scopes` innermost first."""
    node = next((scope[name] for scope in scopes if name in scope), "")
    external = ASR_EXTERNAL.match(node)
    if external:
        return f"{external[1]}::{external[2]}"
    instance = generic_instance(node)
    if instance:
        definition, arguments, own = instance
        inner = [own, scopes[-1]]
        return (spelling(inner, definition) + "{"
                + ", ".join(spelling(inner, argument) for argument in arguments) + "}")
    return str(name)


def implemented_type(symbols, witness):
    """The type declaration of the conformance whose evidence a witness is."""
    fields = re.compile(r"\s*\S+\s+\d+ \S+\s+\d+ (\S+)").match(
        witness, node_end(witness, witness.index("(SymbolTable")))
    implementation = symbols.get(fields[1], "") if fields else ""
    head = re.match(r"\(TraitImplementation\s+\d+\s+\S+\s+", implementation)
    if head is None:
        return None
    declaration = ASR_REFERENCE.match(implementation, node_end(implementation, head.end()))
    return declaration[1] if declaration else None


def simple_sum_evidence(name):
    """Whether a linker symbol is the witness, an entry or the lifecycle of
    SimpleSum's conformance, which simple_library owns."""
    return ("implements_simplesum" in name or "trait_erasure_sum_" in name
            or re.search(r"trait_lifecycle\w*\D9_simplesum$", name) is not None)


STATIC_INSTANCES = sorted([
    "pairwise_library::pairwisesum{simple_library::simplesum}",
    "averager_library::averager{simple_library::simplesum}",
    "averager_library::averager{pairwise_library::pairwisesum{simple_library::simplesum}}",
])


def check_static(logs):
    """Static dispatch: main owns the three instances of the generic types and
    their runtime evidence; the packages hold their definitions only, and
    simple_library alone defines the evidence of its own conformance."""
    units = asr_units(logs["main-asr"])
    require(set(units) >= {"main", "pairwise_library", "averager_library", "simple_library"},
            f"the ASR of main lacks a unit: {sorted(units)}")
    main = units["main"]
    for name, text in units.items():
        instances = text.count("(GenericTypeInstance")
        require(instances == (3 if name == "main" else 0),
                f"{name} has {instances} generic type instances")
    for package in ("pairwise_library", "averager_library"):
        text = units[package]
        require(text.count("(GenericTypeDefinition") == 1 and "(TraitWitness" not in text
                and "(TraitErasure" not in text,
                f"{package} must hold one generic definition and no runtime evidence")
    require(units["simple_library"].count("(TraitWitness") == 1,
            "simple_library must keep the witness of its conformance")
    symbols = asr_symbols(main)
    instances = {name: spelling([symbols], name) for name, node in symbols.items()
                 if node.startswith("(Struct") and generic_instance(node)}
    require(sorted(instances.values()) == STATIC_INSTANCES,
            f"main must own the instances {STATIC_INSTANCES}, not {sorted(instances.values())}")
    witnesses = [implemented_type(symbols, node) for node in symbols.values()
                 if node.startswith("(TraitWitness")]
    require(main.count("(TraitWitness") == 3 and sorted(witnesses) == sorted(instances),
            f"main must own one runtime witness per instance conformance, not {witnesses}")
    # The two outer calls dispatch at runtime; every call inside the
    # instances and their entries is static.
    require(main.count("(TraitFunctionCall") == 2 and "(TraitDeferredCall" not in main,
            "main must make exactly the two av%average calls through the runtime view")
    definitions = [llvm_name(a or b) for a, b in LLVM_DEFINITION.findall(logs["main-llvm"])]
    provider = [name for name in definitions if simple_sum_evidence(name)]
    require(not provider, f"main defines evidence that simple_library owns: {provider}")
    witnesses = [name for name in definitions if name.startswith("__trait_witness_4_main")]
    require(len(witnesses) == 3, f"main must define three witnesses, not {witnesses}")
    objects = nm_definitions(logs["library-symbols"])
    for package in ("pairwise_library.o", "averager_library.o"):
        require(package in objects, f"the archive has no {package}")
        extra = [name for name in objects[package]
                 if "lfortran_intrinsic_iso_fortran_env" not in name]
        require(not extra, f"{package} defines more than its intrinsic module helpers: {extra}")
    require(any("trait_witness" in name and simple_sum_evidence(name)
                for name in objects.get("simple_library.o", [])),
            "simple_library.o must define the witness of its conformance")
    main_symbols = [name for names in nm_definitions(logs["main-symbols"]).values()
                    for name in names]
    copied = [name for name in main_symbols if simple_sum_evidence(name)]
    require(not copied, f"main.o defines evidence that simple_library owns: {copied}")
    require(len([name for name in main_symbols if "trait_witness_4_main" in name]) == 3,
            "main.o must define the three witnesses of its instances")
    for name in instances:
        owned = f"4_main{len(name)}_{name}"
        require(any(symbol.endswith(owned) and "trait_lifecycle" in symbol
                    for symbol in main_symbols),
                f"main.o does not define the lifecycle of its instance {name}")
        require(not any(owned in symbol for names in objects.values() for symbol in names),
                f"the archive defines code of main's instance {name}")
    return ("main owns the instances " + ", ".join(STATIC_INSTANCES) + " and one witness "
            "of each conformance, and makes 2 runtime calls; the packages hold their "
            "definitions only; simple_library alone defines its conformance's evidence")


# The procedures of the providers that the specializations in main call,
# by the linker symbol fragment of their module and name.
OVERLOADING_PROVIDED = {
    "my_type": ["init", "multiply", "multiply_by_int", "output"],
    "real_type": ["output"],
}


def check_overloading(logs):
    """Static protocols: main owns the specializations of products and prod
    for MyType and real, which call the initializer, operators and output
    that my_type and real_type define; no runtime dispatch, witness or
    instance is involved, and no provider holds specialized code."""
    units = asr_units(logs["main-asr"])
    require(set(units) >= {"main", "basic_interfaces", "my_type", "real_type"},
            f"the ASR of main lacks a unit: {sorted(units)}")
    main = units["main"]
    for node in ("(GenericTypeInstance", "(TraitWitness", "(TraitFunctionCall",
                 "(TraitSubroutineCall", "(TraitDeferredCall", "(TraitPack",
                 "(TraitDeferredPack", "(TraitErasure"):
        require(node not in main, f"main must dispatch statically, but has a {node[1:]}")
    real_type = units["real_type"]
    require(real_type.count("(TraitImplementation") == 2 and "(TraitWitness" not in real_type,
            "real_type must declare real's two conformances without runtime evidence")
    definitions = [llvm_name(a or b) for a, b in LLVM_DEFINITION.findall(logs["main-llvm"])]
    copied = [name for name in definitions
              if "module_my_type_" in name or "module_real_type_" in name]
    require(not copied, f"main defines procedures that the providers own: {copied}")
    objects = nm_definitions(logs["library-symbols"])
    main_symbols = [name for names in nm_definitions(logs["main-symbols"]).values()
                    for name in names]
    specializations = [name for name in main_symbols if "__instantiated_" in name]
    require(len([name for name in specializations if "products" in name]) == 1 and
            len([name for name in specializations if "prod" in name and
                 "products" not in name]) == 2,
            f"main.o must define products{{MyType, real}}, prod{{MyType}} and "
            f"prod{{real}}, not {specializations}")
    require(not any("trait_witness" in name or "implements" in name
                    for name in main_symbols),
            "main.o must define no conformance evidence")
    references = nm_references(logs["main-symbols"])
    for module, procedures in OVERLOADING_PROVIDED.items():
        for procedure in procedures:
            symbol = f"module_{module}_{procedure}"
            require(any(name.endswith(symbol) for name in references),
                    f"main.o must call {module}'s {procedure}, which it does not define")
            require(any(name.endswith(symbol) for name in objects.get(f"{module}.o", [])),
                    f"{module}.o must define {procedure}")
    acknowledged = [name for name in references if "module_real_type_ireducible" in name]
    require(len(acknowledged) == 3 and all(
                any(name == symbol for symbol in objects.get("real_type.o", []))
                for name in acknowledged),
            f"main.o must call real's acknowledged initializer and two operator(*) "
            f"procedures in real_type.o, not {acknowledged}")
    for library in ("basic_interfaces.o", "my_type.o", "real_type.o"):
        require(library in objects, f"the archive has no {library}")
        require(not any("__instantiated_" in name for name in objects[library]),
                f"{library} holds specialized code")
    return ("main owns the static specializations products{MyType, real}, prod{MyType} "
            "and prod{real}; they call MyType's init, operators and output in my_type.o "
            "and real's acknowledged initializer and operators and output in real_type.o; "
            "no runtime evidence or dispatch")


STRUCTURE = {"static": check_static, "overloading": check_overloading}


def units_of(content, split):
    """Return the units of a split map after checking them against content."""
    require(split.get("source_sha256") == digest(content) == split.get("rejoined_sha256"),
            "the split map does not record this fixture as its source and rejoined text")
    records = split.get("units")
    require(isinstance(records, list) and records, "the split map lists no units")
    lines = content.splitlines(keepends=True)
    units, next_line = [], 1
    for index, record in enumerate(records):
        name = record.get("unit")
        require(isinstance(name, str) and name not in [unit.name for unit in units],
                f"unit {name!r} is missing or repeated")
        first, last = record.get("first_line"), record.get("last_line")
        require(first == next_line and isinstance(last, int) and first <= last <= len(lines),
                f"{name}: lines {first}-{last} do not continue the split at line {next_line}")
        piece = b"".join(lines[first - 1:last])
        require(record.get("bytes") == len(piece) and record.get("sha256") == digest(piece),
                f"{name}: the piece differs from its recorded size or hash")
        kind = "program" if index == len(records) - 1 else "module"
        require(record.get("kind", kind) == kind, f"{name}: only the last unit is a program")
        text = piece.decode().splitlines()
        header = HEADER.fullmatch(text[0])
        require(header and header[1].lower() == kind and header[2].lower() == name,
                f"{name}: the piece does not start with '{kind} {name}'")
        ends = [i for i, line in enumerate(text) if END.fullmatch(line)]
        require(len(ends) == 1 and END.fullmatch(text[ends[0]])[1].lower() == kind
                and END.fullmatch(text[ends[0]])[2].lower() == name
                and not "".join(text[ends[0] + 1:]).strip(),
                f"{name}: the piece does not end with 'end {kind} {name}'")
        uses = []
        for line in text:
            use = USE.match(line)
            if use and (use[1] or "").lower() != "intrinsic" and use[2].lower() not in uses:
                uses.append(use[2].lower())
        imports = record.get("imports")
        require(isinstance(imports, list) and sorted(imports) == sorted(uses),
                f"{name}: imports {imports} differ from its USE statements {uses}")
        require(all(imported in [unit.name for unit in units] for imported in imports),
                f"{name}: imports a unit that is not compiled before it")
        require(record.get("compile_after", [unit.name for unit in units])
                == [unit.name for unit in units], f"{name}: compile_after differs from the order")
        file = record.get("file")
        require(isinstance(file, str) and re.fullmatch(r"[\w.-]+\.f90", file) is not None,
                f"{name}: bad piece file name {file!r}")
        units.append(Unit(name, kind, piece, imports, file, record))
        next_line = last + 1
    require(next_line == len(lines) + 1 and b"".join(unit.piece for unit in units) == content,
            "the pieces do not rejoin to the whole fixture")
    return units


def original_of(name, content):
    """The paper's own bytes: the fixture without its explicit provider USE."""
    if name not in paper.PROVIDER_IMPORTS:
        return content
    return paper.check_provider_import(name, content)


def verified_units(name, content, split):
    """Check the fixture, its paper original and its split; return the units."""
    require(name in paper.SOURCE_HASHES and digest(content) == paper.SOURCE_HASHES[name],
            f"the authoritative {name}.f90 fixture was changed")
    original = original_of(name, content)
    require(split.get("original_sha256", digest(original)) == digest(original),
            "the split map records a different paper original")
    units = units_of(content, split)
    for unit in units:
        if unit.record.get("contains_adaptation"):
            _, anchor, line, _ = paper.PROVIDER_IMPORTS[name]
            require(digest(unit.piece.replace(anchor + line, anchor, 1))
                    == unit.record.get("original_piece_sha256"),
                    f"{unit.name}: the piece is not its original plus the provider USE")
    modules = [unit.name for unit in units[:-1]]
    require(split.get("frozen_before_main", modules) == modules,
            "every library unit must be frozen before the main program")
    require(sorted(ROLES[name]) == sorted(modules), "the library roles do not match the split")
    return units


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=Path)
    parser.add_argument("--example", required=True, choices=sorted(SPLIT_HASHES))
    parser.add_argument("--mode", required=True, choices=["normal", "fast"])
    parser.add_argument("--work-dir", required=True, type=Path)
    parser.add_argument("--keep-work", action="store_true")
    args = parser.parse_args()
    name = args.example
    require(name in paper.SOURCE_HASHES, f"{name} is not a registered paper fixture")
    compiler = args.lfortran.resolve()
    fixture = HERE / paper.SOURCE_DIRECTORIES[name] / f"{name}.f90"
    split_map = fixture.with_name(f"{name}_split.json")
    require(split_map.is_file() and digest(split_map.read_bytes()) == SPLIT_HASHES[name],
            f"the authoritative {split_map.name} split map is missing or was changed")
    units = verified_units(name, fixture.read_bytes(), json.loads(split_map.read_text()))

    work = (args.work_dir.resolve()
            / f"paper-separate-{name}-{args.mode}-{os.getpid()}-{time.time_ns()}")
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes", "--separate-compilation"]
    if args.mode == "fast":
        flags.append("--fast")
    records, frozen = [], {}

    def freeze(*paths):
        for path in paths:
            frozen[path] = digest(path.read_bytes())
        (work / "frozen.json").write_text(json.dumps(
            {str(path.relative_to(work)): value for path, value in frozen.items()},
            indent=2) + "\n")

    def run(step, command, cwd=work):
        command = list(map(str, command))
        print("+", " ".join(command), flush=True)
        log = work / f"{step}.log"
        with log.open("w") as output:
            process = subprocess.run(command, cwd=cwd, env=environment, stdout=output,
                                     stderr=subprocess.STDOUT, timeout=300)
        records.append({"step": step, "command": command, "cwd": str(cwd),
                        "returncode": process.returncode, "log": str(log)})
        (work / "commands.json").write_text(json.dumps(records, indent=2) + "\n")
        require(process.returncode == 0, f"{step} failed ({process.returncode}):\n"
                                         f"{log.read_text()}")
        for path, value in frozen.items():
            require(path.is_file() and digest(path.read_bytes()) == value,
                    f"{path.relative_to(work)} changed during {step}")

    directories, visible, sources = {}, {}, {}
    for index, unit in enumerate(units, 1):
        directories[unit.name] = work / f"{index}-{unit.name}"
        (directories[unit.name] / "modules").mkdir(parents=True)
        visible[unit.name] = list(dict.fromkeys(
            dependency for imported in unit.imports
            for dependency in [*visible[imported], imported]))
        sources[unit.name] = directories[unit.name] / unit.file
        sources[unit.name].write_bytes(unit.piece)
        freeze(sources[unit.name])

    def compile_unit(unit):
        directory = directories[unit.name]
        includes = [item for dependency in visible[unit.name]
                    for item in ("-I", directories[dependency] / "modules")]
        obj = directory / f"{unit.name}.o"
        run(f"compile-{unit.name}", [compiler, *flags, *includes, "-J", directory / "modules",
                                     "-c", sources[unit.name], "-o", obj], directory)
        require(obj.is_file(), f"{unit.name}: the compiler wrote no object {obj.name}")
        return obj, includes

    *libraries, program = units
    objects = []
    for unit in libraries:
        obj, _ = compile_unit(unit)
        module = directories[unit.name] / "modules" / f"{unit.name}.mod"
        require(module.is_file(), f"{unit.name}: the compiler wrote no module file {module.name}")
        freeze(obj, *sorted((directories[unit.name] / "modules").iterdir()))
        objects.append(obj)
    archive = work / "libraries.a"
    run("archive", ["ar", "rcs", archive, *objects])
    freeze(archive)
    for unit in libraries:
        hidden = sources[unit.name].with_name(unit.file + ".hidden")
        sources[unit.name].rename(hidden)
        del frozen[sources[unit.name]]
        freeze(hidden)
    visible_sources = [str(path.relative_to(work)) for path in work.rglob("*")
                       if path.suffix.lower() in FORTRAN_SUFFIXES
                       and path != sources[program.name]]
    require(not visible_sources, f"library sources are still visible: {visible_sources}")

    obj, includes = compile_unit(program)
    freeze(obj)
    inspection = directories[program.name] / "inspection"
    inspection.mkdir()
    for kind in ("asr", "llvm"):
        run(f"{program.name}-{kind}", [compiler, *flags, *includes, "-J", inspection,
                                       f"--show-{kind}", sources[program.name]],
            directories[program.name])
    run(f"{program.name}-symbols", ["nm", obj])
    run("library-symbols", ["nm", archive])
    structure = STRUCTURE[name]({log: (work / f"{log}.log").read_text() for log in
                                 (f"{program.name}-asr", f"{program.name}-llvm",
                                  f"{program.name}-symbols", "library-symbols")})
    print(f"{name}, {args.mode}: {structure}", flush=True)
    executable = work / name
    run("link", [compiler, *flags, obj, archive, "-o", executable])
    run("output", [sys.executable, HERE / "traits_paper_functional.py", "--executable",
                   executable, "--source", fixture, "--work-dir", work / "output"])

    roles = ", ".join(f"{library} ({ROLES[name][library]})" for library in ROLES[name])
    print(f"{name}, {args.mode}: libraries {roles} compiled in dependency order, archived "
          f"and hidden; {len(frozen)} frozen files unchanged through compiling, inspecting, "
          f"linking and running {program.name}", flush=True)
    if not args.keep_work:
        shutil.rmtree(work)

if __name__ == "__main__":
    main()
