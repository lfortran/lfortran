"""Compile clients against the module file of a private module that owns
generic derived type instances, without its sources.

traits_generic_type_04_owner owns instances of PairwiseSum and Averager, their
procedure copies, initializers, conformances and runtime evidence. Its
libraries and the module itself are compiled separately into their own module
directories and frozen. Then:

* clients that name an instance or its procedure copies and conformance
  the way earlier compilers did, also with the leading underscores LFortran
  accepts in identifiers, are rejected whether a USE lists or renames them:
  no entity of the module has those names, and a USE that names symbols
  never imports an instance entity;
* a client that forms a runtime view of a value of the module's instance gets
  the located diagnostic of an instance it does not own;
* a client that uses all of the module and names no instance type compiles
  and runs, and its ASR imports none of the module's instance entities.

The module directories and objects stay byte-identical throughout.
"""

import argparse
import hashlib
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import traits_paper_separate as separate  # noqa: E402

LIBRARIES = ["traits_generic_type_04_lib", "traits_generic_type_04_owner"]
OWNER = "traits_generic_type_04_owner"
NOT_FOUND = "The symbol '{}' not found in the module 'traits_generic_type_04_owner'"
# Each rejected client, compiled with --continue-compilation, and every
# diagnostic it must get: the message and the line it is located at.
REJECTED = {
    "traits_generic_type_04_use_internal": [(NOT_FOUND.format("averager__simplesum"), 4)],
    "traits_generic_type_04_use_view": [
        ("no visible nominal implementation of trait 'iaverager' for type "
         "'averager{simplesum}'", 9)],
    "traits_generic_type_04_use_reserved": [
        (NOT_FOUND.format("__averager__simplesum"), 7),
        (NOT_FOUND.format("__averager__simplesum_average"), 11),
        (NOT_FOUND.format("__averager__simplesum"), 15),
        (NOT_FOUND.format("__averager__simplesum_init"), 19),
        (NOT_FOUND.format("__implements___averager__simplesum_iaverager"), 23)],
}
DIAGNOSTIC = re.compile(r"^semantic error: (.*)\n\s*--> (\S+):(\d+):\d+", re.MULTILINE)
ASR_LIST = re.compile(r"\s*\[([^\[\]]*)\]")


def instance_entities(owner):
    """The names of the owner unit's instances, their recorded procedure
    copies, a generic procedure of only such copies, and the conformances of
    the instances."""
    symbols = separate.asr_symbols(owner)
    instances, procedures = set(), set()
    for name, node in symbols.items():
        if not node.startswith("(Struct") or not separate.generic_instance(node):
            continue
        instances.add(name)
        start = node.index("(GenericTypeInstance")
        block = node[start:separate.node_end(node, start)]
        # The definition, the argument list, then the recorded copies.
        head = re.match(r"\(GenericTypeInstance\s+\d+ \S+\s+", block)
        position, depth = head.end(), 0
        for index in range(position, len(block)):
            depth += {"[": 1, "]": -1}.get(block[index], 0)
            if depth == 0:
                position = index + 1
                break
        copies = ASR_LIST.match(block, position)
        separate.require(copies is not None, f"{name} records no procedure copies")
        procedures.update(re.findall(r"\d+ (\S+)", copies[1]))
    separate.require(instances and procedures,
                     f"{OWNER} owns no completed generic type instance")
    entities = instances | procedures
    for name, node in symbols.items():
        if node.startswith("(TraitImplementation"):
            head = re.match(r"\(TraitImplementation\s+\d+\s+\S+\s+", node)
            declaration = separate.ASR_REFERENCE.match(
                node, separate.node_end(node, head.end()))
            if declaration and declaration[1] in instances:
                entities.add(name)
        elif node.startswith("(GenericProcedure"):
            listed = re.match(r"\(GenericProcedure\s+\d+\s+\S+\s+\[([^\]]*)\]", node)
            members = set(re.findall(r"\d+ (\S+)", listed[1])) if listed else set()
            if members and members <= procedures:
                entities.add(name)
    return entities


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=Path)
    parser.add_argument("--mode", required=True, choices=["normal", "fast"])
    parser.add_argument("--work-dir", required=True, type=Path)
    parser.add_argument("--keep-work", action="store_true")
    args = parser.parse_args()
    compiler = args.lfortran.resolve()
    work = (args.work_dir.resolve()
            / f"generic-type-private-{args.mode}-{os.getpid()}-{time.time_ns()}")
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes", "--separate-compilation"]
    if args.mode == "fast":
        flags.append("--fast")

    def run(step, command, expect_success=True):
        command = list(map(str, command))
        print("+", " ".join(command), flush=True)
        log = work / f"{step}.log"
        with log.open("w") as output:
            process = subprocess.run(command, cwd=work, env=environment, stdout=output,
                                     stderr=subprocess.STDOUT, timeout=300)
        text = log.read_text()
        if expect_success:
            separate.require(process.returncode == 0,
                             f"{step} failed ({process.returncode}):\n{text}")
        return process.returncode, text

    includes, objects = [], []
    for library in LIBRARIES:
        directory = work / library
        (directory / "modules").mkdir(parents=True)
        obj = directory / f"{library}.o"
        run(f"compile-{library}", [compiler, *flags, *includes, "-J", directory / "modules",
                                   "-c", HERE / f"{library}.f90", "-o", obj])
        includes += ["-I", directory / "modules"]
        objects.append(obj)
    frozen = {path: hashlib.sha256(path.read_bytes()).hexdigest()
              for library in LIBRARIES for path in (work / library).rglob("*")
              if path.is_file()}

    for client, expected in REJECTED.items():
        modules = work / client / "modules"
        modules.mkdir(parents=True)
        code, text = run(f"{client}-compile", [compiler, *flags, "--continue-compilation",
                                               *includes, "-J", modules, "-c",
                                               HERE / f"{client}.f90", "-o",
                                               work / client / f"{client}.o"],
                         expect_success=False)
        located = sorted((message, int(line)) for message, path, line in DIAGNOSTIC.findall(text)
                         if Path(path).name == f"{client}.f90")
        separate.require(code != 0 and located == sorted(expected) and
                         "Internal Compiler Error" not in text,
                         f"{client} must fail with exactly {sorted(expected)}, "
                         f"not {located}:\n{text}")

    client = "traits_generic_type_04_use_all"
    modules = work / client / "modules"
    modules.mkdir(parents=True)
    _, dump = run(f"{client}-asr", [compiler, *flags, *includes, "-J", modules, "--show-asr",
                                    HERE / f"{client}.f90"])
    units = separate.asr_units(dump)
    separate.require(OWNER in units and client in units, f"the ASR lacks {OWNER} or {client}")
    entities = instance_entities(units[OWNER])
    imported = []
    for name, node in separate.asr_symbols(units[client]).items():
        external = separate.ASR_EXTERNAL.match(node)
        if external and external[1] == OWNER and external[2] in entities:
            imported.append(name)
    separate.require(not imported, f"{client} imports instance entities of {OWNER}: {imported}")
    obj = work / client / f"{client}.o"
    run(f"{client}-compile", [compiler, *flags, *includes, "-J", modules, "-c",
                              HERE / f"{client}.f90", "-o", obj])
    executable = work / client / client
    run(f"{client}-link", [compiler, *flags, obj, *objects, "-o", executable])
    _, output = run(f"{client}-run", [executable])
    separate.require("owner entities only" in output, f"{client} printed:\n{output}")

    for path, value in frozen.items():
        separate.require(hashlib.sha256(path.read_bytes()).hexdigest() == value,
                         f"{path.relative_to(work)} changed")
    print(f"generic type private, {args.mode}: none of the {len(entities)} instance "
          f"entities of {OWNER} imported by a client using all of it; "
          f"{sum(map(len, REJECTED.values()))} uses in {len(REJECTED)} clients rejected "
          f"with located diagnostics; "
          f"{len(frozen)} frozen files unchanged", flush=True)
    if not args.keep_work:
        shutil.rmtree(work)


if __name__ == "__main__":
    main()
