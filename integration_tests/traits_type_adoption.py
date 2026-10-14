"""Compile the module-only paper examples and separate dispatch controls.

The fixtures are the paper's files with the TD-36 explicit receivers that
traits_paper_type_adoption/provenance.json records; reverting them must
reproduce the paper's bytes.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time


SOURCE_HASHES = {
    "extends_parent.f90": "f309085c7cfb53e308c17b6d9a9bc3d5184cbbb0846dd93921e883175a7e13c4",
    "abstract_new.f90": "0ae1b5353a02a7bb588b06f904300f911d4744d7130c55e4417e359d38a9a4f4",
    "simple_sum.f90": "7f1f76499ef81707e74505cc2580c4cf1683fea642ed6f94a6cc0c2a4a545658",
}
# The paper's files (simple_sum.f90: the first 55 lines of mixed.f90).
PAPER_HASHES = {
    "extends_parent.f90": "9141332b540c768055e84b80a6170bb692b0366be0c8c410dcf6b15bf5b8cba3",
    "abstract_new.f90": "1c9b1882ac60448fad1060b265f22b15c5a0c8316bc7142f8f980a1a091991b0",
    "simple_sum.f90": "6c4842ae7b415ce84b958ab0e9029dc9fa4b7c922d6143d15010dcd50dac5bba",
}


def check_paper_original(directory, name, content):
    """Revert the recorded TD-36 receivers of a fixture, the last one first,
    and compare the result with the paper's bytes."""
    record = json.loads((directory / "provenance.json").read_text())["fixtures"][name]
    if record["sha256"] != SOURCE_HASHES[name] or record["original_sha256"] != PAPER_HASHES[name]:
        raise RuntimeError(f"provenance.json does not record {name} with its pinned hashes")
    for replacement in reversed(record["receiver_migration"]["replacements"]):
        fixture, paper = replacement["fixture"].encode(), replacement["paper"].encode()
        if content.count(fixture) != 1:
            raise RuntimeError(f"{name}: a recorded receiver does not occur exactly once")
        content = content.replace(fixture, paper)
    if hashlib.sha256(content).hexdigest() != PAPER_HASHES[name]:
        raise RuntimeError(f"{name} without its TD-36 receivers is not the paper's file")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=Path)
    parser.add_argument("--work-dir", type=Path, default=Path.cwd())
    parser.add_argument("--fast", action="store_true")
    args = parser.parse_args()
    source_dir = Path(__file__).resolve().parent
    compiler = args.lfortran.resolve()
    work = args.work_dir.resolve() / f"type-adoption-{os.getpid()}-{time.time_ns()}"
    work.mkdir(parents=True)
    environment = {**os.environ, "TMPDIR": str(work), "TMP": str(work), "TEMP": str(work)}
    flags = ["--no-color", "--verify-all-passes"]
    if args.fast:
        flags.append("--fast")
    commands = []

    def run(name, command, cwd):
        command = list(map(str, command))
        log = work / f"{name}.log"
        with log.open("w") as output:
            result = subprocess.run(command, cwd=cwd, env=environment, text=True,
                                    stdout=output, stderr=subprocess.STDOUT, timeout=120)
        commands.append({"name": name, "command": command, "cwd": str(cwd),
                         "returncode": result.returncode, "log": str(log)})
        (work / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        text = log.read_text()
        if result.returncode:
            raise RuntimeError(f"{name} failed ({result.returncode}):\n{text}")
        return text

    for name, expected in SOURCE_HASHES.items():
        source = source_dir / "traits_paper_type_adoption" / name
        if hashlib.sha256(source.read_bytes()).hexdigest() != expected:
            raise RuntimeError(f"the authoritative {name} fixture was changed")
        check_paper_original(source.parent, name, source.read_bytes())
        private = work / source.stem
        private.mkdir()
        run(source.stem + "-asr",
            [compiler, *flags, "--show-asr", source], private)
        obj = private / (source.stem + ".o")
        run(source.stem + "-object",
            [compiler, *flags, "-c", source, "-J", private, "-o", obj], private)
        if not obj.is_file() or not obj.stat().st_size:
            raise RuntimeError(f"{name} did not produce an object")
        print(f"{name}: verified ASR and object; no execution", flush=True)

    private = work / "separate"
    modules = private / "modules"
    modules.mkdir(parents=True)
    objects = []
    # The provider is completed before the client exists. Each module is
    # compiled in its own process, so inheritance must survive .mod metadata.
    for part in ("contracts", "parent", "child", "facade", "consumer", ""):
        name = "traits_type_adoption_02" + ("_" + part if part else "")
        source = source_dir / (name + ".f90")
        obj = private / (name + ".o")
        run(name + "-asr",
            [compiler, *flags, "--show-asr", "-I", modules, source], private)
        run(name + "-object",
            [compiler, *flags, "-c", "-I", modules, "-J", modules,
             source, "-o", obj], private)
        objects.append(obj)
    exe = private / "dispatch"
    run("separate-link", [compiler, *flags, *objects, "-o", exe], private)
    output = run("separate-run", [exe], private)
    if "abstract adoption: 39 23 29; legacy: 4" not in output:
        raise RuntimeError(f"unexpected separate dispatch result:\n{output}")
    print("separate modules, ONLY/renaming, abstract obligations and runtime subsets: pass",
          flush=True)

    private = work / "sealed"
    modules = private / "modules"
    modules.mkdir(parents=True)
    objects = []
    for part in ("provider", "consumer", ""):
        name = "traits_type_adoption_04" + ("_" + part if part else "")
        source = source_dir / (name + ".f90")
        obj = private / (name + ".o")
        run(name + "-object",
            [compiler, *flags, "-c", "-I", modules, "-J", modules,
             source, "-o", obj], private)
        objects.append(obj)
    exe = private / "dispatch"
    run("sealed-link", [compiler, *flags, *objects, "-o", exe], private)
    output = run("sealed-run", [exe], private)
    if "sealed ancestor dispatch: 19 39 36 26; finals: 6 42" not in output:
        raise RuntimeError(f"unexpected sealed ancestor dispatch result:\n{output}")
    print("sealed ancestor dispatch, named PASS, optional arguments and lifecycle: pass",
          flush=True)


if __name__ == "__main__":
    main()
