"""Exercise trait dispatch/ownership without implementer modules in the consumer."""

import argparse
import hashlib
import os
from pathlib import Path
import re
import shutil
import subprocess


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--work-dir", required=True)
    parser.add_argument("--fast", action="store_true")
    parser.add_argument("--owning", action="store_true")
    args = parser.parse_args()
    compiler = str(Path(args.lfortran).resolve())
    sources = Path(__file__).resolve().parent
    prefix = "traits_runtime_owning_separate_01" if args.owning else "traits_runtime_separate_01"
    work = Path(args.work_dir).resolve() / f"runtime-traits-{os.getpid()}"
    work.mkdir(parents=True)
    environment = os.environ.copy()
    environment.update(TMPDIR=str(work), TMP=str(work), TEMP=str(work))
    flags = ["--verify-all-passes", "--separate-compilation", "--no-color"]
    if args.fast:
        flags.append("--fast")

    def run(command, cwd=work, output=None):
        command = list(map(str, command))
        print("+", " ".join(command), flush=True)
        subprocess.run(command, cwd=cwd, env=environment, check=True, stdout=output)

    def compile_part(part, imports):
        directory = work / part
        directory.mkdir()
        modules = directory / "modules"
        modules.mkdir()
        includes = []
        for imported in imports:
            includes += ["-I", work / imported / "modules"]
        suffix = "" if part == "driver" else "_" + part
        source = sources / (prefix + suffix + ".f90")
        obj = directory / (part + ".o")
        run([compiler, *flags, *includes, "-J", modules, "-c", source, "-o", obj],
            directory)
        return obj, source, directory, includes

    try:
        contracts, _, _, _ = compile_part("contracts", [])
        consumer, source, directory, includes = compile_part("consumer", ["contracts"])
        assert not (work / "a").exists() and not (work / "b").exists()
        with (directory / "consumer.ll").open("w") as output:
            run([compiler, *flags, *includes, "--show-llvm", source], directory, output)
        llvm = (directory / "consumer.ll").read_text()
        assert re.search(r"call i32 %", llvm), "consumer must load and call a function slot"
        assert re.search(r"call void %", llvm), "consumer must load and call a subroutine slot"
        assert "__implements_" not in llvm, "consumer must not contain provider specializations"
        with (directory / "consumer.asr").open("w") as output:
            run([compiler, "--no-color", *includes, "--show-asr", source], directory, output)
        semantic = (directory / "consumer.asr").read_text()
        assert "TraitFunctionCall" in semantic and "TraitSubroutineCall" in semantic
        assert "TraitPack" not in semantic, "forwarding must not reselect a witness"
        if args.owning:
            assert "TraitAssignment" in semantic and "TraitBorrow" in semantic
            assert "TraitWitness" not in semantic, "consumer must not know implementers"
            assert re.search(r"call (?:i8\*|ptr) %", llvm), "copy must use the carried lifecycle"

        provider_imports = ["contracts"]
        provider_objects = []
        if args.owning:
            types, _, _, _ = compile_part("types", [])
            provider_objects.append(types)
            provider_imports.append("types")
        first, _, _, _ = compile_part("a", provider_imports)
        second, _, _, _ = compile_part("b", provider_imports)
        archive = work / "providers.a"
        run(["ar", "rcs", archive, *provider_objects, first, second])
        archive_hash = hashlib.sha256(archive.read_bytes()).hexdigest()
        driver, source, directory, includes = compile_part(
            "driver", [*provider_imports, "consumer", "a", "b"])
        with (directory / "driver.ll").open("w") as output:
            run([compiler, *flags, *includes, "--show-llvm", source], directory, output)
        llvm = (directory / "driver.ll").read_text()
        metadata = re.findall(r"^@([^ ]*Type_Info_[^ ]*) = ", llvm, re.MULTILINE)
        if not args.owning:
            assert any("traits_runtime_separate_01_a_m" in name for name in metadata)
            assert any("traits_runtime_separate_01_b_m" in name for name in metadata)
        executable = work / "program"
        run([compiler, driver, consumer, archive, contracts, "-o", executable])
        run([executable])
        run([executable, "select-second-first"])
        assert hashlib.sha256(archive.read_bytes()).hexdigest() == archive_hash
        print("contract-only consumer, distinct nominal metadata, frozen witnesses: passed")
    finally:
        shutil.rmtree(work)


if __name__ == "__main__":
    main()
