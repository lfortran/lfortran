"""Exercise borrowed trait dispatch without implementation modules in the consumer."""

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
    args = parser.parse_args()
    compiler = str(Path(args.lfortran).resolve())
    sources = Path(__file__).resolve().parent
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
        source = sources / ("traits_runtime_separate_01" + suffix + ".f90")
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

        first, _, _, _ = compile_part("a", ["contracts"])
        second, _, _, _ = compile_part("b", ["contracts"])
        archive = work / "providers.a"
        run(["ar", "rcs", archive, first, second])
        archive_hash = hashlib.sha256(archive.read_bytes()).hexdigest()
        driver, source, directory, includes = compile_part(
            "driver", ["contracts", "consumer", "a", "b"])
        with (directory / "driver.ll").open("w") as output:
            run([compiler, *flags, *includes, "--show-llvm", source], directory, output)
        llvm = (directory / "driver.ll").read_text()
        metadata = re.findall(r"^@([^ ]*Type_Info_[^ ]*) = ", llvm, re.MULTILINE)
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
