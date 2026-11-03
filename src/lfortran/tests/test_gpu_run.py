import argparse
import os
from pathlib import Path
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--workdir", type=Path, required=True)
    args = parser.parse_args()
    workdir = args.workdir.resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    runner = Path(__file__).resolve().parents[3] / "examples/nbody/gpu_run.sh"
    child = """
from pathlib import Path
import sys

if not Path(sys.argv[1]).is_dir():
    raise SystemExit("chosen lock is not held")
if sys.argv[3:] != ["argument with spaces", "", "*"]:
    raise SystemExit("child arguments changed")
print("chosen lock held", flush=True)
sys.exit(int(sys.argv[2]))
"""
    with tempfile.TemporaryDirectory(dir=workdir) as temporary:
        lock = Path(temporary) / "shared gpu lock.d"
        env = dict(os.environ, LFORTRAN_GPU_LOCK_DIR=str(lock),
                   GPU_TIMEOUT="5", GPU_MAXMEM_MB="1024")
        command = ["bash", str(runner), sys.executable, "-c", child, str(lock)]
        for status in (0, 37):
            result = subprocess.run(
                command + [str(status), "argument with spaces", "", "*"],
                env=env, text=True, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT, timeout=15)
            print(f"child status {status}: exit {result.returncode}\n"
                  f"{result.stdout}", flush=True)
            if result.returncode != status or result.stdout != "chosen lock held\n":
                raise AssertionError("lock override or child status/arguments changed")
            if lock.exists():
                raise AssertionError("owned lock was not removed")

        lock.mkdir()
        original_lock = lock.stat()
        # Exercise the unchanged acquisition deadline, without a command shim.
        result = subprocess.run(
            command + ["0", "argument with spaces", "", "*"],
            env=env, text=True, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, timeout=75)
        print(f"pre-existing lock: exit {result.returncode}\n"
              f"{result.stdout}", flush=True)
        expected = (f"GPU lock is busy: {lock} "
                    "(a stale lock must be inspected manually)\n")
        if result.returncode != 126 or result.stdout != expected:
            raise AssertionError("busy lock was acquired or child was executed")
        if not lock.is_dir() or lock.stat().st_ino != original_lock.st_ino:
            raise AssertionError("pre-existing lock was removed or replaced")
        lock.rmdir()
    print("3 CPU-only GPU runner cases passed", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
