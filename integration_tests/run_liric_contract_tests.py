import pathlib
import subprocess
import sys
import tempfile

compiler = sys.argv[1]
source_dir = pathlib.Path(__file__).resolve().parent
with tempfile.TemporaryDirectory() as temporary_dir:
    output = pathlib.Path(temporary_dir) / "program"
    for fixture in ("liric_unsupported_01", "liric_array_01"):
        result = subprocess.run(
            [compiler, "--backend=liric", str(source_dir / f"{fixture}.f90"),
             "-o", str(output)], capture_output=True, text=True)
        diagnostic = result.stdout + result.stderr
        if (result.returncode <= 0 or "liric:" not in diagnostic
                or "supported" not in diagnostic or output.exists()
                or any(text in diagnostic for text in
                       ("LCompilersException", "visit_", "Traceback"))):
            raise SystemExit(f"{fixture}: expected a clean unsupported diagnostic\n{diagnostic}")
        print(f"{fixture}: clean unsupported diagnostic")

    result = subprocess.run(
        [compiler, "--backend=liric", str(source_dir / "liric_error_stop.f90"),
         "-o", str(output)], capture_output=True, text=True)
    if result.returncode != 0:
        raise SystemExit(result.stdout + result.stderr)
    result = subprocess.run([str(output)], capture_output=True, text=True)
    if result.returncode != 1:
        raise SystemExit(f"error stop: expected exit status 1, got {result.returncode}")
    print("error stop: exit status 1")
