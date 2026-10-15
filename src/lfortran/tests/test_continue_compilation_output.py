#!/usr/bin/env python3
"""Check that `-c --continue-compilation` writes no output for invalid code.

The errors must all be reported and the compiler must exit with a nonzero
status, without writing an object file or module files (#14800, #14572).
"""

import argparse
import os
import subprocess
import sys
import tempfile

# A program, with a module that is also erroneous.
PROGRAM = """\
module cc_output_m
implicit none
contains
subroutine s()
integer :: i
i = "a"
end subroutine
end module

program cc_output_p
implicit none
print *, undeclared_x
end program
"""

# A file with only modules takes a separate path in the driver.
MODULES_ONLY = """\
module cc_output_m1
implicit none
contains
subroutine s()
integer :: i
i = "a"
end subroutine
end module

module cc_output_m2
implicit none
contains
subroutine t()
print *, undeclared_x
end subroutine
end module
"""

EXPECTED_ERRORS = [
    "Type mismatch in assignment",
    "Variable 'undeclared_x' is not declared",
]


def check(lfortran, workdir, backend, name, source):
    with tempfile.TemporaryDirectory(dir=workdir) as cwd:
        src = name + ".f90"
        with open(os.path.join(cwd, src), "w") as f:
            f.write(source)
        cmd = [lfortran, "--no-color", "--backend=" + backend,
            "--continue-compilation", "-c", src, "-o", name + ".o"]
        p = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, universal_newlines=True)
        failures = []
        if p.returncode == 0:
            failures.append("exited with status 0")
        for error in EXPECTED_ERRORS:
            if error not in p.stdout:
                failures.append("did not report %r" % error)
        written = sorted(set(os.listdir(cwd)) - {src})
        if written:
            failures.append("wrote %s" % ", ".join(written))
        if failures:
            return ["%s: %s\n%s" % (" ".join(cmd), "; ".join(failures),
                p.stdout)]
        return []


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--workdir", required=True)
    args = parser.parse_args()
    lfortran = os.path.abspath(args.lfortran)
    os.makedirs(args.workdir, exist_ok=True)
    failures = []
    for backend in ["llvm", "c", "cpp", "x86", "wasm", "fortran"]:
        for name, source in [("program", PROGRAM),
                ("modules_only", MODULES_ONLY)]:
            failures += check(lfortran, args.workdir, backend, name, source)
    if failures:
        sys.exit("\n".join(failures))
    print("ok")


if __name__ == "__main__":
    main()
