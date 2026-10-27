#!/usr/bin/env python3
"""Check that a submodule's code is in the object file of its own source.

The module, the submodule and the program are compiled with `-c` into
separate object files and linked. Then only the submodule's source is
changed, only it is recompiled, and the objects are linked again: the
program must run the new code of the submodule, as it does when a build
system rebuilds just what changed (#14167).
"""

import argparse
import os
import subprocess
import sys
import tempfile

# The pointer initialization needs code that runs at startup, which only the
# object file defining the module's variables may define.
MODULE = """\
module sof_m
implicit none
integer, target :: arr(3) = [1, 2, 3]
integer, pointer :: pa(:) => arr
interface
    module function f() result(r)
        integer :: r
    end function
end interface
end module
"""

SUBMODULE = """\
submodule (sof_m) sof_s
implicit none
contains
    module procedure f
        r = {value} * size(pa)
    end procedure
end submodule
"""

PROGRAM = """\
program sof_p
use sof_m
implicit none
print *, f()
end program
"""


def run(cmd, cwd):
    p = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, universal_newlines=True)
    if p.returncode != 0:
        sys.exit("%s: exited with status %d\n%s" % (" ".join(cmd),
            p.returncode, p.stdout))
    return p.stdout


def write(cwd, name, source):
    with open(os.path.join(cwd, name), "w") as f:
        f.write(source)


def link_and_run(lfortran, cwd, expected):
    run([lfortran, "m.o", "s.o", "p.o", "-o", "a.out"], cwd)
    out = run([os.path.join(cwd, "a.out")], cwd).strip()
    if out != expected:
        sys.exit("expected %r, got %r" % (expected, out))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--workdir", required=True)
    args = parser.parse_args()
    lfortran = os.path.abspath(args.lfortran)
    os.makedirs(args.workdir, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=args.workdir) as cwd:
        write(cwd, "m.f90", MODULE)
        write(cwd, "s.f90", SUBMODULE.format(value=1))
        write(cwd, "p.f90", PROGRAM)
        for name in ["m", "s", "p"]:
            run([lfortran, "--no-color", "-c", name + ".f90", "-o",
                name + ".o"], cwd)
        link_and_run(lfortran, cwd, "3")
        write(cwd, "s.f90", SUBMODULE.format(value=2))
        run([lfortran, "--no-color", "-c", "s.f90", "-o", "s.o"], cwd)
        link_and_run(lfortran, cwd, "6")
    print("ok")


if __name__ == "__main__":
    main()
