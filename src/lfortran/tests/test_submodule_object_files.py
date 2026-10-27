#!/usr/bin/env python3
"""Check that a submodule's code is in the object file of its own source.

The module, the submodule and the program are compiled with `-c` into
separate object files and linked. Then only the submodule's source is
changed, only it is recompiled, and the objects are linked again: the
program must run the new code of the submodule, as it does when a build
system rebuilds just what changed (#14167); the same holds for a module
that only the submodule uses, whose code is then in the submodule's object
file, as is its initialization. The submodule gains variables
that need setting up at run time, which the program's object file cannot
know about, and loses them again, which the program's object file must not
depend on. The program is also compiled before the submodule, which calls a
private procedure of its parent. Everything is checked with and without
`--fast`.
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

# Variables set up at run time: an array pointer associated with a target,
# a derived type with an array pointer component, and an array of a derived
# type with a character component. gfortran does not support the first one
# in a submodule, which is why this is not an integration test.
SUBMODULE_VARS = """\
submodule (sof_m) sof_s
implicit none
type :: tp
    integer, pointer :: q(:) => null()
end type
type :: tc
    character(len=4) :: nm = "abcd"
end type
integer, target :: sarr(4) = [1, 2, 3, 4]
integer, pointer :: sp(:) => sarr
type(tp) :: obj
type(tc) :: objc(2)
contains
    module procedure f
        r = {value} * size(pa)
        if (associated(sp)) r = r + 10 * size(sp)
        if (.not. associated(obj%q)) r = r + 100
        if (objc(2)%nm == "abcd") r = r + 1000
    end procedure
end submodule
"""

# A private procedure of the parent that only the submodule calls. The
# program's object file, which emits the parent, must still define it, under
# a name the submodule's object file can refer to. gfortran 13 does not
# export such a procedure, which is why this is not in an integration test.
MODULE_PRIVATE = """\
module sof_m
implicit none
private
public :: f
interface
    module function f() result(r)
        integer :: r
    end function
end interface
contains
    integer function helper(i)
        integer, intent(in) :: i
        helper = i + 100
    end function
end module
"""

SUBMODULE_PRIVATE = """\
submodule (sof_m) sof_s
implicit none
contains
    module procedure f
        r = helper({value})
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

# A module that only the submodule uses (#14167). Its array pointer is
# associated with a target in its declaration, which needs code that runs
# before the module is used, and its function has a saved counter.
MODULE_R = """\
module sof_r
implicit none
integer, target :: rarr(4) = [1, 2, 3, 4]
integer, target :: rother(2) = [5, 6]
integer, pointer :: rp(:) => rarr
contains
    integer function g()
        integer, save :: k = 0
        k = k + 1
        g = {value} * k + 10 * size(rp)
    end function
end module
"""

MODULE_PARENT = """\
module sof_m
implicit none
interface
    module function f() result(r)
        integer :: r
    end function
end interface
end module
"""

SUBMODULE_R = """\
submodule (sof_m) sof_s
use sof_r
implicit none
contains
    module procedure f
        r = g()
    end procedure
end submodule
"""

# The program uses the module too, after pointing its array pointer
# elsewhere: the submodule's procedure must see that, and the module's
# initializer must not run again on entry into the submodule.
PROGRAM_R = """\
program sof_p
use sof_m
use sof_r
implicit none
integer :: a, b
a = g()
rp => rother
b = f()
print *, a, b
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


def compile(lfortran, cwd, flags, names):
    for name in names:
        run([lfortran, "--no-color"] + flags + ["-c", name + ".f90", "-o",
            name + ".o"], cwd)


def link_and_run(lfortran, cwd, flags, expected, objects=("m", "s", "p")):
    run([lfortran] + flags + [o + ".o" for o in objects] + ["-o", "a.out"],
        cwd)
    out = " ".join(run([os.path.join(cwd, "a.out")], cwd).split())
    if out != expected:
        sys.exit("%s: expected %r, got %r" % (" ".join(flags) or "default",
            expected, out))


def check(lfortran, workdir, flags):
    with tempfile.TemporaryDirectory(dir=workdir) as cwd:
        write(cwd, "m.f90", MODULE)
        write(cwd, "s.f90", SUBMODULE.format(value=1))
        write(cwd, "p.f90", PROGRAM)
        compile(lfortran, cwd, flags, ["m", "s", "p"])
        link_and_run(lfortran, cwd, flags, "3")
        write(cwd, "s.f90", SUBMODULE.format(value=2))
        compile(lfortran, cwd, flags, ["s"])
        link_and_run(lfortran, cwd, flags, "6")
        write(cwd, "s.f90", SUBMODULE_VARS.format(value=3))
        compile(lfortran, cwd, flags, ["s"])
        link_and_run(lfortran, cwd, flags, "1149")
        write(cwd, "s.f90", SUBMODULE.format(value=4))
        compile(lfortran, cwd, flags, ["s"])
        link_and_run(lfortran, cwd, flags, "12")
    with tempfile.TemporaryDirectory(dir=workdir) as cwd:
        write(cwd, "m.f90", MODULE)
        write(cwd, "s.f90", SUBMODULE_VARS.format(value=1))
        write(cwd, "p.f90", PROGRAM)
        compile(lfortran, cwd, flags, ["m", "p", "s"])
        link_and_run(lfortran, cwd, flags, "1143")
    with tempfile.TemporaryDirectory(dir=workdir) as cwd:
        write(cwd, "m.f90", MODULE_PRIVATE)
        write(cwd, "s.f90", SUBMODULE_PRIVATE.format(value=1))
        write(cwd, "p.f90", PROGRAM)
        compile(lfortran, cwd, flags, ["m", "p", "s"])
        link_and_run(lfortran, cwd, flags, "101")
        write(cwd, "s.f90", SUBMODULE_PRIVATE.format(value=2))
        compile(lfortran, cwd, flags, ["s"])
        link_and_run(lfortran, cwd, flags, "102")
    # A module only the submodule uses: the program is compiled before the
    # submodule, and then the module alone is edited and it and the
    # submodule, which depends on its module file, are recompiled.
    rmsp = ("r", "m", "s", "p")
    for order in [["r", "m", "p", "s"], ["r", "m", "s", "p"]]:
        with tempfile.TemporaryDirectory(dir=workdir) as cwd:
            write(cwd, "r.f90", MODULE_R.format(value=1))
            write(cwd, "m.f90", MODULE_PARENT)
            write(cwd, "s.f90", SUBMODULE_R)
            write(cwd, "p.f90", PROGRAM)
            compile(lfortran, cwd, flags, order)
            link_and_run(lfortran, cwd, flags, "41", rmsp)
            write(cwd, "r.f90", MODULE_R.format(value=2))
            compile(lfortran, cwd, flags, ["r", "s"])
            link_and_run(lfortran, cwd, flags, "42", rmsp)
    # The program uses that module as well, so its object file defines the
    # module too.
    for order in [["r", "m", "p", "s"], ["r", "m", "s", "p"]]:
        with tempfile.TemporaryDirectory(dir=workdir) as cwd:
            write(cwd, "r.f90", MODULE_R.format(value=1))
            write(cwd, "m.f90", MODULE_PARENT)
            write(cwd, "s.f90", SUBMODULE_R)
            write(cwd, "p.f90", PROGRAM_R)
            compile(lfortran, cwd, flags, order)
            link_and_run(lfortran, cwd, flags, "41 22", rmsp)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--workdir", required=True)
    args = parser.parse_args()
    lfortran = os.path.abspath(args.lfortran)
    os.makedirs(args.workdir, exist_ok=True)
    for flags in [[], ["--fast"]]:
        check(lfortran, args.workdir, flags)
    print("ok")


if __name__ == "__main__":
    main()
