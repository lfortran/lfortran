#!/usr/bin/env python3
"""Check that `--continue-compilation` writes no output for invalid code.

The errors must all be reported and the compiler must exit with a nonzero
status, without writing an object file, module files or Python wrapper files
(#14800, #14572). Valid code must still produce its output.
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

# The parser recovers from the syntax error, so the semantic error must
# still be reported.
SYNTAX_AND_SEMANTIC = """\
program cc_output_syntax
implicit none
integer :: i
i = "a"
print *, 1 +
end program
"""

VALID = """\
module cc_output_ok
implicit none
contains
subroutine s()
integer :: i
i = 1
end subroutine
end module
"""

SEMANTIC_ERRORS = [
    "Type mismatch in assignment",
    "Variable 'undeclared_x' is not declared",
]

SYNTAX_AND_SEMANTIC_ERRORS = [
    "syntax error: Newline is unexpected here",
    "Type mismatch in assignment",
]


def check(lfortran, workdir, name, source, args, outputs=None, errors=()):
    """Run `lfortran args` on `source` saved as `name`.f90.

    For invalid code (`outputs` is None) the compiler must fail, report all
    `errors` and write no files. For valid code it must succeed and write the
    files in `outputs`.
    """
    with tempfile.TemporaryDirectory(dir=workdir) as cwd:
        src = name + ".f90"
        with open(os.path.join(cwd, src), "w") as f:
            f.write(source)
        cmd = [lfortran, "--no-color"] + [a.format(name=name) for a in args]
        p = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, universal_newlines=True)
        failures = []
        written = set(os.listdir(cwd)) - {src}
        if outputs is None:
            if p.returncode == 0:
                failures.append("exited with status 0")
            for error in errors:
                if error not in p.stdout:
                    failures.append("did not report %r" % error)
            if written:
                failures.append("wrote %s" % ", ".join(sorted(written)))
        else:
            if p.returncode != 0:
                failures.append("exited with status %d" % p.returncode)
            missing = [o.format(name=name) for o in outputs]
            missing = sorted(set(missing) - written)
            if missing:
                failures.append("did not write %s" % ", ".join(missing))
        if failures:
            return ["%s: %s\n%s" % (" ".join(cmd), "; ".join(failures),
                p.stdout)]
        return []


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lfortran", required=True)
    parser.add_argument("--workdir", required=True)
    parser.add_argument("--mlir", action="store_true",
        help="LFortran is built with the MLIR backend")
    args = parser.parse_args()
    lfortran = os.path.abspath(args.lfortran)
    os.makedirs(args.workdir, exist_ok=True)
    backends = ["llvm", "c", "cpp", "x86", "wasm", "fortran"]
    if args.mlir:
        backends.append("mlir")
    cc = ["--continue-compilation"]
    pywrap = cc + ["pywrap", "{name}.f90"]
    invalid = [
        ("program", PROGRAM, SEMANTIC_ERRORS),
        ("modules_only", MODULES_ONLY, SEMANTIC_ERRORS),
        ("syntax", SYNTAX_AND_SEMANTIC, SYNTAX_AND_SEMANTIC_ERRORS),
    ]
    failures = []
    for backend in backends:
        compile_ = cc + ["--backend=" + backend, "-c", "{name}.f90",
            "-o", "{name}.o"]
        for name, source, errors in invalid:
            failures += check(lfortran, args.workdir, name, source, compile_,
                errors=errors)
    for name, source, errors in invalid:
        failures += check(lfortran, args.workdir, name, source, pywrap,
            errors=errors)
    pywrap_outputs = ["{name}.h", "{name}_pxd.pxd", "{name}.pyx"]
    failures += check(lfortran, args.workdir, "valid", VALID, pywrap,
        pywrap_outputs)
    for backend in ["llvm"] + (["mlir"] if args.mlir else []):
        compile_ = cc + ["--backend=" + backend, "-c", "{name}.f90",
            "-o", "{name}.o"]
        failures += check(lfortran, args.workdir, "valid", VALID, compile_,
            ["{name}.o"])
    if failures:
        sys.exit("\n".join(failures))
    print("ok")


if __name__ == "__main__":
    main()
