#!/usr/bin/env bash
# The program object must not define a module procedure the program never
# calls. `unused_functions` removes that procedure later, so this skips it:
# on main the object still defines `unused`, and this test fails.
set -eu
LF=${1:?compiler}
SRC=${2:?source directory}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"
"$LF" -c "$SRC/reachable_proc_symbols_mod.f90" -o mod.o
"$LF" -c "$SRC/reachable_proc_symbols.f90" --skip-pass=unused_functions -o prog.o
if ! nm prog.o | grep -q ' T __module_reachable_proc_symbols_mod_used'; then
    echo "the reached procedure is missing from the program object"
    nm prog.o
    exit 1
fi
if nm prog.o | grep -q ' T __module_reachable_proc_symbols_mod_unused'; then
    echo "unreached procedure unused is defined in the program object"
    nm prog.o | grep unused
    exit 1
fi
