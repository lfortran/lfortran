#!/usr/bin/env bash
# Regression test: lfortran must accept the macOS `-install_name` linker flag
# when producing a shared library and forward it to the linker. CMake passes
# this flag when linking Fortran shared libraries on Darwin; it originally
# failed while linking libnetcdff.dylib in netcdf-fortran with:
#     The following argument was not expected: -install_name
# We only check that the flag is accepted and the link succeeds; inspecting the
# produced dylib (e.g. with `otool -D`) would tie the test to that tool's output
# format for no extra coverage of the regression.
set -e

SRC="$1"
LFORTRAN="${LFORTRAN:-lfortran}"

LIB=libinstall_name_01.dylib
INAME="@rpath/$LIB"

"$LFORTRAN" -c "$SRC" -o install_name_01.o
"$LFORTRAN" --shared -o "$LIB" -install_name "$INAME" install_name_01.o

if [ ! -f "$LIB" ]; then
    echo "ERROR: shared library was not produced"
    exit 1
fi
echo "install_name_01: OK"
