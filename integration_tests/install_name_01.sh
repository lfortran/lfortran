#!/usr/bin/env bash
# Regression test: lfortran must accept the macOS `-install_name` linker flag
# when producing a shared library and forward it to the linker, so the dylib's
# install name is set as requested. CMake passes this flag when linking Fortran
# shared libraries on Darwin; it originally failed while linking
# libnetcdff.dylib in netcdf-fortran with:
#     The following argument was not expected: -install_name
set -e

SRC="$1"
LFORTRAN="${LFORTRAN:-lfortran}"

LIB=libinstall_name_01.dylib
INAME="@rpath/$LIB"

"$LFORTRAN" -c "$SRC" -o install_name_01.o
"$LFORTRAN" --shared -o "$LIB" -install_name "$INAME" install_name_01.o

got=$(otool -D "$LIB" | tail -1 | tr -d '[:space:]')
echo "requested install name: $INAME"
echo "actual install name:    $got"
if [ "$got" != "$INAME" ]; then
    echo "ERROR: install name was not set to the requested value"
    exit 1
fi
echo "install_name_01: OK"
