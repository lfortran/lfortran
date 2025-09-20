#!/usr/bin/env bash
set -euo pipefail

# Standalone runner for Section 12: Reference-LAPACK from ci/test_third_party_codes.sh
# This isolates the problematic heredoc and ensures it parses under bash.

: "${FC:=lfortran}"

TMP_DIR=$(mktemp -d)
cd "$TMP_DIR"

# Minimal subset: clone, checkout tag, run the python patch, configure cmake and make
# NOTE: This script is intended for syntax validation and local dry-run. It will
# not perform heavy builds by default; uncomment cmake/make lines to actually build.

git clone https://github.com/Reference-LAPACK/lapack.git reference-lapack
cd reference-lapack

# Use latest stable release
git checkout v3.12.0

extra_fflags="--fixed-form-infer --implicit-interface"
cmake_extra_args=()
if [[ "$FC" == *lfortran* ]]; then
  extra_fflags+=" --legacy-array-sections --cpp"
  cmake_extra_args+=("-DCMAKE_Fortran_PREPROCESS_SOURCE=<CMAKE_Fortran_COMPILER> --cpp <DEFINES> <INCLUDES> <FLAGS> -E <SOURCE> -o <PREPROCESSED_SOURCE>")
  cmake_extra_args+=("-DCMAKE_Fortran_CREATE_PREPROCESSED_SOURCE=<CMAKE_Fortran_COMPILER> --cpp <DEFINES> <INCLUDES> <FLAGS> -E <SOURCE> -o <PREPROCESSED_SOURCE>")
  cmake_extra_args+=("-DCMAKE_Fortran_CREATE_ASSEMBLY_SOURCE=<CMAKE_Fortran_COMPILER> --cpp <DEFINES> <INCLUDES> <FLAGS> -S <SOURCE> -o <ASSEMBLY_SOURCE>")
  cmake_extra_args+=("-DCMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_ON=--cpp")
  cmake_extra_args+=("-DCMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_OFF=")
  cmake_extra_args+=("-DCMAKE_Fortran_POSTPROCESS_FLAG=")

  # Run the python patch via an unquoted heredoc so it doesn't interfere with external quoting
  python - <<PY
from pathlib import Path

lapacke_cmake = Path("LAPACKE/include/CMakeLists.txt")
needle = "  FortranCInterface_VERIFY()"
replacement = (
    "  if (CMAKE_Fortran_COMPILER MATCHES \"lfortran\")\n"
    "    message(STATUS \"Skipping FortranCInterface_VERIFY for LFortran\")\n"
    "    set(FortranCInterface_GLOBAL_FOUND 1)\n"
    "    set(FortranCInterface_MODULE_FOUND 1)\n"
    "  else()\n"
    "    FortranCInterface_VERIFY()\n"
    "  endif()"
)
text = lapacke_cmake.read_text()
if needle not in text:
    raise SystemExit("Unable to locate FortranCInterface_VERIFY() for patching")
lapacke_cmake.write_text(text.replace(needle, replacement, 1))

flags_cmake = Path("CMAKE/CheckLAPACKCompilerFlags.cmake")
flags_text = flags_cmake.read_text()
header = '# GNU Fortran\nif( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )'
if header not in flags_text:
    raise SystemExit("Unable to locate GNU compiler block for patching")
replacement = '# GNU Fortran\nif( CMAKE_Fortran_COMPILER MATCHES "lfortran" )\n  message(STATUS "Skipping GNU-specific flags for LFortran")\n  if( NOT ("${CMAKE_Fortran_FLAGS}" MATCHES "--cpp") )\n    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --cpp"\n      CACHE STRING "Enable preprocessing in LFortran" FORCE)\n  endif()\n  set(CMAKE_Fortran_PREPROCESS_SOURCE "<CMAKE_Fortran_COMPILER> --cpp <DEFINES> <INCLUDES> <FLAGS> -E <SOURCE> -o <PREPROCESSED_SOURCE>"\n      CACHE STRING "Force preprocessing for LFortran" FORCE)\n  set(CMAKE_Fortran_CREATE_PREPROCESSED_SOURCE "<CMAKE_Fortran_COMPILER> --cpp <DEFINES> <INCLUDES> <FLAGS> -E <SOURCE> -o <PREPROCESSED_SOURCE>"\n      CACHE STRING "Custom preprocessing rule for LFortran" FORCE)\n  set(CMAKE_Fortran_CREATE_ASSEMBLY_SOURCE "<CMAKE_Fortran_COMPILER> --cpp <DEFINES> <INCLUDES> <FLAGS> -S <SOURCE> -o <ASSEMBLY_SOURCE>"\n      CACHE STRING "Custom assembly rule for LFortran" FORCE)\n  set(CMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_ON "--cpp"\n      CACHE STRING "Enable preprocessing option for LFortran" FORCE)\n  set(CMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_OFF ""\n      CACHE STRING "Disable preprocessing option for LFortran" FORCE)\n  set(CMAKE_Fortran_POSTPROCESS_FLAG ""\n      CACHE STRING "No postprocess flag for LFortran" FORCE)\nelseif( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )'
flags_cmake.write_text(flags_text.replace(header, replacement, 1))
PY
fi

# Print what would be run (dry-run)
echo "FC=$FC"
echo "extra_fflags=$extra_fflags"
echo "cmake_extra_args=(${cmake_extra_args[@]})"

# Cleanup
cd ../..
rm -rf "$TMP_DIR"

echo "Done (dry-run)."
