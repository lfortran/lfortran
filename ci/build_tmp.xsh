#!/usr/bin/env xonsh

# A platform independent Xonsh script to build LFortran. Works on Linux, macOS
# and Windows. The prerequisites such as bison, re2c or cython must be already
# installed.
#
# Some issues (with Xonsh):
#
# * Some output seems to be lost on Windows
#   (https://github.com/xonsh/xonsh/issues/3291)
#
# * The commands that are executed are sometimes printed after the output,
#   which makes the log unreadable (https://github.com/xonsh/xonsh/issues/3293)
#
# * This script is too slow (due to https://github.com/xonsh/xonsh/issues/3064)
#   to be suitable for day to day development, but on the CI the few extra
#   seconds do not matter much
#
# * One must be careful to ensure this Xonsh script is in the path. If Xonsh
#   cannot find the script, it will return success, making the CI tests
#   "succeed" (https://github.com/xonsh/xonsh/issues/3292)

$RAISE_SUBPROC_ERROR = True
trace on

import platform
$IS_MAC = platform.system() == "Darwin"
$IS_WIN = platform.system() == "Windows"
$IS_LINUX = platform.system() == "Linux"

echo "CONDA_PREFIX=$CONDA_PREFIX"
llvm-config --components

# Generate the `version` file
bash ci/version.sh

# Generate a Fortran AST from AST.asdl (C++)
python src/libasr/asdl_cpp.py grammar/AST.asdl src/lfortran/ast.h
# Generate a Fortran ASR from ASR.asdl (C++)
python src/libasr/asdl_cpp.py src/libasr/ASR.asdl src/libasr/asr.h
# Generate a wasm_visitor.h from src/libasr/wasm_instructions.txt (C++)
python src/libasr/wasm_instructions_visitor.py

# Generate the tokenizer and parser
pushd src/lfortran/parser && re2c -W -b tokenizer.re -o tokenizer.cpp && popd
pushd src/lfortran/parser && re2c -W -b preprocessor.re -o preprocessor.cpp && popd
pushd src/lfortran/parser && bison -Wall -d -r all parser.yy && popd

$lfortran_version=$(cat version).strip()
$dest="lfortran-" + $lfortran_version
bash ci/create_source_tarball0.sh
tar xzf dist/lfortran-$lfortran_version.tar.gz
cd lfortran-$lfortran_version

mkdir test-bld
cd test-bld
# Note: we have to build in Release mode on Windows, because `llvmdev` is
# compiled in Release mode and we get link failures if we mix and match build
# modes:
if $IS_LINUX:
    BUILD_TYPE = "Debug"
else:
    BUILD_TYPE = "Release"
cmake -G$LFORTRAN_CMAKE_GENERATOR -DCMAKE_VERBOSE_MAKEFILE=ON -DWITH_LLVM=yes -DWITH_XEUS=yes -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$CONDA_PREFIX -DCMAKE_BUILD_TYPE=@(BUILD_TYPE) ..
cmake --build . --target install
./src/lfortran/tests/test_lfortran
./src/bin/lfortran < ../src/bin/example_input.txt
ctest --output-on-failure
cpack -V
cd ../..

jupyter kernelspec list --json
#python ci/test_fortran_kernel.py -v
#
cd share/lfortran/nb
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output Demo1_out.ipynb Demo1.ipynb
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output Demo2_out.ipynb Demo2.ipynb
cat Demo1_out.ipynb
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output "Hello World_out.ipynb" "Hello World.ipynb"
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output "Operators Control Flow_out.ipynb" "Operators Control Flow.ipynb"
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output Variables_out.ipynb Variables.ipynb
cd ../../..

cp lfortran-$lfortran_version/test-bld/src/bin/lfortran src/bin
cp lfortran-$lfortran_version/test-bld/src/bin/cpptranslate src/bin
if $IS_WIN:
    cp lfortran-$lfortran_version/test-bld/src/runtime/legacy/lfortran_runtime* src/runtime/
else:
    cp lfortran-$lfortran_version/test-bld/src/runtime/liblfortran_runtime* src/runtime/
cp lfortran-$lfortran_version/test-bld/src/runtime/*.mod src/runtime/
