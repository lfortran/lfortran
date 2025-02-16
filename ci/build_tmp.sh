#!/usr/bin/env shell

# A platform independent Shell script to build LFortran. Works on Linux, macOS
# and Windows. The prerequisites such as bison, re2c or cython must be already
# installed.

set -ex

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
# Generate the intrinsic_function_registry_util.h (C++)
python src/libasr/intrinsic_func_registry_util_gen.py

# Generate the tokenizer and parser
cd src/lfortran/parser
re2c -W -b tokenizer.re -o tokenizer.cpp
re2c -W -b preprocessor.re -o preprocessor.cpp
bison -Wall -d -r all parser.yy
cd ../../..

pandoc --standalone --to man doc/man/lfortran.md -o doc/man/lfortran.1

lfortran_version=$(cat version)
dest="lfortran-" + $lfortran_version
bash ci/create_source_tarball0.sh
tar xzf dist/lfortran-$lfortran_version.tar.gz
cd lfortran-$lfortran_version

mkdir test-bld
cd test-bld
# Note: we have to build in Release mode on Windows, because `llvmdev` is
# compiled in Release mode and we get link failures if we mix and match build
# modes:
if [[ $IS_LINUX == "1" ]]; then
    BUILD_TYPE="Debug"
else
    BUILD_TYPE="Release"
fi
cmake -G$LFORTRAN_CMAKE_GENERATOR -DCMAKE_VERBOSE_MAKEFILE=ON -DWITH_LSP=yes -DWITH_LLVM=yes -DWITH_XEUS=yes -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$CONDA_PREFIX -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DWITH_RUNTIME_STACKTRACE=$ENABLE_RUNTIME_STACKTRACE ..
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
if [[ $IS_WIN == "1" ]]; then
    cp lfortran-$lfortran_version/test-bld/src/runtime/legacy/lfortran_runtime* src/runtime/
else
    cp lfortran-$lfortran_version/test-bld/src/runtime/liblfortran_runtime* src/runtime/
fi
cp lfortran-$lfortran_version/test-bld/src/runtime/*.mod src/runtime/
