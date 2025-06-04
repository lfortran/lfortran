#!/usr/bin/env shell

# using debugging option i.e. `-x` causes a bug here, and hence is currently turned off
set -e

echo "Running SHELL"

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
echo $(pwd)
cd src/lfortran/parser && re2c -W -b tokenizer.re -o tokenizer.cpp && cd ../../..
cd src/lfortran/parser && re2c -W -b preprocessor.re -o preprocessor.cpp && cd ../../..
cd src/lfortran/parser && bison -Wall -d -r all parser.yy && cd ../../..

# Generate the LSP sources
python src/server/generator/generate_lsp_code.py --schema src/server/generator/metaModel.json --target-language c++ --output-dir src/server

pandoc --standalone --to man doc/man/lfortran.md -o doc/man/lfortran.1

lfortran_version=$(cat version)
bash ci/create_source_tarball.sh "$lfortran_version"
tar xzf dist/lfortran-$lfortran_version.tar.gz
cd lfortran-$lfortran_version

mkdir test-bld
cd test-bld
# Note: we have to build in Release mode on Windows, because `llvmdev` is
# compiled in Release mode and we get link failures if we mix and match build
# modes:
if [[ $WIN == "1" ]]; then # Windows
    BUILD_TYPE="Release"
else # Linux or macOS
    BUILD_TYPE="Debug"
fi

cmake -G$LFORTRAN_CMAKE_GENERATOR -DCMAKE_VERBOSE_MAKEFILE=ON -DWITH_LSP=yes -DWITH_LLVM=yes -DWITH_XEUS=yes -DCMAKE_PREFIX_PATH=$CONDA_PREFIX -DCMAKE_INSTALL_PREFIX=$CONDA_PREFIX -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DWITH_RUNTIME_STACKTRACE=$ENABLE_RUNTIME_STACKTRACE ..
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

if [[ $WIN == "1" ]]; then # Windows
    echo "We are here1"
    LFORTRAN_BINARY="lfortran-$lfortran_version/test-bld/src/bin/lfortran.exe"
    echo "We are here2"
    if [[ -f "$LFORTRAN_BINARY" ]]; then
        echo "Binary $LFORTRAN_BINARY found."
    else
        echo "Error: Binary $LFORTRAN_BINARY not found."
    fi
    # echo "Listing files in lfortran-$lfortran_version/test-bld/src/bin:"
    # dir "lfortran-$lfortran_version\test-bld\src\bin"
fi

cp lfortran-$lfortran_version/test-bld/src/bin/lfortran src/bin
if [[ $WIN == "1" ]]; then # Windows
    cp lfortran-$lfortran_version/test-bld/src/runtime/legacy/lfortran_runtime* src/runtime/
else # Linux or macOS
    cp lfortran-$lfortran_version/test-bld/src/runtime/liblfortran_runtime* src/runtime/
fi
cp lfortran-$lfortran_version/test-bld/src/runtime/*.mod src/runtime/
