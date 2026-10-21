#!/usr/bin/env bash

set -e
set -x

RE2C=${RE2C:-re2c}
BISON=${BISON:-bison}

# The generators below rewrite their output on every run, and CMake runs this
# script on every (re)configure. Remember the generated sources, so that the
# ones that come out byte-identical keep their old mtime and do not trigger a
# rebuild of everything that includes them.
mtime_state=$(mktemp "${TMPDIR:-/tmp}/lfortran-mtime.XXXXXX")
python ci/keep_unchanged_mtime.py save "$mtime_state" src version

# Generate the `version` file
ci/version.sh

# Generate a Fortran AST from AST.asdl (C++)
python src/libasr/asdl_cpp.py grammar/AST.asdl src/lfortran/ast.h
# Generate a Fortran ASR from ASR.asdl (C++)
python src/libasr/asdl_cpp.py src/libasr/ASR.asdl src/libasr/asr.h
# Generate a wasm_visitor.h from src/libasr/wasm_instructions.txt (C++)
python src/libasr/wasm_instructions_visitor.py
# Generate the intrinsic_function_registry_util.h (C++)
python src/libasr/intrinsic_func_registry_util_gen.py

# Generate the tokenizer and parser
(cd src/lfortran && ${RE2C} -W -b --no-generation-date parser/tokenizer.re -o parser/tokenizer.cpp)
(cd src/lfortran && ${RE2C} -W -b --no-generation-date parser/preprocessor.re -o parser/preprocessor.cpp)
# Use `-r all` in Bison for detailed report. Turned off by default to avoid a
# Bison timeout issue with it.
(cd src/lfortran/parser && ${BISON} -Wall -d parser.yy)

# Generate the LSP sources
python src/server/generator/generate_lsp_code.py --schema src/server/generator/metaModel.json --target-language c++ --output-dir src/server

python ci/keep_unchanged_mtime.py restore "$mtime_state"
rm -f "$mtime_state"

grep -n "'" src/lfortran/parser/parser.yy && echo "Single quote not allowed" && exit 1
echo "OK"
