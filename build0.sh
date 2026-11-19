#!/usr/bin/env bash

# CMake runs this with the Prefix `shell` (Pixi and CI), or with Bash when
# `shell` is not installed; keep to syntax that both support.

set -e
set -x

RE2C=${RE2C:-re2c}
PYTHON=${PYTHON:-python}

# Generate the `version` file (sourced: Windows cannot run the script directly)
source ci/version.sh

# Generate a Fortran AST from AST.asdl (C++)
"${PYTHON}" src/libasr/asdl_cpp.py grammar/AST.asdl src/lfortran/ast.h
# Generate a Fortran ASR from ASR.asdl (C++)
"${PYTHON}" src/libasr/asdl_cpp.py src/libasr/ASR.asdl src/libasr/asr.h
# Generate a wasm_visitor.h from src/libasr/wasm_instructions.txt (C++)
"${PYTHON}" src/libasr/wasm_instructions_visitor.py
# Generate the intrinsic_function_registry_util.h (C++)
"${PYTHON}" src/libasr/intrinsic_func_registry_util_gen.py

# Generate the tokenizer and parser
(cd src/lfortran && "${RE2C}" -W -b parser/tokenizer.re -o parser/tokenizer.cpp)
(cd src/lfortran && "${RE2C}" -W -b parser/preprocessor.re -o parser/preprocessor.cpp)
# Generate the rules of the parser and the tokens from the grammar
"${PYTHON}" src/lfortran/parser/llgen.py src/lfortran/parser/ll_parser.ll src/lfortran/parser/ll_parser_rules.h src/lfortran/parser/tokens.h

# Generate the LSP sources
"${PYTHON}" src/server/generator/generate_lsp_code.py --schema src/server/generator/metaModel.json --target-language c++ --output-dir src/server

echo "OK"
