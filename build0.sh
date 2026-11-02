#!/usr/bin/env bash

# CMake runs this with the Prefix `shell` (Pixi and CI), or with Bash when
# `shell` is not installed; keep to syntax that both support.

set -e
set -x

RE2C=${RE2C:-re2c}
BISON=${BISON:-bison}

# Generate the `version` file (sourced: Windows cannot run the script directly)
source ci/version.sh

# Generate a Fortran AST from AST.asdl (C++)
python src/libasr/asdl_cpp.py grammar/AST.asdl src/lfortran/ast.h
# Generate a Fortran ASR from ASR.asdl (C++)
python src/libasr/asdl_cpp.py src/libasr/ASR.asdl src/libasr/asr.h
# Generate a wasm_visitor.h from src/libasr/wasm_instructions.txt (C++)
python src/libasr/wasm_instructions_visitor.py
# Generate the intrinsic_function_registry_util.h (C++)
python src/libasr/intrinsic_func_registry_util_gen.py

# Generate the tokenizer and parser
(cd src/lfortran && "${RE2C}" -W -b parser/tokenizer.re -o parser/tokenizer.cpp)
(cd src/lfortran && "${RE2C}" -W -b parser/preprocessor.re -o parser/preprocessor.cpp)
# Use `-r all` in Bison for detailed report. Turned off by default to avoid a
# Bison timeout issue with it.
(cd src/lfortran/parser && "${BISON}" -Wall -d parser.yy)

# Generate the LSP sources
python src/server/generator/generate_lsp_code.py --schema src/server/generator/metaModel.json --target-language c++ --output-dir src/server

# Single quotes are not allowed in parser.yy (`shell` has no `grep`)
python -c 'import sys; bad = [f"{i}: {l}" for i, l in enumerate(open("src/lfortran/parser/parser.yy", encoding="utf-8"), 1) if chr(39) in l]; sys.exit("".join(bad) + "Single quote not allowed" if bad else 0)'
echo "OK"
