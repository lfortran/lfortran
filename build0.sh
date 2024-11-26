#!/usr/bin/env bash

# Generate the `version` file
shell ci/version.sh

# Generate a Fortran AST from AST.asdl (C++)
python src/libasr/asdl_cpp.py grammar/AST.asdl src/lfortran/ast.h
# Generate a Fortran ASR from ASR.asdl (C++)
python src/libasr/asdl_cpp.py src/libasr/ASR.asdl src/libasr/asr.h
# Generate a wasm_visitor.h from src/libasr/wasm_instructions.txt (C++)
python src/libasr/wasm_instructions_visitor.py
# Generate the intrinsic_function_registry_util.h (C++)
python src/libasr/intrinsic_func_registry_util_gen.py

# Generate the tokenizer and parser
(cd src/lfortran && re2c -W -b parser/tokenizer.re -o parser/tokenizer.cpp)
(cd src/lfortran && re2c -W -b parser/preprocessor.re -o parser/preprocessor.cpp)
(cd src/lfortran/parser && bison -Wall -d -r all parser.yy)

#grep -n "'" src/lfortran/parser/parser.yy && echo "Single quote not allowed" && exit 1
echo "OK"
