#!/usr/bin/env xonsh

#bash ci/version.sh
version=$(git describe --tags --dirty).strip()[1:]
echo @(version) > version

python src/libasr/asdl_cpp.py grammar/AST.asdl src/lfortran/ast.h
python src/libasr/asdl_cpp.py src/libasr/ASR.asdl src/libasr/asr.h
pushd src/lfortran/parser && re2c -W -b tokenizer.re -o tokenizer.cpp && popd
pushd src/lfortran/parser && re2c -W -b preprocessor.re -o preprocessor.cpp && popd
python src/lfortran/parser/llgen.py src/lfortran/parser/ll_parser.ll src/lfortran/parser/ll_parser_rules.h src/lfortran/parser/tokens.h
python src/libasr/wasm_instructions_visitor.py
python src/libasr/intrinsic_func_registry_util_gen.py
python src/server/generator/generate_lsp_code.py --schema src/server/generator/metaModel.json --target-language c++ --output-dir src/server
