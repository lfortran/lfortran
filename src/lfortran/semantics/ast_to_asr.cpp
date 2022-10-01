#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <cmath>

#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <lfortran/semantics/asr_implicit_cast_rules.h>
#include <lfortran/semantics/ast_common_visitor.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <lfortran/parser/parser_stype.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>


namespace LFortran {

Result<ASR::asr_t*> symbol_table_visitor(Allocator &al, AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        SymbolTable *symbol_table,
        CompilerOptions &compiler_options,
        std::map<std::string, std::vector<ASR::asr_t*>>& template_type_parameters,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping);

Result<ASR::TranslationUnit_t*> body_visitor(Allocator &al,
        AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        ASR::asr_t *unit,
        CompilerOptions &compiler_options,
        std::map<std::string, std::vector<ASR::asr_t*>>& template_type_parameters);

Result<ASR::TranslationUnit_t*> ast_to_asr(Allocator &al,
    AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics,
    SymbolTable *symbol_table, bool symtab_only,
    CompilerOptions &compiler_options)
{
    std::map<std::string, std::vector<ASR::asr_t*>> template_type_parameters;
    std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> implicit_mapping;
    ASR::asr_t *unit;
    auto res = symbol_table_visitor(al, ast, diagnostics, symbol_table,
        compiler_options, template_type_parameters, implicit_mapping);
    if (res.ok) {
        unit = res.result;
    } else {
        return res.error;
    }

    ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(unit);
    LFORTRAN_ASSERT(asr_verify(*tu));

    if (!symtab_only) {
        auto res = body_visitor(al, ast, diagnostics, unit, compiler_options, template_type_parameters);
        if (res.ok) {
            tu = res.result;
        } else {
            return res.error;
        }
        LFORTRAN_ASSERT(asr_verify(*tu));
    }
    return tu;
}

} // namespace LFortran
