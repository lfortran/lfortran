#include <fstream>
#include <map>
#include <string>
#include <cmath>

#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <lfortran/semantics/asr_implicit_cast_rules.h>
#include <lfortran/semantics/ast_common_visitor.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <lfortran/semantics/comptime_eval.h>
#include <lfortran/parser/parser_stype.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/codegen/asr_to_fortran.h>
#include <libasr/pickle.h>

namespace LCompilers::LFortran {

Result<ASR::asr_t*> symbol_table_visitor(Allocator &al, AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        SymbolTable *symbol_table,
        CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::vector<ASR::stmt_t*> &data_structure,
        LCompilers::LocationManager &lm);

Result<ASR::TranslationUnit_t*> body_visitor(Allocator &al,
        AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        ASR::asr_t *unit,
        CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::vector<ASR::stmt_t*> &data_structure,
        LCompilers::LocationManager &lm);

void load_rtlib() {
    const std::string m_builtin = "lfortran_intrinsic_builtin";
    const std::string m_ieee_arithmetic = "lfortran_intrinsic_ieee_arithmetic";
}

Result<ASR::TranslationUnit_t*> ast_to_asr(Allocator &al,
    AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics,
    SymbolTable *symbol_table, bool symtab_only,
    CompilerOptions &compiler_options, LCompilers::LocationManager &lm)
{
    std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> implicit_mapping;
    std::map<uint64_t, ASR::symbol_t*> common_variables_hash;
    std::map<uint64_t, std::vector<std::string>> external_procedures_mapping;
    std::map<uint64_t, std::vector<std::string>> explicit_intrinsic_procedures_mapping;
    std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> instantiate_types;
    std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> instantiate_symbols;
    std::map<std::string, std::map<std::string, std::vector<AST::stmt_t*>>> entry_functions;
    std::map<std::string, std::vector<int>> entry_function_arguments_mapping;
    std::vector<ASR::stmt_t*> data_structure;
    ASR::asr_t *unit;
    auto res = symbol_table_visitor(al, ast, diagnostics, symbol_table,
        compiler_options, implicit_mapping, common_variables_hash, external_procedures_mapping,
        explicit_intrinsic_procedures_mapping, instantiate_types, instantiate_symbols, entry_functions,
        entry_function_arguments_mapping, data_structure, lm);
    if (res.ok) {
        unit = res.result;
    } else {
        return res.error;
    }
    ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(unit);
    if (compiler_options.po.dump_all_passes) {
        std::ofstream outfile ("pass_00_initial_asr_01.clj");
        outfile << ";; ASR after SymbolTable Visitor\n" << LCompilers::pickle(*tu, false, true, compiler_options.po.with_intrinsic_mods) << "\n";
        outfile.close();
    }
    if (compiler_options.po.dump_fortran) {
        LCompilers::Result<std::string> fortran_code = LCompilers::asr_to_fortran(*tu, diagnostics, false, 4);
        if (!fortran_code.ok) {
            LCOMPILERS_ASSERT(diagnostics.has_error());
            throw LCompilersException("Fortran code could not be generated after symbol_table_visitor");
        }
        std::ofstream outfile ("pass_fortran_00_initial_code_01.f90");
        outfile << "! Fortran code after SymbolTable Visitor\n" << fortran_code.result << "\n";
        outfile.close();
    }
#if defined(WITH_LFORTRAN_ASSERT)
    if (!asr_verify(*tu, true, diagnostics)) {
        return Error();
    };
#endif
    if (!symtab_only) {
        auto res = body_visitor(
            al, ast, diagnostics, unit, compiler_options,
            implicit_mapping, common_variables_hash, external_procedures_mapping,
            explicit_intrinsic_procedures_mapping, instantiate_types,
            instantiate_symbols, entry_functions, entry_function_arguments_mapping,
            data_structure, lm
        );
        if (res.ok) {
            tu = res.result;
        } else {
            return res.error;
        }
        if (compiler_options.rtlib) load_rtlib();
        if (compiler_options.po.dump_all_passes) {
            std::ofstream outfile ("pass_00_initial_asr_02.clj");
            outfile << ";; Initial ASR after Body Visitor\n" << LCompilers::pickle(*tu, false, true, compiler_options.po.with_intrinsic_mods) << "\n";
            outfile.close();
        }
        if (compiler_options.po.dump_fortran) {
            LCompilers::Result<std::string> fortran_code = LCompilers::asr_to_fortran(*tu, diagnostics, false, 4);
            if (!fortran_code.ok) {
                LCOMPILERS_ASSERT(diagnostics.has_error());
                throw LCompilersException("Fortran code could not be generated after body_visitor");
            }
            std::ofstream outfile ("pass_fortran_00_initial_code_02.f90");
            outfile << "! Fortran code after Body Visitor\n" << fortran_code.result << "\n";
            outfile.close();
        }
#if defined(WITH_LFORTRAN_ASSERT)
        if (!asr_verify(*tu, true, diagnostics)) {
            return Error();
        };
#endif
    }
    return tu;
}

} // namespace LCompilers::LFortran
