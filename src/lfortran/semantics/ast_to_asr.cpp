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
        std::map<uint64_t, size_t>& common_variables_byte_offset,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::decl_stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::map<uint32_t, std::vector<ASR::stmt_t*>> &data_structure,
        LCompilers::LocationManager &lm);

Result<ASR::TranslationUnit_t*> body_visitor(Allocator &al,
        AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        ASR::asr_t *unit,
        CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping,
        std::map<uint64_t, ASR::symbol_t*>& common_variables_hash,
        std::map<uint64_t, size_t>& common_variables_byte_offset,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::decl_stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::map<uint32_t, std::vector<ASR::stmt_t*>> &data_structure,
        LCompilers::LocationManager &lm);

/*
    Templates (generics) are a prototype of a feature proposed to the Fortran
    standard. They are not part of any standard yet, so they are disabled
    unless `--enable-experimental-feature templates` is passed.

    The gate lives here, in one place, right before AST -> ASR: the keywords
    involved (`template`, `requirement`, `require`, `instantiate`, `deferred`)
    are not reserved words in Fortran, so they cannot be rejected in the
    grammar without breaking programs that use them as ordinary identifiers.
    By the time the AST exists, a template construct has its own AST node (or
    a non-empty `temp_args` list), which is an exact and complete signal.
*/
class TemplateGateVisitor : public AST::BaseWalkVisitor<TemplateGateVisitor>
{
private:
    diag::Diagnostics &diag;
    bool reported = false;

    void report(const Location &loc) {
        if (reported) return;
        reported = true;
        // Point at the start of the construct only: a template can span many
        // lines and the whole span carries no extra information here.
        Location start_loc;
        start_loc.first = loc.first;
        start_loc.last = loc.first;
        diag.add(diag::Diagnostic(
            "templates are an experimental prototype of a proposed Fortran "
            "feature and are disabled by default",
            diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("pass `--enable-experimental-feature templates` "
                    "to enable them", {start_loc})}));
    }

public:

    TemplateGateVisitor(diag::Diagnostics &diag) : diag{diag} {}

    bool found_template() const { return reported; }

    // BaseWalkVisitor::visit_TranslationUnit is a no-op, walk the units here
    void visit_TranslationUnit(const AST::TranslationUnit_t &x) {
        for (size_t i = 0; i < x.n_items; i++) {
            visit_ast(*x.m_items[i]);
        }
    }

    void visit_Template(const AST::Template_t &x) {
        report(x.base.base.loc);
    }

    void visit_Requirement(const AST::Requirement_t &x) {
        report(x.base.base.loc);
    }

    void visit_Require(const AST::Require_t &x) {
        report(x.base.base.loc);
    }

    void visit_Instantiate(const AST::Instantiate_t &x) {
        report(x.base.base.loc);
    }

    void visit_DerivedType(const AST::DerivedType_t &x) {
        // `type, deferred :: T` declares a deferred (generic) type. The same
        // `deferred` attribute on a type bound procedure is standard Fortran
        // and is a different AST node, so it is not affected here.
        for (size_t i = 0; i < x.n_attrtype; i++) {
            if (AST::is_a<AST::SimpleAttribute_t>(*x.m_attrtype[i])) {
                AST::SimpleAttribute_t *a
                    = AST::down_cast<AST::SimpleAttribute_t>(x.m_attrtype[i]);
                if (a->m_attr == AST::simple_attributeType::AttrDeferred) {
                    report(x.base.base.loc);
                }
            }
        }
        AST::BaseWalkVisitor<TemplateGateVisitor>::visit_DerivedType(x);
    }

    void visit_Function(const AST::Function_t &x) {
        if (x.n_temp_args > 0) report(x.base.base.loc);
        AST::BaseWalkVisitor<TemplateGateVisitor>::visit_Function(x);
    }

    void visit_Subroutine(const AST::Subroutine_t &x) {
        if (x.n_temp_args > 0) report(x.base.base.loc);
        AST::BaseWalkVisitor<TemplateGateVisitor>::visit_Subroutine(x);
    }

    void visit_FuncCallOrArray(const AST::FuncCallOrArray_t &x) {
        // The `f{T, ...}(...)` instantiation call syntax
        if (x.n_temp_args > 0) report(x.base.base.loc);
        AST::BaseWalkVisitor<TemplateGateVisitor>::visit_FuncCallOrArray(x);
    }

    void visit_SubroutineCall(const AST::SubroutineCall_t &x) {
        // The `call s{T, ...}(...)` instantiation call syntax
        if (x.n_temp_args > 0) report(x.base.base.loc);
        AST::BaseWalkVisitor<TemplateGateVisitor>::visit_SubroutineCall(x);
    }
};

// Returns true if a template construct was used while templates are disabled
static bool check_experimental_templates(AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics, CompilerOptions &compiler_options) {
    if (compiler_options.experimental_templates) return false;
    TemplateGateVisitor v(diagnostics);
    v.visit_TranslationUnit(ast);
    return v.found_template();
}

void load_rtlib() {
    const std::string m_builtin = "lfortran_intrinsic_builtin";
    const std::string m_ieee_arithmetic = "lfortran_intrinsic_ieee_arithmetic";
}

Result<ASR::TranslationUnit_t*> ast_to_asr(Allocator &al,
    AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics,
    SymbolTable *symbol_table, bool symtab_only,
    CompilerOptions &compiler_options, LCompilers::LocationManager &lm)
{
    if (check_experimental_templates(ast, diagnostics, compiler_options)) {
        return Error();
    }
    std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> implicit_mapping;
    std::map<uint64_t, ASR::symbol_t*> common_variables_hash;
    std::map<uint64_t, size_t> common_variables_byte_offset;
    std::map<uint64_t, std::vector<std::string>> external_procedures_mapping;
    std::map<uint64_t, std::vector<std::string>> explicit_intrinsic_procedures_mapping;
    std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> instantiate_types;
    std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> instantiate_symbols;
    std::map<std::string, std::map<std::string, std::vector<AST::decl_stmt_t*>>> entry_functions;
    std::map<std::string, std::vector<int>> entry_function_arguments_mapping;
    std::map<uint32_t, std::vector<ASR::stmt_t*>> data_structure;
    ASR::asr_t *unit;
    auto res = symbol_table_visitor(al, ast, diagnostics, symbol_table,
        compiler_options, implicit_mapping, common_variables_hash, common_variables_byte_offset,
        external_procedures_mapping, explicit_intrinsic_procedures_mapping, instantiate_types,
        instantiate_symbols, entry_functions, entry_function_arguments_mapping, data_structure, lm);
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
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(*tu);
#if defined(WITH_LFORTRAN_ASSERT)
    if (!asr_verify(*tu, true, diagnostics)) {
        return Error();
    };
#endif
    if (!symtab_only) {
        auto res = body_visitor(
            al, ast, diagnostics, unit, compiler_options,
            implicit_mapping, common_variables_hash, common_variables_byte_offset,
            external_procedures_mapping, explicit_intrinsic_procedures_mapping, instantiate_types,
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
