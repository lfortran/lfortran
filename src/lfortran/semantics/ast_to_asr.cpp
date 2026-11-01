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

namespace {

// Finds the first reference to the value of `v` in the statements it
// visits, in order, until one of them may assign `v`: an assignment, a DO
// loop with `v` as its variable or a READ into `v`, even if it only runs
// conditionally, so that a suggestion is never made for a value that may
// not come from the loop. The values of an implied DO loop and the body of
// a DO CONCURRENT or FORALL that have `v` as their index do not refer to
// its value.
class FirstReference : public ASR::BaseWalkVisitor<FirstReference> {
    ASR::symbol_t *v;

    bool is_v(ASR::expr_t *e) const {
        return e && ASR::is_a<ASR::Var_t>(*e) && ASR::down_cast<ASR::Var_t>(e)->m_v == v;
    }
    bool done() const { return loc != nullptr || assigned; }
public:
    const Location *loc = nullptr;
    bool assigned = false;
    explicit FirstReference(ASR::symbol_t *v_) : v{v_} {}
    void visit_stmt(const ASR::stmt_t &x) {
        if (!done()) ASR::BaseWalkVisitor<FirstReference>::visit_stmt(x);
    }
    void visit_expr(const ASR::expr_t &x) {
        if (!done()) ASR::BaseWalkVisitor<FirstReference>::visit_expr(x);
    }
    void visit_Var(const ASR::Var_t &x) {
        if (!done() && x.m_v == v) loc = &x.base.base.loc;
    }
    void visit_Assignment(const ASR::Assignment_t &x) {
        visit_expr(*x.m_value);
        if (is_v(x.m_target)) {
            assigned = true;
        } else {
            visit_expr(*x.m_target);
        }
    }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (!is_v(x.m_head.m_v)) {
            ASR::BaseWalkVisitor<FirstReference>::visit_DoLoop(x);
            return;
        }
        visit_expr(*x.m_head.m_start);
        visit_expr(*x.m_head.m_end);
        if (x.m_head.m_increment) visit_expr(*x.m_head.m_increment);
        assigned = true;
    }
    void visit_FileRead(const ASR::FileRead_t &x) {
        bool reads_v = false;
        for (size_t i = 0; i < x.n_values; i++) {
            reads_v = reads_v || is_v(x.m_values[i]);
        }
        if (!reads_v) {
            ASR::BaseWalkVisitor<FirstReference>::visit_FileRead(x);
            return;
        }
        assigned = true;
    }
    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t &x) {
        if (!is_v(x.m_var)) {
            ASR::BaseWalkVisitor<FirstReference>::visit_ImpliedDoLoop(x);
            return;
        }
        visit_expr(*x.m_start);
        visit_expr(*x.m_end);
        if (x.m_increment) visit_expr(*x.m_increment);
    }
    void visit_ForAllSingle(const ASR::ForAllSingle_t &x) {
        if (!is_v(x.m_head.m_v)) {
            ASR::BaseWalkVisitor<FirstReference>::visit_ForAllSingle(x);
            return;
        }
        visit_expr(*x.m_head.m_start);
        visit_expr(*x.m_head.m_end);
        if (x.m_head.m_increment) visit_expr(*x.m_head.m_increment);
    }
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        bool index = false;
        for (size_t i = 0; i < x.n_head; i++) {
            index = index || is_v(x.m_head[i].m_v);
        }
        if (!index) {
            ASR::BaseWalkVisitor<FirstReference>::visit_DoConcurrentLoop(x);
            return;
        }
        for (size_t i = 0; i < x.n_head; i++) {
            if (x.m_head[i].m_start) visit_expr(*x.m_head[i].m_start);
            if (x.m_head[i].m_end) visit_expr(*x.m_head[i].m_end);
            if (x.m_head[i].m_increment) visit_expr(*x.m_head[i].m_increment);
        }
    }
};

// Whether a statement of a loop body leaves the loop with an EXIT: one
// without a construct name that is not in a nested loop, or one that names
// the loop.
class ExitsLoop : public ASR::BaseWalkVisitor<ExitsLoop> {
    const char *name;
    int nested_loops = 0;
public:
    bool found = false;
    explicit ExitsLoop(const char *name_) : name{name_} {}
    void visit_Exit(const ASR::Exit_t &x) {
        if (x.m_stmt_name == nullptr ? nested_loops == 0
                : (name && std::string(x.m_stmt_name) == name)) {
            found = true;
        }
    }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        nested_loops++;
        ASR::BaseWalkVisitor<ExitsLoop>::visit_DoLoop(x);
        nested_loops--;
    }
    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        nested_loops++;
        ASR::BaseWalkVisitor<ExitsLoop>::visit_WhileLoop(x);
        nested_loops--;
    }
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        nested_loops++;
        ASR::BaseWalkVisitor<ExitsLoop>::visit_DoConcurrentLoop(x);
        nested_loops--;
    }
};

// Suggests not to rely on the variable of a DO loop after the loop: there
// it is one step past the last iteration, or the value it had at an EXIT,
// which is easy to get wrong. A reference before the variable is assigned
// again is reported, once per loop.
class DoVariableAfterLoop : public ASR::ASRPassBaseWalkVisitor<DoVariableAfterLoop> {
    diag::Diagnostics &diagnostics;

    void check_loop(const ASR::DoLoop_t &loop, ASR::stmt_t **after, size_t n_after) {
        if (loop.m_head.m_v == nullptr || !ASR::is_a<ASR::Var_t>(*loop.m_head.m_v)) {
            return;
        }
        ASR::symbol_t *v = ASR::down_cast<ASR::Var_t>(loop.m_head.m_v)->m_v;
        FirstReference reference(v);
        for (size_t i = 0; i < n_after; i++) {
            reference.visit_stmt(*after[i]);
        }
        if (reference.loc) report(loop, v, *reference.loc);
    }

    void report(const ASR::DoLoop_t &loop, ASR::symbol_t *v, const Location &loc) {
        std::string name = ASRUtils::symbol_name(v);
        ExitsLoop exits(loop.m_name);
        for (size_t i = 0; i < loop.n_body; i++) exits.visit_stmt(*loop.m_body[i]);
        std::string value;
        if (exits.found) {
            value = "'" + name + "' has the value it had at the exit, or one step "
                "past the last iteration if the loop ends normally";
        } else {
            int64_t start = 0, end = 0, step = 0;
            if (ASRUtils::extract_value(ASRUtils::expr_value(loop.m_head.m_start), start)
                    && ASRUtils::extract_value(ASRUtils::expr_value(loop.m_head.m_end), end)
                    && ASRUtils::extract_value(ASRUtils::expr_value(loop.m_head.m_increment), step)
                    && step != 0) {
                int64_t trips = std::max<int64_t>((end - start + step) / step, 0);
                value = "after the loop, '" + name + "' is "
                    + std::to_string(start + trips * step);
            } else {
                value = "after the loop, '" + name + "' is one step past the "
                    "last iteration";
            }
        }
        diagnostics.add(diag::Diagnostic(
            "the do loop variable '" + name + "' is used after the loop",
            diag::Level::Style, diag::Stage::Semantic, {
                diag::Label(value, {loc}),
                diag::Label("the loop sets '" + name + "' here",
                    {loop.m_head.m_v->base.loc}, false)
            }));
    }

public:
    explicit DoVariableAfterLoop(diag::Diagnostics &diagnostics_)
        : diagnostics{diagnostics_} {}

    void visit_Module(const ASR::Module_t &x) {
        if (x.m_intrinsic || x.m_loaded_from_mod) return;
        ASR::ASRPassBaseWalkVisitor<DoVariableAfterLoop>::visit_Module(x);
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        for (size_t i = 0; i < n_body; i++) {
            if (ASR::is_a<ASR::DoLoop_t>(*m_body[i])) {
                check_loop(*ASR::down_cast<ASR::DoLoop_t>(m_body[i]),
                    m_body + i + 1, n_body - i - 1);
            }
            visit_stmt(*m_body[i]);
        }
    }
};

} // namespace

Result<ASR::asr_t*> symbol_table_visitor(Allocator &al, AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        SymbolTable *symbol_table,
        CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::decl_stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::map<uint32_t, std::vector<ASR::stmt_t*>> &data_structure,
        std::set<ASR::symbol_t*> &getter_functions,
        LCompilers::LocationManager &lm,
        const std::map<std::string, ASR::ttype_t*> *interactive_implicit_types);

Result<ASR::TranslationUnit_t*> body_visitor(Allocator &al,
        AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics,
        ASR::asr_t *unit,
        CompilerOptions &compiler_options,
        std::map<uint64_t, std::map<std::string, ASR::ttype_t*>>& implicit_mapping,
        std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
        std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
        std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
        std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
        std::map<std::string, std::map<std::string, std::vector<AST::decl_stmt_t*>>> &entry_functions,
        std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
        std::map<uint32_t, std::vector<ASR::stmt_t*>> &data_structure,
        std::set<ASR::symbol_t*> &getter_functions,
        LCompilers::LocationManager &lm);

void load_rtlib() {
    const std::string m_builtin = "lfortran_intrinsic_builtin";
    const std::string m_ieee_arithmetic = "lfortran_intrinsic_ieee_arithmetic";
}

Result<ASR::TranslationUnit_t*> ast_to_asr(Allocator &al,
    AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics,
    SymbolTable *symbol_table, bool symtab_only,
    CompilerOptions &compiler_options, LCompilers::LocationManager &lm,
    std::map<std::string, ASR::ttype_t*> *interactive_implicit_types)
{
    std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> implicit_mapping;
    std::map<uint64_t, std::vector<std::string>> external_procedures_mapping;
    std::map<uint64_t, std::vector<std::string>> explicit_intrinsic_procedures_mapping;
    std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> instantiate_types;
    std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> instantiate_symbols;
    std::map<std::string, std::map<std::string, std::vector<AST::decl_stmt_t*>>> entry_functions;
    std::map<std::string, std::vector<int>> entry_function_arguments_mapping;
    std::map<uint32_t, std::vector<ASR::stmt_t*>> data_structure;
    std::set<ASR::symbol_t*> getter_functions;
    ASR::asr_t *unit;
    auto res = symbol_table_visitor(al, ast, diagnostics, symbol_table,
        compiler_options, implicit_mapping,
        external_procedures_mapping, explicit_intrinsic_procedures_mapping, instantiate_types,
        instantiate_symbols, entry_functions, entry_function_arguments_mapping, data_structure,
        getter_functions, lm,
        interactive_implicit_types);
    if (res.ok) {
        unit = res.result;
    } else {
        return res.error;
    }
    // Some errors are reported without aborting, so that the rest of the
    // code is still checked. Without --continue-compilation, any reported
    // error must still fail the compilation.
    if (!compiler_options.continue_compilation && diagnostics.has_error()) {
        return Error();
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
            implicit_mapping,
            external_procedures_mapping, explicit_intrinsic_procedures_mapping, instantiate_types,
            instantiate_symbols, entry_functions, entry_function_arguments_mapping,
            data_structure, getter_functions, lm
        );
        if (res.ok) {
            tu = res.result;
        } else {
            return res.error;
        }
        if (!compiler_options.continue_compilation && diagnostics.has_error()) {
            return Error();
        }
        if (compiler_options.show_style_suggestions && !diagnostics.has_error()) {
            DoVariableAfterLoop do_variable_after_loop(diagnostics);
            do_variable_after_loop.visit_TranslationUnit(*tu);
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
    if (interactive_implicit_types) {
        *interactive_implicit_types = implicit_mapping[get_hash(&tu->base.base)];
    }
    return tu;
}

} // namespace LCompilers::LFortran
