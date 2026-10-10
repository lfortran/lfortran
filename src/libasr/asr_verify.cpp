#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_side_effect.h>
#include <libasr/asr_verify.h>
#include <libasr/utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <set>

namespace LCompilers {

namespace ASR {

using ASRUtils::symbol_name;
using ASRUtils::symbol_parent_symtab;
using ASRUtils::typed_expr_type;
using ASRUtils::is_procedure_type;
using ASRUtils::is_struct_like_type;

bool valid_char(char c) {
    if (c >= 'a' && c <= 'z') return true;
    if (c >= 'A' && c <= 'Z') return true;
    if (c >= '0' && c <= '9') return true;
    if (c == '_') return true;
    return false;
}

bool valid_name(const char *s) {
    if (s == nullptr) return false;
    std::string name = s;
    if (name.size() == 0) return false;
    for (size_t i=0; i<name.size(); i++) {
        if (!valid_char(s[i])) return false;
    }
    return true;
}

// The first local of the scope of `local` that its initializer references
// and that is not defined when the initializer is evaluated, on entry: a
// local that is neither a parameter nor itself initialized on entry. An
// inquiry about the bounds or the length of a local does not reference its
// value.
class EntryInitializerReference : public BaseWalkVisitor<EntryInitializerReference>
{
public:
    const Variable_t &local;
    const Variable_t *undefined = nullptr;

    EntryInitializerReference(const Variable_t &local_) : local(local_) {}

    void visit_Var(const Var_t &x) {
        symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
        if (undefined != nullptr || !is_a<Variable_t>(*sym)) return;
        const Variable_t *v = down_cast<Variable_t>(sym);
        if (v->m_parent_symtab == local.m_parent_symtab &&
                v->m_intent == intentType::Local &&
                v->m_storage != storage_typeType::Parameter &&
                !ASRUtils::is_entry_initialized_local(*v)) {
            undefined = v;
        }
    }

    void visit_ArraySize(const ArraySize_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_ArrayBound(const ArrayBound_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_StringLen(const StringLen_t & /*x*/) {}

    void visit_TypeInquiry(const TypeInquiry_t & /*x*/) {}
};

class VerifyVisitor : public BaseWalkVisitor<VerifyVisitor>
{
private:
    // For checking correct parent symbtab relationship
    SymbolTable *current_symtab;
    bool check_external;
    diag::Diagnostics &diagnostics;
    std::string current_name;

    // For checking that all symtabs have a unique ID.
    // We first walk all symtabs, and then we check that everything else
    // points to them (i.e., that nothing points to some symbol table that
    // is not part of this ASR).
    std::map<uint64_t,SymbolTable*> id_symtab_map;
    std::vector<std::string> function_dependencies;
    std::vector<std::string> module_dependencies;
    std::vector<std::string> variable_dependencies;

    std::set<std::pair<uint64_t, std::string>> const_assigned;
    std::map<std::pair<ASR::symbol_t*, std::string>, ASR::symbol_t*> trait_callables;

    // checks whether we've visited any `Var`, which isn't a global `Variable`
    bool non_global_symbol_visited;
    bool _is_return_type_string;
    bool _return_var_or_intent_out = false;
    bool _processing_dims = false;
    bool _inside_call = false;
    bool _inside_array_physical_cast_type = false;
    bool _processing_assumed_rank_array = false;
    bool _processing_unbounded_pointer_array = false;
    // True while the symbols of a template are visited. A named constant of a
    // template whose initializer reads a deferred constant has no compile-time
    // value until the template is instantiated.
    bool _inside_template = false;
    bool _inside_trait_subroutine = false;
    const ASR::expr_t* current_expr {}; // current expression being visited 
    // Procedures whose effect flags must retain the trait lifecycle effects
    // they reach. They are checked once the whole unit has been verified, so
    // the summary only follows references the verifier has already checked.
    std::vector<const Function_t*> trait_lifecycle_callers;

public:
    // See ASRVerifyOptions::string_length_arguments.
    bool check_string_length_arguments = false;

    VerifyVisitor(bool check_external,
        diag::Diagnostics &diagnostics) : check_external{check_external},
        diagnostics{diagnostics}, non_global_symbol_visited{false}, _is_return_type_string{false} {}

    // Requires the condition `cond` to be true. Raise an exception otherwise.
    #define require(cond, error_msg) ASRUtils::require_impl((cond), (error_msg), x.base.base.loc, diagnostics);
    #define require_with_loc(cond, error_msg, loc) ASRUtils::require_impl((cond), (error_msg), loc, diagnostics);
    #define require_id(cond, error_code, error_msg) ASRUtils::require_impl((cond), (error_code), (error_msg), x.base.base.loc, diagnostics);
    #define require_with_loc_id(cond, error_code, error_msg, loc) ASRUtils::require_impl((cond), (error_code), (error_msg), loc, diagnostics);
    // Type equality uses the expression only to resolve the struct symbol it
    // refers to, which requires dereferencing ExternalSymbol. Before externals
    // are resolved that is not possible, so drop the expression context and
    // let the comparison fall back to a structural one.
    ASR::expr_t* type_context(ASR::expr_t *e) {
        return check_external ? e : nullptr;
    }

    static bool in_loaded_module(const SymbolTable *scope) {
        for (; scope; scope = scope->parent) {
            if (scope->asr_owner && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
                auto *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
                if (ASR::is_a<ASR::Module_t>(*owner)) {
                    return ASR::down_cast<ASR::Module_t>(owner)->m_loaded_from_mod;
                }
            }
        }
        return false;
    }

    // Returns true if the `symtab_ID` (sym->symtab->parent) is the current
    // symbol table `symtab` or any of its parents *and* if the symbol in the
    // symbol table is equal to `sym`. It returns false otherwise, such as in the
    // case when the symtab is in a different module or if the `sym`'s symbol table
    // does not actually contain it.
    bool symtab_in_scope(const SymbolTable *symtab, const ASR::symbol_t *sym) {
        unsigned int symtab_ID = symbol_parent_symtab(sym)->counter;
        char *sym_name = symbol_name(sym);
        const SymbolTable *s = symtab;
        while (s != nullptr) {
            if (s->counter == symtab_ID) {
                ASR::symbol_t *sym2 = s->get_symbol(sym_name);
                if (sym2) {
                    if (sym2 == sym) {
                        // The symbol table was found and the symbol `sym` is in it
                        return true;
                    } else {
                        diagnostics.message_label("The symbol table was found and the symbol in it shares the name, but is not equal to `sym`",
                        {sym->base.loc}, "failed here", diag::Level::Error, diag::Stage::ASRVerify);
                        return false;
                    }
                } else {
                    diagnostics.message_label("The symbol table was found, but the symbol `sym` is not in it",
                        {sym->base.loc}, "failed here", diag::Level::Error, diag::Stage::ASRVerify);
                    return false;
                }
            }
            s = s->parent;
        }
        diagnostics.message_label("The symbol table was not found in the scope of `symtab`.",
                        {sym->base.loc}, "failed here", diag::Level::Error, diag::Stage::ASRVerify);
        return false;
    }

    // The initializer a Module, a Program or the TranslationUnit names must
    // be a real, argument-less procedure of that owner's own scope, so that a
    // backend can lower the link without searching or guessing.
    void verify_global_init(const char *global_init, SymbolTable *scope,
            const std::string &owner, const Location &loc) {
        if (global_init == nullptr) return;
        ASR::symbol_t *sym = scope->get_symbol(global_init);
        ASRUtils::require_impl(sym != nullptr,
            owner + "::m_global_init must name a symbol of " + owner +
            "'s own symbol table, but " + std::string(global_init) +
            " is not in it", loc, diagnostics);
        ASRUtils::require_impl(sym != nullptr && ASR::is_a<ASR::Function_t>(*sym),
            owner + "::m_global_init must name a Function", loc, diagnostics);
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
        ASRUtils::require_impl(fn->n_args == 0 && fn->m_return_var == nullptr,
            owner + "::m_global_init must name a subroutine taking no "
            "arguments", loc, diagnostics);
    }

    void visit_TranslationUnit(const TranslationUnit_t &x) {
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The TranslationUnit::m_symtab cannot be nullptr");
        // Interactive evaluation chains one TranslationUnit per cell, each
        // scope parented to the previous cell's, so that later cells see
        // earlier declarations and may shadow them. Outside that, a
        // TranslationUnit is the root and has no parent.
        require(x.m_symtab->parent == nullptr ||
                ASRUtils::is_tu_scope(x.m_symtab->parent),
            "The TranslationUnit::m_symtab->parent must be nullptr or the "
            "symbol table of another TranslationUnit");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "TranslationUnit::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The TranslationUnit::m_symtab::asr_owner must point to itself");
        require(down_cast2<TranslationUnit_t>(current_symtab->asr_owner)->m_symtab == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        verify_global_init(x.m_global_init, x.m_symtab, "TranslationUnit",
            x.base.base.loc);
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        for (size_t i=0; i<x.n_items; i++) {
            asr_t *item = x.m_items[i];
            require(is_a<stmt_t>(*item) || is_a<expr_t>(*item),
                "TranslationUnit::m_items must be either stmt or expr");
            if (is_a<stmt_t>(*item)) {
                this->visit_stmt(*down_cast<stmt_t>(item));
            } else {
                this->visit_expr(*down_cast<expr_t>(item));
            }
        }
        verify_reached_trait_lifecycle_effects(x);
        current_symtab = nullptr;
    }

    void verify_reached_trait_lifecycle_effects(const TranslationUnit_t &unit) {
        if (trait_lifecycle_callers.empty()) return;
        ASR::TraitLifecycleSummary summary(unit.m_symtab);
        for (const Function_t *caller : trait_lifecycle_callers) {
            require_with_loc_id(summary.effect(const_cast<Function_t&>(*caller)) !=
                    ASR::TraitLifecycleSummary::Effect::Lifecycle,
                "asr.verify.trait_owner.reached_lifecycle_effects",
                "A procedure must retain the unchecked dynamic trait lifecycle "
                "effects of the procedures it calls", caller->base.base.loc);
        }
    }

    void visit_Select(const Select_t& x) {
        bool fall_through = false;
        for( size_t i = 0; i < x.n_body; i++ ) {
            if( ASR::is_a<ASR::CaseStmt_t>(*x.m_body[i]) ) {
                ASR::CaseStmt_t* case_stmt_t = ASR::down_cast<ASR::CaseStmt_t>(x.m_body[i]);
                fall_through = fall_through || case_stmt_t->m_fall_through;
            }
        }
        require(fall_through == x.m_enable_fall_through,
            "Select_t::m_enable_fall_through should be " +
            std::to_string(x.m_enable_fall_through));
        BaseWalkVisitor<VerifyVisitor>::visit_Select(x);
    }

    // --------------------------------------------------------
    // symbol instances:

    void visit_Program(const Program_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The Program::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The Program::m_symtab->parent is not the right parent");
        require(ASRUtils::is_tu_scope(x.m_symtab->parent),
            "The Program::m_symtab's parent must be TranslationUnit");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "Program::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        require(x.m_name, "Program name is required");
        if (x.n_dependencies > 0) {
            require(x.m_dependencies,
            std::string(x.m_name) + "::m_dependencies is required");
        }
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        verify_global_init(x.m_global_init, x.m_symtab, "Program",
            x.base.base.loc);
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        for (size_t i=0; i<x.n_body; i++) {
            LCOMPILERS_ASSERT(x.m_body[i]);
            visit_stmt(*x.m_body[i]);
        }
        current_symtab = parent_symtab;
    }

    void visit_AssociateBlock(const AssociateBlock_t& x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The AssociateBlock::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The AssociateBlock::m_symtab->parent is not the right parent");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "AssociateBlock::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        for (size_t i=0; i<x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }
        current_symtab = parent_symtab;
    }

    // A generic name resolves to one of its specific procedures, so every
    // entry has to be something that can be called.
    void verify_specific_procedures(const std::string &what,
            symbol_t **procs, size_t n_procs, const Location &loc) {
        for (size_t i = 0; i < n_procs; i++) {
            require_with_loc_id(procs[i] != nullptr,
                "asr.verify.generic_procedure.specific_is_procedure",
                what + " cannot have a null specific procedure", loc);
            ASR::symbol_t *proc = check_external
                ? ASRUtils::symbol_get_past_external(procs[i]) : procs[i];
            require_with_loc_id(proc != nullptr &&
                    (ASR::is_a<ASR::Function_t>(*proc) ||
                     ASR::is_a<ASR::StructMethodDeclaration_t>(*proc) ||
                     ASR::is_a<ASR::GenericProcedure_t>(*proc) ||
                     ASR::is_a<ASR::ExternalSymbol_t>(*proc)),
                "asr.verify.generic_procedure.specific_is_procedure",
                what + " specific procedure '" +
                std::string(ASRUtils::symbol_name(procs[i])) +
                "' must be a procedure, not " +
                ASRUtils::symbol_type_name(*procs[i]), loc);
        }
    }

    void visit_GenericProcedure(const GenericProcedure_t& x) {
        require(x.m_name != nullptr,
            "GenericProcedure::m_name cannot be nullptr");
        std::string gen_name = x.m_name;
        require(x.m_parent_symtab != nullptr,
            gen_name + "::m_parent_symtab cannot be nullptr");
        verify_specific_procedures("GenericProcedure '" + gen_name + "'",
            x.m_procs, x.n_procs, x.base.base.loc);
    }

    // A namelist group is a list of variables; I/O reads and writes each one
    // by its declared type.
    void visit_Namelist(const Namelist_t& x) {
        require(x.m_group_name != nullptr,
            "Namelist::m_group_name cannot be nullptr");
        for (size_t i = 0; i < x.n_var_list; i++) {
            require(x.m_var_list[i] != nullptr,
                "Namelist '" + std::string(x.m_group_name) +
                "' cannot have a null member");
            ASR::symbol_t *member = check_external
                ? ASRUtils::symbol_get_past_external(x.m_var_list[i])
                : x.m_var_list[i];
            require_id(member != nullptr &&
                    (ASR::is_a<ASR::Variable_t>(*member) ||
                     ASR::is_a<ASR::ExternalSymbol_t>(*member)),
                "asr.verify.namelist.member_is_variable",
                "Namelist '" + std::string(x.m_group_name) + "' member '" +
                std::string(ASRUtils::symbol_name(x.m_var_list[i])) +
                "' must be a variable, not " +
                ASRUtils::symbol_type_name(*x.m_var_list[i]));
        }
    }

    void visit_CustomOperator(const CustomOperator_t& x) {
        require(x.m_name != nullptr,
            "CustomOperator::m_name cannot be nullptr");
        std::string cus_name = x.m_name;
        require(x.m_parent_symtab != nullptr,
            cus_name + "::m_parent_symtab cannot be nullptr");
        verify_specific_procedures("CustomOperator '" + cus_name + "'",
            x.m_procs, x.n_procs, x.base.base.loc);
    }

    void visit_Block(const Block_t& x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The AssociateBlock::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The AssociateBlock::m_symtab->parent is not the right parent");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "AssociateBlock::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        for (size_t i=0; i<x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }
        current_symtab = parent_symtab;
    }

    void visit_Requirement(const Requirement_t& x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The Requirement::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The Requirement::m_symtab->parent is not the right parent");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "Requirement::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_symtab = parent_symtab;
    }

    void visit_Template(const Template_t& x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The Requirement::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The Requirement::m_symtab->parent is not the right parent");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "Requirement::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        bool inside_template = _inside_template;
        _inside_template = true;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        _inside_template = inside_template;
        current_symtab = parent_symtab;
    }

    void visit_BlockCall(const BlockCall_t& x) {
        require(x.m_m != nullptr, "Block call made to inexisting block");
        require(symtab_in_scope(current_symtab, x.m_m),
            "Block " + std::string(ASRUtils::symbol_name(x.m_m)) +
            " should resolve in current scope.");
        require_id(ASR::is_a<ASR::Block_t>(*x.m_m),
            "asr.verify.block_call.target_is_block",
            "BlockCall::m_m '" + std::string(ASRUtils::symbol_name(x.m_m)) +
            "' must be a block");
        SymbolTable *parent_symtab = current_symtab;
        ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(x.m_m);
        current_symtab = block->m_symtab;
        for (size_t i=0; i<block->n_body; i++) {
            visit_stmt(*(block->m_body[i]));
        }
        current_symtab = parent_symtab;
    }

    void verify_unique_dependencies(char** m_dependencies,
        size_t n_dependencies, std::string m_name, const Location& loc) {
        // Check if any dependency is duplicated
        // in the dependency list of the function
        std::set<std::string> dependencies_set;
        for( size_t i = 0; i < n_dependencies; i++ ) {
            std::string found_dep = m_dependencies[i];
            require_with_loc(dependencies_set.find(found_dep) == dependencies_set.end(),
                    "Symbol " + found_dep + " is duplicated in the dependency "
                    "list of " + m_name, loc);
            dependencies_set.insert(found_dep);
        }
    }

    void visit_Module(const Module_t &x) {
        module_dependencies.clear();
        module_dependencies.reserve(x.n_dependencies);
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The Module::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The Module::m_symtab->parent is not the right parent");
        require(ASRUtils::is_tu_scope(x.m_symtab->parent),
            "The Module::m_symtab's parent must be TranslationUnit");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "Module::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(x.m_name, "Module name is required");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        verify_global_init(x.m_global_init, x.m_symtab, "Module",
            x.base.base.loc);
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }

        verify_unique_dependencies(x.m_dependencies, x.n_dependencies,
                                   x.m_name, x.base.base.loc);

        for (size_t i=0; i < x.n_dependencies; i++) {
            require(x.m_dependencies[i] != nullptr,
                "A module dependency must not be a nullptr");
            require(std::string(x.m_dependencies[i]) != "",
                "A module dependency must not be an empty string");
            require(valid_name(x.m_dependencies[i]),
                "A module dependency must be a valid string");
        }
        verify_separate_module_procedures(x, parent_symtab);
        for( auto& dep: module_dependencies ) {
            if( dep != x.m_name ) {
                require(present(x.m_dependencies, x.n_dependencies, dep),
                        "Module " + std::string(x.m_name) +
                        " dependencies must contain " + dep +
                        " because a function present in it is getting called in "
                        + std::string(x.m_name) + ".");
            }
        }
        current_symtab = parent_symtab;
    }

    // The interface a separate module procedure was declared with, searched
    // up the chain of ancestor modules a submodule extends, or nullptr.
    ASR::Function_t* declared_module_interface(SymbolTable *tu_scope,
            const char *parent_module, const std::string &name) {
        std::set<std::string> seen;
        while (parent_module != nullptr && tu_scope != nullptr) {
            std::string ancestor = parent_module;
            if (!seen.insert(ancestor).second) return nullptr;
            ASR::symbol_t *sym = tu_scope->get_symbol(ancestor);
            if (sym == nullptr || !ASR::is_a<ASR::Module_t>(*sym)) {
                return nullptr;
            }
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(sym);
            ASR::symbol_t *declared = m->m_symtab->get_symbol(name);
            if (declared != nullptr && ASR::is_a<ASR::Function_t>(*declared)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(declared);
                if (ASRUtils::get_FunctionType(f)->m_deftype ==
                        ASR::deftypeType::Interface) {
                    return f;
                }
            }
            parent_module = m->m_parent_module;
        }
        return nullptr;
    }

    // A submodule supplies the body of a procedure whose interface its
    // ancestor module published. Every caller compiled against that module
    // was checked against the published interface and against nothing else,
    // so the body has to match it.
    void verify_separate_module_procedures(const Module_t &x,
            SymbolTable *tu_scope) {
        if (!check_external || x.m_parent_module == nullptr) return;
        for (auto &item : x.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *impl =
                ASR::down_cast<ASR::Function_t>(item.second);
            if (ASRUtils::get_FunctionType(impl)->m_deftype !=
                    ASR::deftypeType::Implementation) {
                continue;
            }
            ASR::Function_t *declared = declared_module_interface(
                tu_scope, x.m_parent_module, item.first);
            if (declared == nullptr || declared == impl) continue;
            require_conforming(ASRUtils::interface_mismatch(
                    "Module procedure '" + std::string(impl->m_name) +
                    "' implementing the interface in module '" +
                    std::string(x.m_parent_module) + "'",
                    impl, declared, declared->n_args + 1, check_external),
                "asr.verify.module_procedure", x.base.base.loc);
        }
    }

    // Associating a procedure pointer fixes what every later call through it
    // is compiled against, so the procedure has to have the interface the
    // pointer was declared with.
    static bool contains_retained_result_storage(ASR::ttype_t *type) {
        if (!type) return false;
        class FindStorage : public BaseWalkVisitor<FindStorage> {
        public:
            bool found = false;
            void visit_TraitOwnerList(const TraitOwnerList_t &) { found = true; }
        } visitor;
        visitor.visit_ttype(*type);
        return visitor.found;
    }

    bool guarded_data_association(const Variable_t &variable, const Cast_t &cast) {
        auto *scope = variable.m_parent_symtab;
        auto *parent = scope ? scope->parent : nullptr;
        auto *owner = parent ? parent->asr_owner : nullptr;
        if (!owner || !ASR::is_a<symbol_t>(*owner)) return false;
        stmt_t **body = nullptr;
        size_t n = 0;
        auto *symbol = ASR::down_cast<symbol_t>(owner);
        if (ASR::is_a<Block_t>(*symbol)) {
            auto *block = ASR::down_cast<Block_t>(symbol);
            body = block->m_body; n = block->n_body;
        } else if (ASR::is_a<AssociateBlock_t>(*symbol)) {
            auto *block = ASR::down_cast<AssociateBlock_t>(symbol);
            body = block->m_body; n = block->n_body;
        }
        if (!cast.m_dest || !ASR::is_a<Var_t>(*cast.m_dest) ||
                ASR::down_cast<Var_t>(cast.m_dest)->m_v != &variable.base) return false;
        for (size_t i = 0; i < n; i++) {
            if (!ASR::is_a<SelectType_t>(*body[i])) continue;
            auto *selection = ASR::down_cast<SelectType_t>(body[i]);
            if (!same_variable(selection->m_selector, cast.m_arg)) continue;
            for (size_t j = 0; j < selection->n_body; j++) {
                auto *guard = selection->m_body[j];
                symbol_t *declaration = nullptr;
                stmt_t **statements = nullptr;
                size_t count = 0;
                if (ASR::is_a<TypeStmtName_t>(*guard) &&
                        cast.m_kind == cast_kindType::ClassToStruct) {
                    auto *g = ASR::down_cast<TypeStmtName_t>(guard);
                    declaration = g->m_sym; statements = g->m_body; count = g->n_body;
                } else if (ASR::is_a<ClassStmt_t>(*guard) &&
                        cast.m_kind == cast_kindType::ClassToClass) {
                    auto *g = ASR::down_cast<ClassStmt_t>(guard);
                    declaration = g->m_sym; statements = g->m_body; count = g->n_body;
                }
                if (count == 1 && statements && ASR::is_a<BlockCall_t>(*statements[0]) &&
                        (asr_t*)ASR::down_cast<BlockCall_t>(statements[0])->m_m == scope->asr_owner &&
                        ASRUtils::symbol_get_past_external(declaration) ==
                            ASRUtils::symbol_get_past_external(variable.m_type_declaration)) return true;
            }
        }
        return false;
    }

    void visit_Associate(const Associate_t &x) {
        BaseWalkVisitor<VerifyVisitor>::visit_Associate(x);
        if (!check_external || x.m_target == nullptr || x.m_value == nullptr) {
            return;
        }
        if (ASR::is_a<Var_t>(*x.m_target) &&
                ASRUtils::EXPR2VAR(x.m_target)->m_storage == storage_typeType::Association) {
            auto *variable = ASRUtils::EXPR2VAR(x.m_target);
            require_id(variable->m_parent_symtab == current_symtab &&
                    ASRUtils::association_value(*variable) == x.m_value,
                "asr.verify.association.binding",
                "A data association must be bound exactly once in its own construct");
            require_id(ASRUtils::types_equal(variable->m_type,
                    typed_expr_type(x.m_value), x.m_target, x.m_value),
                "asr.verify.association.type",
                "A data association must have the explicit type of its view");
            if (ASR::is_a<Cast_t>(*x.m_value)) {
                require_id(guarded_data_association(*variable,
                        *ASR::down_cast<Cast_t>(x.m_value)),
                    "asr.verify.association.guarded_type",
                    "A narrowed data association requires its matching nominal SELECT TYPE guard");
            } else {
                require_id(ASR::is_a<TraitProject_t>(*x.m_value) ||
                        ASR::is_a<TraitInspect_t>(*x.m_value) ||
                        ASRUtils::association_variable(x.m_value),
                    "asr.verify.association.source",
                    "A data association must preserve a captured or explicitly inspected view");
            }
            return;
        }
        ASR::ttype_t *target = typed_expr_type(x.m_target);
        ASR::ttype_t *value = typed_expr_type(x.m_value);
        if (target == nullptr || value == nullptr) return;
        require_id(!contains_retained_result_storage(target) &&
                !contains_retained_result_storage(value),
            "asr.verify.trait_results.no_copy",
            "Retained-result storage cannot be copied or associated");
        require_id(!ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(target)) &&
                !ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(value)),
            "asr.verify.trait_owner.explicit_protocol",
            "An association cannot create another owner or an untracked borrowed trait header");
        verify_procedure_interface(value, target,
            "Procedure pointer association", x.base.base.loc);
    }

    // A type guard names a type the selector could actually be at run time.
    // One that names an unrelated type selects a branch nothing can enter,
    // and the branch then reads the selector as a type it never holds.
    void visit_SelectType(const SelectType_t &x) {
        BaseWalkVisitor<VerifyVisitor>::visit_SelectType(x);
        if (!check_external || x.m_selector == nullptr) return;
        auto *selector_type = typed_expr_type(x.m_selector);
        require_id(selector_type && ASRUtils::is_class_type(ASRUtils::extract_type(selector_type)),
            "asr.verify.select_type.polymorphic",
            "SELECT TYPE requires an ordinary polymorphic selector; trait inspection must be explicit");
        for (size_t i = 0; i < x.n_body; i++) {
            ASR::symbol_t *guard = nullptr;
            if (ASR::is_a<ASR::TypeStmtName_t>(*x.m_body[i])) {
                guard = ASR::down_cast<ASR::TypeStmtName_t>(x.m_body[i])->m_sym;
            } else if (ASR::is_a<ASR::ClassStmt_t>(*x.m_body[i])) {
                guard = ASR::down_cast<ASR::ClassStmt_t>(x.m_body[i])->m_sym;
            }
            if (guard == nullptr) continue;
            auto *concrete = ASRUtils::symbol_get_past_external(guard);
            require_id(concrete && ASR::is_a<Struct_t>(*concrete),
                "asr.verify.select_type.concrete_guard",
                "A type guard names a concrete derived type, not a trait contract");
            require_id(
                dynamic_type_is_compatible(guard, x.m_selector),
                "asr.verify.select_type.guard_extends_selector",
                "The type guard '" +
                std::string(ASRUtils::symbol_name(guard)) +
                "' does not extend the declared type of the selector");
            if (ASR::is_a<ClassStmt_t>(*x.m_body[i])) {
                for (size_t j = i + 1; j < x.n_body; j++) {
                    if (!ASR::is_a<ClassStmt_t>(*x.m_body[j])) continue;
                    auto *later = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ClassStmt_t>(x.m_body[j])->m_sym);
                    require_id(!later || !ASR::is_a<Struct_t>(*later) ||
                            !struct_is_or_extends(ASR::down_cast<Struct_t>(later),
                                ASR::down_cast<Struct_t>(concrete)),
                        "asr.verify.select_type.specificity",
                        "More-specific CLASS IS guards must precede matching ancestors");
                }
            }
        }
    }

    void visit_Assignment(const Assignment_t& x) {
        ASR::expr_t* target = x.m_target;
        auto *association = ASRUtils::association_variable(target);
        require_id(!association || association->m_intent != intentType::In,
            "asr.verify.association.definable",
            "A read-only construct association cannot appear in a variable definition context");
        if( ASR::is_a<ASR::Var_t>(*target) ) {
            ASR::Var_t* target_Var = ASR::down_cast<ASR::Var_t>(target);
            bool is_target_const = false;
            ASR::ttype_t* target_type = nullptr;
            ASR::symbol_t* target_sym = ASRUtils::symbol_get_past_external(target_Var->m_v);
            if( target_sym && ASR::is_a<ASR::Variable_t>(*target_sym) ) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(target_sym);
                require(var->m_intent != ASR::intentType::In, "Assignment target `"
                    + std::string(var->m_name) + "` with intent `IN` not allowed");
                target_type = var->m_type;
                is_target_const = var->m_storage == ASR::storage_typeType::Parameter;
            }
            if( is_target_const ) {
                std::string variable_name = ASRUtils::symbol_name(target_Var->m_v);
                require(const_assigned.find(std::make_pair(current_symtab->counter,
                    variable_name)) == const_assigned.end(),
                    "Assignment target with " + ASRUtils::type_to_str_python_expr(target_type, target)
                    + " cannot be re-assigned.");
                const_assigned.insert(std::make_pair(current_symtab->counter, variable_name));
            }
        }
        // A defined assignment is lowered to a call in `m_overloaded`, so its
        // target and value types are unrelated by design.
        ASR::ttype_t *assign_target_type = typed_expr_type(x.m_target);
        ASR::ttype_t *assign_value_type = typed_expr_type(x.m_value);
        require_id(!contains_retained_result_storage(assign_target_type) &&
                !contains_retained_result_storage(assign_value_type),
            "asr.verify.trait_results.no_copy",
            "Retained-result storage cannot be copied or associated");
        bool trait_result_capture = false;
        if (ASRUtils::is_trait_owner(assign_target_type) && x.m_move_allocation &&
                !x.m_realloc_lhs && !x.m_overloaded &&
                ASR::is_a<Var_t>(*x.m_target) &&
                ASR::is_a<FunctionCall_t>(*x.m_value) &&
                ASRUtils::is_trait_owner(assign_value_type)) {
            auto *variable = ASRUtils::EXPR2VAR(x.m_target);
            auto *scope = variable->m_parent_symtab;
            auto *owner = scope && scope->asr_owner &&
                ASR::is_a<symbol_t>(*scope->asr_owner)
                    ? ASR::down_cast<symbol_t>(scope->asr_owner) : nullptr;
            trait_result_capture = scope == current_symtab &&
                variable->m_intent == intentType::Local &&
                variable->m_storage == storage_typeType::Default && owner &&
                (ASR::is_a<Block_t>(*owner) || ASR::is_a<AssociateBlock_t>(*owner));
            require_id(!trait_result_capture || (!variable->m_target_attr &&
                    !ASRUtils::expr_references_symbol(x.m_value, &variable->base)),
                "asr.verify.trait_result.capture_nonalias",
                "A result capture slot must not be targetable or referenced by its producing call");
        }
        require_id(!assign_target_type || !ASR::is_a<TraitObjectType_t>(
                *ASRUtils::extract_type(assign_target_type)) || trait_result_capture,
            "asr.verify.trait_owner.value_copy",
            "Trait assignment requires value-copy semantics; only scoped function-result capture can move ownership");
        if (!diagnostics.has_error() && x.m_overloaded == nullptr
                && assign_target_type && assign_value_type
                && !is_procedure_type(assign_target_type)
                && !is_procedure_type(assign_value_type)
                && !is_struct_like_type(assign_target_type)
                && !is_struct_like_type(assign_value_type)) {
            require_with_loc_id(
                ASRUtils::check_equal_type(
                    assign_target_type, assign_value_type,
                    type_context(x.m_target), type_context(x.m_value)),
                "asr.verify.assignment.value_type_matches_target",
                "Assignment value type " +
                    ASRUtils::get_type_code(assign_value_type) +
                    " does not match target type " +
                    ASRUtils::get_type_code(assign_target_type),
                x.m_value->base.loc);
        }
        // it's possible that the target is an external symbol, and during
        // initial deserialization pass, so we don't do the below verification
        if ( check_external && x.m_realloc_lhs ) {
            ASR::expr_t* a_target = x.m_target;
            bool is_allocatable = ASRUtils::is_allocatable(a_target);
            if ( !is_allocatable && ASR::is_a<ASR::ArrayPhysicalCast_t>(*a_target) ) {
                is_allocatable = ASRUtils::is_allocatable(
                    ASRUtils::get_past_array_physical_cast(a_target));
            }
            if ( ASR::is_a<ASR::StructInstanceMember_t>(*a_target) ) {
                ASR::StructInstanceMember_t* a_target_struct = ASR::down_cast<ASR::StructInstanceMember_t>(a_target);
                is_allocatable |= ASRUtils::is_allocatable(a_target_struct->m_v);
            }
            require_id(is_allocatable,
                "asr.verify.assignment.realloc_lhs_requires_allocatable",
                "Reallocation of non allocatable variable is not allowed");
        }
        if (x.m_move_allocation && !trait_result_capture) {
            ASR::ttype_t* target_type = ASRUtils::expr_type(x.m_target);
            ASR::ttype_t* value_type = ASRUtils::expr_type(x.m_value);

            bool is_target_allocatable_array = ASRUtils::is_array(target_type) &&
                                            ASRUtils::is_allocatable(target_type) &&
                                            ASRUtils::extract_physical_type(target_type) == ASR::array_physical_typeType::DescriptorArray;

            bool is_value_allocatable_array = ASRUtils::is_array(value_type) &&
                                            ASRUtils::is_allocatable(value_type) &&
                                            ASRUtils::extract_physical_type(value_type) == ASR::array_physical_typeType::DescriptorArray;

            require_id(is_target_allocatable_array,
                "asr.verify.assignment.move_target_allocatable_array",
                "Move assignment target must be an allocatable array");
            require_id(is_value_allocatable_array,
                "asr.verify.assignment.move_value_allocatable_array",
                "Move assignment value must be an allocatable array");
        }
        BaseWalkVisitor<VerifyVisitor>::visit_Assignment(x);
    }

    void visit_StructMethodDeclaration(const StructMethodDeclaration_t &x) {
        require(x.m_name != nullptr,
            "The StructMethodDeclaration::m_name cannot be nullptr");
        require(x.m_proc != nullptr,
            "The StructMethodDeclaration::m_proc cannot be nullptr");
        require(x.m_proc_name != nullptr,
            "The StructMethodDeclaration::m_proc_name cannot be nullptr");

        SymbolTable *symtab = x.m_parent_symtab;
        require(symtab != nullptr,
            "StructMethodDeclaration::m_parent_symtab cannot be nullptr");
        require(symtab->get_symbol(std::string(x.m_name)) != nullptr,
            "StructMethodDeclaration '" + std::string(x.m_name) + "' not found in parent_symtab symbol table");
        symbol_t *symtab_sym = symtab->get_symbol(std::string(x.m_name));
        const symbol_t *current_sym = &x.base;
        require(symtab_sym == current_sym,
            "StructMethodDeclaration's parent symbol table does not point to it");
        require(id_symtab_map.find(symtab->counter) != id_symtab_map.end(),
            "StructMethodDeclaration::m_parent_symtab must be present in the ASR ("
                + std::string(x.m_name) + ")");

        // A binding names a procedure. It may name it through an
        // ExternalSymbol, and a generic binding names a GenericProcedure
        // whose specifics carry the signatures, so only a Function has an
        // argument list to look at here.
        ASR::symbol_t *proc_sym = check_external
            ? ASRUtils::symbol_get_past_external(x.m_proc) : x.m_proc;
        require_id(proc_sym != nullptr &&
                (ASR::is_a<ASR::Function_t>(*proc_sym) ||
                 ASR::is_a<ASR::GenericProcedure_t>(*proc_sym) ||
                 ASR::is_a<ASR::StructMethodDeclaration_t>(*proc_sym) ||
                 ASR::is_a<ASR::ExternalSymbol_t>(*proc_sym)),
            "asr.verify.struct_method.proc_is_procedure",
            "StructMethodDeclaration::m_proc of '" + std::string(x.m_name) +
            "' must be a procedure, not " +
            ASRUtils::symbol_type_name(*x.m_proc));
        if (!ASR::is_a<ASR::Function_t>(*proc_sym)) {
            return;
        }
        ASR::Function_t* x_m_proc = ASR::down_cast<ASR::Function_t>(proc_sym);
        if( x.m_self_argument ) {
            bool arg_found = false;
            std::string self_arg_name = std::string(x.m_self_argument);
            for( size_t i = 0; i < x_m_proc->n_args; i++ ) {
                std::string arg_name = std::string(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(x_m_proc->m_args[i])->m_v));
                if( self_arg_name == arg_name ) {
                    arg_found = true;
                    break ;
                }
            }
            require(arg_found, self_arg_name + " must be present in " +
                    std::string(x.m_name) + " procedures.");
        }
        verify_binding_override(x, x_m_proc);
        if (!check_external) return;
        require_id(x.m_dispatch_proc ||
                !ASRUtils::sealed_override_needs_adapter(x, x_m_proc),
            "asr.verify.struct_method.dispatch_required",
            "A sealed nonpolymorphic override requires an inherited-slot dispatch adapter");
        if (x.m_dispatch_proc) {
            auto *dispatch = ASRUtils::symbol_get_past_external(x.m_dispatch_proc);
            require_id(dispatch && ASR::is_a<ASR::Function_t>(*dispatch) &&
                    symtab_in_scope(symtab, x.m_dispatch_proc),
                "asr.verify.struct_method.dispatch_procedure",
                "A dispatch adapter must be an in-scope function");
            auto *adapter = ASR::down_cast<ASR::Function_t>(dispatch);
            require_id(adapter->m_symtab && adapter->m_symtab->parent == symtab->parent &&
                    adapter->m_access == ASR::accessType::Private,
                "asr.verify.struct_method.dispatch_owner",
                "A dispatch adapter must be private to its type's defining scope");
            require_conforming(ASRUtils::sealed_dispatch_adapter_mismatch(
                    x, *x_m_proc, *adapter),
                "asr.verify.struct_method.dispatch", x.base.base.loc);
        }
    }

    void visit_trait_requirement(const trait_requirement_t &x) {
        auto check = [&](bool condition, const std::string &code,
                const std::string &message) {
            ASRUtils::require_impl(condition, code, message, x.loc, diagnostics);
        };
        check(x.m_member && x.m_procedure,
            "asr.verify.trait_requirement.symbols_required",
            "trait requirement member and procedure must be present");
        if (!check_external) return;
        ASR::symbol_t *member = check_external
            ? ASRUtils::symbol_get_past_external(x.m_member) : x.m_member;
        ASR::symbol_t *procedure = check_external
            ? ASRUtils::symbol_get_past_external(x.m_procedure) : x.m_procedure;
        check(member != nullptr && ASR::is_a<ASR::Function_t>(*member),
            "asr.verify.trait_requirement.member_is_function",
            "A trait requirement member must be a Function, not " +
                std::string(x.m_member ? ASRUtils::symbol_type_name(*x.m_member) : "<null>"));
        if (member) {
            ASR::Function_t *member_fn = ASR::down_cast<ASR::Function_t>(member);
            check(member_fn->m_function_signature != nullptr &&
                    ASR::is_a<ASR::FunctionType_t>(*member_fn->m_function_signature),
                "asr.verify.trait_requirement.member_signature_required",
                "A trait requirement member must have a function signature");
            check(member_fn->n_body == 0,
                "asr.verify.trait_requirement.member_is_abstract",
                "A trait requirement member must be abstract");
            check(ASRUtils::get_FunctionType(*member_fn)->m_deftype == ASR::deftypeType::Interface,
                "asr.verify.trait_requirement.member_is_interface",
                "A trait requirement member must be an interface procedure");
        }
        check(procedure != nullptr && ASR::is_a<ASR::Function_t>(*procedure),
            "asr.verify.trait_requirement.procedure_is_function",
            "A trait requirement procedure must be a Function, not " +
                std::string(x.m_procedure ? ASRUtils::symbol_type_name(*x.m_procedure) : "<null>"));
        if (procedure) {
            ASR::Function_t *proc_fn = ASR::down_cast<ASR::Function_t>(procedure);
            check(proc_fn->m_function_signature != nullptr &&
                    ASR::is_a<ASR::FunctionType_t>(*proc_fn->m_function_signature),
                "asr.verify.trait_requirement.procedure_signature_required",
                "A trait requirement procedure must have a function signature");
            check(proc_fn->n_body == 0,
                "asr.verify.trait_requirement.procedure_is_abstract",
                "A trait requirement procedure must be abstract");
            check(ASRUtils::get_FunctionType(*proc_fn)->m_deftype == ASR::deftypeType::Interface,
                "asr.verify.trait_requirement.procedure_is_interface",
                "A trait requirement procedure must be an interface procedure");
            if (member) {
                ASR::Function_t *member_fn = ASR::down_cast<ASR::Function_t>(member);
                check(proc_fn->n_args == member_fn->n_args + 1,
                    "asr.verify.trait_requirement.normalized_arg_count",
                    "A trait requirement procedure must have one receiver argument more than its trait member");
                check(ASRUtils::get_FunctionType(*proc_fn)->m_is_restriction,
                    "asr.verify.trait_requirement.procedure_is_restriction",
                    "A normalized trait requirement must be a restriction procedure");
                auto mismatch = ASRUtils::trait_method_mismatch(*member_fn, *proc_fn, 0, 1);
                check(mismatch.difference == ASRUtils::TraitMethodDifference::None,
                    "asr.verify.trait_requirement.signature_matches",
                    "A normalized trait requirement must preserve its member's signature: "
                        + mismatch.message);
            }
        }
    }

    void visit_trait_binding(const trait_binding_t &x) {
        auto check = [&](bool condition, const std::string &code,
                const std::string &message) {
            ASRUtils::require_impl(condition, code, message, x.loc, diagnostics);
        };
        check(x.m_member && x.m_procedure,
            "asr.verify.trait_binding.symbols_required",
            "trait binding member and procedure must be present");
        if (x.m_is_nopass) {
            check(x.m_self_argument == nullptr,
                "asr.verify.trait_binding.nopass_has_no_receiver",
                "nopass trait bindings must not name a self argument");
        } else {
            check(x.m_self_argument != nullptr,
                "asr.verify.trait_binding.receiver_required",
                "trait bindings with pass receivers must name a self argument");
        }
        if (!check_external) return;
        ASR::symbol_t *member = check_external
            ? ASRUtils::symbol_get_past_external(x.m_member) : x.m_member;
        ASR::symbol_t *procedure = check_external
            ? ASRUtils::symbol_get_past_external(x.m_procedure) : x.m_procedure;
        check(ASRUtils::trait_method_function(member) != nullptr,
            "asr.verify.trait_binding.member_is_function",
            "A trait binding member must be an ordinary or generic procedure, not " +
                std::string(x.m_member ? ASRUtils::symbol_type_name(*x.m_member) : "<null>"));
        check(ASRUtils::trait_method_function(procedure) != nullptr,
            "asr.verify.trait_binding.procedure_is_function",
            "A trait binding procedure must be an ordinary or generic procedure, not " +
                std::string(x.m_procedure ? ASRUtils::symbol_type_name(*x.m_procedure) : "<null>"));
        if (procedure) {
            ASR::Function_t *proc = ASRUtils::trait_method_function(procedure);
            check(proc->m_function_signature != nullptr &&
                    ASR::is_a<ASR::FunctionType_t>(*proc->m_function_signature),
                "asr.verify.trait_binding.procedure_signature_required",
                "A trait binding procedure must have a function signature");
            check(ASRUtils::get_FunctionType(*proc)->m_deftype != ASR::deftypeType::Interface,
                "asr.verify.trait_binding.procedure_is_implementation",
                "A trait binding procedure must be a concrete implementation");
        }
        std::map<ASR::symbol_t*, ASR::symbol_t*> parameters;
        auto mismatch = ASRUtils::trait_generic_correspondence(
            *ASRUtils::trait_method_function(member),
            *ASRUtils::trait_method_function(procedure), parameters);
        check(mismatch.empty(), "asr.verify.trait_binding.generic_contract",
            "A generic binding must preserve its universally quantified contract: " + mismatch);
        if (x.m_self_argument) {
            ASR::Function_t *proc = ASRUtils::trait_method_function(procedure);
            bool found = false;
            for (size_t i = 0; i < proc->n_args; i++) {
                if (!ASR::is_a<ASR::Var_t>(*proc->m_args[i])) continue;
                ASR::symbol_t *arg = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(proc->m_args[i])->m_v);
                if (arg && ASR::is_a<ASR::Variable_t>(*arg) &&
                        std::string(ASRUtils::symbol_name(arg)) == x.m_self_argument) {
                    found = true;
                    break;
                }
            }
            check(found, "asr.verify.trait_binding.receiver_is_argument",
                "Trait binding self argument '" +
                std::string(x.m_self_argument) + "' is not present in procedure '" +
                std::string(x.m_procedure ? ASRUtils::symbol_name(x.m_procedure) : "<null>") + "'");
        }
    }

    ASRUtils::TraitHierarchy verify_trait_hierarchy(const ASR::Trait_t &trait,
            const Location &loc) {
        auto hierarchy = ASRUtils::trait_hierarchy(trait, check_external);
        ASRUtils::require_impl(hierarchy.error != ASRUtils::TraitHierarchyError::Cycle,
            "asr.verify.trait.inheritance_cycle",
            "Trait inheritance must not contain a cycle", loc, diagnostics);
        ASRUtils::require_impl(hierarchy.error != ASRUtils::TraitHierarchyError::Parent,
            "asr.verify.trait.parent_is_trait",
            "Trait parents must be resolved Trait symbols", loc, diagnostics);
        ASRUtils::require_impl(hierarchy.error != ASRUtils::TraitHierarchyError::Member,
            "asr.verify.trait.member_provenance",
            "Trait members must be Functions owned by their defining trait", loc, diagnostics);
        return hierarchy;
    }

    static bool type_set_concrete_kind(ASR::ttype_t *type) {
        if (!type || !(ASR::is_a<ASR::Integer_t>(*type) ||
                ASR::is_a<ASR::Real_t>(*type) ||
                ASR::is_a<ASR::Complex_t>(*type) ||
                ASR::is_a<ASR::Logical_t>(*type))) return false;
        int64_t kind = ASRUtils::extract_kind_from_ttype_t(type);
        // The ordinary type verifier also admits unresolved PDT kinds >= 1000.
        return kind > 0 && kind < 1000;
    }

    void visit_Trait(const Trait_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_name != nullptr,
            "The Trait::m_name cannot be nullptr");
        require(x.m_symtab != nullptr,
            "The Trait::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The Trait::m_symtab->parent is not the right parent");
        ASR::symbol_t *trait_scope_owner = nullptr;
        if (x.m_symtab->parent != nullptr &&
                x.m_symtab->parent->asr_owner != nullptr &&
                ASR::is_a<ASR::symbol_t>(*x.m_symtab->parent->asr_owner)) {
            trait_scope_owner = ASR::down_cast<ASR::symbol_t>(x.m_symtab->parent->asr_owner);
        }
        require_id(trait_scope_owner != nullptr &&
                (ASR::is_a<ASR::Module_t>(*trait_scope_owner) ||
                 ASR::is_a<ASR::Program_t>(*trait_scope_owner) ||
                 ASR::is_a<ASR::Template_t>(*trait_scope_owner) ||
                 (ASR::is_a<ASR::Function_t>(*trait_scope_owner) &&
                  ASRUtils::trait_runtime_contract(const_cast<symbol_t*>(&x.base)) &&
                  ASRUtils::trait_runtime_contract(const_cast<symbol_t*>(&x.base))->m_anonymous)),
            "asr.verify.trait.scope_is_module",
            "Traits must be declared in a module, program or defining template scope");
        if (ASR::is_a<ASR::Template_t>(*trait_scope_owner)) {
            require_id(x.m_kind == ASR::trait_kindType::IntrinsicTypeSet &&
                    x.m_access == ASR::accessType::Private,
                "asr.verify.trait.inline_is_private_type_set",
                "A template-owned trait must be a private intrinsic type set");
            size_t bindings = 0;
            for (const auto &entry : parent_symtab->get_scope()) {
                if (!ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) continue;
                auto *constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
                if (constraint->m_trait == &x.base) bindings++;
            }
            require_id(bindings == 1,
                "asr.verify.trait.inline_has_one_binder",
                "An inline type set must belong to exactly one defining generic binder");
        }
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "Trait::m_symtab->counter must be unique");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The Trait::m_symtab::asr_owner must point to Trait");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        require_id(x.m_kind == ASR::trait_kindType::UniversalTrait ||
                x.m_kind == ASR::trait_kindType::IntrinsicTypeSet,
            "asr.verify.trait.category", "Trait category must be valid");
        require_id(x.m_kind != ASR::trait_kindType::UniversalTrait ||
                x.n_member_types == 0,
            "asr.verify.trait.universal_has_no_members",
            "A universal trait must not have finite type members");
        if (x.m_kind == ASR::trait_kindType::IntrinsicTypeSet) {
            require_id(x.n_member_types > 0 && x.m_member_types &&
                    x.n_parents == 0 && x.m_symtab->get_scope().empty(),
                "asr.verify.trait.finite_category",
                "An intrinsic type set must be nonempty and have no nominal methods or parents");
            for (size_t i = 0; i < x.n_member_types; i++) {
                auto *type = x.m_member_types[i];
                require_id(type && (ASR::is_a<ASR::Integer_t>(*type) ||
                        ASR::is_a<ASR::Real_t>(*type) ||
                        ASR::is_a<ASR::Complex_t>(*type)) &&
                        type_set_concrete_kind(type),
                    "asr.verify.trait.numeric_member",
                    "An intrinsic type-set member must be a concrete scalar numeric type");
                visit_ttype(*type);
                for (size_t j = 0; j < i; j++) {
                    require_id(!ASRUtils::types_equal(
                            type, x.m_member_types[j], nullptr, nullptr),
                        "asr.verify.trait.unique_member",
                        "Type-set members must be unique");
                }
            }
        }
        for (auto &a : x.m_symtab->get_scope()) {
            require_id(ASRUtils::trait_method_function(a.second) != nullptr,
                "asr.verify.trait.member_is_function",
                "Trait '" + std::string(x.m_name) + "' members must be abstract procedures");
            ASR::Function_t *fn = ASRUtils::trait_method_function(a.second);
            require_id(fn->m_function_signature != nullptr,
                "asr.verify.trait.member_signature_required",
                "Trait '" + std::string(x.m_name) +
                "' members must have a signature");
            ASR::FunctionType_t *ftype = ASRUtils::get_FunctionType(*fn);
            require_id(ftype->m_deftype == ASR::deftypeType::Interface,
                "asr.verify.trait.member_is_interface",
                "Trait '" + std::string(x.m_name) +
                "' members must be abstract interface procedures");
            require_id(fn->n_body == 0,
                "asr.verify.trait.member_has_no_body",
                "Trait '" + std::string(x.m_name) +
                "' members must not have a body");
            this->visit_symbol(*a.second);
        }
        require(x.n_parents == 0 || x.m_parents != nullptr,
            "Trait parent references must be present");
        for (size_t i = 0; i < x.n_parents; i++) {
            require(x.m_parents[i] != nullptr,
                "Trait parent cannot be nullptr");
            require_id(symtab_in_scope(x.m_symtab->parent, x.m_parents[i]),
                "asr.verify.trait.parent_in_scope",
                "Trait parents must be referenced through visible symbols");
        }
        auto hierarchy = verify_trait_hierarchy(x, x.base.base.loc);
        if (check_external) {
            for (auto *trait : hierarchy.traits) {
                require_id(trait == &x ||
                        trait->m_kind == ASR::trait_kindType::UniversalTrait,
                    "asr.verify.trait.universal_parent",
                    "Inheritance from a type-set trait is not supported");
            }
            std::map<std::string, ASR::Function_t*> methods;
            for (ASR::symbol_t *member : hierarchy.members) {
                auto *method = ASRUtils::trait_method_function(member);
                auto previous = methods.emplace(method->m_name, method);
                if (!previous.second) {
                    auto mismatch = ASRUtils::trait_method_mismatch(
                        *previous.first->second, *method);
                    require_id(mismatch.difference == ASRUtils::TraitMethodDifference::None,
                        "asr.verify.trait.inherited_signature_matches",
                        "Same-name trait requirements must have equivalent signatures: " +
                            mismatch.message);
                }
            }
        }
        current_symtab = parent_symtab;
    }

    static bool same_variable(ASR::expr_t *left, ASR::expr_t *right) {
        return left && right && ASR::is_a<ASR::Var_t>(*left) &&
            ASR::is_a<ASR::Var_t>(*right) &&
            ASR::down_cast<ASR::Var_t>(left)->m_v ==
                ASR::down_cast<ASR::Var_t>(right)->m_v;
    }

    bool type_set_witness_expression(const ASR::type_set_requirement_t &requirement,
            ASR::Function_t *witness, ASR::ttype_t *member, ASR::expr_t *value) {
        auto *operation = requirement.m_operation;
        if (ASR::is_a<ASR::TypeSetBinary_t>(*operation)) {
            ASR::expr_t *left = nullptr, *right = nullptr, *constant = nullptr;
            ASR::binopType op = ASR::binopType::Pow;
            if (ASR::is_a<ASR::IntegerBinOp_t>(*value) && ASR::is_a<ASR::Integer_t>(*member)) {
                auto *expr = ASR::down_cast<ASR::IntegerBinOp_t>(value);
                left = expr->m_left; right = expr->m_right; op = expr->m_op; constant = expr->m_value;
            } else if (ASR::is_a<ASR::RealBinOp_t>(*value) && ASR::is_a<ASR::Real_t>(*member)) {
                auto *expr = ASR::down_cast<ASR::RealBinOp_t>(value);
                left = expr->m_left; right = expr->m_right; op = expr->m_op; constant = expr->m_value;
            } else if (ASR::is_a<ASR::ComplexBinOp_t>(*value) && ASR::is_a<ASR::Complex_t>(*member)) {
                auto *expr = ASR::down_cast<ASR::ComplexBinOp_t>(value);
                left = expr->m_left; right = expr->m_right; op = expr->m_op; constant = expr->m_value;
            }
            return witness->n_args == 2 && !constant &&
                op == ASR::down_cast<ASR::TypeSetBinary_t>(operation)->m_op &&
                (op == ASR::Add || op == ASR::Sub || op == ASR::Mul || op == ASR::Div) &&
                same_variable(left, witness->m_args[0]) &&
                same_variable(right, witness->m_args[1]);
        }
        if (ASR::is_a<ASR::TypeSetComparison_t>(*operation)) {
            ASR::expr_t *left = nullptr, *right = nullptr, *constant = nullptr;
            auto op = ASR::down_cast<ASR::TypeSetComparison_t>(operation)->m_op;
            if (ASR::is_a<ASR::IntegerCompare_t>(*value) && ASR::is_a<ASR::Integer_t>(*member)) {
                auto *expr = ASR::down_cast<ASR::IntegerCompare_t>(value);
                if (expr->m_op != op) return false;
                left = expr->m_left; right = expr->m_right; constant = expr->m_value;
            } else if (ASR::is_a<ASR::RealCompare_t>(*value) && ASR::is_a<ASR::Real_t>(*member)) {
                auto *expr = ASR::down_cast<ASR::RealCompare_t>(value);
                if (expr->m_op != op) return false;
                left = expr->m_left; right = expr->m_right; constant = expr->m_value;
            } else if (ASR::is_a<ASR::ComplexCompare_t>(*value) && ASR::is_a<ASR::Complex_t>(*member)) {
                auto *expr = ASR::down_cast<ASR::ComplexCompare_t>(value);
                if (expr->m_op != op || (op != ASR::Eq && op != ASR::NotEq)) return false;
                left = expr->m_left; right = expr->m_right; constant = expr->m_value;
            }
            return witness->n_args == 2 && !constant &&
                same_variable(left, witness->m_args[0]) &&
                same_variable(right, witness->m_args[1]);
        }
        if (witness->n_args != 1) return false;
        auto *arg = witness->m_args[0];
        auto *source = ASRUtils::expr_type(arg);
        if (!ASR::is_a<ASR::Integer_t>(*source) && !ASR::is_a<ASR::Real_t>(*source) &&
                !ASR::is_a<ASR::Complex_t>(*source)) return false;
        if (same_variable(value, arg)) {
            return ASRUtils::types_equal(source, member, nullptr, nullptr);
        }
        if (ASR::is_a<ASR::Cast_t>(*value)) {
            auto *cast = ASR::down_cast<ASR::Cast_t>(value);
            if (!same_variable(cast->m_arg, arg) || cast->m_value ||
                    cast->m_dest) return false;
            ASR::cast_kindType expected;
            if (ASR::is_a<ASR::Integer_t>(*member)) {
                expected = ASR::is_a<ASR::Integer_t>(*source) ? ASR::cast_kindType::IntegerToInteger :
                    ASR::is_a<ASR::Real_t>(*source) ? ASR::cast_kindType::RealToInteger :
                    ASR::cast_kindType::ComplexToInteger;
            } else if (ASR::is_a<ASR::Real_t>(*member)) {
                expected = ASR::is_a<ASR::Integer_t>(*source) ? ASR::cast_kindType::IntegerToReal :
                    ASR::is_a<ASR::Real_t>(*source) ? ASR::cast_kindType::RealToReal :
                    ASR::cast_kindType::ComplexToReal;
            } else {
                expected = ASR::is_a<ASR::Integer_t>(*source) ? ASR::cast_kindType::IntegerToComplex :
                    ASR::is_a<ASR::Real_t>(*source) ? ASR::cast_kindType::RealToComplex :
                    ASR::cast_kindType::ComplexToComplex;
            }
            return cast->m_kind == expected;
        }
        if (!ASR::is_a<ASR::IntrinsicElementalFunction_t>(*value)) return false;
        auto *call = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(value);
        using Intrinsic = ASRUtils::IntrinsicElementalFunctions;
        auto expected = ASR::is_a<ASR::Integer_t>(*member) ? Intrinsic::Int :
            ASR::is_a<ASR::Real_t>(*member) ? Intrinsic::Real : Intrinsic::Cmplx;
        if (call->m_intrinsic_id != static_cast<int64_t>(expected) ||
                call->m_value || call->m_overload_id != 0 || !call->n_args ||
                !same_variable(call->m_args[0], arg)) return false;
        if (expected != Intrinsic::Cmplx) return call->n_args == 1;
        if (call->n_args != 3 || !call->m_args[1] || !call->m_args[2] ||
                !ASR::is_a<ASR::IntegerConstant_t>(*call->m_args[2]) ||
                ASR::down_cast<ASR::IntegerConstant_t>(call->m_args[2])->m_n !=
                    ASRUtils::extract_kind_from_ttype_t(member)) return false;
        return ASR::is_a<ASR::RealConstant_t>(*call->m_args[1]) &&
            ASR::down_cast<ASR::RealConstant_t>(call->m_args[1])->m_r == 0.0;
    }

    void verify_type_set_requirements(const TraitConstraint_t &constraint,
            const Trait_t &trait) {
        const auto &x = constraint;
        require_id(ASR::is_a<ASR::Template_t>(
                *ASRUtils::get_asr_owner(&constraint.base)),
            "asr.verify.type_set.template_scope",
            "An intrinsic type-set constraint must belong to a template");
        auto local_variable = [](ASR::expr_t *expr, ASR::Function_t *function,
                ASR::intentType intent) {
            if (!expr || !ASR::is_a<ASR::Var_t>(*expr)) return false;
            auto *symbol = ASR::down_cast<ASR::Var_t>(expr)->m_v;
            if (!symbol || !ASR::is_a<ASR::Variable_t>(*symbol)) return false;
            auto *variable = ASR::down_cast<ASR::Variable_t>(symbol);
            return variable->m_name && variable->m_type &&
                variable->m_parent_symtab == function->m_symtab &&
                function->m_symtab->get_symbol(variable->m_name) == symbol &&
                variable->m_intent == intent &&
                variable->m_presence == ASR::presenceType::Required;
        };
        require_id(constraint.n_requirements == 0,
            "asr.verify.type_set.no_nominal_requirements",
            "An intrinsic type set has no nominal receiver requirements");
        require_id(constraint.n_intrinsic_requirements == 0 ||
                constraint.m_intrinsic_requirements,
            "asr.verify.type_set.requirements_present",
            "Intrinsic requirements must be present");
        std::set<ASR::symbol_t*> procedures;
        const std::string parameter = ASR::down_cast<ASR::TypeParameter_t>(
            ASRUtils::symbol_type(constraint.m_parameter))->m_param;
        for (size_t i = 0; i < constraint.n_intrinsic_requirements; i++) {
            const auto &requirement = constraint.m_intrinsic_requirements[i];
            require_id(requirement.m_procedure &&
                    ASR::is_a<ASR::Function_t>(*requirement.m_procedure) &&
                    ASR::down_cast<ASR::Function_t>(requirement.m_procedure)->m_symtab &&
                    ASRUtils::symbol_parent_symtab(requirement.m_procedure) ==
                        constraint.m_parent_symtab &&
                    procedures.insert(requirement.m_procedure).second &&
                    requirement.m_operation,
                "asr.verify.type_set.requirement_scope",
                "Each intrinsic restriction must be a unique function owned by its constraint scope");
            auto *procedure = ASR::down_cast<ASR::Function_t>(requirement.m_procedure);
            require_id(procedure->m_function_signature &&
                    ASR::is_a<ASR::FunctionType_t>(*procedure->m_function_signature) &&
                    ASRUtils::get_FunctionType(procedure)->m_is_restriction &&
                    ASRUtils::get_FunctionType(procedure)->m_deftype == ASR::deftypeType::Interface &&
                    procedure->n_body == 0 &&
                    local_variable(procedure->m_return_var, procedure, ASR::intentType::ReturnVar),
                "asr.verify.type_set.restriction",
                "An intrinsic requirement must be a bodyless function restriction");
            const bool conversion = ASR::is_a<ASR::TypeSetConversion_t>(*requirement.m_operation);
            require_id(procedure->n_args == (conversion ? 1u : 2u) && procedure->m_args,
                "asr.verify.type_set.arity",
                "An intrinsic restriction must have the operation's arity");
            for (size_t j = 0; j < procedure->n_args; j++) {
                require_id(local_variable(procedure->m_args[j], procedure, ASR::intentType::In),
                    "asr.verify.type_set.restriction_arguments",
                    "An intrinsic restriction has required read-only arguments");
            }
            for (size_t j = 0; j <= procedure->n_args; j++) {
                auto *type = ASRUtils::expr_type(j == procedure->n_args ?
                    procedure->m_return_var : procedure->m_args[j]);
                bool deferred = ASR::is_a<ASR::TypeParameter_t>(*type);
                require_id((deferred && parameter ==
                            ASR::down_cast<ASR::TypeParameter_t>(type)->m_param) ||
                        (!deferred && type_set_concrete_kind(type) &&
                            (ASR::is_a<ASR::Integer_t>(*type) ||
                            ASR::is_a<ASR::Real_t>(*type) || ASR::is_a<ASR::Complex_t>(*type) ||
                            (j == procedure->n_args && ASR::is_a<ASR::Logical_t>(*type)))),
                    "asr.verify.type_set.signature",
                    "An intrinsic restriction must use its own scalar binder or a concrete scalar type");
                require_id(conversion || j == procedure->n_args || deferred,
                    "asr.verify.type_set.same_binder",
                    "Binary intrinsic requirements must have two operands of their binder type");
            }
            require_id(requirement.n_witnesses == trait.n_member_types &&
                    requirement.m_witnesses,
                "asr.verify.type_set.total_proof",
                "An intrinsic requirement must prove every admitted member");
            std::set<size_t> members;
            for (size_t j = 0; j < requirement.n_witnesses; j++) {
                const auto &proof = requirement.m_witnesses[j];
                size_t index = trait.n_member_types;
                for (size_t k = 0; k < trait.n_member_types; k++) {
                    if (proof.m_member_type && ASRUtils::types_equal(
                            proof.m_member_type, trait.m_member_types[k], nullptr, nullptr)) index = k;
                }
                require_id(index != trait.n_member_types,
                    "asr.verify.type_set.member_in_set",
                    "A capability witness must belong to the declared type set");
                require_id(members.insert(index).second,
                    "asr.verify.type_set.unique_witness",
                    "A capability proof must not repeat a member");
                require_id(proof.m_procedure && ASR::is_a<ASR::Function_t>(*proof.m_procedure) &&
                        ASR::down_cast<ASR::Function_t>(proof.m_procedure)->m_symtab &&
                        ASRUtils::symbol_parent_symtab(proof.m_procedure) ==
                            constraint.m_parent_symtab,
                    "asr.verify.type_set.witness_scope",
                    "A capability witness must be a function owned by its template");
                auto *witness = ASR::down_cast<ASR::Function_t>(proof.m_procedure);
                require_id(witness->m_function_signature &&
                        ASR::is_a<ASR::FunctionType_t>(*witness->m_function_signature) &&
                        ASRUtils::get_FunctionType(witness)->m_deftype == ASR::deftypeType::Implementation &&
                        !ASRUtils::get_FunctionType(witness)->m_is_restriction &&
                        witness->n_args == procedure->n_args && witness->m_args &&
                        local_variable(witness->m_return_var, witness, ASR::intentType::ReturnVar),
                    "asr.verify.type_set.witness_signature",
                    "A capability witness must implement the restriction signature");
                require_id(witness->m_symtab->get_scope().size() == witness->n_args + 1,
                    "asr.verify.type_set.witness_locals",
                    "A one-operation witness has only its arguments and result");
                for (const auto &entry : witness->m_symtab->get_scope()) {
                    require_id(ASR::is_a<ASR::Variable_t>(*entry.second),
                        "asr.verify.type_set.witness_locals",
                        "Capability witness locals must be concrete variables");
                    auto *variable = ASR::down_cast<ASR::Variable_t>(entry.second);
                    require_id(variable->m_type &&
                            type_set_concrete_kind(variable->m_type) &&
                            !variable->m_symbolic_value && !variable->m_value &&
                            variable->m_storage == ASR::storage_typeType::Default &&
                            variable->m_presence == ASR::presenceType::Required,
                        "asr.verify.type_set.witness_locals",
                        "Capability witness variables must be concrete and have no initializers");
                }
                std::set<ASR::symbol_t*> arguments;
                for (size_t k = 0; k < witness->n_args; k++) {
                    require_id(local_variable(witness->m_args[k], witness, ASR::intentType::In) &&
                            arguments.insert(ASR::down_cast<ASR::Var_t>(witness->m_args[k])->m_v).second &&
                            ASRUtils::EXPR2VAR(witness->m_args[k])->m_intent == ASR::intentType::In,
                        "asr.verify.type_set.witness_arguments",
                        "Capability witness arguments must be distinct read-only variables");
                }
                for (size_t k = 0; k <= procedure->n_args; k++) {
                    auto *abstract = ASRUtils::expr_type(k == procedure->n_args ?
                        procedure->m_return_var : procedure->m_args[k]);
                    auto *concrete = ASRUtils::expr_type(k == witness->n_args ?
                        witness->m_return_var : witness->m_args[k]);
                    auto *expected = ASR::is_a<ASR::TypeParameter_t>(*abstract) ?
                        proof.m_member_type : abstract;
                    require_id(ASRUtils::types_equal(concrete, expected, nullptr, nullptr),
                        "asr.verify.type_set.witness_type",
                        "Witness arguments and result must have the substituted category and kind");
                }
                require_id(witness->n_body == 1 && witness->m_body &&
                        ASR::is_a<ASR::Assignment_t>(*witness->m_body[0]),
                    "asr.verify.type_set.witness_body",
                    "A capability witness must contain exactly one operation assignment");
                auto *assignment = ASR::down_cast<ASR::Assignment_t>(witness->m_body[0]);
                require_id(same_variable(assignment->m_target, witness->m_return_var) &&
                        assignment->m_value && !assignment->m_overloaded &&
                        ASRUtils::types_equal(ASRUtils::expr_type(assignment->m_value),
                            ASRUtils::expr_type(witness->m_return_var), nullptr, nullptr) &&
                        type_set_witness_expression(requirement, witness,
                            proof.m_member_type, assignment->m_value),
                    "asr.verify.type_set.witness_operation",
                    "The witness body must be precisely the recorded intrinsic operation");
                bool comparison = ASR::is_a<ASR::TypeSetComparison_t>(*requirement.m_operation);
                auto *result_type = ASRUtils::expr_type(witness->m_return_var);
                require_id(comparison ? ASR::is_a<ASR::Logical_t>(*result_type) :
                        ASRUtils::types_equal(result_type, proof.m_member_type, nullptr, nullptr),
                    "asr.verify.type_set.operation_result",
                    "Comparison results must be logical; arithmetic and conversions must preserve the member kind");
            }
        }
    }

    void visit_TraitConstraint(const TraitConstraint_t &x) {
        require(x.m_name != nullptr,
            "The TraitConstraint::m_name cannot be nullptr");
        require(x.m_parent_symtab != nullptr,
            "TraitConstraint::m_parent_symtab cannot be nullptr");
        require(x.m_parent_symtab->get_symbol(std::string(x.m_name)) != nullptr,
            "TraitConstraint '" + std::string(x.m_name) +
            "' not found in parent_symtab symbol table");
        ASR::symbol_t *constraint_scope_owner = nullptr;
        if (x.m_parent_symtab->asr_owner != nullptr &&
                ASR::is_a<ASR::symbol_t>(*x.m_parent_symtab->asr_owner)) {
            constraint_scope_owner =
                ASR::down_cast<ASR::symbol_t>(x.m_parent_symtab->asr_owner);
        }
        require_id(constraint_scope_owner != nullptr &&
                (ASR::is_a<ASR::Function_t>(*constraint_scope_owner) ||
                 ASR::is_a<ASR::Template_t>(*constraint_scope_owner)),
            "asr.verify.trait_constraint.scope_is_generic",
            "TraitConstraint must be declared in a generic function or template scope");
        symbol_t *symtab_sym = x.m_parent_symtab->get_symbol(std::string(x.m_name));
        const symbol_t *current_sym = &x.base;
        require(symtab_sym == current_sym,
            "TraitConstraint's parent symbol table does not point to it");
        require(id_symtab_map.find(x.m_parent_symtab->counter) != id_symtab_map.end(),
            "TraitConstraint::m_parent_symtab must be present in the ASR ("
                + std::string(x.m_name) + ")");
        if (!check_external) return;
        ASR::symbol_t *parameter = check_external
            ? ASRUtils::symbol_get_past_external(x.m_parameter) : x.m_parameter;
        ASR::symbol_t *trait = check_external
            ? ASRUtils::symbol_get_past_external(x.m_trait) : x.m_trait;
        require_id(parameter != nullptr && ASR::is_a<ASR::Variable_t>(*parameter),
            "asr.verify.trait_constraint.parameter_is_variable",
            "TraitConstraint parameter must be a Variable, not " +
                std::string(x.m_parameter ? ASRUtils::symbol_type_name(*x.m_parameter) : "<null>"));
        if (parameter) {
            ASR::Variable_t *param = ASR::down_cast<ASR::Variable_t>(parameter);
            require_id(ASRUtils::symbol_parent_symtab(parameter) == x.m_parent_symtab,
                "asr.verify.trait_constraint.parameter_scope",
                "TraitConstraint parameter must be declared in its parent symbol table");
            require_id(param->m_type != nullptr &&
                    ASR::is_a<ASR::TypeParameter_t>(*param->m_type),
                "asr.verify.trait_constraint.parameter_is_type_parameter",
                "TraitConstraint parameter must have a TypeParameter type");
        }
        require_id(trait != nullptr && ASR::is_a<ASR::Trait_t>(*trait),
            "asr.verify.trait_constraint.trait_is_trait",
            "TraitConstraint trait must be a Trait, not " +
                std::string(x.m_trait ? ASRUtils::symbol_type_name(*x.m_trait) : "<null>"));
        require_id(ASR::down_cast<ASR::Trait_t>(trait)->m_kind ==
                ASR::trait_kindType::IntrinsicTypeSet || x.n_intrinsic_requirements == 0,
            "asr.verify.trait_constraint.intrinsic_category",
            "Only a type-set constraint can have intrinsic requirements");
        if (ASR::down_cast<ASR::Trait_t>(trait)->m_kind ==
                ASR::trait_kindType::IntrinsicTypeSet) {
            verify_type_set_requirements(x, *ASR::down_cast<ASR::Trait_t>(trait));
        }
        auto hierarchy = verify_trait_hierarchy(
            *ASR::down_cast<ASR::Trait_t>(trait), x.base.base.loc);
        std::set<ASR::symbol_t*> trait_members(
            hierarchy.members.begin(), hierarchy.members.end());
        std::set<ASR::symbol_t*> required_members;
        for (size_t i = 0; i < x.n_requirements; i++) {
            visit_trait_requirement(x.m_requirements[i]);
            ASR::symbol_t *member = check_external
                ? ASRUtils::symbol_get_past_external(x.m_requirements[i].m_member)
                : x.m_requirements[i].m_member;
            require_id(member != nullptr &&
                    ASR::is_a<ASR::Function_t>(*member),
                "asr.verify.trait_constraint.member_is_function",
                "TraitConstraint requirement member must be a Function");
            require_id(trait_members.count(member) != 0,
                "asr.verify.trait_constraint.member_belongs_to_trait",
                "TraitConstraint requirement member must belong to the trait");
            require_id(required_members.insert(member).second,
                "asr.verify.trait_constraint.member_is_unique",
                "TraitConstraint must not repeat an original member");
            ASR::symbol_t *procedure = ASRUtils::symbol_get_past_external(
                x.m_requirements[i].m_procedure);
            require_id(ASRUtils::symbol_parent_symtab(procedure) == x.m_parent_symtab,
                "asr.verify.trait_constraint.procedure_scope",
                "A normalized requirement must belong to its generic parameter's scope");
            auto *function = ASR::down_cast<ASR::Function_t>(procedure);
            ASR::expr_t *receiver = function->m_args[0];
            require_id(receiver && ASR::is_a<ASR::Var_t>(*receiver) &&
                    ASR::down_cast<ASR::Var_t>(receiver)->m_v &&
                    ASR::is_a<ASR::Variable_t>(
                        *ASR::down_cast<ASR::Var_t>(receiver)->m_v),
                "asr.verify.trait_constraint.receiver_is_variable",
                "A normalized requirement must have a receiver variable");
            auto *self = ASRUtils::EXPR2VAR(receiver);
            require_id(self->m_type && ASR::is_a<ASR::TypeParameter_t>(*self->m_type) &&
                    self->m_intent == ASR::intentType::In &&
                    self->m_presence == ASR::presenceType::Required &&
                    !self->m_value_attr && !self->m_type_declaration &&
                    std::string(ASR::down_cast<ASR::TypeParameter_t>(
                        self->m_type)->m_param) ==
                        ASR::down_cast<ASR::TypeParameter_t>(
                            ASRUtils::symbol_type(parameter))->m_param,
                "asr.verify.trait_constraint.receiver_matches_parameter",
                "A normalized receiver must be the constraint's read-only type parameter");
            auto callable = trait_callables.emplace(
                std::make_pair(parameter, std::string(ASRUtils::symbol_name(member))),
                procedure);
            require_id(callable.second || callable.first->second == procedure,
                "asr.verify.trait_constraint.callable_is_coalesced",
                "Same-name requirements of one parameter must share a normalized procedure");
        }
        require_id(required_members.size() == trait_members.size(),
            "asr.verify.trait_constraint.requirements_complete",
            "TraitConstraint requirements must cover every original trait member");
    }

    void visit_TraitImplementation(const TraitImplementation_t &x) {
        require(x.m_name != nullptr,
            "The TraitImplementation::m_name cannot be nullptr");
        require(x.m_parent_symtab != nullptr,
            "TraitImplementation::m_parent_symtab cannot be nullptr");
        require(x.m_parent_symtab->get_symbol(std::string(x.m_name)) != nullptr,
            "TraitImplementation '" + std::string(x.m_name) +
            "' not found in parent_symtab symbol table");
        ASR::symbol_t *impl_scope_owner = nullptr;
        if (x.m_parent_symtab->asr_owner != nullptr &&
                ASR::is_a<ASR::symbol_t>(*x.m_parent_symtab->asr_owner)) {
            impl_scope_owner = ASR::down_cast<ASR::symbol_t>(x.m_parent_symtab->asr_owner);
        }
        require_id(impl_scope_owner != nullptr &&
                (ASR::is_a<ASR::Module_t>(*impl_scope_owner) ||
                 ASR::is_a<ASR::Program_t>(*impl_scope_owner)),
            "asr.verify.trait_implementation.scope_is_module",
            "TraitImplementation must be declared in a module or program scope");
        symbol_t *symtab_sym = x.m_parent_symtab->get_symbol(std::string(x.m_name));
        const symbol_t *current_sym = &x.base;
        require(symtab_sym == current_sym,
            "TraitImplementation's parent symbol table does not point to it");
        require(id_symtab_map.find(x.m_parent_symtab->counter) != id_symtab_map.end(),
            "TraitImplementation::m_parent_symtab must be present in the ASR ("
                + std::string(x.m_name) + ")");
        require(x.m_implementing_type != nullptr,
            "TraitImplementation::m_implementing_type cannot be nullptr");
        require_id(!ASR::is_a<ASR::TypeParameter_t>(*x.m_implementing_type),
            "asr.verify.trait_implementation.type_is_concrete",
            "TraitImplementation implementing type must be concrete");
        if (!check_external) {
            visit_ttype(*x.m_implementing_type);
            return;
        }
        if (ASR::is_a<ASR::StructType_t>(*x.m_implementing_type)) {
            require_id(x.m_type_declaration != nullptr &&
                    ASR::is_a<ASR::Struct_t>(
                        *ASRUtils::symbol_get_past_external(x.m_type_declaration)),
                "asr.verify.trait_implementation.nominal_type_required",
                "a derived-type conformance must identify its nominal type");
        } else {
            require_id(x.m_type_declaration == nullptr &&
                    type_set_concrete_kind(x.m_implementing_type),
                "asr.verify.trait_implementation.intrinsic_type",
                "An intrinsic conformance requires a scalar numeric or logical type and no nominal declaration");
        }
        ASR::symbol_t *trait = check_external
            ? ASRUtils::symbol_get_past_external(x.m_trait) : x.m_trait;
        require_id(trait != nullptr && ASR::is_a<ASR::Trait_t>(*trait),
            "asr.verify.trait_implementation.trait_is_trait",
            "TraitImplementation trait must be a Trait, not " +
                std::string(x.m_trait ? ASRUtils::symbol_type_name(*x.m_trait) : "<null>"));
        require_id(ASR::down_cast<ASR::Trait_t>(trait)->m_kind ==
                ASR::trait_kindType::UniversalTrait,
            "asr.verify.trait_implementation.not_type_set",
            "An intrinsic type set cannot be manually implemented");
        visit_ttype(*x.m_implementing_type);
        auto hierarchy = verify_trait_hierarchy(
            *ASR::down_cast<ASR::Trait_t>(trait), x.base.base.loc);
        std::set<ASR::symbol_t*> trait_members(
            hierarchy.members.begin(), hierarchy.members.end());
        std::set<ASR::symbol_t*> bound_members;
        std::map<std::string, const ASR::trait_binding_t*> methods;
        for (size_t i = 0; i < x.n_bindings; i++) {
            visit_trait_binding(x.m_bindings[i]);
            ASR::symbol_t *member = check_external
                ? ASRUtils::symbol_get_past_external(x.m_bindings[i].m_member)
                : x.m_bindings[i].m_member;
            require_id(member != nullptr &&
                    ASRUtils::trait_method_function(member) != nullptr,
                "asr.verify.trait_implementation.member_is_function",
                "TraitImplementation binding member must be an ordinary or generic procedure");
            std::string member_name = ASRUtils::symbol_name(member);
            require(bound_members.insert(member).second,
                "TraitImplementation member '" + member_name +
                "' appears more than once");
            require_id(trait_members.count(member) > 0,
                "asr.verify.trait_implementation.member_belongs_to_trait",
                "TraitImplementation binding member '" + member_name +
                "' does not belong to the trait");
            auto previous = methods.emplace(member_name, &x.m_bindings[i]);
            require_id(previous.second || ASRUtils::trait_bindings_equal(
                    *previous.first->second, x.m_bindings[i]),
                "asr.verify.trait_implementation.coalesced_binding_agrees",
                "Equivalent inherited methods must have the same procedure and receiver binding");
        }
        require(bound_members.size() == trait_members.size(),
            "TraitImplementation bindings must cover each trait member exactly once");
        for (size_t i = 0; i < x.n_bindings; i++) {
            verify_trait_binding(x.m_type_declaration, x.m_bindings[i], false, x.m_implementing_type);
        }
    }

    Function_t *verify_runtime_trait_procedure(symbol_t *reference,
            const Location &loc, const std::string &code) {
        auto *procedure = ASRUtils::trait_method_function(reference);
        require_with_loc_id(procedure != nullptr,
            code, "A runtime trait procedure reference must name a function", loc);
        require_with_loc_id(procedure->m_name && procedure->m_symtab &&
                procedure->m_symtab->parent && procedure->m_function_signature &&
                ASR::is_a<FunctionType_t>(*procedure->m_function_signature) &&
                (!procedure->n_args || procedure->m_args),
            "asr.verify.trait_procedure.signature",
            "A runtime trait procedure requires a scope, signature and argument declarations", loc);
        for (size_t i = 0; i < procedure->n_args; i++) {
            auto *arg = procedure->m_args[i];
            auto *variable = arg && ASR::is_a<Var_t>(*arg)
                ? ASRUtils::symbol_get_past_external(ASR::down_cast<Var_t>(arg)->m_v)
                : nullptr;
            require_with_loc_id(variable && ASR::is_a<Variable_t>(*variable) &&
                    ASRUtils::symbol_name(variable) && typed_expr_type(arg),
                "asr.verify.trait_procedure.argument",
                "A runtime trait procedure argument must name a typed variable", loc);
        }
        require_with_loc_id(!procedure->m_return_var ||
                typed_expr_type(procedure->m_return_var),
            "asr.verify.trait_procedure.result",
            "A runtime trait function requires a typed result", loc);
        return procedure;
    }

    Function_t *verify_runtime_trait_slot(const TraitRuntimeContract_t &contract,
            size_t index, const Location &loc) {
        require_with_loc_id(index < contract.n_slots && contract.m_slots,
            "asr.verify.trait_contract.complete",
            "A runtime contract must declare each referenced slot", loc);
        auto &slot = contract.m_slots[index];
        require_with_loc_id(slot.n_origins && slot.m_origins,
            "asr.verify.trait_contract.origins",
            "A callable slot must retain its nominal origins", loc);
        auto *procedure = verify_runtime_trait_procedure(slot.m_procedure, loc,
            "asr.verify.trait_contract.procedure");
        require_with_loc_id(procedure->m_symtab->parent == contract.m_symtab,
            "asr.verify.trait_contract.procedure",
            "A callable slot must own its normalized interface", loc);
        require_with_loc_id(procedure->n_args > 0 &&
                ASR::is_a<TraitObjectType_t>(*typed_expr_type(procedure->m_args[0])) &&
                ASRUtils::symbol_get_past_external(ASR::down_cast<TraitObjectType_t>(
                    typed_expr_type(procedure->m_args[0]))->m_contract) == &contract.base &&
                ASRUtils::EXPR2VAR(procedure->m_args[0])->m_intent == intentType::In &&
                ASRUtils::get_FunctionType(procedure)->m_deftype == deftypeType::Interface &&
                !ASRUtils::get_FunctionType(procedure)->m_is_restriction &&
                procedure->n_body == 0,
            "asr.verify.trait_contract.receiver",
            "A callable interface must take its read-only view as its first argument", loc);
        for (size_t i = 0; i < slot.n_origins; i++) {
            auto *method = ASRUtils::trait_method_function(slot.m_origins[i]);
            if (method && ASRUtils::trait_method_template(*method)) {
                verify_runtime_trait_procedure(slot.m_origins[i], loc,
                    "asr.verify.trait_contract.origin");
                require_with_loc_id(ASRUtils::runtime_trait_method_supported(*method) &&
                        ASRUtils::trait_erased_signature_matches(*method, *procedure, 1),
                    "asr.verify.trait_contract.erased_signature",
                    "A generic slot must preserve its quantified nominal argument domains", loc);
            }
        }
        return procedure;
    }

    void verify_runtime_trait_adapter_receiver(const Function_t &procedure) {
        class ReceiverUse : public ASR::BaseWalkVisitor<ReceiverUse> {
        public:
            symbol_t *receiver;
            bool escapes = false;
            explicit ReceiverUse(symbol_t *receiver) : receiver(receiver) {}
            void visit_Var(const Var_t &x) {
                escapes |= ASRUtils::symbol_get_past_external(x.m_v) == receiver;
            }
            void visit_TraitReceiver(const TraitReceiver_t &x) {
                if (x.m_view && ASR::is_a<Var_t>(*x.m_view) &&
                        ASRUtils::symbol_get_past_external(
                            ASR::down_cast<Var_t>(x.m_view)->m_v) == receiver) return;
                ASR::BaseWalkVisitor<ReceiverUse>::visit_TraitReceiver(x);
            }
        } uses(&ASRUtils::EXPR2VAR(procedure.m_args[0])->base);
        for (const auto &entry : procedure.m_symtab->get_scope()) uses.visit_symbol(*entry.second);
        for (size_t i = 0; i < procedure.n_body; i++) uses.visit_stmt(*procedure.m_body[i]);
        require_with_loc_id(!uses.escapes,
            "asr.verify.trait_witness.receiver_prefix",
            "A transferable adapter may use only the concrete prefix of its erased receiver",
            procedure.base.base.loc);
    }

    TraitImplementation_t *verify_runtime_trait_evidence(const TraitWitness_t &witness,
            const Location &loc) {
        auto *contract_symbol = ASRUtils::symbol_get_past_external(witness.m_contract);
        auto *impl_symbol = ASRUtils::symbol_get_past_external(witness.m_implementation);
        require_with_loc_id(contract_symbol && ASR::is_a<TraitRuntimeContract_t>(*contract_symbol) &&
                ((impl_symbol && ASR::is_a<TraitImplementation_t>(*impl_symbol) &&
                  !witness.n_components) || (!impl_symbol && witness.n_components &&
                  witness.m_components)),
            "asr.verify.trait_witness.evidence",
            "A runtime witness requires a contract and nominal implementation", loc);
        auto *contract = ASR::down_cast<TraitRuntimeContract_t>(contract_symbol);
        TraitImplementation_t *implementation = nullptr;
        require_with_loc_id(!contract->n_slots || contract->m_slots,
            "asr.verify.trait_contract.complete",
            "A runtime contract must declare its slots", loc);
        require_with_loc_id(witness.n_procedures == contract->n_slots &&
                (!witness.n_procedures || (witness.m_procedures && witness.m_dependencies)) &&
                witness.n_dependencies == witness.n_procedures,
            "asr.verify.trait_witness.complete",
            "A runtime witness must implement and retain every slot", loc);
        auto *lifecycle = ASRUtils::symbol_get_past_external(
            witness.m_lifecycle.m_type_declaration);
        require_with_loc_id(lifecycle && ASR::is_a<Struct_t>(*lifecycle),
            "asr.verify.trait_witness.lifecycle",
            "A witness must retain its exact concrete nominal lifecycle", loc);
        if (impl_symbol) {
            implementation = ASR::down_cast<TraitImplementation_t>(impl_symbol);
            require_with_loc_id(!implementation->n_bindings || implementation->m_bindings,
                "asr.verify.trait_witness.evidence",
                "A runtime implementation must declare its bindings", loc);
            require_with_loc_id(!contract->m_anonymous &&
                    lifecycle == ASRUtils::symbol_get_past_external(implementation->m_type_declaration),
                "asr.verify.trait_witness.lifecycle",
                "A nominal witness must retain its original implementation and lifecycle", loc);
        } else {
            require_with_loc_id(contract->m_anonymous && !witness.n_projections,
                "asr.verify.trait_witness.combination",
                "Only an anonymous contract composes independently selected witnesses", loc);
            auto requirements = ASRUtils::trait_contract_requirements(*contract);
            require_with_loc_id(witness.n_components == requirements.size(),
                "asr.verify.trait_witness.components",
                "A combination must retain evidence for every normalized nominal requirement", loc);
            for (size_t i = 0; i < witness.n_components; i++) {
                auto *symbol = ASRUtils::symbol_get_past_external(witness.m_components[i]);
                require_with_loc_id(symbol && ASR::is_a<TraitWitness_t>(*symbol),
                    "asr.verify.trait_witness.component_kind",
                    "Combination evidence must reference nominal witnesses", loc);
                auto *component = ASR::down_cast<TraitWitness_t>(symbol);
                require_with_loc_id(component->m_symtab && component->m_symtab->parent &&
                        component->m_implementation && !component->n_components,
                    "asr.verify.trait_witness.component_kind",
                    "Combination components must be original nominal evidence, not recursive compositions", loc);
                auto *original = verify_runtime_trait_evidence(*component, loc);
                auto *provided = ASR::down_cast<TraitRuntimeContract_t>(
                    ASRUtils::symbol_get_past_external(component->m_contract));
                require_with_loc_id(!provided->m_anonymous &&
                        ASRUtils::symbol_get_past_external(provided->m_trait) == requirements[i] &&
                        ASRUtils::symbol_get_past_external(component->m_lifecycle.m_type_declaration) == lifecycle,
                    "asr.verify.trait_witness.component_origin",
                    "A component must prove its original nominal requirement for the same concrete payload", loc);
                if (!implementation) implementation = original;
            }
        }
        return implementation;
    }

    void visit_TraitObjectType(const TraitObjectType_t &x) {
        require_id(x.m_contract && symtab_in_scope(current_symtab, x.m_contract),
            "asr.verify.trait_view.contract_in_scope",
            "A trait view must reference a visible runtime contract");
        if (!check_external) return;
        auto *symbol = ASRUtils::symbol_get_past_external(x.m_contract);
        require_id(symbol && ASR::is_a<TraitRuntimeContract_t>(*symbol),
            "asr.verify.trait_view.contract",
            "A trait view must reference a runtime contract");
        auto *trait = ASRUtils::symbol_get_past_external(
            ASR::down_cast<TraitRuntimeContract_t>(symbol)->m_trait);
        require_id(trait && ASR::is_a<Trait_t>(*trait) &&
                ASR::down_cast<Trait_t>(trait)->m_kind == trait_kindType::UniversalTrait,
            "asr.verify.trait_view.universal",
            "Only a universal trait may be a runtime view");
    }

    void visit_TraitOwnerList(const TraitOwnerList_t &x) {
        require_id(x.m_contract && symtab_in_scope(current_symtab, x.m_contract),
            "asr.verify.trait_results.contract_in_scope",
            "Retained trait results must reference a visible runtime contract");
        if (check_external) {
            auto *contract = ASRUtils::symbol_get_past_external(x.m_contract);
            require_id(contract && ASR::is_a<TraitRuntimeContract_t>(*contract),
                "asr.verify.trait_results.contract",
                "Retained trait results require a runtime contract");
        }
    }

    void visit_TraitRetain(const TraitRetain_t &x) {
        require_id(x.m_storage && x.m_owner &&
                ASR::is_a<Var_t>(*x.m_storage) && ASR::is_a<Var_t>(*x.m_owner) &&
                ASR::down_cast<Var_t>(x.m_storage)->m_v &&
                ASR::down_cast<Var_t>(x.m_owner)->m_v &&
                ASR::is_a<Variable_t>(*ASR::down_cast<Var_t>(x.m_storage)->m_v) &&
                ASR::is_a<Variable_t>(*ASR::down_cast<Var_t>(x.m_owner)->m_v),
            "asr.verify.trait_results.variables",
            "Trait retention requires a local store and an owning temporary");
        auto *storage = ASRUtils::EXPR2VAR(x.m_storage);
        auto *owner = ASRUtils::EXPR2VAR(x.m_owner);
        require_id(ASR::is_a<TraitOwnerList_t>(*storage->m_type) &&
                ASRUtils::is_trait_owner(owner->m_type) &&
                owner->m_intent == intentType::Local &&
                owner->m_storage == storage_typeType::Default &&
                !owner->m_target_attr &&
                owner->m_parent_symtab == storage->m_parent_symtab,
            "asr.verify.trait_results.ownership",
            "Trait retention moves a non-target local owner into its own scope's store");
        if (check_external) {
            auto *contract = ASRUtils::symbol_get_past_external(
                ASR::down_cast<TraitOwnerList_t>(storage->m_type)->m_contract);
            require_id(ASRUtils::trait_contracts_equal(
                    contract, &ASRUtils::trait_runtime_contract(owner->m_type)->base),
                "asr.verify.trait_results.contract_matches",
                "Retained owners must have the store's declared contract");
        }
        BaseWalkVisitor<VerifyVisitor>::visit_TraitRetain(x);
    }

    void visit_TraitRuntimeContract(const TraitRuntimeContract_t &x) {
        require_id(x.m_name && x.m_symtab && x.m_symtab->parent == current_symtab &&
                x.m_symtab->asr_owner == (ASR::asr_t*)&x &&
                current_symtab->get_symbol(x.m_name) == &x.base &&
                !id_symtab_map.count(x.m_symtab->counter),
            "asr.verify.trait_contract.scope",
            "A runtime contract must own a unique, correctly parented symbol table");
        require_id(x.m_trait && symtab_in_scope(current_symtab, x.m_trait),
            "asr.verify.trait_contract.trait_in_scope",
            "A runtime contract must reference its visible nominal trait");
        auto *parent = current_symtab;
        current_symtab = x.m_symtab;
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        if (check_external) {
            auto *trait_symbol = ASRUtils::symbol_get_past_external(x.m_trait);
            require_id(trait_symbol && ASR::is_a<Trait_t>(*trait_symbol) &&
                    ASR::down_cast<Trait_t>(trait_symbol)->m_kind == trait_kindType::UniversalTrait &&
                    ASRUtils::symbol_parent_symtab(trait_symbol) == parent,
                "asr.verify.trait_contract.nominal_owner",
                "A runtime contract belongs to its universal trait's defining scope");
            for (const auto &entry : parent->get_scope()) {
                if (entry.second == &x.base ||
                        !ASR::is_a<TraitRuntimeContract_t>(*entry.second)) continue;
                require_id(ASRUtils::symbol_get_past_external(
                        ASR::down_cast<TraitRuntimeContract_t>(entry.second)->m_trait) != trait_symbol,
                    "asr.verify.trait_contract.unique",
                    "A nominal trait has exactly one canonical runtime contract");
            }
            auto hierarchy = verify_trait_hierarchy(
                *ASR::down_cast<Trait_t>(trait_symbol), x.base.base.loc);
            if (x.m_anonymous) {
                auto *trait = ASR::down_cast<Trait_t>(trait_symbol);
                auto requirements = ASRUtils::trait_contract_requirements(x);
                require_id(trait->m_access == accessType::Private &&
                        trait->m_symtab->get_scope().empty() && requirements.size() >= 2 &&
                        ASRUtils::normalized_trait_requirements(requirements) == requirements,
                    "asr.verify.trait_contract.combination",
                    "An anonymous contract is a canonical antichain of original nominal traits");
                for (auto *required : requirements) {
                    auto *contract = ASRUtils::trait_runtime_contract(required);
                    require_id(contract && !contract->m_anonymous,
                        "asr.verify.trait_contract.nominal_requirements",
                        "Anonymous combinations retain original nominal contracts, not synthetic names");
                }
            }
            std::map<std::string, size_t> indices;
            std::vector<std::vector<symbol_t*>> expected;
            for (auto *member : hierarchy.members) {
                auto inserted = indices.emplace(ASRUtils::symbol_name(member), expected.size());
                if (inserted.second) expected.emplace_back();
                expected[inserted.first->second].push_back(member);
            }
            require_id(x.n_slots == expected.size() && (!x.n_slots || x.m_slots),
                "asr.verify.trait_contract.complete",
                "A runtime contract must have one slot for each canonical callable");
            for (size_t i = 0; i < x.n_slots; i++) {
                auto &slot = x.m_slots[i];
                require_id(slot.n_origins == expected[i].size() && slot.m_origins,
                    "asr.verify.trait_contract.origins",
                    "A callable slot must retain every nominal origin exactly once");
                auto *procedure = verify_runtime_trait_slot(x, i, x.base.base.loc);
                for (size_t j = 0; j < slot.n_origins; j++) {
                    auto *origin = ASRUtils::symbol_get_past_external(slot.m_origins[j]);
                    require_id(origin == expected[i][j] &&
                            symtab_in_scope(current_symtab, slot.m_origins[j]),
                        "asr.verify.trait_contract.origin_order",
                        "Runtime slot origins must follow the canonical trait hierarchy");
                    auto *method = ASRUtils::trait_method_function(origin);
                    if (!ASRUtils::trait_method_template(*method)) {
                        auto mismatch = ASRUtils::trait_method_mismatch(*method, *procedure, 0, 1);
                        require_id(mismatch.difference == ASRUtils::TraitMethodDifference::None,
                            "asr.verify.trait_contract.signature",
                            "A runtime slot must preserve its message signature: " + mismatch.message);
                    }
                }
            }
        }
        for (auto &entry : current_symtab->get_scope()) visit_symbol(*entry.second);
        current_symtab = parent;
    }

    void visit_TraitErasure(const TraitErasure_t &x) {
        require_id(x.m_name && x.m_symtab && x.m_symtab->parent == current_symtab &&
                x.m_symtab->asr_owner == (ASR::asr_t*)&x &&
                current_symtab->get_symbol(x.m_name) == &x.base &&
                !id_symtab_map.count(x.m_symtab->counter),
            "asr.verify.trait_erasure.scope",
            "A generic erasure must own a unique provider scope");
        require_id(x.m_generic && symtab_in_scope(current_symtab, x.m_generic) &&
                x.m_procedure && x.m_parameters,
            "asr.verify.trait_erasure.fields",
            "An erasure must retain its checked template, entry and binder substitutions");
        auto *parent = current_symtab;
        current_symtab = x.m_symtab;
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        if (check_external) {
            auto *definition = ASRUtils::symbol_get_past_external(x.m_generic);
            require_id(definition && ASR::is_a<Template_t>(*definition),
                "asr.verify.trait_erasure.generic",
                "An erased entry must originate in a checked template");
            auto *generic = ASR::down_cast<Template_t>(definition);
            auto *original = verify_runtime_trait_procedure(definition, x.base.base.loc,
                "asr.verify.trait_erasure.original");
            auto *entry = verify_runtime_trait_procedure(x.m_procedure, x.base.base.loc,
                "asr.verify.trait_erasure.entry");
            require_id(ASRUtils::symbol_parent_symtab(&entry->base) == current_symtab &&
                    ASRUtils::get_FunctionType(entry)->m_deftype == deftypeType::Implementation &&
                    ASRUtils::trait_erased_signature_matches(*original, *entry),
                "asr.verify.trait_erasure.signature",
                "The reusable entry must preserve its template's arguments and erase only checked scalar binders");
            require_id(x.n_parameters == generic->n_args,
                "asr.verify.trait_erasure.parameters",
                "Erasure must retain exactly one substitution per scoped binder");
            for (size_t i = 0; i < x.n_parameters; i++) {
                auto &parameter = x.m_parameters[i];
                require_id(parameter.m_parameter && parameter.m_contract &&
                        symtab_in_scope(current_symtab, parameter.m_parameter) &&
                        symtab_in_scope(current_symtab, parameter.m_contract) &&
                        (!parameter.n_operations || parameter.m_operations),
                    "asr.verify.trait_erasure.parameter_fields",
                    "Erased binders require visible nominal type and operation evidence");
                auto *binder = generic->m_symtab->get_symbol(generic->m_args[i]);
                auto *contract = ASRUtils::trait_parameter_contract(binder);
                require_id(binder && contract &&
                        ASRUtils::symbol_get_past_external(parameter.m_parameter) == binder &&
                        ASRUtils::symbol_get_past_external(parameter.m_contract) == &contract->base,
                    "asr.verify.trait_erasure.parameter_identity",
                    "An erased binder must preserve its original scope and exact nominal domain");
                std::map<symbol_t*, symbol_t*> required;
                for (const auto &item : generic->m_symtab->get_scope()) {
                    if (!ASR::is_a<TraitConstraint_t>(*item.second)) continue;
                    auto *constraint = ASR::down_cast<TraitConstraint_t>(item.second);
                    if (ASRUtils::symbol_get_past_external(constraint->m_parameter) != binder) continue;
                    for (size_t j = 0; j < constraint->n_requirements; j++) {
                        auto &operation = constraint->m_requirements[j];
                        required.emplace(ASRUtils::symbol_get_past_external(operation.m_procedure),
                            ASRUtils::symbol_get_past_external(operation.m_member));
                    }
                }
                require_id(parameter.n_operations == required.size(),
                    "asr.verify.trait_erasure.operations",
                    "Erasure must supply every checked operation exactly once");
                std::set<symbol_t*> seen;
                for (size_t j = 0; j < parameter.n_operations; j++) {
                    auto &operation = parameter.m_operations[j];
                    auto *requirement = ASRUtils::symbol_get_past_external(operation.m_requirement);
                    require_id(requirement && required.count(requirement) &&
                            seen.insert(requirement).second &&
                            symtab_in_scope(current_symtab, operation.m_requirement),
                        "asr.verify.trait_erasure.operation_identity",
                        "Erased operations must refer to the binder's checked requirements");
                    auto *wrapper = verify_runtime_trait_procedure(operation.m_procedure,
                        operation.loc, "asr.verify.trait_erasure.operation");
                    size_t slot = contract->n_slots;
                    for (size_t k = 0; k < contract->n_slots; k++) {
                        for (size_t m = 0; m < contract->m_slots[k].n_origins; m++) {
                            if (ASRUtils::symbol_get_past_external(contract->m_slots[k].m_origins[m]) ==
                                    required.at(requirement)) slot = k;
                        }
                    }
                    require_id(slot < contract->n_slots &&
                            ASRUtils::symbol_parent_symtab(&wrapper->base) == current_symtab,
                        "asr.verify.trait_erasure.operation_slot",
                        "An erased operation wrapper must select its proved nominal slot");
                    auto comparable = *wrapper;
                    auto signature = *ASRUtils::get_FunctionType(wrapper);
                    if (signature.m_abi == abiType::ExternalUndefined) signature.m_abi = abiType::Source;
                    comparable.m_function_signature = &signature.base;
                    auto mismatch = ASRUtils::trait_method_mismatch(
                        *verify_runtime_trait_slot(*contract, slot, operation.loc), comparable);
                    require_id(mismatch.difference == ASRUtils::TraitMethodDifference::None,
                        "asr.verify.trait_erasure.operation_signature",
                        "An erased operation must preserve its runtime slot signature");
                    if (ASRUtils::get_FunctionType(wrapper)->m_abi == abiType::ExternalUndefined &&
                            wrapper->n_body == 0) continue;
                    require_id(wrapper->n_body == 1 && wrapper->m_body,
                        "asr.verify.trait_erasure.operation_body",
                        "An erased operation wrapper must contain one checked indirect call");
                    TraitFunctionCall_t *function_call = nullptr;
                    TraitSubroutineCall_t *subroutine_call = nullptr;
                    if (wrapper->m_return_var && ASR::is_a<Assignment_t>(*wrapper->m_body[0])) {
                        auto *assignment = ASR::down_cast<Assignment_t>(wrapper->m_body[0]);
                        require_id(same_variable(assignment->m_target, wrapper->m_return_var) &&
                                assignment->m_value && ASR::is_a<TraitFunctionCall_t>(*assignment->m_value),
                            "asr.verify.trait_erasure.operation_body",
                            "An erased operation must return its selected indirect call");
                        function_call = ASR::down_cast<TraitFunctionCall_t>(assignment->m_value);
                    } else if (!wrapper->m_return_var &&
                            ASR::is_a<TraitSubroutineCall_t>(*wrapper->m_body[0])) {
                        subroutine_call = ASR::down_cast<TraitSubroutineCall_t>(wrapper->m_body[0]);
                    }
                    require_id(function_call || subroutine_call,
                        "asr.verify.trait_erasure.operation_body",
                        "Erased operation wrappers must use explicit dynamic dispatch");
                    auto *args = function_call ? function_call->m_args : subroutine_call->m_args;
                    size_t nargs = function_call ? function_call->n_args : subroutine_call->n_args;
                    int64_t index = function_call ? function_call->m_slot : subroutine_call->m_slot;
                    require_id(index == (int64_t)slot && nargs == wrapper->n_args && args,
                        "asr.verify.trait_erasure.operation_body",
                        "An erased operation must forward all of its own arguments in order");
                    for (size_t k = 0; k < nargs; k++) {
                        require_id(same_variable(args[k].m_value, wrapper->m_args[k]),
                            "asr.verify.trait_erasure.operation_body",
                            "An erased operation must borrow the supplied actual view, not provider storage");
                    }
                }
            }
        }
        for (const auto &entry : current_symtab->get_scope()) visit_symbol(*entry.second);
        current_symtab = parent;
    }

    void visit_TraitWitness(const TraitWitness_t &x) {
        require_id(x.m_abi == abiType::Source || x.m_abi == abiType::ExternalUndefined,
            "asr.verify.trait_witness.linkage",
            "A runtime witness is either a provider definition or an imported declaration");
        require_id(x.m_name && x.m_symtab && x.m_symtab->parent == current_symtab &&
                x.m_symtab->asr_owner == (ASR::asr_t*)&x &&
                current_symtab->get_symbol(x.m_name) == &x.base &&
                !id_symtab_map.count(x.m_symtab->counter),
            "asr.verify.trait_witness.scope",
            "A runtime witness must own a unique scope in its defining conformance module");
        require_id(x.m_contract && (x.m_implementation || x.n_components) &&
                symtab_in_scope(current_symtab, x.m_contract) &&
                (!x.m_implementation || symtab_in_scope(current_symtab, x.m_implementation)) &&
                x.m_lifecycle.m_type_declaration &&
                symtab_in_scope(current_symtab, x.m_lifecycle.m_type_declaration),
            "asr.verify.trait_witness.evidence_in_scope",
            "A runtime witness must reference visible contract and implementation evidence");
        require_id(!x.n_projections || x.m_projections,
            "asr.verify.trait_witness.projections",
            "A witness must retain its declared parent projection references");
        require_id(!x.n_components || x.m_components,
            "asr.verify.trait_witness.components",
            "Combination witnesses must retain their original component references");
        for (size_t i = 0; i < x.n_projections; i++) {
            auto *reference = x.m_projections[i];
            require_id(reference && (ASR::is_a<TraitWitness_t>(*reference) ||
                    ASR::is_a<ExternalSymbol_t>(*reference)),
                "asr.verify.trait_witness.projection_kind",
                "A parent projection must name a runtime witness");
            auto *scope = ASR::is_a<TraitWitness_t>(*reference)
                ? ASR::down_cast<TraitWitness_t>(reference)->m_symtab
                : ASR::down_cast<ExternalSymbol_t>(reference)->m_parent_symtab;
            require_id(scope && (ASR::is_a<ExternalSymbol_t>(*reference) || scope->parent) &&
                    symtab_in_scope(current_symtab, reference),
                "asr.verify.trait_witness.projection_in_scope",
                "A parent witness must be visible from its provider");
        }
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        auto visit_adapters = [&]() {
            auto *parent = current_symtab;
            current_symtab = x.m_symtab;
            for (auto &entry : current_symtab->get_scope()) visit_symbol(*entry.second);
            current_symtab = parent;
        };
        if (!check_external) {
            visit_adapters();
            return;
        }
        auto *implementation = verify_runtime_trait_evidence(x, x.base.base.loc);
        auto *contract = ASR::down_cast<TraitRuntimeContract_t>(
            ASRUtils::symbol_get_past_external(x.m_contract));
        for (size_t i = 0; i < x.n_components; i++) {
            require_id(symtab_in_scope(current_symtab, x.m_components[i]),
                "asr.verify.trait_witness.component_in_scope",
                "Original combination witnesses must be visible at concrete construction");
        }
        auto *origin = ASRUtils::symbol_get_past_external(implementation->m_trait);
        auto *target = ASRUtils::symbol_get_past_external(contract->m_trait);
        require_id(origin && target && ASR::is_a<Trait_t>(*origin) &&
                ASR::is_a<Trait_t>(*target),
            "asr.verify.trait_witness.conformance_origin",
            "A runtime witness must retain resolved nominal trait declarations");
        auto hierarchy = verify_trait_hierarchy(*ASR::down_cast<Trait_t>(origin),
            x.base.base.loc);
        bool provided = false;
        for (auto *trait : hierarchy.traits) provided |= &trait->base == target;
        require_id(contract->m_anonymous ||
                (implementation->m_parent_symtab == current_symtab && provided),
            "asr.verify.trait_witness.conformance_origin",
            "A runtime witness must preserve its original conformance and a guaranteed contract");
        auto *trait = ASR::down_cast<Trait_t>(target);
        require_id(x.n_projections == (contract->m_anonymous ? 0 : trait->n_parents),
            "asr.verify.trait_witness.projections",
            "A witness must retain every direct parent view in declaration order");
        for (size_t i = 0; i < x.n_projections; i++) {
            auto *reference = ASRUtils::symbol_get_past_external(x.m_projections[i]);
            require_id(reference && ASR::is_a<TraitWitness_t>(*reference),
                "asr.verify.trait_witness.projection_kind",
                "A parent projection must name a runtime witness");
            auto *parent = ASR::down_cast<TraitWitness_t>(reference);
            auto *parent_contract = ASRUtils::symbol_get_past_external(parent->m_contract);
            require_id(parent_contract && ASR::is_a<TraitRuntimeContract_t>(*parent_contract),
                "asr.verify.trait_witness.projection_contract",
                "A parent witness requires a runtime contract");
            require_id(ASRUtils::symbol_get_past_external(
                        ASR::down_cast<TraitRuntimeContract_t>(parent_contract)->m_trait) ==
                    ASRUtils::symbol_get_past_external(trait->m_parents[i]) &&
                    ASRUtils::symbol_get_past_external(parent->m_implementation) ==
                        &implementation->base &&
                    parent->m_symtab && parent->m_symtab->parent == current_symtab &&
                    parent->m_abi == x.m_abi &&
                    ASRUtils::symbol_get_past_external(parent->m_lifecycle.m_type_declaration) ==
                        ASRUtils::symbol_get_past_external(x.m_lifecycle.m_type_declaration),
                "asr.verify.trait_witness.projection_origin",
                "A projection must retain the parent's contract, selected implementation and lifecycle");
        }
        std::set<symbol_t*> procedures;
        for (size_t i = 0; i < x.n_procedures; i++) {
            auto *procedure = verify_runtime_trait_procedure(x.m_procedures[i],
                x.base.base.loc, "asr.verify.trait_witness.unique_procedure");
            auto *symbol = &procedure->base;
            require_id(ASRUtils::symbol_parent_symtab(symbol) == x.m_symtab &&
                    x.m_symtab->get_symbol(ASRUtils::symbol_name(symbol)) == symbol &&
                    procedures.insert(symbol).second,
                "asr.verify.trait_witness.unique_procedure",
                "Every runtime slot requires its own provider-owned typed adapter");
            require_id(x.m_dependencies[i] &&
                    std::string(x.m_dependencies[i]) == procedure->m_name &&
                    ASRUtils::get_FunctionType(procedure)->m_deftype == deftypeType::Implementation,
                "asr.verify.trait_witness.dependency",
                "A witness slot must retain its executable adapter dependency");
            auto signature = *ASRUtils::get_FunctionType(procedure);
            // Imported definitions keep their ordinary ABI but have unavailable bodies.
            if (signature.m_abi == abiType::ExternalUndefined) signature.m_abi = abiType::Source;
            auto comparable = *procedure;
            comparable.m_function_signature = &signature.base;
            auto *slot_procedure = verify_runtime_trait_slot(*contract, i, x.base.base.loc);
            auto mismatch = ASRUtils::trait_method_mismatch(*slot_procedure, comparable);
            require_id(mismatch.difference == ASRUtils::TraitMethodDifference::None,
                "asr.verify.trait_witness.signature",
                "A runtime witness adapter must match its slot: " + mismatch.message);
            verify_runtime_trait_adapter_receiver(*procedure);
            for (size_t j = 0; j < contract->m_slots[i].n_origins; j++) {
                TraitImplementation_t *selected = nullptr;
                auto *binding = ASRUtils::runtime_trait_binding(
                    x, contract->m_slots[i].m_origins[j], selected);
                require_id(binding != nullptr,
                    "asr.verify.trait_witness.origin_covered",
                    "A runtime witness must prove every nominal origin of a slot");
                verify_runtime_binding(*selected, *binding);
                TraitImplementation_t *first_implementation = nullptr;
                auto *first = ASRUtils::runtime_trait_binding(
                    x, contract->m_slots[i].m_origins[0], first_implementation);
                require_id(first && ASRUtils::trait_bindings_equal(*first, *binding),
                    "asr.verify.trait_witness.coalesced_binding",
                    "All independent origins of a combined callable must agree on procedure and receiver");
                for (size_t k = 0; k < x.n_components; k++) {
                    auto *component = ASR::down_cast<TraitWitness_t>(
                        ASRUtils::symbol_get_past_external(x.m_components[k]));
                    auto *other = ASRUtils::runtime_trait_binding(
                        *component, contract->m_slots[i].m_origins[j], selected);
                    require_id(!other || ASRUtils::trait_bindings_equal(*binding, *other),
                        "asr.verify.trait_witness.coalesced_binding",
                        "Overlapping selected combination evidence must agree");
                }
            }
        }
        visit_adapters();
    }

    void verify_runtime_binding(const TraitImplementation_t &implementation,
            const trait_binding_t &binding) {
        verify_trait_binding(implementation.m_type_declaration, binding, true,
            implementation.m_implementing_type);
    }

    void verify_trait_binding(symbol_t *type_declaration,
            const trait_binding_t &binding, bool runtime, ttype_t *implementing_type = nullptr) {
        const Location &loc = binding.loc;
        auto *required = verify_runtime_trait_procedure(binding.m_member, loc,
            "asr.verify.trait_binding.member_is_function");
        auto *procedure = verify_runtime_trait_procedure(binding.m_procedure, loc,
            "asr.verify.trait_binding.procedure_is_function");
        auto *required_signature = ASRUtils::get_FunctionType(required);
        auto *actual_signature = ASRUtils::get_FunctionType(procedure);
        require_with_loc_id(!runtime || (
                (required_signature->m_abi == abiType::Source ||
                    required_signature->m_abi == abiType::ExternalUndefined) &&
                (actual_signature->m_abi == abiType::Source ||
                    actual_signature->m_abi == abiType::ExternalUndefined)),
            "asr.verify.trait_witness.binding_abi",
            "A runtime witness binding requires the ordinary source calling convention", loc);
        require_with_loc_id(
                (!required_signature->m_pure || actual_signature->m_pure) &&
                (!required_signature->m_elemental || actual_signature->m_elemental),
            "asr.verify.trait_witness.procedure_attributes",
            "A witness binding must preserve required pure and elemental attributes", loc);
        size_t receiver = procedure->n_args;
        if (!binding.m_is_nopass) {
            for (size_t i = 0; i < procedure->n_args; i++) {
                auto *arg = ASRUtils::EXPR2VAR(procedure->m_args[i]);
                if (binding.m_self_argument &&
                        std::string(arg->m_name) == binding.m_self_argument) receiver = i;
            }
            require_with_loc_id(receiver < procedure->n_args,
                "asr.verify.trait_witness.receiver",
                "A passed-object witness requires its declared receiver", loc);
            auto *self = ASRUtils::EXPR2VAR(procedure->m_args[receiver]);
            require_with_loc_id(self->m_intent == intentType::In &&
                    ASRUtils::trait_receiver_type_matches(*self, type_declaration, implementing_type),
                "asr.verify.trait_witness.receiver_type",
                "A witness receiver must borrow its nominal implementing type "
                "or a polymorphic ancestor read-only", loc);
        }
        require_with_loc_id(procedure->n_args ==
                required->n_args + (binding.m_is_nopass ? 0 : 1),
            "asr.verify.trait_witness.binding_signature",
            "A witness binding must have the message's ordinary arguments", loc);
        std::map<symbol_t*, symbol_t*> parameters;
        auto generic_mismatch = ASRUtils::trait_generic_correspondence(
            *required, *procedure, parameters);
        require_with_loc_id(generic_mismatch.empty(),
            "asr.verify.trait_binding.generic_contract",
            "A generic binding must preserve its universal contract: " + generic_mismatch, loc);
        for (size_t i = 0, j = 0; i < procedure->n_args; i++) {
            if (i == receiver) continue;
            parameters.emplace(
                &ASRUtils::EXPR2VAR(required->m_args[j++])->base,
                &ASRUtils::EXPR2VAR(procedure->m_args[i])->base);
        }
        for (size_t i = 0, j = 0; i < procedure->n_args; i++) {
            if (i == receiver) continue;
            auto *actual = ASRUtils::EXPR2VAR(procedure->m_args[i]);
            auto *formal = ASRUtils::EXPR2VAR(required->m_args[j]);
            require_with_loc_id(ASRUtils::trait_types_equal(
                        required->m_args[j], procedure->m_args[i], parameters) &&
                    actual->m_intent == formal->m_intent &&
                    actual->m_presence == formal->m_presence &&
                    actual->m_value_attr == formal->m_value_attr,
                "asr.verify.trait_witness.binding_signature",
                "A witness binding must preserve ordinary argument types and association attributes", loc);
            j++;
        }
        require_with_loc_id(bool(required->m_return_var) == bool(procedure->m_return_var) &&
                (!required->m_return_var || ASRUtils::trait_types_equal(
                    required->m_return_var, procedure->m_return_var, parameters)),
            "asr.verify.trait_witness.binding_signature",
            "A witness binding must preserve its message's result", loc);
    }

    TraitWitness_t *verify_trait_witness_reference(symbol_t *reference, const Location &loc) {
        require_with_loc_id(reference && symtab_in_scope(current_symtab, reference),
            "asr.verify.trait_borrow.witness_in_scope",
            "A borrowed view must reference visible selected evidence", loc);
        auto *symbol = ASRUtils::symbol_get_past_external(reference);
        require_with_loc_id(symbol && ASR::is_a<TraitWitness_t>(*symbol),
            "asr.verify.trait_borrow.witness",
            "A borrowed view requires a selected runtime witness", loc);
        return ASR::down_cast<TraitWitness_t>(symbol);
    }

    void visit_TraitPack(const TraitPack_t &x) {
        require_id(x.m_payload && x.m_witness && x.m_type,
            "asr.verify.trait_pack.required_fields",
            "A borrowed pack requires payload, selected witness, and view type");
        visit_expr(*x.m_payload);
        visit_ttype(*x.m_type);
        if (!check_external) return;
        auto *witness = verify_trait_witness_reference(x.m_witness, x.base.base.loc);
        auto *implementation = verify_runtime_trait_evidence(*witness, x.base.base.loc);
        auto *type = ASRUtils::expr_type(x.m_payload);
        auto *scalar = ASRUtils::extract_type(type);
        require_id(ASR::is_a<TraitObjectType_t>(*x.m_type) &&
                ASRUtils::trait_contracts_equal(
                    &ASRUtils::trait_runtime_contract(x.m_type)->base, witness->m_contract) &&
                ASR::is_a<StructType_t>(*scalar) && !ASRUtils::is_array(type) &&
                !ASRUtils::is_class_type(scalar),
            "asr.verify.trait_pack.exact_scalar",
            "A borrowed pack requires exact, nonpolymorphic concrete storage and its contract");
        require_id(ASR::is_a<Var_t>(*x.m_payload) ||
                ASR::is_a<StructInstanceMember_t>(*x.m_payload) ||
                ASR::is_a<ArrayItem_t>(*x.m_payload),
            "asr.verify.trait_pack.borrowed_designator",
            "A borrowed pack must preserve an existing payload designator");
        require_id(ASRUtils::symbol_get_past_external(
                    ASRUtils::get_struct_sym_from_struct_expr(x.m_payload)) ==
                ASRUtils::symbol_get_past_external(implementation->m_type_declaration),
            "asr.verify.trait_pack.nominal_type",
            "A borrowed payload's nominal type must match its selected witness");
    }

    void visit_TraitDeferredPack(const TraitDeferredPack_t &x) {
        require_id(x.m_payload && x.m_constraint && x.m_type &&
                typed_expr_type(x.m_payload) &&
                ASR::is_a<TypeParameter_t>(*typed_expr_type(x.m_payload)) &&
                ASR::is_a<TraitObjectType_t>(*x.m_type) &&
                symtab_in_scope(current_symtab, x.m_constraint),
            "asr.verify.trait_deferred_pack.fields",
            "A deferred pack requires a scalar type parameter, visible constraint and borrowed contract");
        visit_expr(*x.m_payload);
        visit_ttype(*x.m_type);
        if (!check_external) return;
        auto *symbol = ASRUtils::symbol_get_past_external(x.m_constraint);
        require_id(symbol && ASR::is_a<TraitConstraint_t>(*symbol),
            "asr.verify.trait_deferred_pack.constraint",
            "Deferred packing must retain a checked nominal constraint");
        auto *constraint = ASR::down_cast<TraitConstraint_t>(symbol);
        auto *parameter = ASRUtils::trait_type_parameter(x.m_payload);
        require_id(parameter &&
                parameter == ASRUtils::symbol_get_past_external(constraint->m_parameter),
            "asr.verify.trait_deferred_pack.parameter",
            "Deferred evidence must belong to the payload's scoped generic binder");
        bool in_generic = false;
        for (auto *scope = current_symtab; scope; scope = scope->parent) {
            if (scope == constraint->m_parent_symtab) in_generic = true;
        }
        require_id(in_generic,
            "asr.verify.trait_deferred_pack.scope",
            "A deferred pack may exist only within its checked generic definition");
        auto *trait = ASRUtils::symbol_get_past_external(constraint->m_trait);
        require_id(trait && ASR::is_a<Trait_t>(*trait) &&
                ASR::down_cast<Trait_t>(trait)->m_kind == trait_kindType::UniversalTrait,
            "asr.verify.trait_deferred_pack.nominal",
            "A deferred runtime argument requires an open nominal domain, not a finite type set");
        auto hierarchy = verify_trait_hierarchy(*ASR::down_cast<Trait_t>(trait), x.base.base.loc);
        auto *target = ASRUtils::symbol_get_past_external(
            ASRUtils::trait_runtime_contract(x.m_type)->m_trait);
        bool provided = false;
        for (auto *origin : hierarchy.traits) provided |= &origin->base == target;
        require_id(provided,
            "asr.verify.trait_deferred_pack.implication",
            "The binder's declared constraints must guarantee the requested runtime operations");
    }

    ttype_t *verify_trait_owner(expr_t *owner, const Location &loc,
            bool defining = true) {
        auto *type = typed_expr_type(owner);
        require_with_loc_id(owner && (ASR::is_a<Var_t>(*owner) ||
                ASR::is_a<StructInstanceMember_t>(*owner) ||
                (!defining && ASR::is_a<FunctionCall_t>(*owner))) &&
                ASRUtils::is_trait_owner(type),
            "asr.verify.trait_owner.storage",
            "An owning operation requires a scalar allocatable trait slot or a borrowed function result", loc);
        if (ASR::is_a<FunctionCall_t>(*owner)) {
            visit_expr(*owner);
            return ASRUtils::extract_type(type);
        }
        if (!check_external) {
            visit_expr(*owner);
            return ASRUtils::extract_type(type);
        }
        auto *variable = ASRUtils::trait_owner_variable(owner);
        require_with_loc_id(variable != nullptr,
            "asr.verify.trait_owner.storage",
            "An owning operation must designate a declared allocatable variable", loc);
        require_with_loc_id(!defining || ASRUtils::trait_owner_is_definable(owner),
            "asr.verify.trait_owner.definable",
            "An owning operation cannot define an intent(in) allocation slot", loc);
        visit_expr(*owner);
        auto *saved_scope = current_symtab;
        current_symtab = variable->m_parent_symtab;
        visit_ttype(*variable->m_type);
        current_symtab = saved_scope;
        return ASRUtils::extract_type(type);
    }

    void verify_trait_copy_source(expr_t *source, ttype_t *target,
            symbol_t *selected_witness, const Location &loc) {
        auto *type = typed_expr_type(source);
        if (selected_witness) {
            require_with_loc_id(type && ASR::is_a<StructType_t>(*ASRUtils::extract_type(type)) &&
                    !ASRUtils::is_array(type) && !ASRUtils::is_class_type(ASRUtils::extract_type(type)),
                "asr.verify.trait_owner.exact_scalar",
                "A concrete owning source must be an exact nonpolymorphic scalar value", loc);
            visit_expr(*source);
            if (!check_external) return;
            auto *witness = verify_trait_witness_reference(selected_witness, loc);
            auto *implementation = verify_runtime_trait_evidence(*witness, loc);
            require_with_loc_id(!ASR::down_cast<Struct_t>(
                    ASRUtils::symbol_get_past_external(
                        witness->m_lifecycle.m_type_declaration))->m_is_abstract,
                "asr.verify.trait_owner.concrete_type",
                "An owning copy cannot instantiate an abstract concrete type", loc);
            require_with_loc_id(ASRUtils::symbol_get_past_external(
                        ASRUtils::get_struct_sym_from_struct_expr(source)) ==
                    ASRUtils::symbol_get_past_external(implementation->m_type_declaration) &&
                    ASRUtils::trait_contracts_equal(witness->m_contract,
                        &ASRUtils::trait_runtime_contract(target)->base),
                "asr.verify.trait_owner.nominal_type",
                "An owning value must match its selected nominal witness and target contract", loc);
            return;
        }
        require_with_loc_id(type && ASR::is_a<TraitObjectType_t>(*type),
            "asr.verify.trait_owner.source",
            "An owning copy must explicitly borrow its source, not copy an ownership slot", loc);
        visit_expr(*source);
        visit_ttype(*type);
        if (!check_external) return;
        require_with_loc_id(ASRUtils::trait_contracts_equal(type, target),
            "asr.verify.trait_owner.contract",
            "An owning copy must retain the source's declared contract and selected witness", loc);
    }

    void visit_TraitBorrow(const TraitBorrow_t &x) {
        auto *source_type = typed_expr_type(x.m_owner);
        auto *owner_type = ASRUtils::is_trait_pointer(source_type)
            ? verify_trait_pointer(x.m_owner, x.base.base.loc, false)
            : verify_trait_owner(x.m_owner, x.base.base.loc, false);
        require_id(x.m_type && ASR::is_a<TraitObjectType_t>(*x.m_type),
            "asr.verify.trait_borrow.type", "Borrowing cannot transfer ownership");
        visit_ttype(*x.m_type);
        if (!check_external) return;
        require_id(ASRUtils::trait_contracts_equal(owner_type, x.m_type),
            "asr.verify.trait_borrow.contract",
            "Borrowing an owner must preserve its declared contract");
    }

    void visit_TraitInspect(const TraitInspect_t &x) {
        auto *view_type = typed_expr_type(x.m_view);
        require_id(view_type && ASR::is_a<TraitObjectType_t>(*view_type) &&
                x.m_type && ASR::is_a<StructType_t>(*x.m_type) &&
                ASRUtils::is_unlimited_polymorphic_type(x.m_type) &&
                !ASR::down_cast<StructType_t>(x.m_type)->m_is_cstruct,
            "asr.verify.trait_inspect.view",
            "Concrete inspection requires a borrowed scalar trait and an ordinary class(*) view");
        visit_expr(*x.m_view);
        visit_ttype(*x.m_type);
        require_id(x.m_type_declaration && symtab_in_scope(current_symtab, x.m_type_declaration),
            "asr.verify.trait_inspect.declaration",
            "The ordinary inspection view must carry an in-scope type declaration");
        if (!check_external) return;
        auto *declaration = ASRUtils::symbol_get_past_external(x.m_type_declaration);
        require_id(declaration && ASR::is_a<Struct_t>(*declaration) &&
                ASRUtils::is_unlimited_polymorphic_type(declaration),
            "asr.verify.trait_inspect.declaration",
            "Concrete inspection cannot claim an unchecked concrete type");
    }

    void visit_TraitProject(const TraitProject_t &x) {
        auto *source = typed_expr_type(x.m_view);
        require_id(source && x.m_type &&
                ((ASR::is_a<TraitObjectType_t>(*source) &&
                  ASR::is_a<TraitObjectType_t>(*x.m_type)) ||
                 (ASRUtils::is_trait_pointer(source) &&
                  ASRUtils::is_trait_pointer(x.m_type))),
            "asr.verify.trait_project.view_kind",
            "A projection preserves borrowed or nonowning pointer storage");
        visit_expr(*x.m_view);
        visit_ttype(*x.m_type);
        if (!check_external) return;
        auto *source_contract = ASRUtils::symbol_get_past_external(
            ASR::down_cast<TraitObjectType_t>(ASRUtils::extract_type(source))->m_contract);
        auto *target_contract = ASRUtils::symbol_get_past_external(
            ASR::down_cast<TraitObjectType_t>(ASRUtils::extract_type(x.m_type))->m_contract);
        require_id(source_contract && target_contract &&
                ASR::is_a<TraitRuntimeContract_t>(*source_contract) &&
                ASR::is_a<TraitRuntimeContract_t>(*target_contract),
            "asr.verify.trait_project.contract",
            "A projection requires resolved source and result contracts");
        auto *from = ASR::down_cast<TraitRuntimeContract_t>(source_contract);
        auto *to = ASR::down_cast<TraitRuntimeContract_t>(target_contract);
        auto *source_trait = ASRUtils::symbol_get_past_external(from->m_trait);
        auto *target_trait = ASRUtils::symbol_get_past_external(to->m_trait);
        require_id(source_trait && target_trait && ASR::is_a<Trait_t>(*source_trait) &&
                ASR::is_a<Trait_t>(*target_trait),
            "asr.verify.trait_project.contract", "Projection contracts require resolved traits");
        verify_trait_hierarchy(*ASR::down_cast<Trait_t>(source_trait), x.base.base.loc);
        verify_trait_hierarchy(*ASR::down_cast<Trait_t>(target_trait), x.base.base.loc);
        require_id(ASRUtils::trait_contract_implies(*from, *to),
            "asr.verify.trait_project.parent",
            "A projection may select only guaranteed original nominal requirements");
        std::vector<int64_t> expected;
        require_id(ASRUtils::trait_projection_slots(*from, *to, expected) &&
                x.n_slots == expected.size() && (!x.n_slots || x.m_slots),
            "asr.verify.trait_project.slots",
            "A projection must explicitly map every target callable from the source view");
        for (size_t i = 0; i < x.n_slots; i++) {
            require_id(x.m_slots[i].m_source == expected[i],
                "asr.verify.trait_project.slot_origin",
                "A projection slot must preserve every target origin and its selected implementation");
            auto *source_procedure = verify_runtime_trait_slot(*from, x.m_slots[i].m_source, x.base.base.loc);
            auto *target_procedure = verify_runtime_trait_slot(*to, i, x.base.base.loc);
            auto mismatch = ASRUtils::trait_method_mismatch(*source_procedure, *target_procedure, 1, 1);
            require_id(mismatch.difference == ASRUtils::TraitMethodDifference::None,
                "asr.verify.trait_project.signature", "Projection must preserve the ordinary callable signature");
        }
    }

    ttype_t *verify_trait_pointer(expr_t *pointer, const Location &loc,
            bool defining) {
        auto *type = typed_expr_type(pointer);
        require_with_loc_id(pointer && ASR::is_a<Var_t>(*pointer) &&
                ASRUtils::is_trait_pointer(type),
            "asr.verify.trait_pointer.storage",
            "A trait pointer operation requires a scalar pointer variable", loc);
        auto *variable = ASRUtils::get_variable_from_symbol(
            ASR::down_cast<Var_t>(pointer)->m_v);
        require_with_loc_id(variable &&
                (!defining || variable->m_intent != intentType::In),
            "asr.verify.trait_pointer.definable",
            "An intent(in) trait pointer cannot change association", loc);
        visit_expr(*pointer);
        return ASRUtils::extract_type(type);
    }

    void verify_trait_pointer_value(expr_t *value, ttype_t *target,
            const Location &loc) {
        auto *type = typed_expr_type(value);
        require_with_loc_id(type &&
                (ASR::is_a<TraitObjectType_t>(*type) ||
                 ASRUtils::is_trait_pointer(type)) &&
                ASRUtils::is_valid_pointer_assignment_target(value),
            "asr.verify.trait_pointer.target",
            "A persistent trait view requires an existing target or pointer", loc);
        visit_expr(*value);
        if (!check_external) return;
        require_with_loc_id(ASRUtils::trait_contracts_equal(type, target),
            "asr.verify.trait_pointer.contract",
            "Pointer association must preserve the declared trait contract", loc);
    }

    void visit_TraitAssociate(const TraitAssociate_t &x) {
        auto *type = verify_trait_pointer(x.m_target, x.base.base.loc, true);
        if (x.m_value) verify_trait_pointer_value(x.m_value, type, x.base.base.loc);
    }

    void visit_PointerAssociated(const PointerAssociated_t &x) {
        require_id(!x.m_ptr || !ASR::is_a<Var_t>(*x.m_ptr) ||
                !ASRUtils::association_variable(x.m_ptr),
            "asr.verify.association.pointer_attribute",
            "A construct association is not a pointer inquiry's POINTER argument");
        require_id(!check_external || !ASRUtils::association_variable(x.m_tgt) ||
                ASRUtils::is_valid_pointer_assignment_target(x.m_tgt),
            "asr.verify.association.target",
            "An association used as a pointer target must retain TARGET");
        if (ASRUtils::is_trait_pointer(typed_expr_type(x.m_ptr)) &&
                !ASR::is_a<PointerNullConstant_t>(*x.m_ptr)) {
            auto *type = verify_trait_pointer(x.m_ptr, x.base.base.loc, false);
            if (x.m_tgt) verify_trait_pointer_value(x.m_tgt, type, x.base.base.loc);
        }
        BaseWalkVisitor::visit_PointerAssociated(x);
    }

    void visit_TraitAllocate(const TraitAllocate_t &x) {
        auto *type = verify_trait_owner(x.m_target, x.base.base.loc);
        require_id((x.m_source || x.m_witness) &&
                (!x.m_copy_value || x.m_source) &&
                (bool(x.m_type_declaration) == !bool(x.m_source)),
            "asr.verify.trait_allocate.initialization",
            "Allocation requires a source or typed witness, and only SOURCE copies a value");
        if (x.m_source) {
            verify_trait_copy_source(x.m_source, type, x.m_witness, x.base.base.loc);
        } else if (check_external) {
            auto *witness = verify_trait_witness_reference(x.m_witness, x.base.base.loc);
            verify_runtime_trait_evidence(*witness, x.base.base.loc);
            require_id(symtab_in_scope(current_symtab, x.m_type_declaration) &&
                    ASRUtils::symbol_get_past_external(x.m_type_declaration) ==
                    ASRUtils::symbol_get_past_external(witness->m_lifecycle.m_type_declaration),
                "asr.verify.trait_allocate.nominal_type",
                "Typed allocation must preserve its explicitly selected nominal type");
            require_id(!ASR::down_cast<Struct_t>(ASRUtils::symbol_get_past_external(
                    witness->m_lifecycle.m_type_declaration))->m_is_abstract,
                "asr.verify.trait_owner.concrete_type",
                "Typed allocation requires an instantiable concrete type");
            require_id(ASRUtils::trait_contracts_equal(witness->m_contract,
                    &ASRUtils::trait_runtime_contract(type)->base),
                "asr.verify.trait_allocate.contract",
                "Typed allocation requires a witness of the owner's exact contract");
        }
    }

    void visit_TraitAssignment(const TraitAssignment_t &x) {
        auto *type = verify_trait_owner(x.m_target, x.base.base.loc);
        verify_trait_copy_source(x.m_value, type, x.m_witness, x.base.base.loc);
    }

    void reject_implicit_trait_type(ttype_t *type, const Location &loc) {
        require_with_loc_id(!type || !ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(type)),
            "asr.verify.trait_owner.explicit_protocol",
            "Trait storage requires checked allocation, copying and cleanup operations", loc);
    }

    void reject_implicit_trait_storage(expr_t *value, const Location &loc) {
        reject_implicit_trait_type(typed_expr_type(value), loc);
    }

    void visit_ReAlloc(const ReAlloc_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            reject_implicit_trait_storage(x.m_args[i].m_a, x.base.base.loc);
        }
        BaseWalkVisitor::visit_ReAlloc(x);
    }

    void visit_Nullify(const Nullify_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            auto *association = ASRUtils::association_variable(x.m_vars[i]);
            require_id(!association || (!ASR::is_a<Var_t>(*x.m_vars[i]) &&
                    association->m_intent != intentType::In),
                "asr.verify.association.pointer_attribute",
                "A construct association is not a pointer and cannot nullify a read-only subobject");
            if (ASRUtils::is_trait_pointer(typed_expr_type(x.m_vars[i]))) {
                verify_trait_pointer(x.m_vars[i], x.base.base.loc, true);
            } else {
                reject_implicit_trait_storage(x.m_vars[i], x.base.base.loc);
            }
        }
        BaseWalkVisitor::visit_Nullify(x);
    }

    void visit_TraitReceiver(const TraitReceiver_t &x) {
        require_id(x.m_view && x.m_witness && x.m_type_declaration && x.m_type,
            "asr.verify.trait_receiver.required_fields",
            "Concrete recovery requires a view, witness, and nominal type");
        visit_expr(*x.m_view);
        visit_ttype(*x.m_type);
        if (!check_external) return;
        auto *witness = verify_trait_witness_reference(x.m_witness, x.base.base.loc);
        auto *implementation = verify_runtime_trait_evidence(*witness, x.base.base.loc);
        auto *view_type = typed_expr_type(x.m_view);
        require_id(view_type && ASR::is_a<TraitObjectType_t>(*view_type),
            "asr.verify.trait_receiver.nominal_type",
            "Concrete recovery requires a scalar trait view");
        visit_ttype(*view_type);
        require_id(ASRUtils::trait_contracts_equal(
                    &ASRUtils::trait_runtime_contract(view_type)->base, witness->m_contract) &&
                ASR::is_a<StructType_t>(*x.m_type) && !ASRUtils::is_class_type(x.m_type) &&
                x.m_type_declaration && symtab_in_scope(current_symtab, x.m_type_declaration) &&
                ASRUtils::symbol_get_past_external(x.m_type_declaration) ==
                    ASRUtils::symbol_get_past_external(implementation->m_type_declaration),
            "asr.verify.trait_receiver.nominal_type",
            "Concrete recovery must preserve the witness's proven nominal payload type");
        auto *scope = current_symtab;
        while (scope && scope->asr_owner && ASR::is_a<symbol_t>(*scope->asr_owner) &&
                !ASR::is_a<Function_t>(*ASR::down_cast<symbol_t>(scope->asr_owner))) {
            scope = scope->parent;
        }
        bool authorized = false;
        for (size_t i = 0; i < witness->n_procedures; i++) {
            auto *function = verify_runtime_trait_procedure(witness->m_procedures[i],
                x.base.base.loc, "asr.verify.trait_witness.unique_procedure");
            if (scope && scope->parent == witness->m_symtab &&
                    (ASR::asr_t*)function == scope->asr_owner) {
                authorized = function->n_args && same_variable(x.m_view, function->m_args[0]);
            }
        }
        require_id(authorized, "asr.verify.trait_receiver.authorized_adapter",
            "Only a selected witness's adapter may recover its own borrowed receiver");
    }

    template <typename T>
    void verify_trait_call(const T &x) {
        require_id(x.n_args > 0 && x.m_args && x.m_args[0].m_value &&
                typed_expr_type(x.m_args[0].m_value) &&
                ASR::is_a<TraitObjectType_t>(*typed_expr_type(x.m_args[0].m_value)),
            "asr.verify.trait_call.receiver",
            "A runtime trait call requires a borrowed view as its first argument");
        for (size_t i = 1; i < x.n_args; i++) {
            if (x.m_args[i].m_value && ASR::is_a<TraitDeferredPack_t>(*x.m_args[i].m_value)) {
                visit_TraitDeferredPack(*ASR::down_cast<TraitDeferredPack_t>(x.m_args[i].m_value));
            }
        }
        if (!check_external) return;
        visit_ttype(*typed_expr_type(x.m_args[0].m_value));
        auto *contract = ASRUtils::trait_runtime_contract(
            typed_expr_type(x.m_args[0].m_value));
        require_id(x.m_slot >= 0 && (size_t)x.m_slot < contract->n_slots,
            "asr.verify.trait_call.slot",
            "A runtime call must select a declared slot");
        auto *procedure = verify_runtime_trait_slot(*contract, x.m_slot, x.base.base.loc);
        require_id(
                ASRUtils::symbol_get_past_external(x.m_name) ==
                    &procedure->base,
            "asr.verify.trait_call.slot",
            "A runtime call must name the canonical interface at its view's slot");
    }

    void visit_TraitFunctionCall(const TraitFunctionCall_t &x) {
        verify_trait_call(x);
        FunctionCall_t call{};
        call.base.base.loc = x.base.base.loc;
        call.m_name = x.m_name;
        call.m_args = x.m_args;
        call.n_args = x.n_args;
        call.m_type = x.m_type;
        visit_FunctionCall(call);
    }

    void visit_TraitSubroutineCall(const TraitSubroutineCall_t &x) {
        verify_trait_call(x);
        SubroutineCall_t call{};
        call.base.base.loc = x.base.base.loc;
        call.m_name = x.m_name;
        call.m_args = x.m_args;
        call.n_args = x.n_args;
        bool saved = _inside_trait_subroutine;
        _inside_trait_subroutine = true;
        visit_SubroutineCall(call);
        _inside_trait_subroutine = saved;
    }

    // Raises the mismatch `m` as a verifier error under `prefix`.
    void require_conforming(const ASRUtils::InterfaceMismatch &m,
            const std::string &prefix, const Location &loc) {
        require_with_loc_id(!m.mismatch, prefix + "." + m.code, m.message, loc);
    }

    void verify_binding_override(const StructMethodDeclaration_t &x,
            ASR::Function_t *proc) {
        if (!check_external) return;
        ASR::StructMethodDeclaration_t *base_decl =
            ASRUtils::overridden_binding(x);
        if (base_decl == nullptr) return;
        ASR::symbol_t *base_sym =
            ASRUtils::symbol_get_past_external(base_decl->m_proc);
        if (base_sym == nullptr || !ASR::is_a<ASR::Function_t>(*base_sym)) {
            return;
        }
        std::string what = "Type bound procedure '" + std::string(x.m_name) +
            "' overriding '" +
            std::string(ASR::down_cast<ASR::Function_t>(base_sym)->m_name) +
            "'";
        require_conforming(
            ASRUtils::binding_override_mismatch(x, proc, what, check_external),
            "asr.verify.binding_override", x.base.base.loc);
    }

    // A procedure's dummy variables and its result variable are declared by
    // the procedure itself. One that resolves in an enclosing scope instead
    // is a host variable the procedure would then write through as if it
    // owned it. A dummy procedure is exempt: it names the procedure symbol
    // itself, which lives where that procedure was declared.
    void require_own_symbol(ASR::expr_t *e, const std::string &owner,
            const std::string &what) {
        if (e == nullptr || !ASR::is_a<ASR::Var_t>(*e)) return;
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(e)->m_v;
        if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return;
        require_with_loc_id(
            ASRUtils::symbol_parent_symtab(sym) == current_symtab,
            "asr.verify.function.argument_declared_locally",
            "The " + what + " of '" + owner + "', '" +
            std::string(ASRUtils::symbol_name(sym)) +
            "', is not declared in it",
            e->base.loc);
    }

    // An elemental procedure is defined on scalars and applied elementwise,
    // which is what lets a caller pass arrays of any shape to it. A dummy
    // argument that is itself an array leaves that rewrite with no shape to
    // agree on.
    void verify_elemental_arguments(const Function_t &x) {
        if (!ASRUtils::get_FunctionType(x)->m_elemental) return;
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::ttype_t *type = typed_expr_type(x.m_args[i]);
            if (type == nullptr) continue;
            require_id(!ASRUtils::is_array(type),
                "asr.verify.function.elemental_arguments_scalar",
                "Elemental procedure '" + std::string(x.m_name) +
                "' declares argument " + std::to_string(i + 1) +
                " as an array");
        }
    }

    static ASR::memory_spaceType array_memory_space(ASR::ttype_t *type) {
        ASR::ttype_t *base =
            ASRUtils::type_get_past_allocatable_pointer(type);
        if (ASR::is_a<ASR::Array_t>(*base)) {
            return ASR::down_cast<ASR::Array_t>(base)->m_memory_space;
        }
        return ASR::memory_spaceType::Global;
    }

    // Whether a scope belongs to code the host runs, which has one flat
    // memory and so knows only the Global space. Everything device_partition
    // did not take is host code, the clones the memory space pass makes for
    // the device included.
    static bool is_host_only_scope(SymbolTable *symtab) {
        while (symtab) {
            ASR::asr_t *owner = symtab->asr_owner;
            if (owner && ASR::is_a<ASR::symbol_t>(*owner)) {
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(owner);
                if (ASRUtils::runs_on_device(sym)) return false;
            }
            symtab = symtab->parent;
        }
        return true;
    }

    // The memory space of an array is part of a routine's interface, so the
    // signature and the dummy it describes have to name the same one. A code
    // generator that qualifies a parameter from one and indexes it from the
    // other would otherwise read the wrong memory.
    void verify_argument_memory_spaces(const Function_t &x) {
        ASR::FunctionType_t *ftype = ASRUtils::get_FunctionType(x);
        for (size_t i = 0; i < x.n_args && i < ftype->n_arg_types; i++) {
            if (!ASR::is_a<ASR::Var_t>(*x.m_args[i])) continue;
            ASR::symbol_t *arg_sym =
                ASR::down_cast<ASR::Var_t>(x.m_args[i])->m_v;
            if (!arg_sym || !ASR::is_a<ASR::Variable_t>(*arg_sym)) continue;
            ASR::ttype_t *declared =
                ASR::down_cast<ASR::Variable_t>(arg_sym)->m_type;
            if (!declared || !ftype->m_arg_types[i]) continue;
            if (!ASRUtils::is_array(declared) ||
                    !ASRUtils::is_array(ftype->m_arg_types[i])) {
                continue;
            }
            require(array_memory_space(declared)
                        == array_memory_space(ftype->m_arg_types[i]),
                "Argument " + std::to_string(i + 1) + " of `"
                    + std::string(x.m_name) + "` is declared in a different "
                    "memory space than its signature gives it");
        }
    }

    // The number of character dummies of `fn` that are passed with a hidden
    // length (ASRUtils::is_string_dummy_with_hidden_length). Their hidden
    // lengths are its last that many dummies.
    static size_t count_hidden_string_lengths(const Function_t &fn) {
        size_t n = 0;
        for (size_t i = 0; i < fn.n_args; i++) {
            if (ASRUtils::is_string_dummy_with_hidden_length(fn, fn.m_args[i])) {
                n++;
            }
        }
        return n;
    }

    // A template is only compiled once it is instantiated, and the
    // string_length_arguments pass leaves it alone.
    static bool is_in_template(SymbolTable *symtab) {
        for (; symtab != nullptr; symtab = symtab->parent) {
            ASR::asr_t *owner = symtab->asr_owner;
            if (owner && ASR::is_a<ASR::symbol_t>(*owner) &&
                    (ASR::is_a<ASR::Template_t>(*ASR::down_cast<ASR::symbol_t>(owner)) ||
                     ASR::is_a<ASR::Requirement_t>(*ASR::down_cast<ASR::symbol_t>(owner)))) {
                return true;
            }
        }
        return false;
    }

    static bool is_hidden_string_length_type(ASR::ttype_t *t) {
        return t != nullptr && ASR::is_a<ASR::Integer_t>(*t) &&
            ASR::down_cast<ASR::Integer_t>(t)->m_kind == 8;
    }

    // After the string_length_arguments pass, each character dummy passed
    // with a hidden length has it: the n-th one, in the order of the
    // dummies, is the n-th of the `integer(8), value, intent(in)` dummies
    // that end the argument list.
    void verify_hidden_string_lengths(const Function_t &x) {
        size_t n_hidden = count_hidden_string_lengths(x);
        if (n_hidden == 0 || is_in_template(x.m_symtab)) return;
        std::string func_name = x.m_name;
        require_id(x.n_args >= 2 * n_hidden,
            "asr.verify.function.hidden_string_length_missing",
            "Function '" + func_name + "' has " + std::to_string(n_hidden) +
            " character dummies passed with a hidden length, but not as many "
            "hidden length dummies");
        size_t first_hidden = x.n_args - n_hidden;
        size_t j = first_hidden;
        for (size_t i = 0; i < x.n_args; i++) {
            if (!ASRUtils::is_string_dummy_with_hidden_length(x, x.m_args[i])) {
                continue;
            }
            require_id(i < first_hidden,
                "asr.verify.function.hidden_string_length_missing",
                "Function '" + func_name + "': the hidden length dummies "
                "must follow all character dummies");
            ASR::symbol_t *hidden = ASR::is_a<ASR::Var_t>(*x.m_args[j])
                ? ASR::down_cast<ASR::Var_t>(x.m_args[j])->m_v : nullptr;
            ASR::Variable_t *hidden_var = hidden && ASR::is_a<ASR::Variable_t>(*hidden)
                ? ASR::down_cast<ASR::Variable_t>(hidden) : nullptr;
            require_with_loc_id(hidden_var != nullptr &&
                    is_hidden_string_length_type(hidden_var->m_type) &&
                    hidden_var->m_value_attr &&
                    hidden_var->m_intent == ASR::intentType::In &&
                    hidden_var->m_presence == ASR::presenceType::Required,
                "asr.verify.function.hidden_string_length_missing",
                "Function '" + func_name + "': dummy " + std::to_string(j + 1) +
                " must be the hidden `integer(8), value, intent(in)` length of "
                "character dummy " + std::to_string(i + 1),
                x.m_args[i]->base.loc);
            ASR::String_t *str = ASRUtils::get_string_type(
                ASRUtils::expr_type(x.m_args[i]));
            require_with_loc_id(str->m_len_kind !=
                    ASR::string_length_kindType::AssumedLength,
                "asr.verify.function.hidden_string_length_missing",
                "Function '" + func_name + "': assumed-length character dummy " +
                std::to_string(i + 1) + " must take its length from its hidden "
                "length dummy",
                x.m_args[i]->base.loc);
            j++;
        }
    }

    // After the string_length_arguments pass, a call passes the hidden length
    // of each character argument that has one.
    template <typename T>
    void verify_hidden_string_length_actuals(const T &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
        if (s && ASR::is_a<ASR::StructMethodDeclaration_t>(*s)) {
            s = ASR::down_cast<ASR::StructMethodDeclaration_t>(s)->m_proc;
        } else if (s && ASR::is_a<ASR::Variable_t>(*s)) {
            s = ASR::down_cast<ASR::Variable_t>(s)->m_type_declaration;
        }
        if (s) s = ASRUtils::symbol_get_past_external(s);
        if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) return;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
        size_t n_hidden = count_hidden_string_lengths(*fn);
        if (n_hidden == 0 || is_in_template(current_symtab)) return;
        require_id(x.n_args == fn->n_args,
            "asr.verify.call.hidden_string_length_missing",
            "Call to '" + std::string(fn->m_name) + "' must pass the hidden "
            "length of each of its " + std::to_string(n_hidden) +
            " character arguments");
        for (size_t i = fn->n_args - n_hidden; i < x.n_args; i++) {
            require_with_loc_id(x.m_args[i].m_value != nullptr &&
                    is_hidden_string_length_type(
                        ASRUtils::expr_type(x.m_args[i].m_value)),
                "asr.verify.call.hidden_string_length_missing",
                "Call to '" + std::string(fn->m_name) + "': argument " +
                std::to_string(i + 1) + " must be the `integer(8)` length of "
                "a character argument",
                x.m_args[i].loc);
        }
    }

    void visit_Function(const Function_t &x) {
        std::vector<std::string> function_dependencies_copy = function_dependencies;
        function_dependencies.clear();
        function_dependencies.reserve(x.n_dependencies);
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "The Function::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The Function::m_symtab->parent is not the right parent");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "Function::m_symtab->counter must be unique");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        require(x.m_name, "Function name is required");
        std::string func_name = x.m_name;
        require(x.m_function_signature,
                    "Type signature is required for `" + func_name + "`");
        if (check_external && ASRUtils::get_FunctionType(x)->m_is_restriction) {
            bool numeric = false, recorded = false;
            for (const auto &entry : parent_symtab->get_scope()) {
                if (!ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) continue;
                auto *constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
                auto *trait = ASRUtils::symbol_get_past_external(constraint->m_trait);
                if (trait && ASR::is_a<ASR::Trait_t>(*trait)) {
                    numeric |= ASR::down_cast<ASR::Trait_t>(trait)->m_kind ==
                        ASR::trait_kindType::IntrinsicTypeSet;
                }
                for (size_t i = 0; i < constraint->n_requirements; i++) {
                    recorded |= constraint->m_requirements[i].m_procedure == &x.base;
                }
                for (size_t i = 0; i < constraint->n_intrinsic_requirements; i++) {
                    recorded |= constraint->m_intrinsic_requirements[i].m_procedure == &x.base;
                }
            }
            require_id(!numeric || recorded, "asr.verify.type_set.restriction_record",
                "A type-set restriction must have an explicit capability proof");
        }
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            LCOMPILERS_ASSERT(a.second);
            this->visit_symbol(*a.second);
        }
        visit_ttype(*x.m_function_signature);
        for (size_t i=0; i<x.n_args; i++) {
            LCOMPILERS_ASSERT(x.m_args[i]);
            require_own_symbol(x.m_args[i], func_name,
                "dummy argument " + std::to_string(i + 1));
            visit_expr(*x.m_args[i]);
        }
        if (ASRUtils::has_trait_out_cleanup(x) ||
                ASRUtils::has_trait_component_cleanup(x.m_symtab)) {
            require_id(!ASRUtils::get_FunctionType(x)->m_pure &&
                    !x.m_side_effect_free && !x.m_deterministic,
                "asr.verify.trait_component.cleanup_effects",
                "Trait ownership cleanup must retain its unchecked dynamic lifecycle effects");
        }
        for (size_t i=0; i<x.n_body; i++) {
            LCOMPILERS_ASSERT(x.m_body[i]);
            visit_stmt(*x.m_body[i]);
        }
        if (check_external && (x.m_side_effect_free || x.m_deterministic)) {
            bool own_effects = ASR::has_trait_lifecycle_effects(x.m_body, x.n_body);
            require_id(!own_effects,
                "asr.verify.trait_owner.lifecycle_effects",
                "Trait lifecycle operations must retain their unchecked dynamic effects");
            // A loaded procedure retains what its own compilation could see;
            // overrides compiled here may add to what it reaches.
            if (!own_effects && !in_loaded_module(x.m_symtab) &&
                    ASR::TraitLifecycleSummary::analyzable(x)) {
                trait_lifecycle_callers.push_back(&x);
            }
        }
        if (x.m_return_var) {
            require_own_symbol(x.m_return_var, func_name, "result variable");
            visit_expr(*x.m_return_var);
        }

        verify_unique_dependencies(x.m_dependencies, x.n_dependencies,
                                   x.m_name, x.base.base.loc);
        verify_elemental_arguments(x);
        verify_argument_memory_spaces(x);
        if (check_string_length_arguments) verify_hidden_string_lengths(x);
        if (x.m_gpu) verify_gpu_kernel_layout(x);

        // Get the x parent symtab.
        SymbolTable *x_parent_symtab = x.m_symtab->parent;

        // Dependencies of the function should be from function's parent symbol table.
        for( size_t i = 0; i < x.n_dependencies; i++ ) {
            std::string found_dep = x.m_dependencies[i];

            // Get the symbol of the found_dep.
            ASR::symbol_t* dep_sym = x_parent_symtab->resolve_symbol(found_dep);

            require(dep_sym != nullptr,
                            "Dependency " + found_dep +  " is inside symbol table " + std::string(x.m_name));
        }
        // Check if there are unnecessary dependencies
        // present in the dependency list of the function
        for( size_t i = 0; i < x.n_dependencies; i++ ) {
            std::string found_dep = x.m_dependencies[i];
            require(std::find(function_dependencies.begin(), function_dependencies.end(), found_dep) != function_dependencies.end(),
                    "Function " + std::string(x.m_name) + " doesn't depend on " + found_dep +
                    " but is found in its dependency list.");
        }

        // Check if all the dependencies found are
        // present in the dependency list of the function
        for( auto& found_dep: function_dependencies ) {
            require(present(x.m_dependencies, x.n_dependencies, found_dep),
                    "Function " + std::string(x.m_name) + " depends on " + found_dep +
                    " but isn't found in its dependency list.");
        }

        ASR::FunctionType_t *function_type =
            ASRUtils::get_FunctionType(x);
        require(function_type->n_arg_types == x.n_args,
            "Number of argument types in FunctionType must be exactly same as "
            "number of arguments in the function");
        if (ASRUtils::is_bare_implicit_interface(x)) {
            require_id(x.n_args == 0,
                "asr.verify.function.implicit_interface_has_no_args",
                "Function '" + func_name + "' has deftype ImplicitInterface, "
                "so it must have no dummy arguments");
            require_id(x.n_body == 0,
                "asr.verify.function.implicit_interface_has_no_body",
                "Function '" + func_name + "' has deftype ImplicitInterface, "
                "so it must have no body");
            require_id(function_type->m_abi == ASR::abiType::Source
                    || function_type->m_abi == ASR::abiType::ExternalUndefined,
                "asr.verify.function.implicit_interface_is_source",
                "Function '" + func_name + "' has deftype ImplicitInterface, "
                "so its abi must be Source or ExternalUndefined");
        }
        if (!diagnostics.has_error()) {
            for (size_t i = 0; i < x.n_args; i++) {
                ASR::ttype_t *argument_type =
                    typed_expr_type(x.m_args[i]);
                if (argument_type == nullptr
                        || is_procedure_type(argument_type)
                        || is_procedure_type(
                            function_type->m_arg_types[i])
                        || is_struct_like_type(argument_type)
                        || is_struct_like_type(
                            function_type->m_arg_types[i])) {
                    continue;
                }
                require_with_loc_id(
                    ASRUtils::check_equal_type(
                        function_type->m_arg_types[i], argument_type,
                        nullptr, type_context(x.m_args[i])),
                    "asr.verify.function.argument_type_matches_signature",
                    "Function argument type " +
                        ASRUtils::get_type_code(argument_type) +
                        " does not match signature type " +
                        ASRUtils::get_type_code(
                            function_type->m_arg_types[i]),
                    x.m_args[i]->base.loc);
            }

            // An implicit interface is synthesised from a bare `external`
            // declaration, so its signature carries an assumed return type
            // that no return variable corresponds to.
            bool is_implementation = function_type->m_deftype
                == ASR::deftypeType::Implementation;
            bool signature_has_return =
                function_type->m_return_var_type != nullptr;
            bool function_has_return = x.m_return_var != nullptr;
            if (is_implementation) {
                require_id(
                    signature_has_return == function_has_return,
                    "asr.verify.function.return_presence_matches_signature",
                    "Function return variable presence does not match "
                    "signature");
            }
            ASR::ttype_t *return_type =
                signature_has_return
                    ? typed_expr_type(x.m_return_var) : nullptr;
            if (return_type && !is_procedure_type(return_type)
                    && !is_procedure_type(
                        function_type->m_return_var_type)
                    && !is_struct_like_type(return_type)
                    && !is_struct_like_type(
                        function_type->m_return_var_type)) {
                require_with_loc_id(
                    ASRUtils::check_equal_type(
                        function_type->m_return_var_type, return_type,
                        nullptr, type_context(x.m_return_var)),
                    "asr.verify.function.return_type_matches_signature",
                    "Function return type " +
                        ASRUtils::get_type_code(return_type) +
                        " does not match signature type " +
                        ASRUtils::get_type_code(
                            function_type->m_return_var_type),
                    x.m_return_var->base.loc);
            }
        }

        visit_ttype(*x.m_function_signature);
        current_symtab = parent_symtab;
        function_dependencies = function_dependencies_copy;
    }

    template <typename T>
    void visit_UserDefinedType(const T &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_name != nullptr,
            "The Struct::m_name cannot be nullptr");
        require(x.m_symtab != nullptr,
            "The Struct::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "The Struct::m_symtab->parent is not the right parent");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "The X::m_symtab::asr_owner must point to X");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "Struct::m_symtab->counter must be unique");
        require(ASRUtils::symbol_symtab(down_cast<symbol_t>(current_symtab->asr_owner)) == current_symtab,
            "The asr_owner invariant failed");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        // A member name is how the rest of the compiler finds the member's
        // declaration, and every lookup of one that is not there has to
        // invent an answer.
        for (size_t i = 0; i < x.n_members; i++) {
            require_id(x.m_symtab->get_symbol(std::string(x.m_members[i]))
                    != nullptr,
                "asr.verify.user_defined_type.member_is_declared",
                "'" + std::string(x.m_name) + "' lists the member '" +
                std::string(x.m_members[i]) + "', which it does not declare");
        }
        std::vector<std::string> struct_dependencies;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
            if( ASR::is_a<ASR::StructMethodDeclaration_t>(*a.second) ||
                ASR::is_a<ASR::GenericProcedure_t>(*a.second) ||
                ASR::is_a<ASR::Struct_t>(*a.second) ||
                ASR::is_a<ASR::Union_t>(*a.second) ||
                ASR::is_a<ASR::ExternalSymbol_t>(*a.second) ||
                ASR::is_a<ASR::CustomOperator_t>(*a.second) ) {
                continue ;
            }
            if ( ASR::is_a<ASR::Variable_t>(*a.second) ) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(a.second);
                if ( var->m_type_declaration ) {
                    struct_dependencies.push_back(
                        std::string(ASRUtils::symbol_name(var->m_type_declaration)));
                }
            }
            // TODO: Uncomment the following line
            // ASR::ttype_t* var_type = ASRUtils::extract_type(ASRUtils::symbol_type(a.second));
            ASR::ttype_t* var_type = ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(a.second));
            char* aggregate_type_name = nullptr;
            ASR::symbol_t* sym = nullptr;
            if( ASR::is_a<ASR::EnumType_t>(*var_type) ) {
                sym = ASR::down_cast<ASR::EnumType_t>(var_type)->m_enum_type;
                aggregate_type_name = ASRUtils::symbol_name(sym);
            }
            if( aggregate_type_name && ASRUtils::symbol_parent_symtab(sym) != current_symtab ) {
                struct_dependencies.push_back(std::string(aggregate_type_name));
                require(present(x.m_dependencies, x.n_dependencies, std::string(aggregate_type_name)),
                    std::string(x.m_name) + " depends on " + std::string(aggregate_type_name)
                    + " but it isn't found in its dependency list.");
            }
        }
        for( size_t i = 0; i < x.n_dependencies; i++ ) {
            require(std::find(struct_dependencies.begin(), struct_dependencies.end(),
                    std::string(x.m_dependencies[i])) != struct_dependencies.end(),
                std::string(x.m_dependencies[i]) + " is not a dependency of " + std::string(x.m_name)
                + " but it is present in its dependency list.");
        }

        verify_unique_dependencies(x.m_dependencies, x.n_dependencies,
                                   x.m_name, x.base.base.loc);
        current_symtab = parent_symtab;
    }

    // `class(*)`, and the assumed-type temporaries the array passes build
    // from it, resolve to a type that is polymorphic by construction.
    static bool declares_unlimited_polymorphic(ASR::Struct_t *s) {
        if (s->m_struct_signature == nullptr) return false;
        if (!ASR::is_a<ASR::StructType_t>(*s->m_struct_signature)) return false;
        return ASR::down_cast<ASR::StructType_t>(
            s->m_struct_signature)->m_is_unlimited_polymorphic;
    }

    // A final subroutine is called by the compiler, never by the program, so
    // there is no call site to check it against. It takes exactly one
    // argument, the entity being finalized, and returns nothing.
    void verify_final_procedures(const Struct_t &x) {
        if (!check_external || x.m_symtab->parent == nullptr) return;
        for (size_t i = 0; i < x.n_member_functions; i++) {
            ASR::symbol_t *sym =
                x.m_symtab->parent->resolve_symbol(x.m_member_functions[i]);
            if (sym == nullptr) continue;
            sym = ASRUtils::symbol_get_past_external(sym);
            if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym)) continue;
            ASR::Function_t *final_proc = ASR::down_cast<ASR::Function_t>(sym);
            std::string which = "Final procedure '" +
                std::string(x.m_member_functions[i]) + "' of '" +
                std::string(x.m_name) + "'";
            require_id(final_proc->m_return_var == nullptr,
                "asr.verify.struct.final_procedure_signature",
                which + " must be a subroutine");
            require_id(final_proc->n_args == 1,
                "asr.verify.struct.final_procedure_signature",
                which + " must take exactly one argument, not " +
                std::to_string(final_proc->n_args));
        }
    }

    // A deferred binding promises that every concrete type in the hierarchy
    // supplies a body for it. A type that is not abstract and never overrides
    // one leaves the dispatch table with a hole nothing fills, which is a
    // call through a null slot rather than a diagnostic.
    void verify_deferred_bindings(const Struct_t &x) {
        if (!check_external || x.m_is_abstract) return;
        std::set<std::string> nearest;
        std::set<const ASR::Struct_t*> seen;
        const ASR::Struct_t *s = &x;
        while (s != nullptr) {
            if (!seen.insert(s).second) return;
            for (auto &item : s->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::StructMethodDeclaration_t>(*item.second)) {
                    continue;
                }
                // The nearest declaration of a name is the one in effect;
                // anything it hides has already been overridden.
                if (!nearest.insert(item.first).second) continue;
                ASR::StructMethodDeclaration_t *binding =
                    ASR::down_cast<ASR::StructMethodDeclaration_t>(
                        item.second);
                require_id(!binding->m_is_deferred,
                    "asr.verify.struct.deferred_binding_overridden",
                    "'" + std::string(x.m_name) + "' is not abstract but "
                    "does not override the deferred type bound procedure '" +
                    item.first + "'");
            }
            ASR::symbol_t *parent = s->m_parent == nullptr ? nullptr
                : ASRUtils::symbol_get_past_external(s->m_parent);
            s = (parent != nullptr && ASR::is_a<ASR::Struct_t>(*parent))
                ? ASR::down_cast<ASR::Struct_t>(parent) : nullptr;
        }
    }

    void verify_type_trait_obligations(const Struct_t &x) {
        require_id(!x.n_trait_obligations || x.m_trait_obligations,
            "asr.verify.struct.trait_obligations",
            "Type adoption must retain its nominal trait obligations");
        for (size_t i = 0; i < x.n_trait_obligations; i++) {
            require_id(x.m_trait_obligations[i] &&
                    symtab_in_scope(current_symtab, x.m_trait_obligations[i]),
                "asr.verify.struct.trait_obligation_in_scope",
                "An adopted trait must be visible in the type's declaring scope");
        }
        if (!check_external) return;
        std::set<symbol_t*> traits;
        for (size_t i = 0; i < x.n_trait_obligations; i++) {
            auto *trait = ASRUtils::symbol_get_past_external(x.m_trait_obligations[i]);
            require_id(trait && ASR::is_a<Trait_t>(*trait) &&
                    ASR::down_cast<Trait_t>(trait)->m_kind == trait_kindType::UniversalTrait &&
                    traits.insert(trait).second,
                "asr.verify.struct.trait_obligations",
                "Type obligations must identify distinct universal traits");
        }
        auto *parent = ASRUtils::symbol_get_past_external(x.m_parent);
        if (parent && ASR::is_a<Struct_t>(*parent)) {
            auto *structure = ASR::down_cast<Struct_t>(parent);
            for (size_t i = 0; i < structure->n_trait_obligations; i++) {
                require_id(traits.count(ASRUtils::symbol_get_past_external(
                        structure->m_trait_obligations[i])),
                    "asr.verify.struct.inherited_trait_obligations",
                    "A type must retain every nominal obligation of its parent");
            }
        }
        std::map<symbol_t*, TraitImplementation_t*> implementations;
        if (!traits.empty()) {
            for (const auto &entry : x.m_symtab->parent->get_scope()) {
                if (!ASR::is_a<TraitImplementation_t>(*entry.second)) continue;
                auto *implementation = ASR::down_cast<TraitImplementation_t>(entry.second);
                if (ASRUtils::symbol_get_past_external(implementation->m_type_declaration) ==
                        &x.base) {
                    implementations.emplace(ASRUtils::symbol_get_past_external(
                        implementation->m_trait), implementation);
                }
            }
        }
        std::map<std::string, Function_t*> methods;
        for (auto *trait : traits) {
            auto hierarchy = verify_trait_hierarchy(
                *ASR::down_cast<Trait_t>(trait), x.base.base.loc);
            auto proof = implementations.find(trait);
            require_id(x.m_is_abstract || proof != implementations.end(),
                "asr.verify.struct.concrete_trait_obligations",
                "A concrete type must provide complete evidence for every adopted trait");
            for (auto *member : hierarchy.members) {
                auto *required = ASRUtils::trait_method_function(member);
                auto previous = methods.emplace(required->m_name, required);
                require_id(previous.second || ASRUtils::trait_method_mismatch(
                        *previous.first->second, *required).difference ==
                            ASRUtils::TraitMethodDifference::None,
                    "asr.verify.struct.compatible_trait_obligations",
                    "Adopted traits must agree on shared method signatures");
                symbol_t *method = nullptr;
                std::set<const Struct_t*> seen;
                for (const Struct_t *s = &x; s && !method && seen.insert(s).second;) {
                    method = s->m_symtab->get_symbol(required->m_name);
                    auto *base = ASRUtils::symbol_get_past_external(s->m_parent);
                    s = base && ASR::is_a<Struct_t>(*base)
                        ? ASR::down_cast<Struct_t>(base) : nullptr;
                }
                auto *declaration = method && ASR::is_a<StructMethodDeclaration_t>(*method)
                    ? ASR::down_cast<StructMethodDeclaration_t>(method) : nullptr;
                if (!declaration || declaration->m_is_deferred) {
                    require_id(x.m_is_abstract && proof == implementations.end(),
                        "asr.verify.struct.trait_method_implemented",
                        "A type adoption must use a concrete ordinary binding for each method");
                    continue;
                }
                auto *procedure = verify_runtime_trait_procedure(declaration->m_proc,
                    declaration->base.base.loc, "asr.verify.struct.trait_method_procedure");
                trait_binding_t binding;
                binding.loc = declaration->base.base.loc;
                binding.m_member = member;
                binding.m_procedure = declaration->m_proc;
                binding.m_is_nopass = declaration->m_is_nopass;
                size_t self = ASRUtils::passed_object_index(*declaration, procedure);
                binding.m_self_argument = self < procedure->n_args
                    ? ASRUtils::EXPR2VAR(procedure->m_args[self])->m_name : nullptr;
                verify_trait_binding(const_cast<symbol_t*>(&x.base), binding, false);
                if (proof != implementations.end()) {
                    auto *bound = ASRUtils::find_trait_binding(*proof->second, member);
                    require_id(bound && ASRUtils::trait_bindings_equal(*bound, binding),
                        "asr.verify.struct.trait_method_binding",
                        "Nominal conformance must use the effective ordinary type-bound procedure");
                }
            }
        }
    }

    // A derived type extends another derived type and nothing else. Every
    // member lookup, every dispatch and every layout decision walks this
    // chain, so a parent that is not a type is followed straight into the
    // wrong node.
    void visit_Struct(const Struct_t& x) {
        require_id(!x.m_is_sealed || !x.m_is_abstract,
            "asr.verify.struct.sealed_not_abstract",
            "A sealed type cannot be abstract");
        if (x.m_parent != nullptr) {
            ASR::symbol_t *parent = check_external
                ? ASRUtils::symbol_get_past_external(x.m_parent) : x.m_parent;
            // A sequence type fixes its storage layout, which is what makes
            // it usable across a COMMON block or a BIND(C) boundary; adding
            // an extension's components to it would move what the other side
            // of that boundary already agreed on.
            require_id(!x.m_is_sequence,
                "asr.verify.struct.sequence_type_not_extended",
                "'" + std::string(x.m_name) +
                "' is a sequence type, so it cannot extend another type");
            if (parent != nullptr && ASR::is_a<ASR::Struct_t>(*parent)) {
                require_id(!ASR::down_cast<ASR::Struct_t>(parent)->m_is_sealed,
                    "asr.verify.struct.parent_not_sealed",
                    "A sealed type cannot be extended");
                require_id(
                    !ASR::down_cast<ASR::Struct_t>(parent)->m_is_sequence,
                    "asr.verify.struct.sequence_type_not_extended",
                    "'" + std::string(x.m_name) + "' extends '" +
                    std::string(ASR::down_cast<ASR::Struct_t>(parent)->m_name)
                    + "', which is a sequence type");
            }
            require_id(parent != nullptr &&
                    (ASR::is_a<ASR::Struct_t>(*parent) ||
                     ASR::is_a<ASR::ExternalSymbol_t>(*parent)),
                "asr.verify.struct.parent_is_struct",
                "Struct::m_parent of '" + std::string(x.m_name) +
                "' must be a derived type, not " +
                ASRUtils::symbol_type_name(*x.m_parent));
            require_id(symtab_in_scope(current_symtab, x.m_parent),
                "asr.verify.struct.parent_in_scope",
                "Struct::m_parent of '" + std::string(x.m_name) +
                "' cannot point outside of its symbol table");
        }
        verify_type_trait_obligations(x);
        verify_deferred_bindings(x);
        verify_final_procedures(x);
        visit_UserDefinedType(x);
        if( !x.m_alignment ) {
            return ;
        }
        ASR::expr_t* aligned_expr_value = ASRUtils::expr_value(x.m_alignment);
        std::string msg = "Alignment should always evaluate to a constant expressions.";
        require(aligned_expr_value, msg);
        int64_t alignment_int = 0;
        require(ASRUtils::extract_value(aligned_expr_value, alignment_int), msg);
        require(alignment_int != 0 && (alignment_int & (alignment_int - 1)) == 0,
                "Alignment " + std::to_string(alignment_int) +
                " is not a positive power of 2.");
    }

    void visit_Enum(const Enum_t& x) {
        visit_UserDefinedType(x);
        require(x.m_type != nullptr,
            "The common type of EnumType cannot be nullptr. " +
            std::string(x.m_name) + " doesn't seem to follow this rule.");
        ASR::ttype_t* common_type = x.m_type;
        std::map<int64_t, int64_t> value2count;
        for( auto itr: x.m_symtab->get_scope() ) {
            ASR::Variable_t* itr_var = ASR::down_cast<ASR::Variable_t>(itr.second);
            require(itr_var->m_symbolic_value != nullptr,
                "All members of EnumType must have their values to be set. " +
                std::string(itr_var->m_name) + " doesn't seem to follow this rule in "
                + std::string(x.m_name) + " EnumType.");
            require(ASRUtils::check_equal_type(itr_var->m_type, common_type, nullptr, nullptr),
                "All members of EnumType must the same type. " +
                std::string(itr_var->m_name) + " doesn't seem to follow this rule in " +
                std::string(x.m_name) + " EnumType.");
            ASR::expr_t* value = ASRUtils::expr_value(itr_var->m_symbolic_value);
            int64_t value_int64 = -1;
            ASRUtils::extract_value(value, value_int64);
            if( value2count.find(value_int64) == value2count.end() ) {
                value2count[value_int64] = 0;
            }
            value2count[value_int64] += 1;
        }

        bool is_enumtype_correct = false;
        bool is_enum_integer = ASR::is_a<ASR::Integer_t>(*x.m_type);
        if( x.m_enum_value_type == ASR::enumtypeType::IntegerConsecutiveFromZero ) {
            is_enumtype_correct = (is_enum_integer &&
                                   (value2count.find(0) != value2count.end()) &&
                                   (value2count.size() == x.n_members));
            int64_t prev = -1;
            if( is_enumtype_correct ) {
                for( auto enum_value: value2count ) {
                    if( enum_value.first - prev != 1 ) {
                        is_enumtype_correct = false;
                        break ;
                    }
                    prev = enum_value.first;
                }
            }
        } else if( x.m_enum_value_type == ASR::enumtypeType::IntegerNotUnique ) {
            is_enumtype_correct = is_enum_integer && (value2count.size() != x.n_members);
        } else if( x.m_enum_value_type == ASR::enumtypeType::IntegerUnique ) {
            is_enumtype_correct = is_enum_integer && (value2count.size() == x.n_members);
        } else if( x.m_enum_value_type == ASR::enumtypeType::NonInteger ) {
            is_enumtype_correct = !is_enum_integer;
        }
        require(is_enumtype_correct, "Properties of enum value members don't match correspond "
                                     "to Enum::m_enum_value_type");
    }

    void visit_Union(const Union_t& x) {
        visit_UserDefinedType(x);
    }

    void visit_Variable(const Variable_t &x) {
        if (x.m_storage == storage_typeType::Association) {
            auto *value = ASRUtils::association_value(x);
            require_id(x.m_type && !ASRUtils::is_array(x.m_type) &&
                    !ASRUtils::is_pointer(x.m_type) && !ASRUtils::is_allocatable(x.m_type) &&
                    (ASR::is_a<StructType_t>(*x.m_type) ||
                     ASR::is_a<TraitObjectType_t>(*x.m_type) ||
                     ASR::is_a<Integer_t>(*x.m_type) || ASR::is_a<Real_t>(*x.m_type) ||
                     ASR::is_a<Complex_t>(*x.m_type) || ASR::is_a<Logical_t>(*x.m_type) ||
                     ASR::is_a<String_t>(*x.m_type) || ASR::is_a<CPtr_t>(*x.m_type)) &&
                    (x.m_intent == intentType::Local || x.m_intent == intentType::In) &&
                    x.m_presence == presenceType::Required && !x.m_symbolic_value &&
                    !x.m_value && !x.m_value_attr && !x.n_codims && value,
                "asr.verify.association.storage",
                "A data association is a once-bound scalar construct local, not an owner or pointer");
            require_id(!check_external || ASRUtils::association_is_definable(value) ||
                    x.m_intent == intentType::In,
                "asr.verify.association.definable",
                "A data association must preserve its selector's nondefinability");
            require_id(!check_external || !x.m_target_attr || ASRUtils::association_has_target(value),
                "asr.verify.association.target",
                "A data association cannot acquire TARGET from a selector without TARGET or POINTER");
        }
        if (contains_retained_result_storage(x.m_type)) {
            auto *scope_owner = x.m_parent_symtab ? x.m_parent_symtab->asr_owner : nullptr;
            require_id(ASR::is_a<TraitOwnerList_t>(*x.m_type) &&
                    x.m_intent == intentType::Local &&
                    x.m_storage == storage_typeType::Default &&
                    x.m_presence == presenceType::Required && !x.m_value_attr &&
                    !x.m_target_attr && !x.m_symbolic_value && !x.m_value &&
                    !x.m_type_declaration && !x.n_codims && scope_owner &&
                    ASR::is_a<symbol_t>(*scope_owner) &&
                    (ASR::is_a<Block_t>(*ASR::down_cast<symbol_t>(scope_owner)) ||
                     ASR::is_a<AssociateBlock_t>(*ASR::down_cast<symbol_t>(scope_owner))),
                "asr.verify.trait_results.local_storage",
                "A retained-result store must be an initially empty local of an executable scope");
        }
        if (x.m_type && ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(x.m_type))) {
            bool dummy = false;
            bool returned = false;
            Function_t *function = nullptr;
            if (x.m_parent_symtab && x.m_parent_symtab->asr_owner &&
                    ASR::is_a<symbol_t>(*x.m_parent_symtab->asr_owner) &&
                    ASR::is_a<Function_t>(*ASR::down_cast<symbol_t>(
                        x.m_parent_symtab->asr_owner))) {
                function = ASR::down_cast2<Function_t>(x.m_parent_symtab->asr_owner);
                returned = function->m_return_var &&
                    ASR::is_a<Var_t>(*function->m_return_var) &&
                    ASR::down_cast<Var_t>(function->m_return_var)->m_v == &x.base;
                for (size_t i = 0; i < function->n_args; i++) {
                    dummy |= function->m_args[i] && ASR::is_a<Var_t>(*function->m_args[i]) &&
                        ASR::down_cast<Var_t>(function->m_args[i])->m_v == &x.base;
                }
            }
            bool borrowed = ASR::is_a<TraitObjectType_t>(*x.m_type) &&
                dummy && x.m_storage == storage_typeType::Default &&
                x.m_intent == intentType::In;
            bool slot = ASRUtils::is_trait_owner(x.m_type) && dummy &&
                x.m_storage == storage_typeType::Default &&
                ASRUtils::is_arg_dummy(x.m_intent);
            bool result = ASRUtils::is_trait_owner(x.m_type) && returned &&
                x.m_storage == storage_typeType::Default && x.m_intent == intentType::ReturnVar;
            bool pointer = ASRUtils::is_trait_pointer(x.m_type) && !returned &&
                ((dummy && x.m_storage == storage_typeType::Default &&
                  ASRUtils::is_arg_dummy(x.m_intent)) ||
                 (!dummy && x.m_intent == intentType::Local &&
                  x.m_parent_symtab && x.m_parent_symtab->asr_owner &&
                  ASR::is_a<symbol_t>(*x.m_parent_symtab->asr_owner) &&
                  !ASR::is_a<Struct_t>(*ASR::down_cast<symbol_t>(
                      x.m_parent_symtab->asr_owner)) &&
                  !ASR::is_a<Union_t>(*ASR::down_cast<symbol_t>(
                      x.m_parent_symtab->asr_owner)) &&
                  (x.m_storage == storage_typeType::Default ||
                   x.m_storage == storage_typeType::Save)));
            bool null_initialized = pointer && !dummy &&
                (!x.m_symbolic_value || ASR::is_a<PointerNullConstant_t>(*x.m_symbolic_value)) &&
                (!x.m_value || ASR::is_a<PointerNullConstant_t>(*x.m_value));
            if (null_initialized && check_external) {
                for (auto *value : {x.m_symbolic_value, x.m_value}) {
                    if (!value) continue;
                    auto *source = typed_expr_type(value);
                    require_id(source && ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(source)),
                        "asr.verify.trait_pointer.null_contract",
                        "A null initializer must use the pointer's declared runtime trait contract");
                    visit_ttype(*source);
                    visit_ttype(*x.m_type);
                    require_id(ASRUtils::trait_contracts_equal(source, x.m_type),
                        "asr.verify.trait_pointer.null_contract",
                        "A null initializer must use the pointer's declared runtime trait contract");
                }
            }
            bool owner = ASRUtils::is_trait_owner(x.m_type) && !dummy &&
                x.m_intent == intentType::Local && x.m_parent_symtab &&
                x.m_parent_symtab->asr_owner &&
                ASR::is_a<symbol_t>(*x.m_parent_symtab->asr_owner) &&
                (x.m_storage == storage_typeType::Default || x.m_storage == storage_typeType::Save) &&
                !ASR::is_a<Union_t>(*ASR::down_cast<symbol_t>(
                    x.m_parent_symtab->asr_owner));
            bool association = x.m_storage == storage_typeType::Association &&
                ASR::is_a<TraitObjectType_t>(*x.m_type);
            require_id((borrowed || owner || slot || result || pointer || association) &&
                    x.m_presence == presenceType::Required &&
                    !x.m_value_attr &&
                    ((!x.m_symbolic_value && !x.m_value) || null_initialized) &&
                    !x.m_type_declaration && !x.n_codims,
                "asr.verify.trait_view.borrowed_storage",
                "Trait storage must be a required read-only view, a scalar allocatable slot/result, "
                "or an initially unallocated local owner");
            if (slot || result || (pointer && dummy)) {
                auto *signature = ASRUtils::get_FunctionType(function);
                require_id(signature->m_abi != abiType::BindC &&
                        (!result || ((signature->m_abi == abiType::Source ||
                            signature->m_abi == abiType::ExternalUndefined) &&
                            !ASRUtils::is_bare_implicit_interface(*signature))) &&
                        !(signature->m_pure && (result || x.m_intent == intentType::Out)),
                    "asr.verify.trait_owner.slot_effects",
                    "An allocatable trait slot or result requires the source ABI and proven lifecycle effects");
                require_id(!slot || x.m_intent != intentType::Out ||
                        (!function->m_side_effect_free && !function->m_deterministic),
                    "asr.verify.trait_owner.entry_effects",
                    "A trait OUT-slot procedure must retain its unchecked entry cleanup effects");
            }
        }
        std::string current_name_copy = current_name;
        current_name = x.m_name;
        variable_dependencies.clear();
        // A compile time value is stored into the variable's own storage,
        // so a value whose type disagrees with the declaration produces a
        // store LLVM rejects. The frontend casts such initializers; a graph
        // from another producer may not have.
        for (ASR::expr_t *initial : {x.m_symbolic_value, x.m_value}) {
            ASR::ttype_t *initial_type = typed_expr_type(initial);
            if (diagnostics.has_error() || initial_type == nullptr
                    || x.m_type == nullptr) {
                continue;
            }
            bool scalar_struct_initializer =
                ASR::is_a<ASR::StructConstant_t>(*initial)
                || ASR::is_a<ASR::StructConstructor_t>(*initial);
            if (ASRUtils::is_array(x.m_type)
                    && !ASRUtils::is_array(initial_type)
                    && ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::type_get_past_array(x.m_type))
                    && scalar_struct_initializer) {
                require_id(false,
                    "asr.verify.variable.array_struct_initializer_is_array",
                    "Variable '" + std::string(x.m_name) +
                        "' is an array of derived type, so its initializer "
                        "must be an array expression");
            }
            ASR::ttype_t *declared = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable_pointer(x.m_type));
            ASR::ttype_t *actual = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable_pointer(initial_type));
            // A character initializer is padded or truncated to the
            // declared length, so the two legitimately differ. A kind at or
            // above the parameterized derived type sentinel is a type
            // parameter rather than a storage size, and a parameterized type
            // carries it on the declaration or on the initializer depending
            // on where it has been substituted, so it is not comparable.
            if (is_struct_like_type(declared) || is_procedure_type(declared)
                    || is_struct_like_type(actual) || is_procedure_type(actual)
                    || ASR::is_a<ASR::String_t>(*declared)
                    || ASRUtils::extract_kind_from_ttype_t(declared) >= 1000
                    || ASRUtils::extract_kind_from_ttype_t(actual) >= 1000) {
                continue;
            }
            require_id(
                ASRUtils::check_equal_type(
                    declared, actual, nullptr, nullptr),
                "asr.verify.variable.initializer_type_matches",
                "Variable '" + std::string(x.m_name) + "' initializer type " +
                    ASRUtils::get_type_code(actual) +
                    " does not match declared type " +
                    ASRUtils::get_type_code(declared));
        }
        SymbolTable *symtab = x.m_parent_symtab;
        require(symtab != nullptr,
            "Variable::m_parent_symtab cannot be nullptr");
        require(symtab->get_symbol(std::string(x.m_name)) != nullptr,
            "Variable '" + std::string(x.m_name) + "' not found in parent_symtab symbol table");
        symbol_t *symtab_sym = symtab->get_symbol(std::string(x.m_name));
        const symbol_t *current_sym = &x.base;
        require(symtab_sym == current_sym,
            "Variable's parent symbol table does not point to it");
        require(current_symtab == symtab,
            "Variable's parent-symbolTable and actuall parent symbolTable don't match (Maybe inserted from another symbolTable)");
        require(id_symtab_map.find(symtab->counter) != id_symtab_map.end(),
            "Variable::m_parent_symtab must be present in the ASR ("
                + std::string(x.m_name) + ")");
        if (x.m_type && ASRUtils::is_array(x.m_type)) {
            require(array_memory_space(x.m_type)
                        == ASR::memory_spaceType::Global
                    || !is_host_only_scope(symtab),
                "Variable '" + std::string(x.m_name) + "' is host code, so "
                "its array cannot live in a device memory space");
        }

        ASR::asr_t* asr_owner = symtab->asr_owner;
        bool is_module = false, is_struct = false;
        if( ASR::is_a<ASR::symbol_t>(*asr_owner)) {
            ASR::symbol_t* asr_owner_sym = ASR::down_cast<ASR::symbol_t>(asr_owner);
            if (ASR::is_a<ASR::Module_t>(*asr_owner_sym)) {
                is_module = true;
            }
            if (ASR::is_a<ASR::Struct_t>(*asr_owner_sym)) {
                is_struct = true;
            }
        }
        if( symtab->parent != nullptr &&
            !is_module && !is_struct) {
            // For now restrict this check only to variables which are present
            // inside symbols which have a body.
            ASR::ArrayConstructor_t *array_construct = nullptr;
            if (x.m_symbolic_value && ASR::is_a<ASR::ArrayConstructor_t>(*x.m_symbolic_value)) {
                array_construct = ASR::down_cast<ASR::ArrayConstructor_t>(x.m_symbolic_value);
            }

            if (array_construct && array_construct->n_args > 0) {
                for (size_t j = 0; j < array_construct->n_args; j++) {
                    require( (x.m_symbolic_value == nullptr && x.m_value == nullptr) ||
                            (x.m_symbolic_value != nullptr && x.m_value != nullptr) ||
                            (x.m_symbolic_value != nullptr && ASRUtils::is_value_constant(array_construct->m_args[j])) ||
                            (_inside_template && x.m_storage == ASR::storage_typeType::Parameter &&
                                ASRUtils::reads_valueless_parameter(x.m_symbolic_value)),
                            "Initialisation of " + std::string(x.m_name) +
                            " must reduce to a compile time constant.");
                }
            } else {
                require( (x.m_symbolic_value == nullptr && x.m_value == nullptr) ||
                        (x.m_symbolic_value != nullptr && x.m_value != nullptr) ||
                        (x.m_symbolic_value != nullptr && ASRUtils::is_value_constant(x.m_symbolic_value)) ||
                        ASRUtils::is_entry_initialized_local(x) ||
                        (_inside_template && x.m_storage == ASR::storage_typeType::Parameter &&
                            ASRUtils::reads_valueless_parameter(x.m_symbolic_value)),
                        "Initialisation of " + std::string(x.m_name) +
                        " must reduce to a compile time constant.");
                if (ASRUtils::is_entry_initialized_local(x)) {
                    EntryInitializerReference reference(x);
                    reference.visit_expr(*x.m_symbolic_value);
                    require(reference.undefined == nullptr,
                        "The initializer of " + std::string(x.m_name) +
                        ", evaluated on entry, references the local " +
                        (reference.undefined ? std::string(reference.undefined->m_name) : "") +
                        ", which is not defined on entry");
                }
            }
        }
        if(ASRUtils::is_character(*x.m_type)){
            String_t* str = down_cast<String_t>(ASRUtils::extract_type(x.m_type));
            require(str->m_len_kind != ASR::ImplicitLength,
                "Variable symbol of string type can't have a length of kind \"ImplicitLength\"")
            if(str->m_len_kind == ASR::DeferredLength){
                /* 
                    String type Varaible + DeferredLength ==> Must be allocatable or pointer(atleast for Fortran frontend)
                    String type Expressions + DeferredLength ==> Dont' have to be allocatable or pointer.
                */ 
                require(ASRUtils::is_allocatable(x.m_type) || ASRUtils::is_pointer(x.m_type) ,
                    "Variable of string type with length kind \"DeferredLength\" must be allocatable OR pointer");
            }
            if(x.m_abi == abiType::BindC && 
                x.m_intent != ASR::Local /*Input OR Output*/){
                if(ASRUtils::is_string_only(x.m_type) && 
                    str->m_physical_type == CChar){ // Exclude array of strings
                    if(str->m_len_kind != ASR::DeferredLength
                            && str->m_len_kind != ASR::AssumedLength){
                        require(str->m_len_kind == ASR::ExpressionLength, 
                            "Cbind character variable that isn't local must have length kind \"ExpressionLength\"");
                        int64_t len = 0; ASRUtils::extract_value(str->m_len, len);
                        require(len == 1,
                            "Cbind character variable that isn't local must have length 1");
                    }
                }
            }
            if(str->m_physical_type == ASR::CChar){
                require(x.m_intent != ASR::Local,
                    "CChar-string-physical type shouldn't be used with local variables");
            }
            if(str->m_len_kind == ASR::AssumedLength && 
                x.m_storage !=ASR::Parameter &&
                !ASRUtils::is_pointer(x.m_type) /*Tolerate pointer*/){
                require(x.m_intent != ASR::Local,
                    "AssumedLength-string variable should be a dummy variable (intent IN or OUT or INOUT) or a function return variable.");
            }
        }
        if (x.m_symbolic_value)
            visit_expr(*x.m_symbolic_value);
        if (x.m_value)
             visit_expr(*x.m_value);
        _return_var_or_intent_out = x.m_intent == ASR::intentType::Out ||
                                    x.m_intent == ASR::intentType::InOut ||
                                    x.m_intent == ASR::intentType::ReturnVar;
        visit_ttype(*x.m_type);
        _return_var_or_intent_out = false;

        for (size_t i = 0; i < x.n_codims; i++) {
            if (x.m_codims[i].m_start) {
                visit_expr(*x.m_codims[i].m_start);
            }
            if (x.m_codims[i].m_end) {
                visit_expr(*x.m_codims[i].m_end);
            }
        }

        verify_unique_dependencies(x.m_dependencies, x.n_dependencies,
                                   x.m_name, x.base.base.loc);

        // Verify dependencies
        for( size_t i = 0; i < x.n_dependencies; i++ ) {
            require(std::find(
                variable_dependencies.begin(),
                variable_dependencies.end(),
                std::string(x.m_dependencies[i])
            ) != variable_dependencies.end(),
                "Variable " + std::string(x.m_name) + " doesn't depend on " +
                std::string(x.m_dependencies[i]) + " but is found in its dependency list.");
        }

        for( size_t i = 0; i < variable_dependencies.size(); i++ ) {
            require(present(x.m_dependencies, x.n_dependencies, variable_dependencies[i]),
                "Variable " + std::string(x.m_name) + " depends on " +
                std::string(variable_dependencies[i]) + " but isn't found in its dependency list.");
        }
        if ( ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(x.m_type)) ) {
            require(x.m_type_declaration != nullptr,
                "Variable " + std::string(x.m_name) + " of type StructType must have a type declaration.");
        }
        // The declared type of a variable is what the backend asks for its
        // layout, and a procedure pointer names the procedure it points at.
        // Anything else is a symbol the backend cannot make a type from.
        if (x.m_type_declaration != nullptr) {
            ASR::symbol_t *decl = check_external
                ? ASRUtils::symbol_get_past_external(x.m_type_declaration)
                : x.m_type_declaration;
            require_id(decl != nullptr &&
                    (ASR::is_a<ASR::Struct_t>(*decl) ||
                     ASR::is_a<ASR::Enum_t>(*decl) ||
                     ASR::is_a<ASR::Union_t>(*decl) ||
                     ASR::is_a<ASR::Function_t>(*decl) ||
                     ASR::is_a<ASR::Variable_t>(*decl) ||
                     ASR::is_a<ASR::ExternalSymbol_t>(*decl)),
                "asr.verify.variable.type_declaration_is_type",
                "Variable '" + std::string(x.m_name) +
                "' declares its type with " +
                ASRUtils::symbol_type_name(*x.m_type_declaration) +
                ", which does not name a type or a procedure");
            // An unresolved ExternalSymbol says nothing about what it names,
            // so what it declares can only be checked once it resolves.
            bool declares_a_type = decl == nullptr ||
                ASR::is_a<ASR::ExternalSymbol_t>(*decl) ||
                ASR::is_a<ASR::Struct_t>(*decl) ||
                ASR::is_a<ASR::Enum_t>(*decl) ||
                ASR::is_a<ASR::Union_t>(*decl);
            bool needs_a_type = ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(x.m_type)) ||
                ASRUtils::is_class_type(ASRUtils::extract_type(x.m_type));
            require_id(!needs_a_type || declares_a_type,
                "asr.verify.variable.type_declaration_is_type",
                "Variable '" + std::string(x.m_name) +
                "' has a derived type but declares it with " +
                ASRUtils::symbol_type_name(*x.m_type_declaration));
            // Whatever scope the named symbol belongs to must still hold it.
            // A pass that drops a procedure, or the import of one, that it
            // thought unused leaves the variable naming a symbol no lookup
            // can reach any more.
            SymbolTable *owner =
                ASRUtils::symbol_parent_symtab(x.m_type_declaration);
            require_id(owner != nullptr &&
                    owner->get_symbol(std::string(ASRUtils::symbol_name(
                        x.m_type_declaration))) == x.m_type_declaration,
                "asr.verify.variable.type_declaration_resolves",
                "Variable '" + std::string(x.m_name) +
                "' declares its type with '" +
                std::string(ASRUtils::symbol_name(x.m_type_declaration)) +
                "', which its own scope no longer holds");
            // An abstract type exists to be extended, never to be an entity
            // of its own: it may have deferred bindings with no body, so a
            // non-polymorphic entity of that type has no dispatch target.
            if (decl != nullptr && ASR::is_a<ASR::Struct_t>(*decl) &&
                    ASR::down_cast<ASR::Struct_t>(decl)->m_is_abstract &&
                    !declares_unlimited_polymorphic(
                        ASR::down_cast<ASR::Struct_t>(decl))) {
                require_id(ASRUtils::is_class_type(
                        ASRUtils::extract_type(x.m_type)),
                    "asr.verify.variable.abstract_type_not_instantiated",
                    "Variable '" + std::string(x.m_name) +
                    "' has the abstract type '" +
                    std::string(ASR::down_cast<ASR::Struct_t>(decl)->m_name) +
                    "', which only a polymorphic entity may have");
            }
            require_id(
                symtab_in_scope(current_symtab, x.m_type_declaration),
                "asr.verify.variable.type_declaration_in_scope",
                "Variable '" + std::string(x.m_name) +
                "' declares its type with '" +
                std::string(ASRUtils::symbol_name(x.m_type_declaration)) +
                "', which is not in scope");
        }

        // Verify pass_attr and self_argument consistency
        bool is_proc_pointer = ASRUtils::is_symbol_procedure_variable(
            const_cast<ASR::symbol_t*>(&x.base));
        bool is_struct_member = is_struct;
        if (x.m_pass_attr == ASR::pass_attrType::Pass ||
            x.m_pass_attr == ASR::pass_attrType::NoPass) {
            require(is_proc_pointer && is_struct_member,
                "Variable '" + std::string(x.m_name) +
                "' has Pass/NoPass but is not a procedure pointer component of a struct.");
        }
        if (x.m_pass_attr == ASR::pass_attrType::NotMethod) {
            require(x.m_self_argument == nullptr,
                "Variable '" + std::string(x.m_name) +
                "' has pass_attr=NotMethod but self_argument is set.");
        }
        if (x.m_pass_attr == ASR::pass_attrType::NoPass) {
            require(x.m_self_argument == nullptr,
                "Variable '" + std::string(x.m_name) +
                "' has pass_attr=NoPass but self_argument is set.");
        }

        current_name = current_name_copy;
    }

    void visit_expr(const expr_t &b){
        const ASR::expr_t* expr_tmp = current_expr;
        current_expr = &b;
        BaseWalkVisitor<VerifyVisitor>::visit_expr(b);
        current_expr = expr_tmp;
    }
    
    void visit_ExternalSymbol(const ExternalSymbol_t &x) {
        if (check_external) {
            require(x.m_external != nullptr,
                "ExternalSymbol::m_external cannot be nullptr");
            require(!is_a<ExternalSymbol_t>(*x.m_external),
                "ExternalSymbol::m_external cannot be an ExternalSymbol");
            char *orig_name = symbol_name(x.m_external);
            require(std::string(x.m_original_name) == std::string(orig_name),
                "ExternalSymbol::m_original_name must match external->m_name");
            ASR::Module_t *m = ASRUtils::get_sym_module(x.m_external);
            ASR::Struct_t* sm = nullptr;
            ASR::Enum_t* em = nullptr;
            ASR::Union_t* um = nullptr;
            ASR::Function_t* fm = nullptr;
            ASR::Trait_t* tm = nullptr;
            ASR::TraitRuntimeContract_t* runtime_contract = nullptr;
            bool is_valid_owner = false;
            is_valid_owner = m != nullptr && ((ASR::symbol_t*) m == ASRUtils::get_asr_owner(x.m_external));
            std::string asr_owner_name = "";
            if( !is_valid_owner ) {
                ASR::symbol_t* asr_owner_sym = ASRUtils::get_asr_owner(x.m_external);
                // A symbol owned by the global scope, such as a program, has
                // no owning symbol at all. Nothing can import it, so reject it
                // here rather than dereferencing the null owner below.
                require_id(asr_owner_sym != nullptr,
                    "asr.verify.external_symbol.owner_is_importable",
                    "ExternalSymbol::m_external '" + std::string(x.m_name) +
                    "' is owned by the global scope, which cannot be imported "
                    "from");
                is_valid_owner = (ASR::is_a<ASR::Struct_t>(*asr_owner_sym) ||
                                  ASR::is_a<ASR::Enum_t>(*asr_owner_sym) ||
                                  ASR::is_a<ASR::Function_t>(*asr_owner_sym) ||
                                  ASR::is_a<ASR::Trait_t>(*asr_owner_sym) ||
                                  ASR::is_a<ASR::TraitRuntimeContract_t>(*asr_owner_sym) ||
                                  ((ASR::is_a<ASR::Template_t>(*asr_owner_sym) ||
                                    ASR::is_a<ASR::TraitErasure_t>(*asr_owner_sym) ||
                                    ASR::is_a<ASR::TraitWitness_t>(*asr_owner_sym)) &&
                                    m && x.n_scope_names > 0) ||
                                  ASR::is_a<ASR::Union_t>(*asr_owner_sym));
                if( ASR::is_a<ASR::Struct_t>(*asr_owner_sym) ) {
                    sm = ASR::down_cast<ASR::Struct_t>(asr_owner_sym);
                    asr_owner_name = sm->m_name;
                } else if( ASR::is_a<ASR::Enum_t>(*asr_owner_sym) ) {
                    em = ASR::down_cast<ASR::Enum_t>(asr_owner_sym);
                    asr_owner_name = em->m_name;
                } else if( ASR::is_a<ASR::Union_t>(*asr_owner_sym) ) {
                    um = ASR::down_cast<ASR::Union_t>(asr_owner_sym);
                    asr_owner_name = um->m_name;
                } else if( ASR::is_a<ASR::Function_t>(*asr_owner_sym) ) {
                    fm = ASR::down_cast<ASR::Function_t>(asr_owner_sym);
                    asr_owner_name = fm->m_name;
                } else if (ASR::is_a<ASR::Trait_t>(*asr_owner_sym)) {
                    tm = ASR::down_cast<ASR::Trait_t>(asr_owner_sym);
                    asr_owner_name = tm->m_name;
                } else if (ASR::is_a<ASR::TraitRuntimeContract_t>(*asr_owner_sym)) {
                    runtime_contract = ASR::down_cast<ASR::TraitRuntimeContract_t>(asr_owner_sym);
                    asr_owner_name = runtime_contract->m_name;
                } else if (ASR::is_a<ASR::Template_t>(*asr_owner_sym) ||
                        ASR::is_a<ASR::TraitErasure_t>(*asr_owner_sym) ||
                        ASR::is_a<ASR::TraitWitness_t>(*asr_owner_sym)) {
                    asr_owner_name = ASRUtils::symbol_name(asr_owner_sym);
                }
            } else {
                asr_owner_name = m->m_name;
            }
            std::string x_m_module_name = x.m_module_name;
            if( current_symtab->resolve_symbol(x.m_module_name) ) {
                x_m_module_name = ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(
                        current_symtab->resolve_symbol(x.m_module_name)));
            }
            require(is_valid_owner,
                "ExternalSymbol::m_external '" + std::string(x.m_name) + "' is not in a module or struct type, owner: " +
                x_m_module_name);
            // m_module_name can be either the direct owner or the
            // top-level module when scope_names provides the path.
            bool name_matches = (x_m_module_name == asr_owner_name);
            if (!name_matches && m != nullptr && x.n_scope_names > 0) {
                name_matches = (x_m_module_name == std::string(m->m_name));
            }
            // When the direct owner is a Struct, m_module_name refers
            // to the enclosing Module, not the Struct itself. Walk up
            // to the parent Module to verify the match.
            if (!name_matches && sm != nullptr) {
                ASR::symbol_t* struct_parent = ASRUtils::get_asr_owner((ASR::symbol_t*)sm);
                if (struct_parent != nullptr && ASR::is_a<ASR::Module_t>(*struct_parent)) {
                    ASR::Module_t* parent_mod = ASR::down_cast<ASR::Module_t>(struct_parent);
                    if (x_m_module_name == std::string(parent_mod->m_name)) {
                        name_matches = true;
                        m = parent_mod;
                    }
                }
            }
            require(name_matches,
                "ExternalSymbol::m_module_name `" + x_m_module_name
                + "` must match external's module name `" + asr_owner_name + "`");
            ASR::symbol_t *s = nullptr;
            if( m != nullptr && ((ASR::symbol_t*) m == ASRUtils::get_asr_owner(x.m_external)) ) {
                s = m->m_symtab->find_scoped_symbol(x.m_original_name, x.n_scope_names, x.m_scope_names);
            } else if( m != nullptr && x.n_scope_names > 0
                       && x_m_module_name == std::string(m->m_name) ) {
                // m_module_name refers to the top-level module and
                // scope_names encodes the path to the nested owner.
                s = m->m_symtab->find_scoped_symbol(x.m_original_name, x.n_scope_names, x.m_scope_names);
            } else if( sm ) {
                s = sm->m_symtab->resolve_symbol(std::string(x.m_original_name));
            } else if( em ) {
                s = em->m_symtab->resolve_symbol(std::string(x.m_original_name));
            } else if( fm ) {
                s = fm->m_symtab->resolve_symbol(std::string(x.m_original_name));
            } else if( um ) {
                s = um->m_symtab->resolve_symbol(std::string(x.m_original_name));
            } else if (tm) {
                s = tm->m_symtab->resolve_symbol(std::string(x.m_original_name));
            } else if (runtime_contract) {
                s = runtime_contract->m_symtab->resolve_symbol(std::string(x.m_original_name));
            }
            require(s != nullptr,
                "ExternalSymbol::m_original_name ('"
                + std::string(x.m_original_name)
                + "') + scope_names not found in a module '"
                + asr_owner_name + "'");
            require(s == x.m_external,
                std::string("ExternalSymbol::m_name + scope_names found but not equal to m_external, ") +
                "original_name " + std::string(x.m_original_name) + ".");
        }
    }

    // --------------------------------------------------------
    // nodes that have symbol in their fields:

    void visit_Var(const Var_t &x) {
        require(x.m_v != nullptr,
            "Var_t::m_v cannot be nullptr");
        std::string x_mv_name = ASRUtils::symbol_name(x.m_v);
        ASR::symbol_t *s = x.m_v;
        if (check_external) {
            s = ASRUtils::symbol_get_past_external(x.m_v);
        }

        // Allow any variable that is either external, is not defined in this scope,
        // or is not a function argument (e.g., COMMON variables used as dimension bounds)
        // to pass FunctionType verification.
        // When check_external is false (e.g. during modfile deserialization),
        // s is not dereferenced past ExternalSymbol, so we must also accept
        // ExternalSymbol directly — its target cannot be verified yet.
        if (is_a<ASR::ExternalSymbol_t>(*x.m_v)) {
            non_global_symbol_visited = false;
        } else if (is_a<ASR::Variable_t>(*s) &&
            (_is_return_type_string && !current_symtab->get_symbol(x_mv_name))) {
            non_global_symbol_visited = false;
        } else if (is_a<ASR::Variable_t>(*s) && current_symtab &&
                   ASR::is_a<ASR::symbol_t>(*current_symtab->asr_owner) &&
                   ASR::is_a<ASR::Function_t>(*(ASR::symbol_t*)current_symtab->asr_owner)) {
            // Check if this variable is a function argument — only those should
            // have been replaced by FunctionParam and thus trigger an error
            ASR::Function_t* func = ASR::down_cast2<ASR::Function_t>(current_symtab->asr_owner);
            bool is_arg = false;
            for (size_t i = 0; i < func->n_args; i++) {
                if (ASR::is_a<ASR::Var_t>(*func->m_args[i]) &&
                    ASR::down_cast<ASR::Var_t>(func->m_args[i])->m_v == x.m_v) {
                    is_arg = true;
                    break;
                }
            }
            non_global_symbol_visited = is_arg;
        } else {
            non_global_symbol_visited = true;
        }
        _is_return_type_string = false;

        require(is_a<Variable_t>(*s) || is_a<Function_t>(*s)
                || is_a<ASR::Enum_t>(*s) || is_a<ASR::ExternalSymbol_t>(*s) || is_a<ASR::Struct_t>(*s),
            "Var_t::m_v " + x_mv_name + " does not point to a Variable_t, " \
            "Function_t, or Enum_t (possibly behind ExternalSymbol_t)");
        require(symtab_in_scope(current_symtab, x.m_v),
            "Var::m_v `" + x_mv_name + "` cannot point outside of its symbol table");
        if ( x_mv_name != current_name ) {
            variable_dependencies.push_back(x_mv_name);
        }
    }

    void visit_ImplicitDeallocate(const ImplicitDeallocate_t &x) {
        // TODO: check that every allocated variable is deallocated.
        verify_trait_deallocation(x, true);
        BaseWalkVisitor::visit_ImplicitDeallocate(x);
    }

    template <typename T>
    void verify_trait_deallocation(const T &x, bool implicit = false) {
        std::vector<expr_t*> owners;
        for (size_t i = 0; i < x.n_vars; i++) {
            auto *association = ASRUtils::association_variable(x.m_vars[i]);
            require_id(!association || (association->m_intent != intentType::In &&
                    !ASR::is_a<Var_t>(*x.m_vars[i])),
                "asr.verify.association.deallocate",
                "A data association owns no storage and cannot deallocate a read-only subobject");
            auto *type = typed_expr_type(x.m_vars[i]);
            if (!type || !ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(type))) continue;
            verify_trait_owner(x.m_vars[i], x.base.base.loc);
            // A component is released while its definable containing object
            // takes a new value, such as an omitted constructor component.
            require_id(!implicit || ASR::is_a<StructInstanceMember_t>(*x.m_vars[i]) ||
                    (ASR::is_a<Var_t>(*x.m_vars[i]) &&
                     ASRUtils::EXPR2VAR(x.m_vars[i])->m_intent == intentType::Local),
                "asr.verify.trait_owner.caller_lifetime",
                "Implicit scope cleanup must not destroy a caller-owned dummy slot or result");
            for (auto *previous : owners) {
                require_id(!ASRUtils::trait_owner_same_slot(previous, x.m_vars[i]),
                    "asr.verify.trait_owner.duplicate_cleanup",
                    "One deallocation cannot destroy the same owner twice");
            }
            owners.push_back(x.m_vars[i]);
        }
    }

    void visit_ExplicitDeallocate(const ExplicitDeallocate_t &x) {
        verify_trait_deallocation(x);
        BaseWalkVisitor::visit_ExplicitDeallocate(x);
    }

    void check_var_external(const ASR::expr_t &x) {
        if (ASR::is_a<ASR::Var_t>(x)) {
            ASR::symbol_t *s = ((ASR::Var_t*)&x)->m_v;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*s)) {
                ASR::ExternalSymbol_t *e = ASR::down_cast<ASR::ExternalSymbol_t>(s);
                ASRUtils::require_impl(e->m_external, "m_external cannot be null here",
                        x.base.loc, diagnostics);
            }
        }
    }

    template <typename T>
    void handle_ArrayItemSection(const T &x) {
        visit_expr(*x.m_v);
        for (size_t i=0; i<x.n_args; i++) {
            if( x.m_args[i].m_step != nullptr ) {
                require_with_loc(x.m_args[i].m_left != nullptr &&
                                 x.m_args[i].m_right != nullptr,
                    "Sliced dimension should always have lower and "
                    "upper bounds present.", x.base.base.loc);
            }
            visit_array_index(x.m_args[i]);
        }
        require(x.m_type != nullptr,
            "ArrayItemSection::m_type cannot be nullptr");
        visit_ttype(*x.m_type);
        if (check_external) {
            check_var_external(*x.m_v);
            int n_dims = ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(x.m_v));
            if (ASR::is_a<ASR::String_t>(*x.m_type) && n_dims == 0) {
                // TODO: This seems like a bug, we should not use ArrayItem with
                // strings but StringItem. For now we ignore it, but we should
                // fix it
            } else {
                require(n_dims > 0,
                    "The variable in ArrayItem must be an array, not a scalar");
            }
        }
    }

    void visit_ArrayItem(const ArrayItem_t &x) {
        if( check_external ) {
            // Selecting an element of an array component of an array, as in
            // `w%u(2)`, reads one element of the component out of every
            // element of the base, so the reference is an array shaped like
            // that base even though every subscript is scalar.
            ASR::expr_t *shape_base = ASRUtils::struct_base_lending_shape(
                const_cast<ArrayItem_t*>(&x));
            if( ASRUtils::is_array_indexed_with_array_indices(x.m_args, x.n_args) ) {
                require(ASRUtils::is_array(x.m_type),
                    "ArrayItem::m_type with array indices must be an array.")
            } else if( shape_base != nullptr ) {
                size_t base_rank = ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(shape_base));
                require_id(ASRUtils::is_array(x.m_type),
                    "asr.verify.array_item.array_base",
                    "selecting an element of a component of an array is an "
                    "array, but its type is not an array");
                if (ASRUtils::is_array(x.m_type)) {
                    require_id(
                        (size_t) ASRUtils::extract_n_dims_from_ttype(x.m_type)
                            == base_rank,
                        "asr.verify.array_item.array_base_rank",
                        "selecting an element of a component of an array of "
                        "rank " + std::to_string(base_rank) + " has rank " +
                        std::to_string(
                            ASRUtils::extract_n_dims_from_ttype(x.m_type)));
                }
            } else {
                require(!ASRUtils::is_array(x.m_type),
                    "ArrayItem::m_type cannot be array.")
            }
        }
        // An ArrayItem carries the type of the element it selects, and the
        // backend stores through a pointer derived from that type. If it
        // disagrees with the array's own element type the store is malformed
        // and LLVM rejects the module it produces.
        ASR::ttype_t *array_type = typed_expr_type(x.m_v);
        if (!diagnostics.has_error() && array_type != nullptr
                && x.m_type != nullptr) {
            ASR::ttype_t *element = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable_pointer(array_type));
            ASR::ttype_t *declared = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable_pointer(x.m_type));
            if (!is_struct_like_type(element) && !is_procedure_type(element)
                    && !is_struct_like_type(declared)
                    && !is_procedure_type(declared)) {
                require_id(
                    ASRUtils::check_equal_type(
                        element, declared, nullptr, nullptr),
                    "asr.verify.array_item.type_matches_element",
                    "ArrayItem type " + ASRUtils::get_type_code(declared) +
                        " does not match array element type " +
                        ASRUtils::get_type_code(element));
            }
        }
        handle_ArrayItemSection(x);
    }

    void visit_CoarrayRef(const CoarrayRef_t &x) {
        if (check_external) {
            for (size_t i = 0; i < x.n_coindices; i++) {
                ASR::coarray_index_t ci = x.m_coindices[i];
                if (ci.m_star == ASR::codimension_typeType::CodimensionStar) {
                    require(ci.m_index == nullptr, "coarray_index_t with star must have nullptr index");
                    require(i == x.n_coindices-1, "coarray_index_t with star may only appear in the final codimension");
                } else {
                    require(ci.m_index != nullptr, "coarray_index_t without star must have a valid index");
                }
            }
        }
        BaseWalkVisitor<VerifyVisitor>::visit_CoarrayRef(x);
    }

    void visit_ArraySize(const ArraySize_t& x) {
        if (check_external) {
            require(ASRUtils::is_array(ASRUtils::expr_type(x.m_v)),
                "ArraySize::m_v must be an array");
        }
        verify_dimension_argument("ArraySize", x.m_v, x.m_dim,
            x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_ArraySize(x);
    }

    void visit_DebugCheckArrayBounds(const ASR::DebugCheckArrayBounds_t& x) {
        if (check_external) {
            require(ASRUtils::is_array(ASRUtils::expr_type(x.m_target)), "DebugCheckArrayBounds::m_target must have an Array type");

            require(x.n_components > 0, "DebugCheckArrayBounds::n_components should be greater than 0");
            for (size_t i = 0; i < x.n_components; i++) {
                require(ASR::is_a<ASR::Var_t>(*x.m_components[i]) ||
                        ASR::is_a<ASR::ArrayPhysicalCast_t>(*x.m_components[i]) ||
                        ASR::is_a<ASR::StructInstanceMember_t>(*x.m_components[i]) ||
                        ASR::is_a<ASR::BitCast_t>(*x.m_components[i]) ||
                        ASR::is_a<ASR::ArrayConstant_t>(*x.m_components[i]), "DebugCheckArrayBounds::m_components element must be Var, ArrayPhysicalCast, StructInstanceMember, BitCast, or ArrayConstant");

                require(ASRUtils::is_array(ASRUtils::expr_type(x.m_components[i])), "DebugCheckArrayBounds::m_components element must have an Array type");
            }
        }
        BaseWalkVisitor<VerifyVisitor>::visit_DebugCheckArrayBounds(x);
    }

    void visit_ArraySection(const ArraySection_t &x) {
        require(
            ASR::is_a<ASR::Array_t>(*x.m_type),
            "ArrayItemSection::m_type can only be an Array"
        );
        handle_ArrayItemSection(x);
    }

    // Get the Struct symbol from a dt expression (for method calls).
    // Returns nullptr if the struct cannot be determined.
    ASR::symbol_t* get_struct_from_dt_expr(ASR::expr_t* dt) {
        ASR::ttype_t* dt_type = ASRUtils::expr_type(dt);
        dt_type = ASRUtils::type_get_past_pointer(dt_type);
        dt_type = ASRUtils::type_get_past_allocatable(dt_type);
        if (ASR::is_a<ASR::Array_t>(*dt_type)) {
            dt_type = ASR::down_cast<ASR::Array_t>(dt_type)->m_type;
        }
        if (!ASR::is_a<ASR::StructType_t>(*dt_type)) {
            return nullptr;
        }
        // StructType doesn't directly reference the Struct symbol.
        // Get it from the variable's type_declaration.
        if (ASR::is_a<ASR::Var_t>(*dt)) {
            ASR::symbol_t* v = ASR::down_cast<ASR::Var_t>(dt)->m_v;
            v = ASRUtils::symbol_get_past_external(v);
            if (ASR::is_a<ASR::Variable_t>(*v)) {
                ASR::symbol_t* decl = ASR::down_cast<ASR::Variable_t>(v)->m_type_declaration;
                if (decl) return ASRUtils::symbol_get_past_external(decl);
            }
        } else if (ASR::is_a<ASR::StructInstanceMember_t>(*dt)) {
            ASR::StructInstanceMember_t* sim = ASR::down_cast<ASR::StructInstanceMember_t>(dt);
            ASR::symbol_t* m = ASRUtils::symbol_get_past_external(sim->m_m);
            if (ASR::is_a<ASR::Variable_t>(*m)) {
                ASR::symbol_t* decl = ASR::down_cast<ASR::Variable_t>(m)->m_type_declaration;
                if (decl) return ASRUtils::symbol_get_past_external(decl);
            }
        }
        return nullptr;
    }

    // Check if method_name exists in the struct's symtab (walking parent chain).
    bool struct_has_member(ASR::Struct_t* struct_type, const std::string& method_name) {
        ASR::Struct_t* current = struct_type;
        std::set<ASR::Struct_t*> seen;
        while (current) {
            if (!seen.insert(current).second) {
                break;
            }
            if (current->m_symtab->get_symbol(method_name) != nullptr) {
                return true;
            }
            if (current->m_parent) {
                ASR::symbol_t* parent = ASRUtils::symbol_get_past_external(current->m_parent);
                if (ASR::is_a<ASR::Struct_t>(*parent)) {
                    current = ASR::down_cast<ASR::Struct_t>(parent);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return false;
    }

    // True when `candidate` is `ancestor` or extends it.
    static bool struct_is_or_extends(ASR::Struct_t *candidate,
            ASR::Struct_t *ancestor) {
        std::set<ASR::Struct_t*> seen;
        while (candidate != nullptr) {
            if (candidate == ancestor) return true;
            if (!seen.insert(candidate).second) return false;
            ASR::symbol_t *parent = candidate->m_parent == nullptr ? nullptr
                : ASRUtils::symbol_get_past_external(candidate->m_parent);
            candidate = (parent != nullptr && ASR::is_a<ASR::Struct_t>(*parent))
                ? ASR::down_cast<ASR::Struct_t>(parent) : nullptr;
        }
        return false;
    }

    // The derived type an expression was declared with, or nullptr when it
    // was not declared with one.
    static ASR::Struct_t* declared_struct(ASR::expr_t *e) {
        if (e == nullptr) return nullptr;
        ASR::symbol_t *sym = nullptr;
        if (ASR::is_a<ASR::Var_t>(*e)) {
            sym = ASR::down_cast<ASR::Var_t>(e)->m_v;
        } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            sym = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_m;
        }
        if (sym == nullptr) return nullptr;
        sym = ASRUtils::symbol_get_past_external(sym);
        if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
        ASR::symbol_t *decl =
            ASR::down_cast<ASR::Variable_t>(sym)->m_type_declaration;
        if (decl == nullptr) return nullptr;
        decl = ASRUtils::symbol_get_past_external(decl);
        if (decl == nullptr || !ASR::is_a<ASR::Struct_t>(*decl)) return nullptr;
        return ASR::down_cast<ASR::Struct_t>(decl);
    }

    // A dynamic type is reachable through a declared one only if it is that
    // type or extends it. Unknown on either side means no opinion.
    bool dynamic_type_is_compatible(ASR::symbol_t *dynamic,
            ASR::expr_t *declared_by) {
        if (!check_external || dynamic == nullptr) return true;
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(dynamic);
        if (sym == nullptr || !ASR::is_a<ASR::Struct_t>(*sym)) return true;
        ASR::Struct_t *declared = declared_struct(declared_by);
        if (declared == nullptr) return true;
        // An unlimited polymorphic entity may hold any type at all, so no
        // guard or dynamic type is out of reach through it.
        if (declares_unlimited_polymorphic(declared)) return true;
        return struct_is_or_extends(
            ASR::down_cast<ASR::Struct_t>(sym), declared);
    }

    // True when `name` is a type in the parent chain of `struct_type`, which
    // is what an implicit parent component is named after.
    bool struct_extends(ASR::Struct_t *struct_type, const std::string &name) {
        ASR::symbol_t *parent = struct_type->m_parent;
        std::set<ASR::Struct_t*> seen;
        while (parent != nullptr) {
            parent = ASRUtils::symbol_get_past_external(parent);
            if (parent == nullptr || !ASR::is_a<ASR::Struct_t>(*parent)) {
                return false;
            }
            ASR::Struct_t *s = ASR::down_cast<ASR::Struct_t>(parent);
            if (!seen.insert(s).second) return false;
            if (name == std::string(s->m_name)) return true;
            parent = s->m_parent;
        }
        return false;
    }

    // Verify that the method being called is actually a member of the struct
    // that dt points to.
    template <typename T>
    void verify_dt_member(const T& x) {
        ASR::symbol_t* struct_sym = get_struct_from_dt_expr(x.m_dt);
        if (!struct_sym) return;
        if (!ASR::is_a<ASR::Struct_t>(*struct_sym)) return;
        ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(struct_sym);

        // Get the method name as it appears in the struct's symtab.
        // x.m_name may be an ExternalSymbol; we need the original name
        // in the struct's scope.
        std::string method_name;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name)) {
            ASR::ExternalSymbol_t* ext = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name);
            method_name = ext->m_original_name;
        } else {
            method_name = ASRUtils::symbol_name(x.m_name);
        }

        require(struct_has_member(struct_type, method_name),
            "Method '" + method_name + "' not found in struct '" +
            std::string(struct_type->m_name) + "' (or its parents).");
    }

    // A component reference names a component of the type it is read from.
    // One that names a component of some other type sends the backend
    // looking for a field the type does not have.
    void visit_StructInstanceMember(const StructInstanceMember_t &x) {
        BaseWalkVisitor<VerifyVisitor>::visit_StructInstanceMember(x);
        if (x.m_m == nullptr || x.m_v == nullptr || diagnostics.has_error()) {
            return;
        }
        verify_struct_member_shape(x);
        if (!check_external || diagnostics.has_error()) {
            return;
        }
        ASR::symbol_t *struct_sym = get_struct_from_dt_expr(x.m_v);
        if (struct_sym == nullptr || !ASR::is_a<ASR::Struct_t>(*struct_sym)) {
            return;
        }
        std::string member_name = ASR::is_a<ASR::ExternalSymbol_t>(*x.m_m)
            ? std::string(ASR::down_cast<ASR::ExternalSymbol_t>(
                  x.m_m)->m_original_name)
            : std::string(ASRUtils::symbol_name(x.m_m));
        ASR::Struct_t *struct_type = ASR::down_cast<ASR::Struct_t>(struct_sym);
        // An extended type has an implicit parent component named after the
        // type it extends, and that component is the parent type's symbol
        // rather than an entry in this type's scope.
        require_id(struct_has_member(struct_type, member_name) ||
                struct_extends(struct_type, member_name),
            "asr.verify.struct_member.belongs_to_struct",
            "'" + std::string(struct_type->m_name) +
            "' has no member named '" + member_name + "'");
    }

    // Reading a scalar component of an array base yields an array of the
    // base's shape. A reference left with the component's scalar declared
    // type is malformed ASR that survives semantics and only fails much
    // later, deep inside a pass or the backend (issue #13296).
    void verify_struct_member_shape(const StructInstanceMember_t &x) {
        ASR::symbol_t *member_sym = ASRUtils::symbol_get_past_external(x.m_m);
        if (member_sym == nullptr || !ASR::is_a<ASR::Variable_t>(*member_sym)) {
            return;
        }
        ASR::ttype_t *member_type =
            ASR::down_cast<ASR::Variable_t>(member_sym)->m_type;
        ASR::ttype_t *base_type = ASRUtils::expr_type(x.m_v);
        if (member_type == nullptr || base_type == nullptr ||
                x.m_type == nullptr) {
            return;
        }
        if (!ASRUtils::is_array(base_type) || ASRUtils::is_array(member_type)) {
            return;
        }
        // A `pointer` or `allocatable` component read from an array base
        // denotes an array of indirections, which this type representation
        // cannot express. Fortran forbids such a reference (C919) and
        // LFortran does not diagnose it yet, so the scalar declared type is
        // what survives semantics. Do not claim it is malformed until there
        // is a type that could replace it.
        if (ASR::is_a<ASR::Allocatable_t>(*member_type) ||
                ASR::is_a<ASR::Pointer_t>(*member_type)) {
            return;
        }
        // A zero-size base has no element to read, so the reference denotes
        // nothing and its shape is not observable. A scalar structure
        // constructor for such a component is deliberately left unspread
        // for that reason, which leaves the component's own scalar type in
        // place. That is degenerate, not malformed.
        if (ASRUtils::get_fixed_size_of_array(base_type) == 0) {
            return;
        }
        require_id(ASRUtils::is_array(x.m_type),
            "asr.verify.struct_member.array_base",
            "reading component '" + std::string(ASRUtils::symbol_name(x.m_m)) +
            "' of an array is an array, but its type is not an array");
        if (!ASRUtils::is_array(x.m_type)) {
            return;
        }
        require_id(ASRUtils::extract_n_dims_from_ttype(x.m_type) ==
                ASRUtils::extract_n_dims_from_ttype(base_type),
            "asr.verify.struct_member.array_base_rank",
            "reading component '" + std::string(ASRUtils::symbol_name(x.m_m)) +
            "' of an array of rank " +
            std::to_string(ASRUtils::extract_n_dims_from_ttype(base_type)) +
            " has rank " +
            std::to_string(ASRUtils::extract_n_dims_from_ttype(x.m_type)));
    }

    static ASR::FunctionType_t* as_procedure_type(ASR::ttype_t *t) {
        if (t == nullptr) return nullptr;
        ASR::ttype_t *t2 = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(t));
        if (!ASR::is_a<ASR::FunctionType_t>(*t2)) return nullptr;
        return ASR::down_cast<ASR::FunctionType_t>(t2);
    }

    // A dummy procedure declares the interface the caller must satisfy. The
    // actual procedure is called through that interface, so a disagreement
    // is an indirect call with the wrong signature -- the one thing a
    // dummy procedure exists to rule out.
    void verify_procedure_interface(ASR::ttype_t *actual_type,
            ASR::ttype_t *formal_type, const std::string &which,
            const Location &loc) {
        ASR::FunctionType_t *actual = as_procedure_type(actual_type);
        ASR::FunctionType_t *formal = as_procedure_type(formal_type);
        if (actual == nullptr || formal == nullptr) return;
        // An ImplicitInterface declaration constrains nothing: its argument
        // list is unknown. `procedure()` and `procedure(), pointer` still
        // have empty arg_types and deftype Interface, so they also skip until
        // they have their own ASR state. A genuine zero-argument Interface
        // shares that empty shape and still skips for the same reason.
        auto unconstrained = [](ASR::FunctionType_t *t) {
            return t->m_deftype == ASR::deftypeType::ImplicitInterface
                || t->n_arg_types == 0;
        };
        if (unconstrained(actual) || unconstrained(formal)) return;
        require_with_loc_id(
            (actual->m_return_var_type == nullptr) ==
                (formal->m_return_var_type == nullptr),
            "asr.verify.call.procedure_argument_matches_formal",
            which + " must be a " + std::string(
                formal->m_return_var_type == nullptr
                    ? "subroutine" : "function"),
            loc);
        require_with_loc_id(actual->n_arg_types == formal->n_arg_types,
            "asr.verify.call.procedure_argument_matches_formal",
            which + " must take " + std::to_string(formal->n_arg_types) +
            " arguments, not " + std::to_string(actual->n_arg_types), loc);
        for (size_t i = 0; i < actual->n_arg_types; i++) {
            ASR::ttype_t *a = actual->m_arg_types[i];
            ASR::ttype_t *f = formal->m_arg_types[i];
            if (is_struct_like_type(a) || is_struct_like_type(f) ||
                    is_procedure_type(a) || is_procedure_type(f)) {
                continue;
            }
            require_with_loc_id(
                ASRUtils::check_equal_type(a, f, nullptr, nullptr),
                "asr.verify.call.procedure_argument_matches_formal",
                which + " argument " + std::to_string(i + 1) +
                " must have type " + ASRUtils::get_type_code(f) + ", not " +
                ASRUtils::get_type_code(a), loc);
        }
        if (actual->m_return_var_type != nullptr &&
                formal->m_return_var_type != nullptr &&
                !is_struct_like_type(actual->m_return_var_type) &&
                !is_struct_like_type(formal->m_return_var_type)) {
            require_with_loc_id(
                ASRUtils::check_equal_type(actual->m_return_var_type,
                    formal->m_return_var_type, nullptr, nullptr),
                "asr.verify.call.procedure_argument_matches_formal",
                which + " must return " +
                ASRUtils::get_type_code(formal->m_return_var_type) +
                ", not " +
                ASRUtils::get_type_code(actual->m_return_var_type), loc);
        }
    }

    template <typename T>
    void verify_args(const T& x) {
        ASR::symbol_t* func_sym = ASRUtils::symbol_get_past_external(x.m_name);
        ASR::Function_t* func = nullptr;
        bool is_method = (x.m_dt != nullptr);
        bool nopass = false;
        if (func_sym && ASR::is_a<ASR::StructMethodDeclaration_t>(*func_sym)) {
            ASR::StructMethodDeclaration_t* method = ASR::down_cast<ASR::StructMethodDeclaration_t>(func_sym);
            require(is_method,
                "StructMethodDeclaration '" + std::string(method->m_name) +
                "' called without dt (not as a method).");
            ASR::symbol_t *proc = check_external
                ? ASRUtils::symbol_get_past_external(method->m_proc) : method->m_proc;
            if (proc && ASR::is_a<ASR::Function_t>(*proc)) {
                func = ASR::down_cast<ASR::Function_t>(proc);
                nopass = method->m_is_nopass;
                size_t self = ASRUtils::passed_object_index(*method, func);
                if (!nopass && self < func->n_args && self < x.n_args &&
                        x.m_args[self].m_value) {
                    auto *formal = ASRUtils::expr_to_variable_or_null(func->m_args[self]);
                    auto *type_symbol = formal ? ASRUtils::symbol_get_past_external(
                        formal->m_type_declaration) : nullptr;
                    if (formal && type_symbol && ASR::is_a<Struct_t>(*type_symbol) &&
                            ASR::down_cast<Struct_t>(type_symbol)->m_is_sealed &&
                            ASR::is_a<StructType_t>(*formal->m_type) &&
                            !ASRUtils::is_class_type(formal->m_type)) {
                        auto *actual_type = typed_expr_type(x.m_args[self].m_value);
                        require_id(actual_type &&
                                !ASRUtils::is_class_type(ASRUtils::extract_type(actual_type)),
                            "asr.verify.call.sealed_receiver",
                            "A sealed nonpolymorphic passed object requires an explicit concrete view");
                    }
                }
            }
        } else if (func_sym && ASR::is_a<ASR::Function_t>(*func_sym)) {
            func = ASR::down_cast<ASR::Function_t>(func_sym);
        } else if (func_sym && ASR::is_a<ASR::Variable_t>(*func_sym)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(func_sym);
            if (var->m_type_declaration) {
                auto *interface_symbol = ASRUtils::symbol_get_past_external(var->m_type_declaration);
                // Module loading verifies local structure before resolving imports.
                if (check_external || interface_symbol) {
                    require_id(interface_symbol && ASR::is_a<Function_t>(*interface_symbol),
                        "asr.verify.call.procedure_interface",
                        "A procedure variable call requires a declared function interface");
                    auto *declared = ASR::down_cast<Function_t>(interface_symbol);
                    if (!ASRUtils::is_bare_implicit_interface(*declared)) func = declared;
                }
            }
            if (is_method) {
                require(var->m_pass_attr != ASR::pass_attrType::NotMethod,
                    "Call with dt!=nullptr targets Variable '" +
                    std::string(var->m_name) +
                    "' with pass_attr=NotMethod.");
                nopass = (var->m_pass_attr == ASR::pass_attrType::NoPass);
            } else {
                require(var->m_pass_attr == ASR::pass_attrType::NotMethod,
                    "Variable '" + std::string(var->m_name) +
                    "' with pass_attr=Pass/NoPass called without dt (not as a method).");
            }
        }

        // Verify that a method call's target is actually a member of the
        // struct that dt points to.
        if (is_method && check_external) {
            verify_dt_member(x);
        }

        // Verify self argument is explicit for method calls with PASS
        if (is_method && !nopass && func) {
            require(x.n_args > 0 && x.m_args[0].m_value != nullptr,
                "Method call with PASS must have self as args[0].");
        }

        if (func) {
            require(x.n_args <= func->n_args,
                "More actual arguments than formal arguments in call. "
                "call n_args=" + std::to_string(x.n_args) +
                " func n_args=" + std::to_string(func->n_args) +
                " func=" + std::string(func->m_name));

            for (size_t i = 0; i < x.n_args; i++) {
                require(i < func->n_args,
                    "More actual arguments than formal arguments in call.");
                require(ASR::is_a<ASR::Var_t>(*func->m_args[i]),
                    "Function argument must be a Var.");
                ASR::symbol_t* arg_sym = ASR::down_cast<ASR::Var_t>(func->m_args[i])->m_v;
                if (!ASR::is_a<ASR::Variable_t>(*arg_sym)) {
                    continue;
                }
                ASR::Variable_t* callee_param = ASR::down_cast<ASR::Variable_t>(arg_sym);
                auto *actual = x.m_args[i].m_value;
                auto *association = ASRUtils::association_variable(actual);
                require_id(!association || association->m_intent != intentType::In ||
                        (callee_param->m_intent != intentType::Out &&
                         callee_param->m_intent != intentType::InOut),
                    "asr.verify.association.definable",
                    "A read-only data association cannot be a defining actual argument");
                require_id(!association || !ASR::is_a<Var_t>(*actual) ||
                        (!ASRUtils::is_allocatable(callee_param->m_type) &&
                         (!ASRUtils::is_pointer(callee_param->m_type) ||
                          callee_param->m_intent == intentType::In)),
                    "asr.verify.association.actual_attributes",
                    "A data association is neither an allocation slot nor a defining pointer slot");

                // Skip detailed checks for self argument (args[0] in method calls)
                if (i == 0 && is_method && !nopass) {
                    continue;
                }

                ASR::expr_t* passed_arg_expr = x.m_args[i].m_value;

                if (passed_arg_expr == nullptr) {
                    if (callee_param->m_presence != ASR::presenceType::Optional) {
                        require(false, "Required argument " +
                                    std::string(callee_param->m_name) +
                                    " cannot be nullptr.");
                    }
                    continue;
                }

                ASR::ttype_t *actual_type =
                    typed_expr_type(passed_arg_expr);
                ASR::ttype_t *formal_type = callee_param->m_type;
                if (actual_type && (ASR::is_a<TraitObjectType_t>(
                        *ASRUtils::extract_type(formal_type)) ||
                        ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(actual_type)))) {
                    bool slot = ASRUtils::is_trait_owner(formal_type);
                    bool pointer = ASRUtils::is_trait_pointer(formal_type);
                    bool pointer_actual = pointer &&
                        ((ASRUtils::is_trait_pointer(actual_type) &&
                          (ASR::is_a<Var_t>(*passed_arg_expr) ||
                           (callee_param->m_intent == intentType::In &&
                            (ASR::is_a<PointerNullConstant_t>(*passed_arg_expr) ||
                             ASR::is_a<TraitProject_t>(*passed_arg_expr))))) ||
                         (callee_param->m_intent == intentType::In &&
                          ASR::is_a<TraitObjectType_t>(*actual_type) &&
                          ASRUtils::is_valid_pointer_assignment_target(passed_arg_expr)));
                    require_with_loc_id(
                        ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(formal_type)) &&
                        ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(actual_type)) &&
                        (slot ? ASRUtils::is_trait_owner(actual_type) &&
                            (check_external ? ASRUtils::trait_owner_variable(passed_arg_expr) != nullptr
                                : ASR::is_a<Var_t>(*passed_arg_expr) ||
                                  ASR::is_a<StructInstanceMember_t>(*passed_arg_expr))
                            : pointer ? pointer_actual
                            : ASR::is_a<TraitObjectType_t>(*actual_type)) &&
                        ASRUtils::check_equal_type(formal_type, actual_type,
                            func->m_args[i], passed_arg_expr),
                        "asr.verify.trait_owner.argument",
                        "Trait arguments require the same declared contract and explicit slot or borrow association",
                        passed_arg_expr->base.loc);
                    if (slot) {
                        verify_trait_owner(passed_arg_expr, passed_arg_expr->base.loc,
                            callee_param->m_intent != intentType::In);
                    }
                    if (pointer && callee_param->m_intent != intentType::In) {
                        verify_trait_pointer(passed_arg_expr, passed_arg_expr->base.loc, true);
                    }
                }
                // Derived type arguments are skipped for the same reason
                // as in the signature check above, and this also covers a
                // polymorphic argument passed as a pointer or allocatable,
                // whose type only looks like a plain struct once those
                // wrappers are stripped.
                bool struct_argument = is_struct_like_type(actual_type)
                    || is_struct_like_type(formal_type);
                bool procedure_argument = is_procedure_type(actual_type)
                    || is_procedure_type(formal_type);
                if (procedure_argument) {
                    verify_procedure_interface(
                        actual_type, formal_type,
                        "Procedure argument '" +
                        std::string(callee_param->m_name) + "'",
                        passed_arg_expr->base.loc);
                }
                // A type-bound call is checked like any other: only its
                // passed-object dummy argument is special, and the loop has
                // already skipped that one.
                if (actual_type && !diagnostics.has_error() &&
                        !ASRUtils::is_intrinsic_symbol(x.m_name) &&
                        !struct_argument &&
                        !procedure_argument) {
                    // These three describe how the implementation receives
                    // its arguments. With --implicit-interface the frontend
                    // infers an interface from one call site rather than
                    // reading a declared one, and ASR cannot yet tell the two
                    // apart, so they are only applied where the procedure
                    // itself is in hand.
                    bool callee_is_defined =
                        ASRUtils::get_FunctionType(func)->m_deftype ==
                            ASR::deftypeType::Implementation;
                    // check_equal_type strips Allocatable and Pointer.
                    // An allocatable or pointer dummy requires an actual
                    // of the same wrapper; the other direction is valid
                    // Fortran (an allocatable actual may be passed to a
                    // nonallocatable dummy). A scalar actual for an array
                    // dummy (or the converse) is invalid, except for
                    // assumed-rank and elemental. Sequence association can
                    // pass a 2-D actual to a 1-D dummy, so ranks of two
                    // arrays need not match.
                    if (callee_is_defined &&
                            ASRUtils::is_allocatable(formal_type)) {
                        require_with_loc_id(
                            ASRUtils::is_allocatable(actual_type),
                            "asr.verify.call.actual_allocatable_matches_formal",
                            "Actual argument type " +
                                ASRUtils::get_type_code(actual_type) +
                                " is not allocatable, but the dummy is " +
                                ASRUtils::get_type_code(formal_type),
                            passed_arg_expr->base.loc);
                    }
                    // A pointer dummy takes a pointer actual, except when it
                    // is INTENT(IN): that one may also take any valid target
                    // for it, and becomes associated with the actual.
                    if (callee_is_defined &&
                            ASRUtils::is_pointer(formal_type) &&
                            callee_param->m_intent != ASR::intentType::In) {
                        require_with_loc_id(
                            ASRUtils::is_pointer(actual_type),
                            "asr.verify.call.actual_pointer_matches_formal",
                            "Actual argument type " +
                                ASRUtils::get_type_code(actual_type) +
                                " is not a pointer, but the dummy is " +
                                ASRUtils::get_type_code(formal_type),
                            passed_arg_expr->base.loc);
                    }
                    bool formal_assumed_rank = ASRUtils::is_array(formal_type)
                        && ASRUtils::extract_physical_type(formal_type)
                            == ASR::array_physical_typeType::AssumedRankArray;
                    bool elemental = ASRUtils::get_FunctionType(func)
                        ->m_elemental;
                    if (callee_is_defined && !formal_assumed_rank &&
                            !elemental) {
                        bool actual_is_array =
                            ASRUtils::is_array(actual_type);
                        bool formal_is_array =
                            ASRUtils::is_array(formal_type);
                        // Sequence association: an explicit-shape or
                        // assumed-size dummy may be given an array element,
                        // which is a scalar, and then covers the actual's
                        // array from that element on. No other dummy may:
                        // an assumed-shape one takes its extents from the
                        // actual, and an allocatable or pointer one carries
                        // the actual's own storage.
                        bool formal_takes_element = false;
                        if (formal_is_array && !actual_is_array &&
                                !ASRUtils::is_allocatable(formal_type) &&
                                !ASRUtils::is_pointer(formal_type)) {
                            ASR::Array_t *formal_array =
                                ASR::down_cast<ASR::Array_t>(formal_type);
                            // Assumed size: the last extent is the caller's.
                            formal_takes_element =
                                formal_array->m_physical_type ==
                                    ASR::array_physical_typeType::PointerArray ||
                                formal_array->m_physical_type ==
                                    ASR::array_physical_typeType::UnboundedPointerArray;
                            for (size_t d = 0; d < formal_array->n_dims; d++) {
                                // Explicit shape: the dummy states its own.
                                if (formal_array->m_dims[d].m_length
                                        != nullptr) {
                                    formal_takes_element = true;
                                    break;
                                }
                            }
                        }
                        require_with_loc_id(
                            actual_is_array == formal_is_array ||
                                formal_takes_element,
                            "asr.verify.call.actual_rank_matches_formal",
                            "Actual argument type " +
                                ASRUtils::get_type_code(actual_type) +
                                " does not match formal argument rank of "
                                "type " +
                                ASRUtils::get_type_code(formal_type),
                            passed_arg_expr->base.loc);
                    }
                    require_with_loc_id(
                        ASRUtils::check_equal_type(
                            actual_type, formal_type,
                            type_context(passed_arg_expr),
                            type_context(func->m_args[i])),
                        "asr.verify.call.actual_type_matches_formal",
                        "Actual argument type " +
                            ASRUtils::get_type_code(actual_type) +
                            " does not match formal argument type " +
                            ASRUtils::get_type_code(formal_type),
                        passed_arg_expr->base.loc);
                }

                if (check_external &&
                    !ASR::is_a<ASR::FunctionType_t>(*callee_param->m_type) &&
                    (callee_param->m_intent == ASR::intentType::Out ||
                     callee_param->m_intent == ASR::intentType::InOut)) {
                    require_with_loc(ASRUtils::is_modifiable_actual_argument_expr(passed_arg_expr),
                        "Non-variable expression in variable definition context "
                        "(actual argument to INTENT = OUT/INOUT)",
                        passed_arg_expr->base.loc);

                    if (ASR::is_a<ASR::Var_t>(*passed_arg_expr)) {
                        ASR::symbol_t* passed_sym = ASR::down_cast<ASR::Var_t>(passed_arg_expr)->m_v;
                        if (ASR::is_a<ASR::Variable_t>(*passed_sym)) {
                            ASR::Variable_t* passed_var = ASR::down_cast<ASR::Variable_t>(passed_sym);
                            require_with_loc(
                                passed_var->m_intent != ASR::intentType::In,
                                "Argument `" + std::string(passed_var->m_name) +
                                "` with intent(in) passed to a dummy argument with modifying intent",
                                passed_arg_expr->base.loc
                            );
                        }
                    }
                }
            }

            for (size_t i = x.n_args; i < func->n_args; i++) {
                require(ASR::is_a<ASR::Var_t>(*func->m_args[i]),
                    "Function argument must be a Var.");
                ASR::symbol_t* arg_sym = ASR::down_cast<ASR::Var_t>(func->m_args[i])->m_v;
                if (ASR::is_a<ASR::Variable_t>(*arg_sym)) {
                    ASR::Variable_t* callee_param = ASR::down_cast<ASR::Variable_t>(arg_sym);
                    if (callee_param->m_presence != ASR::presenceType::Optional) {
                        require(false, "Required argument " +
                                    std::string(callee_param->m_name) +
                                    " cannot be nullptr.");
                    }
                }
            }
        }

        bool _inside_call_copy = _inside_call;
        _inside_call = true;
        for (size_t i=0; i<x.n_args; i++) {
            if( x.m_args[i].m_value ) {
                visit_expr(*(x.m_args[i].m_value));
            }
        }
        _inside_call = _inside_call_copy;
    }

    void visit_ArrayPhysicalCast(const ASR::ArrayPhysicalCast_t& x) {
        BaseWalkVisitor<VerifyVisitor>::visit_ArrayPhysicalCast(x);
        if( x.m_old != ASR::array_physical_typeType::DescriptorArray ) {
            require(x.m_new != x.m_old, "ArrayPhysicalCast is redundant, "
                "the old physical type and new physical type must be different.");
        }
        if(check_external){
            // For rank(0): AssumedRankArray → scalar, m_type is scalar so skip physical type check
            bool is_rank0_scalar = (x.m_old == ASR::array_physical_typeType::AssumedRankArray
                                    && !ASRUtils::is_array(x.m_type));
            if (!is_rank0_scalar) {
                require(x.m_new == ASRUtils::extract_physical_type(x.m_type),
                    "Destination physical type conflicts with the physical type of target");
            }
            require(x.m_old == ASRUtils::extract_physical_type(ASRUtils::expr_type(x.m_arg)),
                "Old physical type conflicts with the physical type of argument " + std::to_string(x.m_old)
                + " " + std::to_string(ASRUtils::extract_physical_type(ASRUtils::expr_type(x.m_arg))));
            bool _inside_array_physical_cast_type_copy = _inside_array_physical_cast_type;
            _inside_array_physical_cast_type = true;
            bool _processing_assumed_rank_array_copy = _processing_assumed_rank_array;
            bool _processing_unbounded_pointer_array_copy = _processing_unbounded_pointer_array;
            if (x.m_old == ASR::array_physical_typeType::AssumedRankArray) {
                _processing_assumed_rank_array = true;
            }
            if (x.m_old == ASR::array_physical_typeType::UnboundedPointerArray) {
                _processing_unbounded_pointer_array = true;
            }
            visit_ttype(*x.m_type);
            _processing_assumed_rank_array = _processing_assumed_rank_array_copy;
            _processing_unbounded_pointer_array = _processing_unbounded_pointer_array_copy;
            _inside_array_physical_cast_type = _inside_array_physical_cast_type_copy;
        }
    }

    // A launch and the kernel it launches are made together, and the passes
    // between the two rewrite both: what one of them does to a kernel dummy
    // it has to do to the argument the launch passes in that position. The
    // launch is laid out argument by argument against the kernel's own
    // dummies, so the two lists have to stay the same length, and each dummy
    // has to be a variable to read that layout from.
    void verify_gpu_kernel_launch_signature(const GpuKernelLaunch_t &x,
            const ASR::Function_t &kernel) {
        std::string kernel_name(kernel.m_name);
        require_id(x.n_args == kernel.n_args,
            "asr.verify.gpu_kernel_launch.argument_count",
            "GpuKernelLaunch passes " + std::to_string(x.n_args) +
                " arguments to kernel '" + kernel_name + "', which declares " +
                std::to_string(kernel.n_args));
        for (size_t i = 0; i < x.n_args; i++) {
            std::string at = "GpuKernelLaunch argument " +
                std::to_string(i + 1) + " of '" + kernel_name + "'";
            require_id(x.m_args[i].m_value != nullptr,
                "asr.verify.gpu_kernel_launch.argument_present",
                at + " is absent; a kernel launch has no optional argument");
            ASR::symbol_t *dummy = nullptr;
            if (ASR::is_a<ASR::Var_t>(*kernel.m_args[i])) {
                dummy = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v);
            }
            require_id(dummy && ASR::is_a<ASR::Variable_t>(*dummy),
                "asr.verify.gpu_kernel_launch.dummy_is_a_variable",
                "the kernel dummy for " + at + " is not a variable");
            verify_gpu_kernel_launch_argument(x, at, x.m_args[i].m_value,
                ASR::down_cast<ASR::Variable_t>(dummy));
        }
    }

    // The block of bytes the host hands over for an argument is the one the
    // device reads for the dummy in that position, so the two have to
    // describe the same value. Not the same type: a kernel dummy carries the
    // data and not the descriptor, so it drops the allocatable or pointer
    // wrapper the argument may have, and the passes between the launch and
    // its expansion give the two sides different array physical types and put
    // the kernel's own arrays in a device address space. What is left, and
    // what the layout is read from, is the element type, its kind and the
    // rank.
    void verify_gpu_kernel_launch_argument(const GpuKernelLaunch_t &x,
            const std::string &at, ASR::expr_t *arg,
            ASR::Variable_t *dummy) {
        ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
        ASR::ttype_t *arg_element = ASRUtils::extract_type(arg_type);
        ASR::ttype_t *dummy_element = ASRUtils::extract_type(dummy->m_type);
        std::string mismatch = at + " is " +
            ASRUtils::type_to_str_fortran_symbol(arg_element,
                ASR::is_a<ASR::StructType_t>(*arg_element)
                    ? ASRUtils::get_struct_sym_from_struct_expr(arg)
                    : nullptr, true) +
            ", but the dummy '" + std::string(dummy->m_name) +
            "' the device reads it as is " +
            ASRUtils::type_to_str_fortran_symbol(dummy_element,
                dummy->m_type_declaration, true);
        require_id(arg_element->type == dummy_element->type,
            "asr.verify.gpu_kernel_launch.argument_element_type", mismatch);
        require_id(ASRUtils::extract_kind_from_ttype_t(arg_element) ==
                ASRUtils::extract_kind_from_ttype_t(dummy_element),
            "asr.verify.gpu_kernel_launch.argument_element_kind", mismatch);
        require_id(ASRUtils::extract_n_dims_from_ttype(arg_type) ==
                ASRUtils::extract_n_dims_from_ttype(dummy->m_type),
            "asr.verify.gpu_kernel_launch.argument_rank",
            at + " has rank " +
                std::to_string(ASRUtils::extract_n_dims_from_ttype(arg_type)) +
                ", but the dummy '" + std::string(dummy->m_name) +
                "' the device reads it as has rank " +
                std::to_string(ASRUtils::extract_n_dims_from_ttype(
                    dummy->m_type)));
        require_id(ASRUtils::is_class_type(arg_element) ==
                ASRUtils::is_class_type(dummy_element),
            "asr.verify.gpu_kernel_launch.argument_polymorphism", mismatch);
        if (ASRUtils::is_class_type(arg_element)) {
            verify_gpu_kernel_launch_class_argument(x, at, arg, arg_type);
        }
    }

    // A polymorphic argument is represented by a class container -- a type
    // descriptor beside a pointer to the data -- while the kernel is
    // generated against the declared type, so the launch hands the kernel a
    // copy of the declared type's own components rather than the container
    // itself. A container the launch cannot make that copy of would be
    // uploaded as it stands and read as the declared type, which is the
    // descriptor read as data: an unlimited polymorphic argument has no
    // declared type to copy, an array of a polymorphic type has one container
    // per element, and a declared type the launch cannot look up has no
    // components to copy. `gpu_offload` keeps such a loop on the host rather
    // than launching it, and that is what is required here.
    void verify_gpu_kernel_launch_class_argument(const GpuKernelLaunch_t &x,
            const std::string &at, ASR::expr_t *arg, ASR::ttype_t *arg_type) {
        require_id(!ASRUtils::is_unlimited_polymorphic_type(arg_type),
            "asr.verify.gpu_kernel_launch.unlimited_polymorphic_argument",
            at + " is unlimited polymorphic, which has no declared type for "
                "the device to read it as");
        require_id(!ASRUtils::is_array(arg_type),
            "asr.verify.gpu_kernel_launch.polymorphic_array_argument",
            at + " is an array of a polymorphic type, which the device would "
                "read as an array of class containers");
        ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(
            ASRUtils::get_struct_sym_from_struct_expr(arg));
        require_id(struct_sym && ASR::is_a<ASR::Struct_t>(*struct_sym),
            "asr.verify.gpu_kernel_launch.polymorphic_declared_type",
            at + " is polymorphic and its declared type is not known, so the "
                "device has no layout to read it as");
    }

    void verify_gpu_kernel_layout(const Function_t &x) {
        const Function_t &kernel = x;
        require_id(ASRUtils::is_device_kernel(&kernel.base),
            "asr.verify.gpu_layout.kernel",
            "only a kernel may own a GPU launch layout");
        const auto &layout = *kernel.m_gpu;
        std::set<ASR::symbol_t*> device_functions;
        for (size_t i = 0; i < layout.n_device_functions; i++) {
            ASR::symbol_t *procedure = layout.m_device_functions[i];
            require_id(procedure && ASR::is_a<ASR::Function_t>(*procedure) &&
                    ASRUtils::runs_on_device(
                        *ASR::down_cast<ASR::Function_t>(procedure)) &&
                    device_functions.insert(procedure).second,
                "asr.verify.gpu_layout.device_function",
                "the GPU call graph must contain distinct device procedures");
            const auto *function = ASR::down_cast<ASR::Function_t>(procedure);
            for (auto *callee : ASRUtils::get_called_functions(function->m_body,
                    function->n_body, true)) {
                require_id(device_functions.count(&callee->base) != 0,
                    "asr.verify.gpu_layout.device_call_order",
                    "GPU callees must precede their callers in the device call graph");
            }
        }
        for (auto *callee : ASRUtils::get_called_functions(kernel.m_body,
                kernel.n_body, true)) {
            require_id(device_functions.count(&callee->base) != 0,
                "asr.verify.gpu_layout.device_call",
                "every kernel callee must belong to the verified device call graph");
        }
        require_id(layout.m_source_argument_count >= 0 &&
                (size_t)layout.m_source_argument_count <= kernel.n_args,
            "asr.verify.gpu_layout.source_arguments",
            "GPU layout has an invalid source argument count");
        std::set<ASR::symbol_t*> bound;
        size_t offset_count = 0;
        auto verify_argument = [&](const gpu_kernel_argument_t &arg,
                bool buffer) {
            require_id(arg.m_argument_index >= 0 &&
                    (size_t)arg.m_argument_index < kernel.n_args,
                "asr.verify.gpu_layout.argument_index",
                "GPU layout argument index is outside the kernel signature");
            require_id(ASR::is_a<ASR::Var_t>(
                    *kernel.m_args[arg.m_argument_index]) &&
                    ASR::down_cast<ASR::Var_t>(
                        kernel.m_args[arg.m_argument_index])->m_v ==
                        arg.m_variable,
                "asr.verify.gpu_layout.argument_identity",
                "GPU layout must refer to the kernel dummy in its argument slot");
            require_id(arg.m_type != nullptr,
                "asr.verify.gpu_layout.argument_type",
                "GPU layout argument must have an explicit element type");
            if (arg.m_kind == gpu_argument_kindType::GpuPackedOffset) {
                require_id(!buffer && layout.m_packed &&
                        arg.m_dimension >= 0 &&
                        (size_t)arg.m_dimension < layout.n_buffers,
                    "asr.verify.gpu_layout.packed_offset",
                    "GPU packed offset must identify a buffer in a packed layout");
                offset_count++;
            } else if (arg.m_kind == gpu_argument_kindType::GpuArrayExtent) {
                require_id(!buffer && arg.m_dimension >= 0 &&
                        arg.m_dimension < ASRUtils::extract_n_dims_from_ttype(
                            ASRUtils::symbol_type(arg.m_variable)),
                    "asr.verify.gpu_layout.array_dimension",
                    "GPU array extent must identify a dimension of its argument");
            } else if (!arg.m_member) {
                require_id(buffer
                        ? (arg.m_kind == gpu_argument_kindType::GpuArray ||
                           arg.m_kind == gpu_argument_kindType::GpuStruct ||
                           arg.m_kind == gpu_argument_kindType::GpuClass)
                        : arg.m_kind == gpu_argument_kindType::GpuScalar,
                    "asr.verify.gpu_layout.argument_role",
                    "a primary GPU binding must have an array, struct, class or scalar role");
                require_id(bound.insert(arg.m_variable).second,
                    "asr.verify.gpu_layout.duplicate_argument",
                    "a GPU argument may have only one primary binding");
                bool array = ASRUtils::is_array(
                    ASRUtils::symbol_type(arg.m_variable));
                bool structure = ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(ASRUtils::symbol_type(arg.m_variable)));
                require_id(buffer == (array || structure),
                    "asr.verify.gpu_layout.argument_storage",
                    "GPU arrays and derived types must have buffer bindings");
                if (buffer) {
                    bool polymorphic = ASRUtils::is_class_type(
                        ASRUtils::extract_type(ASRUtils::symbol_type(arg.m_variable)));
                    require_id(array
                            ? arg.m_kind == gpu_argument_kindType::GpuArray
                            : arg.m_kind == (polymorphic
                                ? gpu_argument_kindType::GpuClass
                                : gpu_argument_kindType::GpuStruct),
                        "asr.verify.gpu_layout.buffer_role",
                        "a GPU buffer role must match its declared argument representation");
                }
            } else {
                require_id(buffer && ASR::is_a<ASR::Variable_t>(*arg.m_member) &&
                        (arg.m_kind == gpu_argument_kindType::GpuMemberData ||
                         arg.m_kind == gpu_argument_kindType::GpuMemberOffsets ||
                         arg.m_kind == gpu_argument_kindType::GpuMemberSizes),
                    "asr.verify.gpu_layout.member_binding",
                    "a GPU component buffer must have a component and a buffer role");
                auto *variable = ASR::down_cast<ASR::Variable_t>(arg.m_variable);
                ASR::symbol_t *declaration = ASRUtils::symbol_get_past_external(
                    variable->m_type_declaration);
                require_id(ASRUtils::is_array(variable->m_type) && declaration &&
                        ASR::is_a<ASR::Struct_t>(*declaration),
                    "asr.verify.gpu_layout.member_owner",
                    "a GPU component buffer must belong to a declared derived-type array");
                bool found = false;
                for (auto &member : ASRUtils::collect_allocatable_array_members(
                        ASR::down_cast<ASR::Struct_t>(declaration))) {
                    found |= &member.second->base == arg.m_member;
                }
                require_id(found, "asr.verify.gpu_layout.member_identity",
                    "a GPU component binding must refer to a member of its argument's type");
            }
        };
        for (size_t i = 0; i < layout.n_buffers; i++) {
            verify_argument(layout.m_buffers[i], true);
        }
        for (size_t i = 0; i < layout.n_scalars; i++) {
            verify_argument(layout.m_scalars[i], false);
        }
        require_id(bound.size() == kernel.n_args,
            "asr.verify.gpu_layout.complete_arguments",
            "every kernel dummy must have a GPU layout binding");
        require_id(offset_count == (layout.m_packed ? layout.n_buffers : 0),
            "asr.verify.gpu_layout.complete_offsets",
            "a packed GPU layout must have one offset per buffer");

        struct ExtentVariables : ASR::BaseWalkVisitor<ExtentVariables> {
            std::set<ASR::symbol_t*> variables;
            bool host_evaluable = true;
            void visit_expr(const ASR::expr_t &expression) {
                switch (expression.type) {
                    case ASR::exprType::IntegerConstant:
                    case ASR::exprType::LogicalConstant:
                    case ASR::exprType::Var:
                    case ASR::exprType::IntegerBinOp:
                    case ASR::exprType::IntegerUnaryMinus:
                    case ASR::exprType::IntegerCompare:
                    case ASR::exprType::IfExp:
                    case ASR::exprType::Cast:
                    case ASR::exprType::ArraySize:
                    case ASR::exprType::ArrayBound:
                    case ASR::exprType::ArrayItem:
                    case ASR::exprType::ArrayPhysicalCast:
                    case ASR::exprType::StructInstanceMember:
                        ASR::BaseWalkVisitor<ExtentVariables>::visit_expr(expression);
                        break;
                    default: host_evaluable = false;
                }
            }
            void visit_Var(const ASR::Var_t &x) {
                variables.insert(ASRUtils::symbol_get_past_external(x.m_v));
            }
            void visit_ttype(const ASR::ttype_t &) {}
        };
        std::set<ASR::symbol_t*> source_arguments;
        for (int64_t i = 0; i < layout.m_source_argument_count; i++) {
            source_arguments.insert(
                ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v);
        }
        std::set<ASR::symbol_t*> workspaces, extent_parameters;
        int slot = (layout.m_packed ? 1 : layout.n_buffers) +
            (layout.n_scalars > 0);
        for (size_t i = 0; i < layout.n_workspaces; i++) {
            const auto &workspace = layout.m_workspaces[i];
            require_id(workspace.m_variable &&
                    ASR::is_a<ASR::Variable_t>(*workspace.m_variable) &&
                    workspaces.insert(workspace.m_variable).second,
                "asr.verify.gpu_layout.workspace_identity",
                "a GPU workspace must identify a distinct local variable");
            require_id(workspace.n_dims == (size_t)
                    ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::symbol_type(workspace.m_variable)),
                "asr.verify.gpu_layout.workspace_rank",
                "GPU workspace dimensions must match the local array rank");
            require_id(workspace.m_buffer_index == slot++,
                "asr.verify.gpu_layout.workspace_slot",
                "GPU workspace slots must follow the argument buffers");
            require_id(workspace.m_element_size ==
                    ASRUtils::extract_kind_from_ttype_t(ASRUtils::extract_type(
                        ASRUtils::symbol_type(workspace.m_variable))),
                "asr.verify.gpu_layout.workspace_element_size",
                "GPU workspace element size must match its array element kind");
            for (size_t d = 0; d < workspace.n_dims; d++) {
                const auto &dim = workspace.m_dims[d];
                require_id(dim.m_extent && ASRUtils::is_integer(
                        *ASRUtils::expr_type(dim.m_extent)) &&
                        !ASRUtils::is_array(ASRUtils::expr_type(dim.m_extent)),
                    "asr.verify.gpu_layout.workspace_extent",
                    "a GPU workspace extent must be an integer scalar expression");
                ExtentVariables refs;
                refs.visit_expr(*dim.m_extent);
                require_id(refs.host_evaluable,
                    "asr.verify.gpu_layout.host_evaluable_expression",
                    "a GPU workspace extent must contain only host-evaluable operations");
                for (ASR::symbol_t *symbol : refs.variables) {
                    require_id(source_arguments.count(symbol) != 0,
                        "asr.verify.gpu_layout.host_evaluable_extent",
                        "a GPU workspace extent may reference only source kernel arguments");
                }
                if (dim.m_parameter) {
                    require_id(bound.count(dim.m_parameter) &&
                            !source_arguments.count(dim.m_parameter) &&
                            extent_parameters.insert(dim.m_parameter).second,
                        "asr.verify.gpu_layout.extent_parameter",
                        "a runtime GPU extent must have a distinct generated kernel parameter");
                    require_id(ASRUtils::check_equal_type(
                            ASRUtils::symbol_type(dim.m_parameter),
                            ASRUtils::expr_type(dim.m_extent), nullptr, nullptr),
                        "asr.verify.gpu_layout.extent_parameter_type",
                        "a GPU extent parameter must preserve the extent expression type");
                } else {
                    require_id(ASRUtils::expr_value(dim.m_extent) != nullptr,
                        "asr.verify.gpu_layout.constant_extent",
                        "a GPU extent without a parameter must be constant");
                }
            }
        }
        require_id(extent_parameters.size() + layout.m_source_argument_count ==
                kernel.n_args,
            "asr.verify.gpu_layout.complete_extent_parameters",
            "every generated GPU parameter must belong to a workspace dimension");
    }

    void visit_GpuKernelLaunch(const GpuKernelLaunch_t &x) {
        require_id(ASRUtils::is_device_kernel(x.m_kernel),
            "asr.verify.gpu_kernel_launch.kernel_runs_on_device",
            "GpuKernelLaunch::m_kernel '" +
                std::string(ASRUtils::symbol_name(x.m_kernel)) +
                "' must be a function that runs on the device");
        verify_gpu_kernel_launch_signature(x,
            *ASR::down_cast<ASR::Function_t>(x.m_kernel));
        BaseWalkVisitor<VerifyVisitor>::visit_GpuKernelLaunch(x);
    }

    void visit_SubroutineCall(const SubroutineCall_t &x) {
        if (check_external) {
            auto *owner = ASRUtils::get_asr_owner(
                ASRUtils::symbol_get_past_external(x.m_name));
            require_id(!owner || !ASR::is_a<TraitRuntimeContract_t>(*owner)
                    || _inside_trait_subroutine,
                "asr.verify.trait_call.dynamic_required",
                "A runtime contract interface must be invoked through its witness slot");
        }
        require(symtab_in_scope(current_symtab, x.m_name),
            "SubroutineCall::m_name '" + std::string(symbol_name(x.m_name)) + "' cannot point outside of its symbol table");
        if (check_external) {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                require(v->m_type_declaration && ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(v->m_type_declaration)),
                    "SubroutineCall::m_name '" + std::string(symbol_name(x.m_name)) + "' is a Variable, but does not point to Function");
                require(ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(v->m_type)),
                    "SubroutineCall::m_name '" + std::string(symbol_name(x.m_name)) + "' is a Variable, but the type is not FunctionType");
            } else {
                require(ASR::is_a<ASR::Function_t>(*s) ||
                        ASR::is_a<ASR::StructMethodDeclaration_t>(*s),
                    "SubroutineCall::m_name '" + std::string(symbol_name(x.m_name)) + "' must be a Function or StructMethodDeclaration.");
                require(!ASR::is_a<ASR::Function_t>(*s) ||
                        !ASRUtils::is_bare_implicit_interface(*ASR::down_cast<ASR::Function_t>(s)),
                    "SubroutineCall::m_name '" + std::string(symbol_name(x.m_name)) + "' was declared external with no interface; the call must reference the signature inferred at this call site.");
            }
            // A CALL statement discards no result, because a procedure
            // invoked by one has none to discard.
            ASR::symbol_t *called = s;
            if (ASR::is_a<ASR::StructMethodDeclaration_t>(*called)) {
                called = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::StructMethodDeclaration_t>(
                        called)->m_proc);
            }
            if (called != nullptr && ASR::is_a<ASR::Function_t>(*called)) {
                require_id(ASR::down_cast<ASR::Function_t>(
                        called)->m_return_var == nullptr,
                    "asr.verify.call.subroutine_returns_nothing",
                    "SubroutineCall::m_name '" +
                    std::string(symbol_name(x.m_name)) +
                    "' returns a value, so it cannot be called as a "
                    "subroutine");
            }
        }

        ASR::symbol_t* asr_owner_sym = nullptr;
        if(current_symtab->asr_owner &&  ASR::is_a<ASR::symbol_t>(*current_symtab->asr_owner) ) {
            asr_owner_sym = ASR::down_cast<ASR::symbol_t>(current_symtab->asr_owner);
        }

        SymbolTable* temp_scope = current_symtab;

        if (asr_owner_sym &&
            !ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name) &&
            !ASR::is_a<ASR::Variable_t>(*x.m_name)) {
            while (temp_scope->parent && temp_scope->asr_owner &&
                   ASR::is_a<ASR::symbol_t>(*temp_scope->asr_owner)) {
                ASR::symbol_t* temp_owner_sym =
                    ASR::down_cast<ASR::symbol_t>(temp_scope->asr_owner);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*temp_owner_sym) &&
                    !ASR::is_a<ASR::Block_t>(*temp_owner_sym)) {
                    break;
                }
                temp_scope = temp_scope->parent;
            }
            if (temp_scope->get_counter() != ASRUtils::symbol_parent_symtab(x.m_name)->get_counter()) {
                function_dependencies.push_back(std::string(ASRUtils::symbol_name(x.m_name)));
            }
        }

        if( ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name) ) {
            ASR::ExternalSymbol_t* x_m_name = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name);
            if( x_m_name->m_external && ASR::is_a<ASR::Module_t>(*ASRUtils::get_asr_owner(x_m_name->m_external)) ) {
                module_dependencies.push_back(std::string(x_m_name->m_module_name));
            }
        }

        verify_args(x);
        if (check_string_length_arguments) verify_hidden_string_length_actuals(x);
    }

    void visit_AssociateBlockCall(const AssociateBlockCall_t &x) {
        require(symtab_in_scope(current_symtab, x.m_m),
            "AssociateBlockCall::m_name '" + std::string(symbol_name(x.m_m)) +
                "' cannot point outside of its symbol table");
        require_id(ASR::is_a<ASR::AssociateBlock_t>(*x.m_m),
            "asr.verify.associate_block_call.target_is_associate_block",
            "AssociateBlockCall::m_m '" + std::string(symbol_name(x.m_m)) +
            "' must be an associate block");
    }

    ASR::symbol_t *get_parent_type_dt(ASR::symbol_t *dt) {
        ASR::symbol_t *parent = nullptr;
        switch (dt->type) {
            case (ASR::symbolType::Struct): {
                dt = ASRUtils::symbol_get_past_external(dt);
                ASR::Struct_t* der_type = ASR::down_cast<ASR::Struct_t>(dt);
                parent = der_type->m_parent;
                break;
            }
            default :
                require_with_loc(false,
                    "m_dt::m_v::m_type must point to a StructType type",
                    dt->base.loc);
        }
        return parent;
    }

    void visit_PointerNullConstant(const PointerNullConstant_t& x) {
        require(x.m_type != nullptr, "null() must have a type");
        if (check_external && ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(x.m_type))) {
            visit_ttype(*x.m_type);
            require_id(ASRUtils::is_trait_pointer(x.m_type),
                "asr.verify.trait_pointer.null_type",
                "A runtime trait null value must have pointer type");
            if (x.m_var_expr) {
                auto *mold_type = typed_expr_type(x.m_var_expr);
                bool owner_variable = ASRUtils::is_trait_owner(mold_type) &&
                    ASRUtils::trait_owner_variable(x.m_var_expr);
                require_id(mold_type &&
                        (ASRUtils::is_trait_pointer(mold_type) || owner_variable),
                    "asr.verify.trait_pointer.null_mold",
                    "A runtime trait null mold must be a pointer or allocatable variable");
                visit_ttype(*mold_type);
                require_id(ASRUtils::trait_contracts_equal(x.m_type, mold_type),
                    "asr.verify.trait_pointer.null_mold",
                    "A runtime trait null value must retain its mold's declared contract");
            }
        }
        if ( x.m_var_expr != nullptr ) {
            visit_expr(*x.m_var_expr);
        }
    }

    void visit_FunctionType(const FunctionType_t& x) {

        #define verify_nonscoped_ttype(ttype) non_global_symbol_visited = false; \
            visit_ttype(*ttype); \
            require(non_global_symbol_visited == false, \
                    "ASR::ttype_t in ASR::FunctionType" \
                    " cannot be tied to a scope."); \

        _is_return_type_string = false;
        if (x.m_return_var_type) {
            _is_return_type_string = ASRUtils::is_character(*x.m_return_var_type);
        }

        for( size_t i = 0; i < x.n_arg_types; i++ ) {
            verify_nonscoped_ttype(x.m_arg_types[i]);
        }
        if( x.m_return_var_type ) {
            verify_nonscoped_ttype(x.m_return_var_type);
        }
        require_id(x.m_deftype != ASR::deftypeType::ImplicitInterface ||
                x.n_arg_types == 0,
            "asr.verify.function_type.implicit_interface_has_no_arg_types",
            "a procedure type with an implicit interface must not list "
            "argument types");
    }

    // A FunctionPointerCast views a procedure through another procedure type:
    // either an interface symbol `to` whose signature is the cast's type, or,
    // without `to`, the opaque procedure type.
    void visit_FunctionPointerCast(const FunctionPointerCast_t &x) {
        BaseWalkVisitor<VerifyVisitor>::visit_FunctionPointerCast(x);
        require_id(ASR::is_a<ASR::FunctionType_t>(*x.m_type),
            "asr.verify.function_pointer_cast.type_is_procedure",
            "FunctionPointerCast type must be a procedure type");
        // The argument's type can only be taken once ExternalSymbols are
        // resolved: while a modfile is loaded the argument can be a
        // use-associated procedure of a module that is not loaded yet.
        if (check_external) {
            require_id(as_procedure_type(ASRUtils::expr_type(x.m_arg)) != nullptr,
                "asr.verify.function_pointer_cast.arg_is_procedure",
                "FunctionPointerCast argument must be a procedure");
        }
        if (x.m_to == nullptr) {
            require_id(ASRUtils::is_opaque_procedure_type(x.m_type),
                "asr.verify.function_pointer_cast.no_interface_is_opaque",
                "FunctionPointerCast without an interface must cast to the "
                "opaque procedure type");
            return;
        }
        require(symtab_in_scope(current_symtab, x.m_to),
            "FunctionPointerCast::m_to '" + std::string(symbol_name(x.m_to)) +
            "' cannot point outside of its symbol table");
        if (!check_external) return;
        ASR::symbol_t *to = ASRUtils::symbol_get_past_external(x.m_to);
        require_id(ASR::is_a<ASR::Function_t>(*to),
            "asr.verify.function_pointer_cast.interface_is_function",
            "FunctionPointerCast interface must be a procedure");
        if (!ASR::is_a<ASR::Function_t>(*to) ||
                !ASR::is_a<ASR::FunctionType_t>(*x.m_type)) {
            return;
        }
        ASR::Function_t *to_fn = ASR::down_cast<ASR::Function_t>(to);
        require_id(!ASRUtils::is_bare_implicit_interface(*to_fn),
            "asr.verify.function_pointer_cast.interface_is_explicit",
            "FunctionPointerCast interface '" + std::string(to_fn->m_name) +
            "' must be explicit");
        require_id(ASR::down_cast<ASR::FunctionType_t>(x.m_type)->n_arg_types
                == to_fn->n_args,
            "asr.verify.function_pointer_cast.type_matches_interface",
            "FunctionPointerCast type must have the arguments of interface '" +
            std::string(to_fn->m_name) + "'");
    }

    void visit_IntrinsicImpureFunction(const IntrinsicImpureFunction_t &x) {
        if (x.m_impure_intrinsic_id ==
                    static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated) &&
                x.n_args == 1 && x.m_args &&
                ASRUtils::is_trait_owner(typed_expr_type(x.m_args[0]))) {
            require_id(check_external ? ASRUtils::trait_owner_variable(x.m_args[0]) != nullptr
                    : ASR::is_a<Var_t>(*x.m_args[0]) || ASR::is_a<StructInstanceMember_t>(*x.m_args[0]),
                "asr.verify.trait_owner.inquiry_variable",
                "An allocated inquiry requires a variable, not a function result");
        }
        BaseWalkVisitor::visit_IntrinsicImpureFunction(x);
    }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t& x) {
        if( !check_external ) {
            BaseWalkVisitor<VerifyVisitor>::visit_IntrinsicElementalFunction(x);
            return ;
        }
        ASRUtils::verify_function verify_ = ASRUtils::IntrinsicElementalFunctionRegistry
            ::get_verify_function(x.m_intrinsic_id);
        LCOMPILERS_ASSERT(verify_ != nullptr);
        verify_(x, diagnostics);
        bool _inside_call_copy = _inside_call;
        _inside_call = true;
        BaseWalkVisitor<VerifyVisitor>::visit_IntrinsicElementalFunction(x);
        _inside_call = _inside_call_copy;
    }

    void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t& x) {
        if( !check_external ) {
            BaseWalkVisitor<VerifyVisitor>::visit_IntrinsicArrayFunction(x);
            return ;
        }
        ASRUtils::verify_array_function verify_ = ASRUtils::IntrinsicArrayFunctionRegistry
            ::get_verify_function(x.m_arr_intrinsic_id);
        LCOMPILERS_ASSERT(verify_ != nullptr);
        verify_(x, diagnostics);
        bool _inside_call_copy = _inside_call;
        _inside_call = true;
        BaseWalkVisitor<VerifyVisitor>::visit_IntrinsicArrayFunction(x);
        _inside_call = _inside_call_copy;
    }

    void visit_FunctionCall(const FunctionCall_t &x) {
        require(x.m_name,
            "FunctionCall::m_name must be present");
        if (check_external) {
            auto *owner = ASRUtils::get_asr_owner(
                ASRUtils::symbol_get_past_external(x.m_name));
            require_id(!owner || !ASR::is_a<TraitRuntimeContract_t>(*owner) ||
                    (current_expr && ASR::is_a<TraitFunctionCall_t>(*current_expr)),
                "asr.verify.trait_call.dynamic_required",
                "A runtime contract interface must be invoked through its witness slot");
        }
        if (check_external && !_inside_template) {
            auto *function = ASRUtils::symbol_get_past_external(x.m_name);
            if (function && ASR::is_a<ASR::Function_t>(*function)) {
                bool intrinsic_restriction = false;
                auto *scope = ASRUtils::symbol_parent_symtab(function);
                auto *owner = ASRUtils::get_asr_owner(function);
                if (owner && ASR::is_a<ASR::Template_t>(*owner)) {
                    for (const auto &entry : scope->get_scope()) {
                        if (!ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) continue;
                        auto *constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
                        for (size_t i = 0; i < constraint->n_intrinsic_requirements; i++) {
                            intrinsic_restriction |=
                                constraint->m_intrinsic_requirements[i].m_procedure == function;
                        }
                    }
                }
                require_id(!intrinsic_restriction,
                    "asr.verify.call.unbound_restriction",
                    "A concrete executable cannot call an unbound intrinsic restriction");
            }
        }
        variable_dependencies.push_back(std::string(ASRUtils::symbol_name(x.m_name)));
        if (x.m_dt) {
            visit_expr(*x.m_dt);
        }
        ASR::symbol_t* asr_owner_sym = nullptr;
        if(current_symtab->asr_owner &&  ASR::is_a<ASR::symbol_t>(*current_symtab->asr_owner) ) {
            asr_owner_sym = ASR::down_cast<ASR::symbol_t>(current_symtab->asr_owner);
        }

        SymbolTable* temp_scope = current_symtab;

        if (asr_owner_sym &&
            !ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name) &&
            !ASR::is_a<ASR::Variable_t>(*x.m_name)) {
            while (temp_scope->parent && temp_scope->asr_owner &&
                   ASR::is_a<ASR::symbol_t>(*temp_scope->asr_owner)) {
                ASR::symbol_t* temp_owner_sym =
                    ASR::down_cast<ASR::symbol_t>(temp_scope->asr_owner);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*temp_owner_sym) &&
                    !ASR::is_a<ASR::Block_t>(*temp_owner_sym)) {
                    break;
                }
                temp_scope = temp_scope->parent;
            }
            if (temp_scope->get_counter() != ASRUtils::symbol_parent_symtab(x.m_name)->get_counter()) {
                function_dependencies.push_back(std::string(ASRUtils::symbol_name(x.m_name)));
            }
        }
        if (_return_var_or_intent_out  && _processing_dims &&
            temp_scope->get_counter() != ASRUtils::symbol_parent_symtab(x.m_name)->get_counter() &&
            !ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name)) {
            function_dependencies.push_back(std::string(ASRUtils::symbol_name(x.m_name)));
        }

        if( ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name) ) {
            ASR::ExternalSymbol_t* x_m_name = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name);
            if( x_m_name->m_external && ASR::is_a<ASR::Module_t>(*ASRUtils::get_asr_owner(x_m_name->m_external)) ) {
                module_dependencies.push_back(std::string(x_m_name->m_module_name));
            }
        }

        require(symtab_in_scope(current_symtab, x.m_name),
            "FunctionCall::m_name `" + std::string(symbol_name(x.m_name)) +
            "` cannot point outside of its symbol table");
        // Check both `name` and `orig_name` that `orig_name` points
        // to GenericProcedure (if applicable), both external and non
        // external
        const ASR::symbol_t *fn = ASRUtils::symbol_get_past_external(x.m_name);
        if (check_external) {
            require(ASR::is_a<ASR::Function_t>(*fn) ||
                    (ASR::is_a<ASR::Variable_t>(*fn) &&
                    ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(fn)))) ||
                    ASR::is_a<ASR::StructMethodDeclaration_t>(*fn),
                "FunctionCall::m_name must be a Function or Variable with FunctionType");
        }

        if( fn && ASR::is_a<ASR::Function_t>(*fn) ) {
            ASR::Function_t* fn_ = ASR::down_cast<ASR::Function_t>(fn);
            require(fn_->m_return_var != nullptr,
                    "FunctionCall::m_name " + std::string(fn_->m_name) +
                    " must be returning a non-void value.");
            require(!ASRUtils::is_bare_implicit_interface(*fn_),
                    "FunctionCall::m_name " + std::string(fn_->m_name) +
                    " was declared external with no interface; the call must"
                    " reference the signature inferred at this call site.");
            // The call site's result type is what the surrounding expression
            // was typed against; the callee's is what the call actually
            // produces. Where they disagree, the two disagree about the call.
            ASR::ttype_t *returned = typed_expr_type(fn_->m_return_var);
            if (returned && x.m_type &&
                    (ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(returned)) ||
                     ASR::is_a<TraitObjectType_t>(*ASRUtils::extract_type(x.m_type)))) {
                require_id(ASRUtils::is_trait_owner(returned) &&
                        ASRUtils::is_trait_owner(x.m_type),
                    "asr.verify.trait_result.type",
                    "A trait function call must preserve its owning result type");
            }
            if (returned != nullptr && x.m_type != nullptr &&
                    !ASRUtils::is_intrinsic_symbol(x.m_name) &&
                    !is_struct_like_type(returned) &&
                    !is_struct_like_type(x.m_type) &&
                    !is_procedure_type(returned) &&
                    !is_procedure_type(x.m_type)) {
                require_id(ASRUtils::check_equal_type(x.m_type, returned,
                        nullptr, type_context(fn_->m_return_var)),
                    "asr.verify.call.result_type_matches_callee",
                    "FunctionCall to '" + std::string(fn_->m_name) +
                    "' has type " + ASRUtils::get_type_code(x.m_type) +
                    ", but the function returns " +
                    ASRUtils::get_type_code(returned));
            }
        }
        verify_args(x);
        if (check_string_length_arguments) verify_hidden_string_length_actuals(x);
        visit_ttype(*x.m_type);
    }

    void visit_TypeParameter(const TypeParameter_t &x) {
        if (_inside_template) return;
        for (auto *scope = current_symtab; scope; scope = scope->parent) {
            if (scope->asr_owner && ASR::is_a<ASR::symbol_t>(*scope->asr_owner) &&
                    ASR::is_a<ASR::Requirement_t>(
                        *ASR::down_cast<ASR::symbol_t>(scope->asr_owner))) return;
        }
        for (auto *scope = current_symtab; scope; scope = scope->parent) {
            if (!scope->asr_owner || !ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) continue;
            auto *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
            require_id(!ASR::is_a<ASR::Program_t>(*owner),
                "asr.verify.type_parameter.concrete_executable",
                "A concrete executable cannot contain an unresolved type parameter");
            if (ASR::is_a<ASR::Function_t>(*owner)) {
                require_id(ASRUtils::get_FunctionType(owner)->m_deftype !=
                        ASR::deftypeType::Implementation,
                    "asr.verify.type_parameter.concrete_executable",
                    "A concrete executable cannot contain an unresolved type parameter");
                return;
            }
        }
    }

    void visit_StructType(const StructType_t& x) {
        for (size_t i = 0; i < x.n_data_member_types; i++) {
            visit_ttype(*x.m_data_member_types[i]);
        }
    }

    void visit_ArrayConstructor(const ArrayConstructor_t& x) {
        require(ASRUtils::is_array(x.m_type),
            "Type of ArrayConstructor must be an array");
        if (x.m_struct_var != nullptr) {
            require(ASR::is_a<ASR::Var_t>(*x.m_struct_var),
                "ArrayConstructor::m_struct_vars must be nullptr or var to struct symbol");
        }
        // Every element ends up in one array, so they all have to be the
        // element type the constructor claims. A pass that lowers the
        // constructor builds an assignment per element and asserts on the
        // first one whose type does not match, rather than diagnosing it.
        ASR::ttype_t *element = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(x.m_type));
        if (element != nullptr && !diagnostics.has_error() &&
                !is_struct_like_type(element) && !is_procedure_type(element)) {
            for (size_t i = 0; i < x.n_args; i++) {
                ASR::ttype_t *arg = typed_expr_type(x.m_args[i]);
                if (arg == nullptr || ASRUtils::is_array(arg)) continue;
                if (is_struct_like_type(arg) || is_procedure_type(arg)) {
                    continue;
                }
                require_with_loc_id(
                    ASRUtils::check_equal_type(arg, element, nullptr, nullptr),
                    "asr.verify.array_constructor.element_type_matches",
                    "ArrayConstructor element " + std::to_string(i + 1) +
                    " has type " + ASRUtils::get_type_code(arg) +
                    ", but the constructor builds an array of " +
                    ASRUtils::get_type_code(element),
                    x.m_args[i]->base.loc);
            }
        }
        BaseWalkVisitor<VerifyVisitor>::visit_ArrayConstructor(x);
    }

    void visit_StringFormat(const StringFormat_t& x) {
        require(x.m_kind != ASR::string_format_kindType::FormatFortranLeadingBlank
                || x.m_fmt == nullptr,
            "StringFormat::m_fmt must be nil for FormatFortranLeadingBlank");
        BaseWalkVisitor<VerifyVisitor>::visit_StringFormat(x);
    }

    void visit_ArrayConstant(const ArrayConstant_t& x) {
        require(ASRUtils::is_array(x.m_type),
            "Type of ArrayConstant must be an array");

        ASR::ttype_t* inner = ASRUtils::type_get_past_array(x.m_type);
        if (ASRUtils::is_character(*inner)) {
            int64_t len;
            require(ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(inner)->m_len, len), "Constant array of strings should have constant string length");
        }
        int64_t n_data = ASRUtils::get_ArrayConstant_data_size(x.m_type);
        require(n_data == x.m_n_data, "ArrayConstant::m_n_data must match the byte size of the array");
        visit_ttype(*x.m_type);
    }

    void visit_dimension(const dimension_t &x) {
        if (_inside_array_physical_cast_type && !_inside_call
                && !_processing_assumed_rank_array
                && !_processing_unbounded_pointer_array) {
            require_with_loc(x.m_length != nullptr && x.m_start != nullptr,
                    "Dimensions in ArrayPhysicalCast must be present if not inside a call",
                    x.loc);
        }
        // Reset the flag before visiting dimension expressions so that
        // nested types (e.g. the selector's allocatable array type
        // referenced by ArrayBound/ArraySize nodes) are not subject
        // to the ArrayPhysicalCast dimension check.
        bool _inside_array_physical_cast_type_copy = _inside_array_physical_cast_type;
        _inside_array_physical_cast_type = false;
        if (x.m_start) {
            if(check_external){
                require_with_loc(ASRUtils::is_integer(
                    *ASRUtils::expr_type(x.m_start)),
                    "Start dimension must be a signed integer", x.loc);
            }
            visit_expr(*x.m_start);
        }

        if (x.m_length) {
            if(check_external){
                require_with_loc(ASRUtils::is_integer(
                    *ASRUtils::expr_type(x.m_length)),
                    "Length dimension must be a signed integer", x.loc);
            }
            visit_expr(*x.m_length);
        }
        _inside_array_physical_cast_type = _inside_array_physical_cast_type_copy;
    }

    void visit_Integer(const Integer_t &x) {
        if (diagnostics.has_error()) return;
        require_id(
            x.m_kind == 1 || x.m_kind == 2 ||
            x.m_kind == 4 || x.m_kind == 8 || x.m_kind >= 1000,
            "asr.verify.type.integer_kind_supported",
            "Integer kind " + std::to_string(x.m_kind) +
                " is not supported");
    }

    void visit_UnsignedInteger(const UnsignedInteger_t &x) {
        if (diagnostics.has_error()) return;
        require_id(
            x.m_kind == 1 || x.m_kind == 2 ||
            x.m_kind == 4 || x.m_kind == 8 || x.m_kind >= 1000,
            "asr.verify.type.unsigned_integer_kind_supported",
            "UnsignedInteger kind " + std::to_string(x.m_kind) +
                " is not supported");
    }

    void visit_Real(const Real_t &x) {
        if (diagnostics.has_error()) return;
        require_id(
            x.m_kind == 4 || x.m_kind == 8 || x.m_kind == 10 ||
                x.m_kind == 16 || x.m_kind >= 1000,
            "asr.verify.type.real_kind_supported",
            "Real kind " + std::to_string(x.m_kind) +
                " is not supported");
    }

    void visit_Complex(const Complex_t &x) {
        if (diagnostics.has_error()) return;
        require_id(
            x.m_kind == 4 || x.m_kind == 8 || x.m_kind == 16 ||
                x.m_kind >= 1000,
            "asr.verify.type.complex_kind_supported",
            "Complex kind " + std::to_string(x.m_kind) +
                " is not supported");
    }

    void visit_Logical(const Logical_t &x) {
        if (diagnostics.has_error()) return;
        require_id(
            x.m_kind == 1 || x.m_kind == 2 ||
            x.m_kind == 4 || x.m_kind == 8 || x.m_kind >= 1000,
            "asr.verify.type.logical_kind_supported",
            "Logical kind " + std::to_string(x.m_kind) +
                " is not supported");
    }

    // An assumed rank array has no shape of its own: its rank is only known
    // from the `select rank` block that selects it. An operation on one has
    // no result shape, so the frontend must first pin the rank down with an
    // ArrayPhysicalCast away from AssumedRankArray. Reaching an operation
    // without that cast means the rank was never resolved, and every later
    // stage reads the operand as rank 0.
    void verify_operand_not_assumed_rank(const char *name,
            const char *position, ASR::ttype_t *type, const Location &loc) {
        if (type == nullptr) return;
        require_with_loc_id(
            !ASRUtils::is_assumed_rank_array(type),
            "asr.verify.operation.operand_not_assumed_rank",
            std::string(name) + " " + position + " is an assumed rank "
                "array, which has no known rank; it must be cast to a "
                "descriptor array first",
            loc);
    }

    // An operation combines two operands of one type into a result of that
    // type, and a comparison combines two operands of one type into a
    // logical. The frontend guarantees this by inserting explicit Cast
    // nodes, so a disagreement means the graph came from somewhere that did
    // not, and the backend must not paper over it: LLVM rejects the module
    // it produces from such a node. Array shape is not compared, since an
    // elemental operation legitimately mixes ranks.
    void verify_binary_operands(const char *name, ASR::expr_t *left,
            ASR::expr_t *right, ASR::ttype_t *result, bool is_compare,
            const Location &loc) {
        if (diagnostics.has_error()) return;
        ASR::ttype_t *left_type = typed_expr_type(left);
        ASR::ttype_t *right_type = typed_expr_type(right);
        if (left_type == nullptr || right_type == nullptr) return;
        verify_operand_not_assumed_rank(name, "left operand", left_type, loc);
        verify_operand_not_assumed_rank(name, "right operand", right_type, loc);
        if (diagnostics.has_error()) return;
        if (is_procedure_type(left_type) || is_procedure_type(right_type)
                || is_struct_like_type(left_type)
                || is_struct_like_type(right_type)) {
            return;
        }
        ASR::ttype_t *left_scalar = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(left_type));
        ASR::ttype_t *right_scalar = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(right_type));
        // Only a kind disagreement inside one type family is checked. The
        // frontend still emits a few operations whose operands differ in
        // family, such as a real minus an integer, and the backend converts
        // those; a kind disagreement is what it cannot lower.
        if (left_scalar->type != right_scalar->type) return;
        require_with_loc_id(
            ASRUtils::check_equal_type(
                left_scalar, right_scalar, nullptr, nullptr),
            "asr.verify.binary_op.operand_types_match",
            std::string(name) + " operand types " +
                ASRUtils::get_type_code(left_scalar) + " and " +
                ASRUtils::get_type_code(right_scalar) + " do not match",
            loc);
        if (is_compare || result == nullptr) return;
        ASR::ttype_t *result_scalar = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(result));
        if (left_scalar->type != result_scalar->type) return;
        require_with_loc_id(
            ASRUtils::check_equal_type(
                left_scalar, result_scalar, nullptr, nullptr),
            "asr.verify.binary_op.result_type_matches_operands",
            std::string(name) + " result type " +
                ASRUtils::get_type_code(result_scalar) +
                " does not match operand type " +
                ASRUtils::get_type_code(left_scalar),
            loc);
    }

    void visit_IntegerBinOp(const IntegerBinOp_t &x) {
        verify_binary_operands("IntegerBinOp", x.m_left, x.m_right, x.m_type,
            false, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_IntegerBinOp(x);
    }

    void visit_UnsignedIntegerBinOp(const UnsignedIntegerBinOp_t &x) {
        verify_binary_operands("UnsignedIntegerBinOp", x.m_left, x.m_right, x.m_type,
            false, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_UnsignedIntegerBinOp(x);
    }

    void visit_RealBinOp(const RealBinOp_t &x) {
        verify_binary_operands("RealBinOp", x.m_left, x.m_right, x.m_type,
            false, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_RealBinOp(x);
    }

    void visit_ComplexBinOp(const ComplexBinOp_t &x) {
        verify_binary_operands("ComplexBinOp", x.m_left, x.m_right, x.m_type,
            false, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_ComplexBinOp(x);
    }

    void visit_LogicalBinOp(const LogicalBinOp_t &x) {
        verify_binary_operands("LogicalBinOp", x.m_left, x.m_right, x.m_type,
            false, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_LogicalBinOp(x);
    }

    void visit_IntegerCompare(const IntegerCompare_t &x) {
        verify_binary_operands("IntegerCompare", x.m_left, x.m_right, x.m_type,
            true, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_IntegerCompare(x);
    }

    void visit_UnsignedIntegerCompare(const UnsignedIntegerCompare_t &x) {
        verify_binary_operands("UnsignedIntegerCompare", x.m_left, x.m_right, x.m_type,
            true, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_UnsignedIntegerCompare(x);
    }

    void visit_RealCompare(const RealCompare_t &x) {
        verify_binary_operands("RealCompare", x.m_left, x.m_right, x.m_type,
            true, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_RealCompare(x);
    }

    void visit_ComplexCompare(const ComplexCompare_t &x) {
        verify_binary_operands("ComplexCompare", x.m_left, x.m_right, x.m_type,
            true, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_ComplexCompare(x);
    }

    void visit_LogicalCompare(const LogicalCompare_t &x) {
        verify_binary_operands("LogicalCompare", x.m_left, x.m_right, x.m_type,
            true, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_LogicalCompare(x);
    }

    void verify_unary_operand(const char *name, ASR::expr_t *arg,
            const Location &loc) {
        if (diagnostics.has_error()) return;
        verify_operand_not_assumed_rank(name, "argument",
            typed_expr_type(arg), loc);
    }

    void visit_IntegerUnaryMinus(const IntegerUnaryMinus_t &x) {
        verify_unary_operand("IntegerUnaryMinus", x.m_arg, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_IntegerUnaryMinus(x);
    }

    void visit_UnsignedIntegerUnaryMinus(const UnsignedIntegerUnaryMinus_t &x) {
        verify_unary_operand("UnsignedIntegerUnaryMinus", x.m_arg,
            x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_UnsignedIntegerUnaryMinus(x);
    }

    void visit_RealUnaryMinus(const RealUnaryMinus_t &x) {
        verify_unary_operand("RealUnaryMinus", x.m_arg, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_RealUnaryMinus(x);
    }

    void visit_ComplexUnaryMinus(const ComplexUnaryMinus_t &x) {
        verify_unary_operand("ComplexUnaryMinus", x.m_arg, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_ComplexUnaryMinus(x);
    }

    // A StructConstructor's arguments fill the type's members in
    // declaration order, parent members first, and a later pass lowers it
    // into one assignment per member. A type that disagrees with its member
    // therefore surfaces as a broken assignment inside that pass rather than
    // here, so it is checked up front. The pass also indexes the member list
    // positionally, so a count mismatch is a memory error waiting to happen.
    //
    // A StructConstant is lowered directly by the backends, which read each
    // argument as a value of its member, so its arguments must also have
    // the member's rank and shape. A StructConstructor's arguments are
    // assigned, so a scalar argument may still fill an array member.
    void verify_struct_constructor_arguments(const char *node,
            const std::string &id, ASR::symbol_t *dt_sym,
            const ASR::call_arg_t *args, size_t n_args, bool is_constant,
            const Location &loc) {
        ASR::symbol_t *struct_sym = dt_sym == nullptr
            ? nullptr : ASRUtils::symbol_get_past_external(dt_sym);
        if (diagnostics.has_error() || struct_sym == nullptr
                || !ASR::is_a<ASR::Struct_t>(*struct_sym)) {
            return;
        }
        std::vector<ASR::Struct_t*> chain;
        std::set<ASR::Struct_t*> seen;
        ASR::Struct_t *struct_type =
            ASR::down_cast<ASR::Struct_t>(struct_sym);
        while (struct_type != nullptr) {
            require_with_loc_id(seen.insert(struct_type).second,
                id + ".parent_chain_acyclic",
                std::string(node) + " type '" +
                    std::string(struct_type->m_name) +
                    "' has a cyclic parent chain", loc);
            chain.push_back(struct_type);
            if (struct_type->m_parent == nullptr) break;
            ASR::symbol_t *parent = ASRUtils::symbol_get_past_external(
                struct_type->m_parent);
            if (parent == nullptr || !ASR::is_a<ASR::Struct_t>(*parent)) {
                break;
            }
            struct_type = ASR::down_cast<ASR::Struct_t>(parent);
        }
        std::vector<ASR::symbol_t*> members;
        for (auto it = chain.rbegin(); it != chain.rend(); it++) {
            for (size_t i = 0; i < (*it)->n_members; i++) {
                members.push_back(
                    (*it)->m_symtab->get_symbol((*it)->m_members[i]));
            }
        }
        require_with_loc_id(members.size() == n_args,
            id + ".argument_count",
            std::string(node) + " has " + std::to_string(n_args) +
                " arguments but the type has " +
                std::to_string(members.size()) + " members", loc);
        if (members.size() != n_args) {
            return;
        }
        for (size_t i = 0; i < n_args; i++) {
            ASR::ttype_t *actual = typed_expr_type(args[i].m_value);
            if (actual == nullptr || members[i] == nullptr
                    || !ASR::is_a<ASR::Variable_t>(*members[i])) {
                continue;
            }
            ASR::ttype_t *declared =
                ASR::down_cast<ASR::Variable_t>(members[i])->m_type;
            if (declared == nullptr) continue;
            std::string member_name = ASRUtils::symbol_name(members[i]);
            const Location &arg_loc = args[i].m_value->base.loc;
            ASR::ttype_t *member_scalar =
                ASRUtils::type_get_past_array(
                    ASRUtils::type_get_past_allocatable_pointer(declared));
            ASR::ttype_t *actual_scalar =
                ASRUtils::type_get_past_array(
                    ASRUtils::type_get_past_allocatable_pointer(actual));
            if (ASR::is_a<ASR::PointerNullConstant_t>(*args[i].m_value)) {
                // A null() argument has the type of its member, so it is a
                // derived type or procedure exactly when the member is.
                require_with_loc_id(
                    is_struct_like_type(member_scalar)
                        == is_struct_like_type(actual_scalar)
                    && is_procedure_type(member_scalar)
                        == is_procedure_type(actual_scalar),
                    id + ".null_argument_type_matches_member",
                    "null() argument type does not match member '" +
                        member_name + "'",
                    arg_loc);
                continue;
            }
            if (is_struct_like_type(member_scalar)
                    || is_procedure_type(member_scalar)
                    || is_struct_like_type(actual_scalar)
                    || is_procedure_type(actual_scalar)) {
                continue;
            }
            require_with_loc_id(
                ASRUtils::check_equal_type(
                    member_scalar, actual_scalar, nullptr, nullptr),
                id + ".argument_type_matches_member",
                std::string(node) + " argument type " +
                    ASRUtils::get_type_code(actual_scalar) +
                    " does not match member '" + member_name +
                    "' of type " + ASRUtils::get_type_code(member_scalar),
                arg_loc);
            if (is_constant && ASR::is_a<ASR::String_t>(*member_scalar)
                    && ASR::is_a<ASR::String_t>(*actual_scalar)) {
                ASR::expr_t *member_len_expr =
                    ASR::down_cast<ASR::String_t>(member_scalar)->m_len;
                ASR::expr_t *actual_len_expr =
                    ASR::down_cast<ASR::String_t>(actual_scalar)->m_len;
                int64_t member_len = 0, actual_len = 0;
                if (member_len_expr != nullptr && actual_len_expr != nullptr
                        && ASRUtils::extract_value(
                            ASRUtils::expr_value(member_len_expr), member_len)
                        && ASRUtils::extract_value(
                            ASRUtils::expr_value(actual_len_expr), actual_len)) {
                    require_with_loc_id(member_len == actual_len,
                        id + ".argument_length_matches_member",
                        std::string(node) + " argument of length " +
                            std::to_string(actual_len) +
                            " does not match member '" + member_name +
                            "' of length " + std::to_string(member_len),
                        arg_loc);
                }
            }
            size_t member_rank = ASRUtils::extract_n_dims_from_ttype(declared);
            size_t actual_rank = ASRUtils::extract_n_dims_from_ttype(actual);
            require_with_loc_id(member_rank == actual_rank
                    || (!is_constant && actual_rank == 0),
                id + ".argument_rank_matches_member",
                std::string(node) + " argument of rank " +
                    std::to_string(actual_rank) + " does not match member '" +
                    member_name + "' of rank " + std::to_string(member_rank),
                arg_loc);
            if (is_constant && member_rank == actual_rank && member_rank > 0) {
                ASR::dimension_t *member_dims = nullptr, *actual_dims = nullptr;
                ASRUtils::extract_dimensions_from_ttype(declared, member_dims);
                ASRUtils::extract_dimensions_from_ttype(actual, actual_dims);
                for (size_t d = 0; d < member_rank; d++) {
                    int64_t member_length = 0, actual_length = 0;
                    if (member_dims[d].m_length == nullptr
                            || actual_dims[d].m_length == nullptr
                            || !ASRUtils::extract_value(ASRUtils::expr_value(
                                member_dims[d].m_length), member_length)
                            || !ASRUtils::extract_value(ASRUtils::expr_value(
                                actual_dims[d].m_length), actual_length)) {
                        continue;
                    }
                    require_with_loc_id(member_length == actual_length,
                        id + ".argument_shape_matches_member",
                        std::string(node) + " argument extent " +
                            std::to_string(actual_length) + " in dimension " +
                            std::to_string(d + 1) + " does not match member '" +
                            member_name + "' extent " +
                            std::to_string(member_length),
                        arg_loc);
                }
            }
        }
    }

    void visit_StructConstructor(const StructConstructor_t &x) {
        verify_struct_constructor_arguments("StructConstructor",
            "asr.verify.struct_constructor", x.m_dt_sym, x.m_args, x.n_args,
            false, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_StructConstructor(x);
    }

    // A type parameter of a parameterized derived type, whose value is
    // known only once the type is instantiated
    bool is_struct_type_parameter(ASR::expr_t *arg) {
        if (!ASR::is_a<ASR::Var_t>(*arg)) return false;
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(arg)->m_v);
        if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return false;
        ASR::asr_t *owner =
            ASR::down_cast<ASR::Variable_t>(sym)->m_parent_symtab->asr_owner;
        return owner != nullptr && ASR::is_a<ASR::symbol_t>(*owner)
            && ASR::is_a<ASR::Struct_t>(*ASR::down_cast<ASR::symbol_t>(owner));
    }

    // The backends emit a StructConstant as static data, so each argument
    // must be a constant: a literal, a named constant, or a structure
    // constructor of constants. A variable, such as a temporary created by
    // a pass, cannot be part of static data.
    bool is_struct_constant_argument(ASR::expr_t *arg) {
        if (ASRUtils::is_value_constant(arg)
                || ASRUtils::is_value_constant(ASRUtils::expr_value(arg))
                || is_struct_type_parameter(arg)) {
            return true;
        }
        if (ASR::is_a<ASR::Var_t>(*arg)) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(arg)->m_v);
            return sym != nullptr && ASR::is_a<ASR::Variable_t>(*sym)
                && ASR::down_cast<ASR::Variable_t>(sym)->m_storage
                    == ASR::storage_typeType::Parameter;
        }
        if (ASR::is_a<ASR::StructConstructor_t>(*arg)) {
            ASR::StructConstructor_t *sc =
                ASR::down_cast<ASR::StructConstructor_t>(arg);
            for (size_t i = 0; i < sc->n_args; i++) {
                if (sc->m_args[i].m_value != nullptr
                        && !is_struct_constant_argument(sc->m_args[i].m_value)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    void visit_StructConstant(const StructConstant_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value == nullptr) continue;
            require_with_loc_id(is_struct_constant_argument(x.m_args[i].m_value),
                "asr.verify.struct_constant.argument_is_constant",
                "StructConstant argument " + std::to_string(i + 1)
                    + " is not a constant",
                x.m_args[i].m_value->base.loc);
        }
        verify_struct_constructor_arguments("StructConstant",
            "asr.verify.struct_constant", x.m_dt_sym, x.m_args, x.n_args,
            true, x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_StructConstant(x);
    }

    // `dim` selects one of the array's dimensions, so a constant outside
    // 1..rank is invalid. The pass that folds these intrinsics indexes
    // `dims[dim - 1]` directly, so an out of range constant reads outside the
    // dimension array and crashes the compiler rather than diagnosing it.
    void verify_dimension_argument(const char *name, ASR::expr_t *array,
            ASR::expr_t *dim, const Location &loc) {
        if (diagnostics.has_error()
                || array == nullptr || dim == nullptr) {
            return;
        }
        // Only a literal dimension over a plain variable is checked. The
        // rank of a general expression, such as the result of `spread`, is
        // not reliably known before the array passes run, and a dimension
        // that is merely constant foldable is not worth guessing at here.
        if (!ASR::is_a<ASR::IntegerConstant_t>(*dim)) return;
        if (!ASR::is_a<ASR::Var_t>(*array)) return;
        ASR::ttype_t *array_type = typed_expr_type(array);
        if (array_type == nullptr) return;
        int rank = ASRUtils::extract_n_dims_from_ttype(array_type);
        if (rank <= 0) return;
        int64_t value = ASR::down_cast<ASR::IntegerConstant_t>(dim)->m_n;
        require_with_loc_id(
            value >= 1 && value <= rank,
            "asr.verify.array_dimension.dim_within_rank",
            std::string(name) + " dimension " + std::to_string(value) +
                " is out of range for an array of rank " +
                std::to_string(rank),
            loc);
    }

    void visit_ArrayBound(const ArrayBound_t &x) {
        verify_dimension_argument("ArrayBound", x.m_v, x.m_dim,
            x.base.base.loc);
        BaseWalkVisitor<VerifyVisitor>::visit_ArrayBound(x);
    }

    void visit_Array(const Array_t& x) {
        require(!ASR::is_a<ASR::Allocatable_t>(*x.m_type),
            "Allocatable cannot be inside array");
        bool _inside_array_physical_cast_type_copy = _inside_array_physical_cast_type;
        _inside_array_physical_cast_type = false;
        visit_ttype(*x.m_type);
        _inside_array_physical_cast_type = _inside_array_physical_cast_type_copy;
        if (x.m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
            require(x.n_dims == 0, "Assumed-rank arrays must have 0 dimensions");
            return ;
        }
        require(x.n_dims != 0, "Array type cannot have 0 dimensions.")
        require(!ASR::is_a<ASR::Array_t>(*x.m_type), "Array type cannot be nested.")
        if(ASRUtils::is_character(*x.m_type)){
            require(x.m_physical_type != ASR::FixedSizeArray,
                "Array of strings' physical type shouldn't be \"FixedSizeArray\"")
            // A "StringArraySinglePointer" array is one flat character buffer,
            // so its elements are plain C characters. Pairing it with
            // "DescriptorString" elements describes two different layouts at
            // once, and leaves every consumer to pick one on its own.
            if(x.m_physical_type == ASR::StringArraySinglePointer){
                ASR::String_t* str = ASR::down_cast<ASR::String_t>(
                    ASRUtils::extract_type(x.m_type));
                require(str->m_physical_type == ASR::CChar,
                    "Array of strings with physical type"
                    " \"StringArraySinglePointer\" must have string physical"
                    " type \"CChar\", not \"DescriptorString\"")
            }
        }
        if(ASRUtils::is_class_type(x.m_type)){
            require(x.m_physical_type != ASR::FixedSizeArray,
                "Array of classes can't be of physical type \"FixedSizeArray\"")
        }
        _processing_dims = true;
        for (size_t i = 0; i < x.n_dims; i++) {
            visit_dimension(x.m_dims[i]);
        }
        _processing_dims = false;
    }

    void visit_Pointer(const Pointer_t &x) {
        require(!ASR::is_a<ASR::Allocatable_t>(*x.m_type),
            "Pointer type conflicts with Allocatable type");
        if( ASR::is_a<ASR::Array_t>(*x.m_type) ) {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(x.m_type);
            for (size_t i = 0; i < array_t->n_dims; i++) {
                require(array_t->m_dims[i].m_length == nullptr,
                        "Array type in pointer must have deferred shape");
            }
        }
        visit_ttype(*x.m_type);
    }

    void visit_Allocatable(const Allocatable_t &x) {
        require(!ASR::is_a<ASR::Pointer_t>(*x.m_type) &&
                !ASR::is_a<ASR::Allocatable_t>(*x.m_type),
            "Allocatable type conflicts with Pointer type");
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(x.m_type, m_dims);
        for( size_t i = 0; i < n_dims; i++ ) {
            require(m_dims[i].m_length == nullptr,
                "Length of allocatable should be deferred (empty).");
        }
        visit_ttype(*x.m_type);
    }

    void visit_String(const String_t &x){
/*Check the character kind*/
        require(ASRUtils::is_supported_character_kind(x.m_kind),
            "String kind must be 1 or 4, found " + std::to_string(x.m_kind));
/*General Check on the length*/ 
        // The length may reference an ExternalSymbol, which cannot be
        // dereferenced before externals are resolved (e.g. during modfile
        // deserialization), so only check it when check_external is set.
        if(x.m_len && check_external){
            require(ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(
                ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(x.m_len)))),
                "String length must be of type INTEGER,"
                "found " +
                ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(x.m_len), x.m_len));
        }
// Check Positive Length
        if(x.m_len && check_external && ASRUtils::is_value_constant(x.m_len)){
            int64_t len{};
            ASRUtils::is_value_constant(x.m_len, len);
            require(len >= 0,
                "String length must be length >= 0\nCurrent length is -> " + std::to_string(len));
        }
/*Check Valid String type state based on the physical type*/
        if (x.m_physical_type == DescriptorString ||
            x.m_physical_type == CChar){
            std::string type_as_str = (x.m_physical_type == DescriptorString) ? "\"DescriptorString\"" : "\"CChar\"";
            if(x.m_len){
                require(x.m_len_kind == ExpressionLength,
                    "String of physical type " +
                    type_as_str +
                    " + existing length => must have length kind of \"ExpressionLength\".")
            } else {
                require(x.m_len_kind == AssumedLength ||
                        x.m_len_kind == DeferredLength ||
                        x.m_len_kind == ImplicitLength,
                    "String of physical type " +
                    type_as_str +
                    " + non-existing length => must have length kind of"
                    " \"AssumedLength\" OR \"DeferredLength\" OR \"ImplicitLength\".")
            }
        } else {
            throw LCompilersException("PhysicalType not checked (Probably a new physical type).");
        }
/*Check if implicitLength is used correctly*/
        if(x.m_len_kind == ASR::ImplicitLength){
            require(current_expr && ASR::is_a<ASR::StringPhysicalCast_t>(*current_expr),
                "Implicit length kind must appear in StringPhysicalCast expression.");
        }
        BaseWalkVisitor<VerifyVisitor>::visit_String(x);
    }
    void visit_StringPhysicalCast(const StringPhysicalCast_t &x){
        require(x.m_type, "x.m_type cannot be nullptr");
        ASR::ttype_t* cast_type = ASRUtils::type_get_past_allocatable(x.m_type);
        require(ASR::is_a<ASR::String_t>(*cast_type), "StringPhysicalCast should be of string type");
        ASR::String_t* str = ASR::down_cast<ASR::String_t>(cast_type);
        require(!str->m_len,
            "StringPhysicalCast return type shouldn't have length "
            "(Length should be implicit).")
        require(str->m_len_kind == ImplicitLength,
            "StringPhysicalCast expression should have length kind of \"ImplicitLength\".")
        BaseWalkVisitor<VerifyVisitor>::visit_StringPhysicalCast(x);
    }
    void visit_IfExp(const IfExp_t &x) {
        // Fortran 2023 conditional expression (10.1.2.3) and compiler
        // generated selections both land here. The condition selects one of
        // two arms at run time, so it must be a scalar logical, and both arms
        // must be usable as the result.
        ASR::ttype_t *test_type = typed_expr_type(x.m_test);
        if (test_type != nullptr) {
            require(ASRUtils::is_logical(*test_type),
                "IfExp condition must be logical");
            require(ASRUtils::extract_n_dims_from_ttype(test_type) == 0,
                "IfExp condition must be a scalar");
        }
        ASR::ttype_t *body_type = typed_expr_type(x.m_body);
        ASR::ttype_t *orelse_type = typed_expr_type(x.m_orelse);
        if (body_type != nullptr && orelse_type != nullptr
                && !is_procedure_type(body_type)
                && !is_procedure_type(orelse_type)
                && !is_struct_like_type(body_type)
                && !is_struct_like_type(orelse_type)) {
            require(ASRUtils::check_equal_type(body_type, orelse_type,
                    x.m_body, x.m_orelse),
                "IfExp arms must have the same type and kind, found "
                + ASRUtils::type_to_str_fortran_expr(body_type, x.m_body)
                + " and " + ASRUtils::type_to_str_fortran_expr(orelse_type,
                    x.m_orelse));
            require(ASRUtils::extract_n_dims_from_ttype(body_type)
                    == ASRUtils::extract_n_dims_from_ttype(orelse_type),
                "IfExp arms must have the same rank");
            require(ASRUtils::extract_n_dims_from_ttype(body_type)
                    == ASRUtils::extract_n_dims_from_ttype(x.m_type),
                "IfExp result must have the same rank as its arms");
        }
        BaseWalkVisitor<VerifyVisitor>::visit_IfExp(x);
    }

    void visit_StringSection(const StringSection_t &x){
        require(x.m_start, "StringSection start member must be provided")
        require(x.m_end, "StringSection end member must be provided")
        require(x.m_step, "StringSection step member must be provided")
        require(ASR::is_a<ASR::String_t>(*x.m_type), "StringSection return type must be a string")
        require(ASRUtils::get_string_type(x.m_type)->m_len, "StringSection's string-return node must have length expression (NOT nullptr)")
        BaseWalkVisitor<VerifyVisitor>::visit_StringSection(x);
    }


    void visit_Allocate(const Allocate_t &x) {
        if(check_external){
            if (x.m_source) {
                reject_implicit_trait_storage(x.m_source, x.m_source->base.loc);
            }
            for( size_t i = 0; i < x.n_args; i++ ) {
                auto *association = ASRUtils::association_variable(x.m_args[i].m_a);
                require_id(!association || association->m_intent != intentType::In,
                    "asr.verify.association.definable",
                    "A read-only data association cannot allocate a subobject");
                reject_implicit_trait_storage(x.m_args[i].m_a, x.base.base.loc);
                require(ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(x.m_args[i].m_a)) ||
                        ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x.m_args[i].m_a)),
                    "Allocate should only be called with  Allocatable or Pointer type inputs, found " +
                    std::string(ASRUtils::get_type_code(ASRUtils::expr_type(x.m_args[i].m_a))));
                ASR::ttype_t* alloc_arg_type = x.m_args[i].m_type;
                reject_implicit_trait_type(alloc_arg_type, x.m_args[i].loc);
                if ( alloc_arg_type && ASRUtils::is_struct(*alloc_arg_type) && x.m_args[i].m_sym_subclass != nullptr) {
                    require(ASR::is_a<ASR::Struct_t>(*ASRUtils::symbol_get_past_external(x.m_args[i].m_sym_subclass)),
                        "Allocate::m_sym_subclass must point to a Struct_t when the m_a member is of a type StructType");
                    // A polymorphic entity may only take a dynamic type its
                    // declared type is an ancestor of; anything else could
                    // not be reached through the declared type at all.
                    require_with_loc_id(
                        dynamic_type_is_compatible(
                            x.m_args[i].m_sym_subclass, x.m_args[i].m_a),
                        "asr.verify.allocate.dynamic_type_extends_declared",
                        "Allocate names the dynamic type '" +
                        std::string(ASRUtils::symbol_name(
                            x.m_args[i].m_sym_subclass)) +
                        "', which does not extend the declared type",
                        x.m_args[i].m_a->base.loc);
                }
                // Check Allocating a string OR an array of string with deferred length
                // Not providing length in Allocate statement with non-deferredLength is permissible
                if(!x.m_source &&
                    ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[i].m_a)) && 
                    ASRUtils::get_string_type(ASRUtils::expr_type(x.m_args[i].m_a))->m_len_kind == ASR::DeferredLength){
                    require(x.m_args[i].m_len_expr,
                        "Allocating a variable that's a string of deferred length requires providing a length to allocate with");
                }
            }

            if( x.m_source == nullptr ) {
                for( size_t i = 0; i < x.n_args; i++ ) {
                    if( ASRUtils::is_array(ASRUtils::expr_type(x.m_args[i].m_a)) ) {
                        require(x.m_args[i].n_dims > 0,
                            "Allocate for arrays should have dimensions specified, "
                            "found only array variable with no dimensions");
                    }
                }
            }
        }

        BaseWalkVisitor<VerifyVisitor>::visit_Allocate(x);
    }

    void verify_association_definable(expr_t *value) {
        auto *association = ASRUtils::association_variable(value);
        if (association) {
            require_with_loc_id(association->m_intent != intentType::In,
                "asr.verify.association.definable",
                "A read-only construct association cannot appear in a variable definition context",
                value->base.loc);
        }
    }

    void verify_io_item_definable(expr_t *item, bool input) {
        if (!item) return;
        if (ASR::is_a<ImpliedDoLoop_t>(*item)) {
            auto *loop = ASR::down_cast<ImpliedDoLoop_t>(item);
            verify_association_definable(loop->m_var);
            for (size_t i = 0; i < loop->n_values; i++) {
                verify_io_item_definable(loop->m_values[i], input);
            }
        } else if (!input && ASR::is_a<StringFormat_t>(*item)) {
            auto *format = ASR::down_cast<StringFormat_t>(item);
            for (size_t i = 0; i < format->n_args; i++) {
                verify_io_item_definable(format->m_args[i], false);
            }
        } else if (input) {
            verify_association_definable(item);
        }
    }

    void visit_FileRead(const FileRead_t &x) {
        for (auto *output : {x.m_iostat, x.m_iomsg, x.m_size, x.m_id}) {
            verify_association_definable(output);
        }
        for (size_t i = 0; i < x.n_values; i++) verify_io_item_definable(x.m_values[i], true);
        if (x.m_nml && check_external) {
            auto *group = ASRUtils::symbol_get_past_external(x.m_nml);
            require(group && ASR::is_a<Namelist_t>(*group), "FileRead requires a namelist group");
            auto *namelist = ASR::down_cast<Namelist_t>(group);
            for (size_t i = 0; i < namelist->n_var_list; i++) {
                auto *variable = ASRUtils::get_variable_from_symbol(
                    ASRUtils::symbol_get_past_external(namelist->m_var_list[i]));
                require_id(!variable || variable->m_storage != storage_typeType::Association ||
                        variable->m_intent != intentType::In,
                    "asr.verify.association.definable",
                    "A read-only construct association cannot be a namelist input item");
            }
        }
        BaseWalkVisitor<VerifyVisitor>::visit_FileRead(x);
    }

    void visit_FileWrite(const FileWrite_t &x) {
        for (auto *output : {x.m_iostat, x.m_iomsg, x.m_id}) {
            verify_association_definable(output);
        }
        auto *unit_type = typed_expr_type(x.m_unit);
        if (unit_type && ASRUtils::is_character(*unit_type)) verify_association_definable(x.m_unit);
        for (size_t i = 0; i < x.n_values; i++) verify_io_item_definable(x.m_values[i], false);
        BaseWalkVisitor<VerifyVisitor>::visit_FileWrite(x);
    }

    void visit_FileInquire(const FileInquire_t &x) {
        for (auto *output : {x.m_iostat, x.m_exist, x.m_opened, x.m_number,
                x.m_named, x.m_name, x.m_access, x.m_sequential, x.m_direct,
                x.m_form, x.m_formatted, x.m_unformatted, x.m_recl, x.m_nextrec,
                x.m_blank, x.m_position, x.m_action, x.m_read, x.m_write,
                x.m_readwrite, x.m_delim, x.m_pad, x.m_flen, x.m_blocksize,
                x.m_convert, x.m_carriagecontrol, x.m_size, x.m_pos, x.m_iolength,
                x.m_decimal, x.m_sign, x.m_encoding, x.m_stream, x.m_iomsg,
                x.m_round, x.m_pending, x.m_asynchronous}) {
            verify_association_definable(output);
        }
        for (size_t i = 0; i < x.n_iolength_vars; i++) {
            verify_io_item_definable(x.m_iolength_vars[i], false);
        }
        BaseWalkVisitor<VerifyVisitor>::visit_FileInquire(x);
    }

    void visit_FileOpen(const FileOpen_t &x) {
        // NEWUNIT is defined by the preceding explicit call; this node takes
        // the resulting unit number, just like OPEN(UNIT=...).
        verify_association_definable(x.m_iostat);
        verify_association_definable(x.m_iomsg);
        BaseWalkVisitor<VerifyVisitor>::visit_FileOpen(x);
    }

    void visit_FileClose(const FileClose_t &x) {
        verify_association_definable(x.m_iostat);
        verify_association_definable(x.m_iomsg);
        BaseWalkVisitor<VerifyVisitor>::visit_FileClose(x);
    }

    void visit_FileBackspace(const FileBackspace_t &x) {
        verify_association_definable(x.m_iostat);
        verify_association_definable(x.m_iomsg);
        BaseWalkVisitor<VerifyVisitor>::visit_FileBackspace(x);
    }

    void visit_FileRewind(const FileRewind_t &x) {
        verify_association_definable(x.m_iostat);
        verify_association_definable(x.m_iomsg);
        BaseWalkVisitor<VerifyVisitor>::visit_FileRewind(x);
    }

    void visit_FileEndfile(const FileEndfile_t &x) {
        verify_association_definable(x.m_iostat);
        verify_association_definable(x.m_iomsg);
        BaseWalkVisitor<VerifyVisitor>::visit_FileEndfile(x);
    }

    void visit_Flush(const Flush_t &x) {
        verify_association_definable(x.m_iostat);
        verify_association_definable(x.m_iomsg);
        BaseWalkVisitor<VerifyVisitor>::visit_Flush(x);
    }

    void verify_sync_stat_list(const std::string &stmt_name, const Location &loc, ASR::expr_t *stat, ASR::expr_t *errmsg,
            const std::string &stat_name="m_stat", const std::string &errmsg_name="m_errmsg") {
        if (stat) {
            ASR::ttype_t *stat_type = ASRUtils::expr_type(stat);
            require_with_loc(!ASRUtils::is_array(stat_type),
                stmt_name + "::" + stat_name + " must be a scalar", loc);
            require_with_loc(ASRUtils::is_integer(*stat_type),
                stmt_name + "::" + stat_name + " must be of integer type, found " +
                ASRUtils::type_to_str_fortran_expr(stat_type, stat), loc);
        }
        if (errmsg) {
            ASR::ttype_t *errmsg_type = ASRUtils::expr_type(errmsg);
            require_with_loc(!ASRUtils::is_array(errmsg_type),
                stmt_name + "::" + errmsg_name + " must be a scalar", loc);
            require_with_loc(ASRUtils::is_character(*errmsg_type),
                stmt_name + "::" + errmsg_name + " must be of string type, found " +
                ASRUtils::type_to_str_fortran_expr(errmsg_type, errmsg), loc);
        }
    }

    void visit_SyncAll(const SyncAll_t &x) {
        verify_sync_stat_list("SyncAll", x.base.base.loc, x.m_stat, x.m_errmsg);
        BaseWalkVisitor<VerifyVisitor>::visit_SyncAll(x);
    }

    void visit_SyncImages(const SyncImages_t &x) {
        if (x.m_image_set) {
            ASR::ttype_t *image_set_type = ASRUtils::expr_type(x.m_image_set);
            require(!ASRUtils::is_array(image_set_type) || ASRUtils::extract_n_dims_from_ttype(image_set_type) == 1,
                "SyncImages::m_image_set must be a scalar");
            require(ASRUtils::is_integer(*image_set_type),
                "SyncImages::m_image_set must be of integer type");
        }
        verify_sync_stat_list("SyncImages", x.base.base.loc, x.m_stat, x.m_errmsg);
        BaseWalkVisitor<VerifyVisitor>::visit_SyncImages(x);
    }

    void visit_SyncMemory(const SyncMemory_t &x) {
        verify_sync_stat_list("SyncMemory", x.base.base.loc, x.m_stat, x.m_errmsg);
        BaseWalkVisitor<VerifyVisitor>::visit_SyncMemory(x);
    }

    void visit_SyncTeam(const SyncTeam_t &x) {
        verify_sync_stat_list("SyncTeam", x.base.base.loc, x.m_stat, x.m_errmsg);
        BaseWalkVisitor<VerifyVisitor>::visit_SyncTeam(x);
    }

    void visit_ChangeTeam(const ChangeTeam_t &x) {
        verify_sync_stat_list("ChangeTeam", x.base.base.loc, x.m_stat, x.m_errmsg);
        verify_sync_stat_list("ChangeTeam", x.base.base.loc, x.m_end_stat, x.m_end_errmsg, "m_end_stat", "m_end_errmsg");
        BaseWalkVisitor<VerifyVisitor>::visit_ChangeTeam(x);
    }

    void visit_FormTeam(const FormTeam_t &x) {
        ASR::ttype_t *team_number_type = ASRUtils::expr_type(x.m_team_number);
        require(!ASRUtils::is_array(team_number_type),
            "FormTeam::m_team_number must be a scalar");
        require(ASRUtils::is_integer(*team_number_type),
            "FormTeam::m_team_number must be of integer type");

        if (x.m_new_index) {
            ASR::ttype_t *new_index_type = ASRUtils::expr_type(x.m_new_index);
            require(!ASRUtils::is_array(new_index_type),
                "FormTeam::m_new_index must be a scalar");
            require(ASRUtils::is_integer(*new_index_type),
                "FormTeam::m_new_index must be of integer type");
        }
        verify_sync_stat_list("FormTeam", x.base.base.loc, x.m_stat, x.m_errmsg);
        BaseWalkVisitor<VerifyVisitor>::visit_FormTeam(x);
    }

    void visit_DoConcurrentLoop(const DoConcurrentLoop_t &x) {
        for ( size_t i = 0; i < x.n_local; i++ ) {
            require(ASR::is_a<ASR::Var_t>(*x.m_local[i]),
                "DoConcurrentLoop::m_local must be a Var");
        }
        for ( size_t i = 0; i < x.n_shared; i++ ) {
            require(ASR::is_a<ASR::Var_t>(*x.m_shared[i]),
                "DoConcurrentLoop::m_shared must be a Var");
        }
        BaseWalkVisitor<VerifyVisitor>::visit_DoConcurrentLoop(x);
    }

    void visit_OMPRegion(const OMPRegion_t &x) {
        for ( size_t i = 0; i < x.n_clauses; i++ ) {
            ASR::expr_t **vars = nullptr;
            size_t n_vars = 0;
            switch (x.m_clauses[i]->type) {
                case ASR::omp_clauseType::OMPPrivate: {
                    ASR::OMPPrivate_t *c = ASR::down_cast<ASR::OMPPrivate_t>(
                        x.m_clauses[i]);
                    vars = c->m_vars; n_vars = c->n_vars;
                    break;
                }
                case ASR::omp_clauseType::OMPShared: {
                    ASR::OMPShared_t *c = ASR::down_cast<ASR::OMPShared_t>(
                        x.m_clauses[i]);
                    vars = c->m_vars; n_vars = c->n_vars;
                    break;
                }
                case ASR::omp_clauseType::OMPFirstPrivate: {
                    ASR::OMPFirstPrivate_t *c =
                        ASR::down_cast<ASR::OMPFirstPrivate_t>(x.m_clauses[i]);
                    vars = c->m_vars; n_vars = c->n_vars;
                    break;
                }
                case ASR::omp_clauseType::OMPLastPrivate: {
                    ASR::OMPLastPrivate_t *c =
                        ASR::down_cast<ASR::OMPLastPrivate_t>(x.m_clauses[i]);
                    vars = c->m_vars; n_vars = c->n_vars;
                    break;
                }
                case ASR::omp_clauseType::OMPReduction: {
                    ASR::OMPReduction_t *c =
                        ASR::down_cast<ASR::OMPReduction_t>(x.m_clauses[i]);
                    vars = c->m_vars; n_vars = c->n_vars;
                    break;
                }
                default: break;
            }
            for ( size_t j = 0; j < n_vars; j++ ) {
                require(ASR::is_a<ASR::Var_t>(*vars[j]),
                    "a variable named by an OMPRegion clause must be a Var");
            }
        }
        BaseWalkVisitor<VerifyVisitor>::visit_OMPRegion(x);
    }

};


} // namespace ASR

bool asr_verify(const ASR::TranslationUnit_t &unit,
            const ASRVerifyOptions &options,
            diag::Diagnostics &diagnostics) {
    ASR::VerifyVisitor v(options.check_external, diagnostics);
    v.check_string_length_arguments = options.string_length_arguments;
    try {
        v.visit_TranslationUnit(unit);
    } catch (const ASRUtils::VerifyAbort &) {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return false;
    }
    if (options.require_main_program) {
        const ASR::Program_t *main_program = nullptr;
        for (const auto &item : unit.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Program_t>(*item.second)) {
                continue;
            }
            if (main_program != nullptr) {
                diagnostics.message_label(
                    "standalone ASR must contain exactly one main program",
                    {item.second->base.loc}, "second main program",
                    diag::Level::Error, diag::Stage::ASRVerify,
                    "asr.verify.translation_unit.multiple_main_programs");
                return false;
            }
            main_program = ASR::down_cast<ASR::Program_t>(item.second);
        }
        if (main_program == nullptr) {
            diagnostics.message_label(
                "standalone ASR must contain exactly one main program",
                {unit.base.base.loc}, "main program is missing",
                diag::Level::Error, diag::Stage::ASRVerify,
                "asr.verify.translation_unit.main_program_missing");
            return false;
        }
    }
    return true;
}

bool asr_verify(const ASR::TranslationUnit_t &unit, bool check_external,
            diag::Diagnostics &diagnostics) {
    ASRVerifyOptions options;
    options.check_external = check_external;
    return asr_verify(unit, options, diagnostics);
}

} // namespace LCompilers
