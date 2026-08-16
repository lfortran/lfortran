#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <set>

namespace LCompilers {

namespace ASR {

using ASRUtils::symbol_name;
using ASRUtils::symbol_parent_symtab;

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

// Returns the type of `e`, or nullptr if the expression is malformed in a way
// that makes its type unobtainable (a Var referencing a symbol that carries no
// type, such as a Program). Such expressions have their own dedicated verifier
// checks, so type comparisons must simply be skipped for them instead of
// asking for a type that does not exist.
static ttype_t* typed_expr_type(const expr_t *e)
{
    if (e == nullptr) return nullptr;
    if (!is_a<Var_t>(*e)) return ASRUtils::expr_type(e);
    symbol_t *s = down_cast<Var_t>(e)->m_v;
    if (s == nullptr) return nullptr;
    if (is_a<ExternalSymbol_t>(*s)) {
        s = down_cast<ExternalSymbol_t>(s)->m_external;
        if (s == nullptr || is_a<ExternalSymbol_t>(*s)) return nullptr;
    }
    if (!is_a<Function_t>(*s) && !is_a<Variable_t>(*s)
            && !is_a<Struct_t>(*s)) {
        return nullptr;
    }
    return ASRUtils::expr_type(e);
}

// Procedure types are compared by their own dedicated checks, and they have no
// type code, so they cannot take part in the generic type comparisons below.
static bool is_procedure_type(ttype_t *t)
{
    return t != nullptr
        && is_a<FunctionType_t>(*ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(t)));
}

// A StructType spells out its member types inline, so two StructType nodes for
// the same derived type can differ structurally, for example when a pass
// rewrites the signature of a procedure pointer component in one of them.
// Derived type identity is established from the struct symbol, which a bare
// signature type does not carry, so such types are left to the dedicated
// struct checks.
static bool is_struct_like_type(ttype_t *t)
{
    if (t == nullptr) return false;
    ttype_t *t2 = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(t));
    return is_a<StructType_t>(*t2) || ASRUtils::is_class_type(t2);
}

class VerifyVisitor : public BaseWalkVisitor<VerifyVisitor>
{
private:
    // For checking correct parent symbtab relationship
    SymbolTable *current_symtab;
    bool check_external;
    bool check_standalone_rules;
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

    // checks whether we've visited any `Var`, which isn't a global `Variable`
    bool non_global_symbol_visited;
    bool _is_return_type_string;
    bool _return_var_or_intent_out = false;
    bool _processing_dims = false;
    bool _inside_call = false;
    bool _inside_array_physical_cast_type = false;
    bool _processing_assumed_rank_array = false;
    bool _processing_unbounded_pointer_array = false;
    const ASR::expr_t* current_expr {}; // current expression being visited 

public:
    VerifyVisitor(bool check_external, bool check_standalone_rules,
        diag::Diagnostics &diagnostics) : check_external{check_external},
        check_standalone_rules{check_standalone_rules},
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
        current_symtab = nullptr;
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
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
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
            // A complete graph carries every module it uses. A dependency
            // with nothing behind it names a module whose declarations the
            // compiler will never see, which is the shape a lost `use` takes.
            if (check_standalone_rules) {
                require_id(parent_symtab->get_symbol(
                        std::string(x.m_dependencies[i])) != nullptr,
                    "asr.verify.module.dependency_is_present",
                    "Module '" + std::string(x.m_name) + "' depends on '" +
                    std::string(x.m_dependencies[i]) +
                    "', which is not in this translation unit");
            }
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
            verify_conforming_signatures(
                "Module procedure '" + std::string(impl->m_name) +
                "' implementing the interface in module '" +
                std::string(x.m_parent_module) + "'",
                impl, declared, declared->n_args + 1,
                "asr.verify.module_procedure", x.base.base.loc);
        }
    }

    // Associating a procedure pointer fixes what every later call through it
    // is compiled against, so the procedure has to have the interface the
    // pointer was declared with.
    void visit_Associate(const Associate_t &x) {
        BaseWalkVisitor<VerifyVisitor>::visit_Associate(x);
        if (!check_external || x.m_target == nullptr || x.m_value == nullptr) {
            return;
        }
        // Only for a complete graph: `procedure(), pointer` declares no
        // interface at all and accepts any procedure, and the frontend gives
        // it the same empty FunctionType an explicit no-argument interface
        // gets, so the two cannot be told apart here.
        if (!check_standalone_rules) return;
        ASR::ttype_t *target = typed_expr_type(x.m_target);
        ASR::ttype_t *value = typed_expr_type(x.m_value);
        if (target == nullptr || value == nullptr) return;
        verify_procedure_interface(value, target,
            "Procedure pointer association", x.base.base.loc);
    }

    void visit_Assignment(const Assignment_t& x) {
        ASR::expr_t* target = x.m_target;
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
        if (x.m_move_allocation) {
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
    }

    // The position of the passed-object dummy argument of a binding, or
    // `n_args` when the binding has none.
    size_t passed_object_index(const StructMethodDeclaration_t &x,
            ASR::Function_t *proc) {
        if (x.m_is_nopass) return proc->n_args;
        if (x.m_self_argument == nullptr) return 0;
        std::string self_name = x.m_self_argument;
        for (size_t i = 0; i < proc->n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*proc->m_args[i])) continue;
            if (self_name == std::string(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(proc->m_args[i])->m_v))) {
                return i;
            }
        }
        return proc->n_args;
    }

    // The binding of the same name that `x` overrides, searched up the
    // parent chain of the derived type `x` belongs to, or nullptr.
    ASR::StructMethodDeclaration_t* overridden_binding(
            const StructMethodDeclaration_t &x) {
        SymbolTable *symtab = x.m_parent_symtab;
        if (symtab == nullptr || symtab->asr_owner == nullptr ||
                !ASR::is_a<ASR::symbol_t>(*symtab->asr_owner)) {
            return nullptr;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(symtab->asr_owner);
        if (!ASR::is_a<ASR::Struct_t>(*owner)) return nullptr;
        ASR::symbol_t *parent = ASR::down_cast<ASR::Struct_t>(owner)->m_parent;
        // A parent cycle is diagnosed on its own; here it must only not loop.
        std::set<const ASR::Struct_t*> seen;
        while (parent != nullptr) {
            parent = ASRUtils::symbol_get_past_external(parent);
            if (parent == nullptr || !ASR::is_a<ASR::Struct_t>(*parent)) {
                return nullptr;
            }
            ASR::Struct_t *s = ASR::down_cast<ASR::Struct_t>(parent);
            if (!seen.insert(s).second) return nullptr;
            ASR::symbol_t *sym = s->m_symtab->get_symbol(std::string(x.m_name));
            if (sym != nullptr &&
                    ASR::is_a<ASR::StructMethodDeclaration_t>(*sym)) {
                return ASR::down_cast<ASR::StructMethodDeclaration_t>(sym);
            }
            parent = s->m_parent;
        }
        return nullptr;
    }

    // Fortran 2018 7.5.7.3: an overriding type-bound procedure and the one it
    // overrides must have the same interface apart from the passed-object
    // dummy argument. Nothing downstream re-derives this, so a mismatch means
    // a dispatch through the parent type calls a procedure whose signature
    // does not match the call site the parent's interface promised.
    void verify_binding_override(const StructMethodDeclaration_t &x,
            ASR::Function_t *proc) {
        if (!check_external) return;
        ASR::StructMethodDeclaration_t *base_decl = overridden_binding(x);
        if (base_decl == nullptr) return;
        ASR::symbol_t *base_sym =
            ASRUtils::symbol_get_past_external(base_decl->m_proc);
        if (base_sym == nullptr || !ASR::is_a<ASR::Function_t>(*base_sym)) {
            return;
        }
        ASR::Function_t *base = ASR::down_cast<ASR::Function_t>(base_sym);
        // An inherited binding names the very same procedure; only a binding
        // that names a different one overrides anything.
        if (base == proc) return;

        std::string what = "Type bound procedure '" + std::string(x.m_name) +
            "' overriding '" + std::string(base->m_name) + "'";
        require_id(x.m_is_nopass == base_decl->m_is_nopass,
            "asr.verify.binding_override.nopass_matches",
            what + " must agree on the NOPASS attribute");
        size_t self_index = passed_object_index(x, proc);
        size_t base_self_index = passed_object_index(*base_decl, base);
        require_id(self_index == base_self_index,
            "asr.verify.binding_override.passed_object_matches",
            what + " must take its passed-object dummy argument in the same "
            "position");
        // The passed-object dummy argument is declared with the type it is
        // bound to, so the two deliberately differ there.
        verify_conforming_signatures(what, proc, base, self_index,
            "asr.verify.binding_override", x.base.base.loc);
    }

    // Two procedures that must present the same interface. `skip` is the
    // position of the one dummy argument they may declare differently, or
    // `n_args` when there is none.
    void verify_conforming_signatures(const std::string &what,
            ASR::Function_t *impl, ASR::Function_t *decl, size_t skip,
            const std::string &prefix, const Location &loc) {
        bool decl_is_function = decl->m_return_var != nullptr;
        bool is_function = impl->m_return_var != nullptr;
        require_with_loc_id(decl_is_function == is_function,
            prefix + ".result_kind_matches",
            what + " must be a " +
            std::string(decl_is_function ? "function" : "subroutine"), loc);
        if (decl_is_function && is_function) {
            ASR::ttype_t *decl_type = typed_expr_type(decl->m_return_var);
            ASR::ttype_t *type = typed_expr_type(impl->m_return_var);
            if (decl_type != nullptr && type != nullptr &&
                    !is_struct_like_type(decl_type) &&
                    !is_struct_like_type(type)) {
                require_with_loc_id(ASRUtils::check_equal_type(type, decl_type,
                        type_context(impl->m_return_var),
                        type_context(decl->m_return_var)),
                    prefix + ".result_type_matches",
                    what + " must return " +
                    ASRUtils::get_type_code(decl_type) + ", not " +
                    ASRUtils::get_type_code(type), loc);
            }
        }
        require_with_loc_id(impl->n_args == decl->n_args,
            prefix + ".argument_count_matches",
            what + " must take " + std::to_string(decl->n_args) +
            " arguments, not " + std::to_string(impl->n_args), loc);
        for (size_t i = 0; i < impl->n_args; i++) {
            if (i == skip) continue;
            verify_conforming_argument(what, i, impl, decl, prefix, loc);
        }
    }

    void verify_conforming_argument(const std::string &what, size_t i,
            ASR::Function_t *proc, ASR::Function_t *base,
            const std::string &prefix, const Location &loc) {
        if (!ASR::is_a<ASR::Var_t>(*proc->m_args[i]) ||
                !ASR::is_a<ASR::Var_t>(*base->m_args[i])) {
            return;
        }
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(proc->m_args[i])->m_v;
        ASR::symbol_t *base_sym =
            ASR::down_cast<ASR::Var_t>(base->m_args[i])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*sym) ||
                !ASR::is_a<ASR::Variable_t>(*base_sym)) {
            return;
        }
        ASR::Variable_t *arg = ASR::down_cast<ASR::Variable_t>(sym);
        ASR::Variable_t *base_arg = ASR::down_cast<ASR::Variable_t>(base_sym);
        std::string which = what + ", argument " + std::to_string(i + 1) +
            " '" + std::string(arg->m_name) + "',";
        require_with_loc_id(arg->m_intent == base_arg->m_intent,
            prefix + ".argument_intent_matches",
            which + " must have the same intent as '" +
            std::string(base_arg->m_name) + "'", loc);
        require_with_loc_id(arg->m_presence == base_arg->m_presence,
            prefix + ".argument_presence_matches",
            which + " must agree with '" + std::string(base_arg->m_name) +
            "' on the OPTIONAL attribute", loc);
        ASR::ttype_t *type = arg->m_type;
        ASR::ttype_t *base_type = base_arg->m_type;
        if (type == nullptr || base_type == nullptr) return;
        require_with_loc_id(ASRUtils::is_allocatable(type) ==
                ASRUtils::is_allocatable(base_type),
            prefix + ".argument_allocatable_matches",
            which + " must agree with '" + std::string(base_arg->m_name) +
            "' on the ALLOCATABLE attribute", loc);
        require_with_loc_id(ASRUtils::is_pointer(type) ==
                ASRUtils::is_pointer(base_type),
            prefix + ".argument_pointer_matches",
            which + " must agree with '" + std::string(base_arg->m_name) +
            "' on the POINTER attribute", loc);
        require_with_loc_id(ASRUtils::extract_n_dims_from_ttype(type) ==
                ASRUtils::extract_n_dims_from_ttype(base_type),
            prefix + ".argument_rank_matches",
            which + " must have rank " + std::to_string(
                ASRUtils::extract_n_dims_from_ttype(base_type)), loc);
        // A derived type argument spells its members out inline, so two
        // structurally different types can name the same type; those are
        // compared by the dedicated struct checks instead.
        if (is_struct_like_type(type) || is_struct_like_type(base_type) ||
                is_procedure_type(type) || is_procedure_type(base_type)) {
            return;
        }
        require_with_loc_id(ASRUtils::check_equal_type(type, base_type,
                type_context(proc->m_args[i]),
                type_context(base->m_args[i])),
            prefix + ".argument_type_matches",
            which + " must have type " +
            ASRUtils::get_type_code(base_type) + ", not " +
            ASRUtils::get_type_code(type), loc);
    }

    // A procedure's dummy variables and its result variable are declared by
    // the procedure itself. One that resolves in an enclosing scope instead
    // is a host variable the procedure would then write through as if it
    // owned it. A dummy procedure is exempt: it names the procedure symbol
    // itself, which lives where that procedure was declared. Only for a
    // complete graph: a procedure the frontend synthesizes for an implicit
    // interface borrows both its dummies and its result from the caller.
    void require_own_symbol(ASR::expr_t *e, const std::string &owner,
            const std::string &what) {
        if (!check_standalone_rules) return;
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
        for (size_t i=0; i<x.n_body; i++) {
            LCOMPILERS_ASSERT(x.m_body[i]);
            visit_stmt(*x.m_body[i]);
        }
        if (x.m_return_var) {
            require_own_symbol(x.m_return_var, func_name, "result variable");
            visit_expr(*x.m_return_var);
        }

        verify_unique_dependencies(x.m_dependencies, x.n_dependencies,
                                   x.m_name, x.base.base.loc);
        verify_elemental_arguments(x);

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

    void visit_GpuKernelFunction(const GpuKernelFunction_t &x) {
        SymbolTable *parent_symtab = current_symtab;
        current_symtab = x.m_symtab;
        require(x.m_symtab != nullptr,
            "GpuKernelFunction::m_symtab cannot be nullptr");
        require(x.m_symtab->parent == parent_symtab,
            "GpuKernelFunction::m_symtab->parent is not the right parent");
        require(x.m_symtab->asr_owner == (ASR::asr_t*)&x,
            "GpuKernelFunction::m_symtab::asr_owner must point to it");
        require(id_symtab_map.find(x.m_symtab->counter) == id_symtab_map.end(),
            "GpuKernelFunction::m_symtab->counter must be unique");
        id_symtab_map[x.m_symtab->counter] = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            LCOMPILERS_ASSERT(a.second);
            this->visit_symbol(*a.second);
        }
        visit_ttype(*x.m_function_signature);
        for (size_t i=0; i<x.n_args; i++) {
            visit_expr(*x.m_args[i]);
        }
        for (size_t i=0; i<x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }
        current_symtab = parent_symtab;
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

    // A derived type extends another derived type and nothing else. Every
    // member lookup, every dispatch and every layout decision walks this
    // chain, so a parent that is not a type is followed straight into the
    // wrong node.
    void visit_Struct(const Struct_t& x) {
        if (x.m_parent != nullptr) {
            ASR::symbol_t *parent = check_external
                ? ASRUtils::symbol_get_past_external(x.m_parent) : x.m_parent;
            require_id(parent != nullptr &&
                    (ASR::is_a<ASR::Struct_t>(*parent) ||
                     ASR::is_a<ASR::ExternalSymbol_t>(*parent)),
                "asr.verify.struct.parent_is_struct",
                "Struct::m_parent of '" + std::string(x.m_name) +
                "' must be a derived type, not " +
                ASRUtils::symbol_type_name(*x.m_parent));
            // Only for a complete graph: a specialization of a parameterized
            // derived type is created in the scope that instantiates it and
            // extends a type declared elsewhere.
            if (check_standalone_rules) {
                require_id(symtab_in_scope(current_symtab, x.m_parent),
                    "asr.verify.struct.parent_in_scope",
                    "Struct::m_parent of '" + std::string(x.m_name) +
                    "' cannot point outside of its symbol table");
            }
        }
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
                            (x.m_symbolic_value != nullptr && ASRUtils::is_value_constant(array_construct->m_args[j])),
                            "Initialisation of " + std::string(x.m_name) +
                            " must reduce to a compile time constant.");
                }
            } else {
                require( (x.m_symbolic_value == nullptr && x.m_value == nullptr) ||
                        (x.m_symbolic_value != nullptr && x.m_value != nullptr) ||
                        (x.m_symbolic_value != nullptr && ASRUtils::is_value_constant(x.m_symbolic_value)),
                        "Initialisation of " + std::string(x.m_name) +
                        " must reduce to a compile time constant.");
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
            // Only for a complete graph: pass_array_by_data rebuilds a
            // procedure in a fresh scope while its variables still name the
            // interface in the scope they came from.
            if (check_standalone_rules) {
                require_id(
                    symtab_in_scope(current_symtab, x.m_type_declaration),
                    "asr.verify.variable.type_declaration_in_scope",
                    "Variable '" + std::string(x.m_name) +
                    "' declares its type with a symbol that is not in scope");
            }
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
            ASR::GpuKernelFunction_t* gkfm = nullptr;
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
                                  ASR::is_a<ASR::Union_t>(*asr_owner_sym) ||
                                  ASR::is_a<ASR::GpuKernelFunction_t>(*asr_owner_sym));
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
                } else if( ASR::is_a<ASR::GpuKernelFunction_t>(*asr_owner_sym) ) {
                    gkfm = ASR::down_cast<ASR::GpuKernelFunction_t>(asr_owner_sym);
                    asr_owner_name = gkfm->m_name;
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
            } else if( gkfm ) {
                s = gkfm->m_symtab->resolve_symbol(std::string(x.m_original_name));
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
        BaseWalkVisitor::visit_ImplicitDeallocate(x);
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
            if( ASRUtils::is_array_indexed_with_array_indices(x.m_args, x.n_args) ) {
                require(ASRUtils::is_array(x.m_type),
                    "ArrayItem::m_type with array indices must be an array.")
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
        if (!check_external || x.m_m == nullptr || x.m_v == nullptr ||
                diagnostics.has_error()) {
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
            if (method->m_proc && ASR::is_a<ASR::Function_t>(*method->m_proc)) {
                func = ASR::down_cast<ASR::Function_t>(method->m_proc);
                nopass = method->m_is_nopass;
            }
        } else if (func_sym && ASR::is_a<ASR::Function_t>(*func_sym)) {
            func = ASR::down_cast<ASR::Function_t>(func_sym);
        } else if (func_sym && ASR::is_a<ASR::Variable_t>(*func_sym)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(func_sym);
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
                // Derived type arguments are skipped for the same reason
                // as in the signature check above, and this also covers a
                // polymorphic argument passed as a pointer or allocatable,
                // whose type only looks like a plain struct once those
                // wrappers are stripped.
                bool struct_argument = is_struct_like_type(actual_type)
                    || is_struct_like_type(formal_type);
                bool procedure_argument = is_procedure_type(actual_type)
                    || is_procedure_type(formal_type);
                if (procedure_argument && check_standalone_rules) {
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
                    // These wrapper and rank rules hold for a complete
                    // standalone graph. After a pass the dummy may have been
                    // rewritten (openmp turns allocatable into pointer;
                    // pass_array_by_data changes ranks), so they are not
                    // applied to intermediate ASR.
                    if (check_standalone_rules) {
                        // check_equal_type strips Allocatable and Pointer.
                        // An allocatable or pointer dummy requires an actual
                        // of the same wrapper; the other direction is valid
                        // Fortran (an allocatable actual may be passed to a
                        // nonallocatable dummy). A scalar actual for an array
                        // dummy (or the converse) is invalid, except for
                        // assumed-rank and elemental. Sequence association can
                        // pass a 2-D actual to a 1-D dummy, so ranks of two
                        // arrays need not match.
                        if (ASRUtils::is_allocatable(formal_type)) {
                            require_with_loc_id(
                                ASRUtils::is_allocatable(actual_type),
                                "asr.verify.call.actual_allocatable_matches_formal",
                                "Actual argument type " +
                                    ASRUtils::get_type_code(actual_type) +
                                    " is not allocatable, but the dummy is " +
                                    ASRUtils::get_type_code(formal_type),
                                passed_arg_expr->base.loc);
                        }
                        if (ASRUtils::is_pointer(formal_type)) {
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
                        if (!formal_assumed_rank && !elemental) {
                            bool actual_is_array =
                                ASRUtils::is_array(actual_type);
                            bool formal_is_array =
                                ASRUtils::is_array(formal_type);
                            require_with_loc_id(
                                actual_is_array == formal_is_array,
                                "asr.verify.call.actual_rank_matches_formal",
                                "Actual argument type " +
                                    ASRUtils::get_type_code(actual_type) +
                                    " does not match formal argument rank of "
                                    "type " +
                                    ASRUtils::get_type_code(formal_type),
                                passed_arg_expr->base.loc);
                        }
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

    void visit_SubroutineCall(const SubroutineCall_t &x) {
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
            // The call site's result type is what the surrounding expression
            // was typed against; the callee's is what the call actually
            // produces. Where they disagree, the two disagree about the call.
            ASR::ttype_t *returned = typed_expr_type(fn_->m_return_var);
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
        visit_ttype(*x.m_type);
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
        BaseWalkVisitor<VerifyVisitor>::visit_ArrayConstructor(x);
    }

    void visit_ArrayConstant(const ArrayConstant_t& x) {
        require(ASRUtils::is_array(x.m_type),
            "Type of ArrayConstant must be an array");

        int64_t n_data = ASRUtils::get_fixed_size_of_array(x.m_type) * ASRUtils::extract_kind_from_ttype_t(x.m_type);
        ASR::ttype_t* inner = ASRUtils::type_get_past_array(x.m_type);
        if (ASRUtils::is_character(*x.m_type)) {
            ASR::ttype_t* t = ASRUtils::type_get_past_array(x.m_type);
            int64_t len;
            require(ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(t)->m_len, len), "Constant array of strings should have constant string length");
            n_data = ASRUtils::get_fixed_size_of_array(x.m_type) * len;
        } else if (ASR::is_a<ASR::StructType_t>(*inner)) {
            n_data = ASRUtils::get_fixed_size_of_array(x.m_type) * sizeof(ASR::expr_t*);
        } else if (ASR::is_a<ASR::CPtr_t>(*inner)) {
          // C_PTR and C_FUNPTR have no fortran kind parameter.
          n_data = ASRUtils::get_fixed_size_of_array(x.m_type) * sizeof(void*);
        }
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
            x.m_kind == 4 || x.m_kind == 8 || x.m_kind == 16 ||
                x.m_kind >= 1000,
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

    // A StructConstructor's arguments fill the type's members in
    // declaration order, parent members first, and a later pass lowers it
    // into one assignment per member. A type that disagrees with its member
    // therefore surfaces as a broken assignment inside that pass rather than
    // here, so it is checked up front. The pass also indexes the member list
    // positionally, so a count mismatch is a memory error waiting to happen.
    void visit_StructConstructor(const StructConstructor_t &x) {
        ASR::symbol_t *struct_sym = x.m_dt_sym == nullptr
            ? nullptr : ASRUtils::symbol_get_past_external(x.m_dt_sym);
        if (!diagnostics.has_error() && struct_sym != nullptr
                && ASR::is_a<ASR::Struct_t>(*struct_sym)) {
            std::vector<ASR::Struct_t*> chain;
            std::set<ASR::Struct_t*> seen;
            ASR::Struct_t *struct_type =
                ASR::down_cast<ASR::Struct_t>(struct_sym);
            while (struct_type != nullptr) {
                require_id(seen.insert(struct_type).second,
                    "asr.verify.struct_constructor.parent_chain_acyclic",
                    "StructConstructor type '" +
                        std::string(struct_type->m_name) +
                        "' has a cyclic parent chain");
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
            require_id(members.size() == x.n_args,
                "asr.verify.struct_constructor.argument_count",
                "StructConstructor has " + std::to_string(x.n_args) +
                    " arguments but the type has " +
                    std::to_string(members.size()) + " members");
            if (members.size() == x.n_args) {
                for (size_t i = 0; i < x.n_args; i++) {
                    ASR::ttype_t *actual =
                        typed_expr_type(x.m_args[i].m_value);
                    if (actual == nullptr || members[i] == nullptr
                            || !ASR::is_a<ASR::Variable_t>(*members[i])) {
                        continue;
                    }
                    ASR::ttype_t *declared =
                        ASR::down_cast<ASR::Variable_t>(members[i])->m_type;
                    if (declared == nullptr) continue;
                    ASR::ttype_t *member_scalar =
                        ASRUtils::type_get_past_array(
                            ASRUtils::type_get_past_allocatable_pointer(
                                declared));
                    ASR::ttype_t *actual_scalar =
                        ASRUtils::type_get_past_array(
                            ASRUtils::type_get_past_allocatable_pointer(
                                actual));
                    if (is_struct_like_type(member_scalar)
                            || is_procedure_type(member_scalar)
                            || is_struct_like_type(actual_scalar)
                            || is_procedure_type(actual_scalar)) {
                        continue;
                    }
                    require_with_loc_id(
                        ASRUtils::check_equal_type(
                            member_scalar, actual_scalar, nullptr, nullptr),
                        "asr.verify.struct_constructor.argument_type_matches_member",
                        "StructConstructor argument type " +
                            ASRUtils::get_type_code(actual_scalar) +
                            " does not match member '" +
                            std::string(ASRUtils::symbol_name(members[i])) +
                            "' of type " +
                            ASRUtils::get_type_code(member_scalar),
                        x.m_args[i].m_value->base.loc);
                }
            }
        }
        BaseWalkVisitor<VerifyVisitor>::visit_StructConstructor(x);
    }

    // `dim` selects one of the array's dimensions, so a constant outside
    // 1..rank is invalid. The pass that folds these intrinsics indexes
    // `dims[dim - 1]` directly, so an out of range constant reads outside the
    // dimension array and crashes the compiler rather than diagnosing it.
    void verify_dimension_argument(const char *name, ASR::expr_t *array,
            ASR::expr_t *dim, const Location &loc) {
        if (!check_standalone_rules || diagnostics.has_error()
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
/*General Check on the length*/ 
        if(x.m_len){
            require(ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(
                ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(x.m_len)))),
                "String length must be of type INTEGER,"
                "found " +
                ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(x.m_len), x.m_len));
        }
// Check Positive Length
        if(x.m_len && ASRUtils::is_value_constant(x.m_len)){
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
            for( size_t i = 0; i < x.n_args; i++ ) {
                require(ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(x.m_args[i].m_a)) ||
                        ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x.m_args[i].m_a)),
                    "Allocate should only be called with  Allocatable or Pointer type inputs, found " +
                    std::string(ASRUtils::get_type_code(ASRUtils::expr_type(x.m_args[i].m_a))));
                ASR::ttype_t* alloc_arg_type = x.m_args[i].m_type;
                if ( alloc_arg_type && ASRUtils::is_struct(*alloc_arg_type) && x.m_args[i].m_sym_subclass != nullptr) {
                    require(ASR::is_a<ASR::Struct_t>(*ASRUtils::symbol_get_past_external(x.m_args[i].m_sym_subclass)),
                        "Allocate::m_sym_subclass must point to a Struct_t when the m_a member is of a type StructType");
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

};


} // namespace ASR

bool asr_verify(const ASR::TranslationUnit_t &unit,
            const ASRVerifyOptions &options,
            diag::Diagnostics &diagnostics) {
    ASR::VerifyVisitor v(options.check_external,
        options.check_standalone_rules, diagnostics);
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
