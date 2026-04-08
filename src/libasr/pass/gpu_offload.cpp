#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/pass/replace_gpu_offload.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/stmt_walk_visitor.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/string_utils.h>

#include <filesystem>
#include <map>
#include <set>
#include <string>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

static int gpu_kernel_counter = 0;

// Collects all symbols referenced in expressions/statements
class GpuSymbolCollector : public ASR::BaseWalkVisitor<GpuSymbolCollector> {
public:
    Allocator &al;
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &symbols;
    std::set<SymbolTable*> enclosing_scopes;

    GpuSymbolCollector(Allocator &al_,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &syms,
        const std::set<SymbolTable*> &scopes = {})
        : al(al_), symbols(syms), enclosing_scopes(scopes) {}

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        // Walk variable types in the block's symbol table to collect
        // referenced symbols (e.g., VLA dimension expressions like n(i))
        for (auto &item : block->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                ASR::ttype_t *type = var->m_type;
                if (ASR::is_a<ASR::Array_t>(*type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        if (arr->m_dims[d].m_start) {
                            visit_expr(*arr->m_dims[d].m_start);
                        }
                        if (arr->m_dims[d].m_length) {
                            visit_expr(*arr->m_dims[d].m_length);
                        }
                    }
                }
            }
        }
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(
            x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        // Skip variables local to Block scopes, except those in
        // enclosing Block scopes (which need to become kernel parameters
        // when a do concurrent is inside one or more nested Blocks)
        if (ASR::is_a<ASR::Variable_t>(*x.m_v)) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(x.m_v);
            if (var->m_parent_symtab->asr_owner &&
                ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(
                        var->m_parent_symtab->asr_owner))) {
                if (enclosing_scopes.find(var->m_parent_symtab)
                        == enclosing_scopes.end()) {
                    return;
                }
            }
            // Skip variables local to AssociateBlock scopes — they are
            // internal aliases that stay in the AssociateBlock's symtab
            if (var->m_parent_symtab->asr_owner &&
                ASR::is_a<ASR::AssociateBlock_t>(
                    *ASR::down_cast<ASR::symbol_t>(
                        var->m_parent_symtab->asr_owner))) {
                return;
            }
        }
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (symbols.find(name) == symbols.end()) {
            symbols[name] = {ASRUtils::symbol_type(x.m_v),
                ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, x.m_v))};
        }
    }
};

// Checks whether an expression tree contains a FunctionCall node.
class ContainsFunctionCall : public ASR::BaseWalkVisitor<ContainsFunctionCall> {
public:
    bool found = false;
    void visit_FunctionCall(const ASR::FunctionCall_t &) { found = true; }
};

static bool expr_has_function_call(ASR::expr_t *expr) {
    if (!expr) return false;
    ContainsFunctionCall checker;
    checker.visit_expr(*expr);
    return checker.found;
}

// Replaces all Var references in-place to point to the kernel scope symbols
class GpuReplaceSymbols : public ASR::BaseExprReplacer<GpuReplaceSymbols> {
public:
    SymbolTable &kernel_scope;
    std::set<SymbolTable*> skip_scopes;
    GpuReplaceSymbols(SymbolTable &scope) : kernel_scope(scope) {}

    void replace_Var(ASR::Var_t *x) {
        std::string name = ASRUtils::symbol_name(x->m_v);
        for (auto *ss : skip_scopes) {
            if (ss->get_symbol(name)) return;
        }
        ASR::symbol_t *new_sym = kernel_scope.get_symbol(name);
        if (new_sym) {
            x->m_v = new_sym;
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        // Replace the struct variable expression (e.g., Var x)
        ASR::expr_t **current_expr_copy = current_expr;
        current_expr = &(x->m_v);
        replace_expr(x->m_v);
        current_expr = current_expr_copy;
        // Replace the member symbol to point to kernel scope's ExternalSymbol
        std::string mem_name = ASRUtils::symbol_name(x->m_m);
        ASR::symbol_t *new_mem = kernel_scope.get_symbol(mem_name);
        if (new_mem) {
            x->m_m = new_mem;
        }
    }

    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        // Remap m_name to kernel scope symbol
        std::string name = ASRUtils::symbol_name(x->m_name);
        ASR::symbol_t *new_sym = kernel_scope.get_symbol(name);
        if (!new_sym && ASR::is_a<ASR::ExternalSymbol_t>(*x->m_name)) {
            // Try sanitized ExternalSymbol name (handles disambiguated
            // functions where different modules define same-named functions)
            std::string sanitized = name;
            for (char &c : sanitized) {
                if (c == '~' || c == '@') c = '_';
            }
            new_sym = kernel_scope.get_symbol(sanitized);
            if (!new_sym) {
                // ExternalSymbol name differs from resolved function name;
                // try the underlying function's name (e.g., "construct"
                // instead of "~mytype_t@construct").
                std::string resolved_name = ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(x->m_name));
                new_sym = kernel_scope.get_symbol(resolved_name);
            }
        }
        if (new_sym) {
            x->m_name = new_sym;
        }
        if (x->m_original_name) {
            std::string orig_name = ASRUtils::symbol_name(x->m_original_name);
            ASR::symbol_t *new_orig = kernel_scope.get_symbol(orig_name);
            if (new_orig) {
                x->m_original_name = new_orig;
            }
        }
        // Call base to handle arguments, type, value, dt
        ASR::BaseExprReplacer<GpuReplaceSymbols>::replace_FunctionCall(x);
    }
};

class GpuReplaceSymbolsVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<GpuReplaceSymbolsVisitor> {
public:
    GpuReplaceSymbols replacer;
    GpuReplaceSymbolsVisitor(SymbolTable &scope) : replacer(scope) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Resolves associate variable references to their original targets.
// When a DoConcurrentLoop is inside an AssociateBlock, variables like `nn`
// (associated with `n`) must be resolved to their associate value before
// kernel extraction, because the kernel scope cannot access the
// AssociateBlock's symbol table. The mapped expression may be a simple
// Var (e.g., associate(nn => n)) or a complex expression such as
// ArrayPhysicalCast(StructInstanceMember(...)) for derived-type components.
class AssociateVarResolver : public ASR::BaseExprReplacer<AssociateVarResolver> {
public:
    Allocator &al;
    std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map;
    AssociateVarResolver(Allocator &al_,
                         std::map<ASR::symbol_t*, ASR::expr_t*> &map)
        : al(al_), assoc_map(map) {}

    void replace_Var(ASR::Var_t *x) {
        auto it = assoc_map.find(x->m_v);
        if (it != assoc_map.end()) {
            // Deep-copy so the original Associate expression is not
            // modified when GpuReplaceSymbolsVisitor remaps symbols later
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.success = true;
            ASR::expr_t *copy = dup.duplicate_expr(it->second);
            if (copy) {
                *current_expr = copy;
            }
        }
    }
};

class AssociateVarResolverVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<AssociateVarResolverVisitor> {
public:
    AssociateVarResolver replacer;
    AssociateVarResolverVisitor(Allocator &al,
                                std::map<ASR::symbol_t*, ASR::expr_t*> &map)
        : replacer(al, map) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Collects local variables used in do concurrent body that are NOT
// arrays and NOT the loop variables — these are per-thread temporaries
class GpuLocalVarCollector : public ASR::BaseWalkVisitor<GpuLocalVarCollector> {
public:
    std::set<std::string> &local_vars;
    std::set<std::string> &assigned_vars;
    std::set<SymbolTable*> enclosing_scopes;

    GpuLocalVarCollector(std::set<std::string> &lv, std::set<std::string> &av,
        const std::set<SymbolTable*> &scopes = {})
        : local_vars(lv), assigned_vars(av), enclosing_scopes(scopes) {}

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(
            x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        // Check if target is a simple Var (not ArrayItem)
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_target);
            // Skip variables local to Block or AssociateBlock scopes
            bool is_block_local = false;
            if (ASR::is_a<ASR::Variable_t>(*v->m_v)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v->m_v);
                if (var->m_parent_symtab->asr_owner) {
                    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
                        var->m_parent_symtab->asr_owner);
                    if (ASR::is_a<ASR::Block_t>(*owner)) {
                        if (enclosing_scopes.find(var->m_parent_symtab)
                                == enclosing_scopes.end()) {
                            is_block_local = true;
                        }
                    }
                    if (ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
                        is_block_local = true;
                    }
                }
            }
            if (!is_block_local) {
                std::string name = ASRUtils::symbol_name(v->m_v);
                ASR::ttype_t *type = ASRUtils::symbol_type(v->m_v);
                if (!ASRUtils::is_array(type)) {
                    assigned_vars.insert(name);
                }
            }
        }
        // Check if target is a StructInstanceMember (e.g., x%v = ...)
        if (ASR::is_a<ASR::StructInstanceMember_t>(*x.m_target)) {
            ASR::StructInstanceMember_t *sm =
                ASR::down_cast<ASR::StructInstanceMember_t>(x.m_target);
            if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(sm->m_v);
                std::string name = ASRUtils::symbol_name(v->m_v);
                assigned_vars.insert(name);
            }
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_Assignment(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        // DoLoop loop variables are local temporaries
        if (x.m_head.m_v && ASR::is_a<ASR::Var_t>(*x.m_head.m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_head.m_v);
            std::string name = ASRUtils::symbol_name(v->m_v);
            assigned_vars.insert(name);
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_DoLoop(x);
    }
};

// Collects all Function symbols referenced by FunctionCall/SubroutineCall
// nodes in the do concurrent body so they can be imported into the kernel.
class GpuFunctionCollector : public ASR::BaseWalkVisitor<GpuFunctionCollector> {
public:
    std::map<std::string, ASR::symbol_t*> functions;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*resolved) ||
                ASR::is_a<ASR::StructMethodDeclaration_t>(*resolved)) {
            std::string name = ASRUtils::symbol_name(x.m_name);
            if (functions.find(name) == functions.end()) {
                functions[name] = x.m_name;
            }
        }
        if (x.m_original_name) {
            std::string orig_name = ASRUtils::symbol_name(x.m_original_name);
            if (functions.find(orig_name) == functions.end()) {
                functions[orig_name] = x.m_original_name;
            }
        }
        ASR::BaseWalkVisitor<GpuFunctionCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*resolved) ||
                ASR::is_a<ASR::StructMethodDeclaration_t>(*resolved)) {
            std::string name = ASRUtils::symbol_name(x.m_name);
            if (functions.find(name) == functions.end()) {
                functions[name] = x.m_name;
            }
        }
        ASR::BaseWalkVisitor<GpuFunctionCollector>::visit_SubroutineCall(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }
};

// Collects Var references in function bodies that point to symbols
// not reachable through the function's scope chain. This happens when
// a contained function references host-scope variables (e.g., Parameters)
// that are not present in the kernel scope hierarchy.
class DanglingVarCollector : public ASR::BaseWalkVisitor<DanglingVarCollector> {
public:
    SymbolTable *func_scope;
    std::map<std::string, ASR::symbol_t*> dangling;
    std::set<SymbolTable*> inner_scopes;
    DanglingVarCollector(SymbolTable *fs) : func_scope(fs) {}
    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        for (auto *scope : inner_scopes) {
            if (scope->get_symbol(name)) return;
        }
        if (!func_scope->resolve_symbol(name) &&
                dangling.find(name) == dangling.end()) {
            dangling[name] = x.m_v;
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        inner_scopes.insert(ab->m_symtab);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
        inner_scopes.erase(ab->m_symtab);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        inner_scopes.insert(block->m_symtab);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
        inner_scopes.erase(block->m_symtab);
    }
};

// Fixes dangling Var references in function bodies by resolving symbol
// names through the function's scope chain and replacing the Var target.
class DanglingVarFixer : public ASR::BaseWalkVisitor<DanglingVarFixer> {
public:
    SymbolTable *func_scope;
    std::set<std::string> &target_names;
    DanglingVarFixer(SymbolTable *fs, std::set<std::string> &names)
        : func_scope(fs), target_names(names) {}
    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (target_names.count(name)) {
            ASR::symbol_t *new_sym = func_scope->resolve_symbol(name);
            if (new_sym) {
                const_cast<ASR::Var_t&>(x).m_v = new_sym;
            }
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Collects StructInstanceMember references to allocatable array members
// in the do concurrent body. Used to decompose struct-typed kernel
// parameters into separate flat array buffers for Metal.
class GpuAllocStructMemberCollector :
    public ASR::BaseWalkVisitor<GpuAllocStructMemberCollector> {
public:
    // Maps struct_var_name -> { member_name -> (member_sym, member_type) }
    std::map<std::string,
        std::map<std::string, std::pair<ASR::symbol_t*, ASR::ttype_t*>>>
            alloc_members;
    // Struct var names that have any non-allocatable-array member access
    std::set<std::string> has_non_alloc_access;

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        if (ASR::is_a<ASR::Var_t>(*x.m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_v);
            std::string struct_name = ASRUtils::symbol_name(v->m_v);
            ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(x.m_m);
            std::string mem_name = ASRUtils::symbol_name(mem);
            ASR::ttype_t *mem_type = x.m_type;
            if (ASRUtils::is_allocatable(mem_type)) {
                ASR::ttype_t *inner =
                    ASRUtils::type_get_past_allocatable(mem_type);
                if (ASR::is_a<ASR::Array_t>(*inner)) {
                    alloc_members[struct_name][mem_name] =
                        {x.m_m, mem_type};
                } else {
                    has_non_alloc_access.insert(struct_name);
                }
            } else {
                has_non_alloc_access.insert(struct_name);
            }
        }
        ASR::BaseWalkVisitor<GpuAllocStructMemberCollector>::
            visit_StructInstanceMember(x);
    }
};

// Replaces StructInstanceMember(Var(x), a) with Var(x__a) for
// allocatable array members that have been decomposed into separate
// kernel parameters.
class GpuDecomposeStructReplacer :
    public ASR::BaseExprReplacer<GpuDecomposeStructReplacer> {
public:
    Allocator &al;
    SymbolTable *kernel_scope;
    std::map<std::pair<std::string, std::string>, std::string> &decomp_map;

    GpuDecomposeStructReplacer(Allocator &al_, SymbolTable *scope,
        std::map<std::pair<std::string, std::string>, std::string> &dmap)
        : al(al_), kernel_scope(scope), decomp_map(dmap) {}

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        if (ASR::is_a<ASR::Var_t>(*x->m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x->m_v);
            std::string struct_name = ASRUtils::symbol_name(v->m_v);
            ASR::symbol_t *mem =
                ASRUtils::symbol_get_past_external(x->m_m);
            std::string mem_name = ASRUtils::symbol_name(mem);
            auto key = std::make_pair(struct_name, mem_name);
            auto it = decomp_map.find(key);
            if (it != decomp_map.end()) {
                ASR::symbol_t *param_sym =
                    kernel_scope->get_symbol(it->second);
                if (param_sym) {
                    *current_expr = ASRUtils::EXPR(
                        ASR::make_Var_t(al, x->base.base.loc, param_sym));
                    return;
                }
            }
        }
        ASR::BaseExprReplacer<GpuDecomposeStructReplacer>::
            replace_StructInstanceMember(x);
    }
};

class GpuDecomposeStructVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<GpuDecomposeStructVisitor> {
public:
    GpuDecomposeStructReplacer replacer;
    GpuDecomposeStructVisitor(Allocator &al, SymbolTable *scope,
        std::map<std::pair<std::string, std::string>, std::string> &dmap)
        : replacer(al, scope, dmap) {}
    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

class GpuOffloadVisitor : public ASR::StatementWalkVisitor<GpuOffloadVisitor>
{
public:
    PassOptions pass_options;
    ASR::TranslationUnit_t &tu;

    GpuOffloadVisitor(Allocator &al, PassOptions pass_options_,
                      ASR::TranslationUnit_t &tu_)
        : StatementWalkVisitor(al), pass_options(pass_options_), tu(tu_) {}

    // Load any module dependencies of a loaded submodule TU into
    // the main TU's symbol table so that fix_external_symbols can
    // resolve them.  This handles transitive dependencies: if the
    // submodule uses module A which in turn uses module B, both A
    // and B are loaded.  After loading, fix_external_symbols is
    // called on the main TU to resolve any null m_external pointers
    // in the newly loaded modules.
    void load_submodule_deps(ASR::TranslationUnit_t &sub_tu) {
        std::vector<std::string> pending =
            ASRUtils::determine_module_dependencies(sub_tu);
        std::set<std::string> seen;
        bool loaded_any = false;

        while (!pending.empty()) {
            std::string dep_name = pending.back();
            pending.pop_back();
            if (seen.count(dep_name)) continue;
            seen.insert(dep_name);
            if (tu.m_symtab->get_symbol(dep_name) != nullptr)
                continue;
            if (sub_tu.m_symtab->get_symbol(dep_name) != nullptr)
                continue;
            bool is_intrinsic =
                startswith(dep_name, "lfortran_intrinsic");
            LocationManager lm_dep;
            auto dep_res = ASRUtils::find_and_load_module(
                al, dep_name, *tu.m_symtab, is_intrinsic,
                pass_options, lm_dep);
            if (!dep_res.ok && !is_intrinsic) {
                if (dep_name == "iso_c_binding" ||
                        dep_name == "iso_fortran_env") {
                    LocationManager lm_dep2;
                    auto dep_res2 =
                        ASRUtils::find_and_load_module(
                            al,
                            "lfortran_intrinsic_" + dep_name,
                            *tu.m_symtab, true, pass_options,
                            lm_dep2);
                    if (dep_res2.ok) {
                        ASR::Module_t *dep_mod =
                            ASRUtils::extract_module(
                                *dep_res2.result);
                        tu.m_symtab->add_symbol(dep_name,
                            (ASR::symbol_t*)dep_mod);
                        dep_mod->m_symtab->parent =
                            tu.m_symtab;
                        dep_mod->m_loaded_from_mod = true;
                        loaded_any = true;
                        for (size_t i = 0;
                                i < dep_mod->n_dependencies; i++) {
                            pending.push_back(
                                dep_mod->m_dependencies[i]);
                        }
                    }
                    continue;
                }
            }
            if (!dep_res.ok) continue;
            ASR::Module_t *dep_mod =
                ASRUtils::extract_module(*dep_res.result);
            tu.m_symtab->add_symbol(dep_name,
                (ASR::symbol_t*)dep_mod);
            dep_mod->m_symtab->parent = tu.m_symtab;
            dep_mod->m_loaded_from_mod = true;
            loaded_any = true;
            for (size_t i = 0; i < dep_mod->n_dependencies; i++) {
                pending.push_back(dep_mod->m_dependencies[i]);
            }
        }

        if (loaded_any) {
            fix_external_symbols(tu, *tu.m_symtab);
        }
    }

    // Duplicate an expression, remapping all Var references to point to the
    // given scope. Used to create kernel-scope copies of head expressions.
    ASR::expr_t* dup_expr_to_scope(ASR::expr_t *expr, SymbolTable *scope) {
        if (!expr) return nullptr;
        ASRUtils::ExprStmtDuplicator duplicator(al);
        duplicator.success = true;
        ASR::expr_t *copy = duplicator.duplicate_expr(expr);
        if (!copy) return expr;
        GpuReplaceSymbols replacer(*scope);
        replacer.current_expr = &copy;
        replacer.replace_expr(copy);
        return copy;
    }

    // Recursively remap ExternalSymbol targets and Variable
    // m_type_declarations inside `scope` (and all nested child scopes)
    // so they reference the kernel-scope struct copies instead of the
    // original module definitions.
    void fixup_struct_refs_in_scope(SymbolTable *scope,
            SymbolTable *kernel_scope) {
        for (auto &item : scope->get_scope()) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                ASR::ExternalSymbol_t *es =
                    ASR::down_cast<ASR::ExternalSymbol_t>(item.second);
                if (!es->m_external) continue;
                ASR::symbol_t *target =
                    ASRUtils::symbol_get_past_external(es->m_external);
                if (!target) continue;
                SymbolTable *tp =
                    ASRUtils::symbol_parent_symtab(target);
                if (tp->asr_owner &&
                        tp->asr_owner->type == ASR::asrType::symbol) {
                    ASR::symbol_t *os =
                        ASR::down_cast<ASR::symbol_t>(tp->asr_owner);
                    if (ASR::is_a<ASR::Struct_t>(*os)) {
                        std::string sn =
                            ASR::down_cast<ASR::Struct_t>(os)->m_name;
                        ASR::symbol_t *ks =
                            kernel_scope->get_symbol(sn);
                        if (ks && ASR::is_a<ASR::Struct_t>(*ks)) {
                            ASR::symbol_t *nt =
                                ASR::down_cast<ASR::Struct_t>(ks)
                                    ->m_symtab->get_symbol(
                                        es->m_original_name);
                            if (nt) es->m_external = nt;
                        }
                    }
                }
            } else if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                ASR::symbol_t *tdecl_resolved =
                    var->m_type_declaration
                        ? ASRUtils::symbol_get_past_external(
                              var->m_type_declaration)
                        : nullptr;
                if (tdecl_resolved &&
                        ASR::is_a<ASR::Struct_t>(*tdecl_resolved)) {
                    std::string sn = ASRUtils::symbol_name(
                        tdecl_resolved);
                    ASR::symbol_t *ks =
                        kernel_scope->get_symbol(sn);
                    if (ks) var->m_type_declaration = ks;
                }
            }
            // Recurse into nested scopes
            SymbolTable *nested = nullptr;
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                nested = ASR::down_cast<ASR::Function_t>(
                    item.second)->m_symtab;
            } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
                nested = ASR::down_cast<ASR::Block_t>(
                    item.second)->m_symtab;
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                nested = ASR::down_cast<ASR::AssociateBlock_t>(
                    item.second)->m_symtab;
            }
            if (nested) {
                fixup_struct_refs_in_scope(nested, kernel_scope);
            }
        }
    }

    // Find a Struct in kernel_scope by name, with PDT fallback.
    // If the exact name is not found (e.g., "network_t"), look for a
    // PDT instantiation (e.g., "network_t_4") that has a member named
    // member_name. Returns the Struct symbol or nullptr.
    ASR::symbol_t* find_kernel_struct(SymbolTable *kernel_scope,
            const std::string &struct_name,
            const std::string &member_name) {
        ASR::symbol_t *sym = kernel_scope->get_symbol(struct_name);
        if (sym && is_a<ASR::Struct_t>(*sym)) return sym;
        for (auto &item : kernel_scope->get_scope()) {
            if (!is_a<ASR::Struct_t>(*item.second)) continue;
            ASR::Struct_t *s = down_cast<ASR::Struct_t>(item.second);
            if (s->m_symtab->get_symbol(member_name)) {
                return item.second;
            }
        }
        return nullptr;
    }

    // Import a Struct definition into kernel scope, recursively handling
    // nested struct-typed members. Also creates ExternalSymbol entries
    // in kernel_scope for members referenced from orig_scope.
    ASR::symbol_t* import_struct_def(ASR::Struct_t *orig_struct,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const Location &loc) {
        std::string struct_name = orig_struct->m_name;

        // If already imported, return existing
        ASR::symbol_t *existing = kernel_scope->get_symbol(struct_name);
        if (existing) return existing;

        // Deep-copy the Struct into kernel scope
        SymbolTable *new_st = al.make_new<SymbolTable>(kernel_scope);
        for (auto &item : orig_struct->m_symtab->get_scope()) {
            ASR::symbol_t *member = item.second;
            if (is_a<ASR::Variable_t>(*member)) {
                ASR::Variable_t *mv = down_cast<ASR::Variable_t>(member);
                // If the member itself has StructType, recursively import
                // the inner struct so we can set its type_declaration
                ASR::symbol_t *member_type_decl = nullptr;
                if (ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::extract_type(mv->m_type)) &&
                        mv->m_type_declaration) {
                    ASR::symbol_t *inner_sym =
                        ASRUtils::symbol_get_past_external(
                            mv->m_type_declaration);
                    if (is_a<ASR::Struct_t>(*inner_sym)) {
                        member_type_decl = import_struct_def(
                            down_cast<ASR::Struct_t>(inner_sym),
                            orig_scope, kernel_scope, loc);
                    }
                }
                ASR::symbol_t *new_member = down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, new_st,
                        s2c(al, item.first), nullptr, 0,
                        mv->m_intent, nullptr, nullptr,
                        mv->m_storage, ASRUtils::duplicate_type(al, mv->m_type),
                        member_type_decl, mv->m_abi, mv->m_access,
                        mv->m_presence, false));
                new_st->add_symbol(item.first, new_member);
            } else if (is_a<ASR::StructMethodDeclaration_t>(*member)) {
                ASR::StructMethodDeclaration_t *smd =
                    down_cast<ASR::StructMethodDeclaration_t>(member);
                ASR::asr_t *new_smd = ASR::make_StructMethodDeclaration_t(
                    al, loc, new_st, s2c(al, item.first),
                    smd->m_self_argument, smd->m_proc_name,
                    smd->m_proc, smd->m_abi,
                    smd->m_is_deferred, smd->m_is_nopass);
                new_st->add_symbol(item.first,
                    down_cast<ASR::symbol_t>(new_smd));
            }
        }

        // Duplicate the struct signature type
        ASR::ttype_t *new_sig = ASRUtils::duplicate_type(al, orig_struct->m_struct_signature);

        // Copy member names
        char **new_members = al.allocate<char*>(orig_struct->n_members);
        for (size_t i = 0; i < orig_struct->n_members; i++) {
            new_members[i] = orig_struct->m_members[i];
        }

        ASR::asr_t *new_struct = ASR::make_Struct_t(al, loc,
            new_st, s2c(al, struct_name), new_sig,
            nullptr, 0,
            new_members, orig_struct->n_members,
            nullptr, 0,
            orig_struct->m_abi, orig_struct->m_access,
            orig_struct->m_is_packed, orig_struct->m_is_abstract,
            nullptr, 0, nullptr, nullptr, nullptr, 0);
        ASR::symbol_t *kernel_struct = down_cast<ASR::symbol_t>(new_struct);
        kernel_scope->add_symbol(struct_name, kernel_struct);

        // Create ExternalSymbol entries in kernel scope for each member,
        // so that StructInstanceMember can reference them.
        // Search orig_scope and walk up through AssociateBlock/Block
        // parent scopes, because when the do concurrent is inside an
        // AssociateBlock the ExternalSymbol entries for struct members
        // live in the enclosing function scope, not in the
        // AssociateBlock's scope.
        SymbolTable *search_scope = orig_scope;
        while (search_scope) {
            for (auto &item : search_scope->get_scope()) {
                if (!is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
                ASR::ExternalSymbol_t *es = down_cast<ASR::ExternalSymbol_t>(item.second);
                ASR::symbol_t *es_external = ASRUtils::symbol_get_past_external(es->m_external);
                // Check if this ExternalSymbol refers to a member of our struct.
                // For PDT instantiations (e.g., network_t_4), also match
                // ExternalSymbols pointing to the PDT template struct
                // (e.g., network_t) when the instantiated struct has a
                // member with the same original name.
                SymbolTable *es_parent_st =
                    ASRUtils::symbol_parent_symtab(es_external);
                bool is_member = (es_parent_st == orig_struct->m_symtab);
                if (!is_member && es_parent_st->asr_owner &&
                        es_parent_st->asr_owner->type == ASR::asrType::symbol) {
                    ASR::symbol_t *es_struct_owner =
                        down_cast<ASR::symbol_t>(es_parent_st->asr_owner);
                    if (is_a<ASR::Struct_t>(*es_struct_owner) &&
                            new_st->get_symbol(es->m_original_name)) {
                        is_member = true;
                    }
                }
                if (is_member) {
                    std::string es_name = item.first;
                    if (kernel_scope->get_symbol(es_name)) continue;
                    ASR::symbol_t *new_member_in_struct = new_st->get_symbol(
                        es->m_original_name);
                    if (!new_member_in_struct) continue;
                    ASR::asr_t *new_es = ASR::make_ExternalSymbol_t(al, loc,
                        kernel_scope, s2c(al, es_name),
                        new_member_in_struct, s2c(al, struct_name),
                        nullptr, 0, s2c(al, es->m_original_name),
                        es->m_access);
                    kernel_scope->add_symbol(es_name,
                        down_cast<ASR::symbol_t>(new_es));
                }
            }
            if (search_scope->asr_owner &&
                    search_scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner = down_cast<ASR::symbol_t>(
                    search_scope->asr_owner);
                if (is_a<ASR::AssociateBlock_t>(*owner) ||
                        is_a<ASR::Block_t>(*owner)) {
                    search_scope = search_scope->parent;
                    continue;
                }
            }
            break;
        }

        return kernel_struct;
    }

    // For a struct-typed variable, get the type_declaration symbol
    // from the original scope and ensure the Struct (and its member
    // ExternalSymbols) exist in the kernel scope.
    ASR::symbol_t* import_struct_type(ASR::symbol_t *orig_sym,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const Location &loc) {
        if (!is_a<ASR::Variable_t>(*orig_sym)) return nullptr;
        ASR::Variable_t *var = down_cast<ASR::Variable_t>(orig_sym);
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(var->m_type))) return nullptr;
        ASR::symbol_t *type_decl = var->m_type_declaration;
        if (!type_decl) return nullptr;
        ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(type_decl);
        if (!is_a<ASR::Struct_t>(*struct_sym)) return nullptr;
        // Use orig_scope (the do concurrent's enclosing scope) rather
        // than var->m_parent_symtab. When the do concurrent is inside
        // an AssociateBlock, ExternalSymbol entries for struct members
        // (e.g., type-bound procedure references) are migrated from
        // inner associate scopes into orig_scope during associate
        // resolution. The variable's declaring scope may be a parent
        // of orig_scope and would not contain these migrated symbols.
        return import_struct_def(down_cast<ASR::Struct_t>(struct_sym),
            orig_scope, kernel_scope, loc);
    }

    // Walk a mask expression to find the first ArraySection and extract
    // its loop bounds (start, end).
    void find_array_section_bounds(ASR::expr_t *e,
            ASR::expr_t *&loop_start, ASR::expr_t *&loop_end) {
        if (loop_start) return;
        if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
            if (as->n_args > 0 && as->m_args[0].m_left && as->m_args[0].m_right) {
                loop_start = as->m_args[0].m_left;
                loop_end = as->m_args[0].m_right;
            }
        } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
            ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
            find_array_section_bounds(rc->m_left, loop_start, loop_end);
            find_array_section_bounds(rc->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
            ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
            find_array_section_bounds(ic->m_left, loop_start, loop_end);
            find_array_section_bounds(ic->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            find_array_section_bounds(lb->m_left, loop_start, loop_end);
            find_array_section_bounds(lb->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
            ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
            find_array_section_bounds(rb->m_left, loop_start, loop_end);
            find_array_section_bounds(rb->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            find_array_section_bounds(ib->m_left, loop_start, loop_end);
            find_array_section_bounds(ib->m_right, loop_start, loop_end);
        } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
            ASR::IntrinsicElementalFunction_t *ief =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            for (size_t i = 0; i < ief->n_args; i++) {
                if (ief->m_args[i])
                    find_array_section_bounds(ief->m_args[i],
                        loop_start, loop_end);
            }
        }
    }

    // Build an element-wise expression by replacing ArraySection nodes
    // with ArrayItem nodes indexed by loop_var.
    ASR::expr_t* elementize_mask(ASR::expr_t *e, ASR::expr_t *loop_var,
            ASR::ttype_t *logical_type, const Location &loc) {
        if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
            Vec<ASR::array_index_t> new_args;
            new_args.reserve(al, as->n_args);
            for (size_t i = 0; i < as->n_args; i++) {
                ASR::array_index_t idx;
                idx.loc = as->m_args[i].loc;
                if (as->m_args[i].m_left && as->m_args[i].m_right) {
                    idx.m_left = nullptr;
                    idx.m_right = loop_var;
                    idx.m_step = nullptr;
                } else {
                    idx.m_left = as->m_args[i].m_left;
                    idx.m_right = as->m_args[i].m_right;
                    idx.m_step = as->m_args[i].m_step;
                }
                new_args.push_back(al, idx);
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(as->m_v));
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                as->m_v, new_args.p, new_args.n,
                elem_type, ASR::arraystorageType::ColMajor, nullptr));
        } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
            ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
            return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
                elementize_mask(rc->m_left, loop_var, logical_type, loc),
                rc->m_op,
                elementize_mask(rc->m_right, loop_var, logical_type, loc),
                logical_type, nullptr));
        } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
            ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
            return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                elementize_mask(ic->m_left, loop_var, logical_type, loc),
                ic->m_op,
                elementize_mask(ic->m_right, loop_var, logical_type, loc),
                logical_type, nullptr));
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
                elementize_mask(lb->m_left, loop_var, logical_type, loc),
                lb->m_op,
                elementize_mask(lb->m_right, loop_var, logical_type, loc),
                logical_type, nullptr));
        } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
            ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
            ASR::ttype_t *real_type = ASRUtils::extract_type(
                ASRUtils::expr_type(e));
            return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                elementize_mask(rb->m_left, loop_var, logical_type, loc),
                rb->m_op,
                elementize_mask(rb->m_right, loop_var, logical_type, loc),
                real_type, nullptr));
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            ASR::ttype_t *int_elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(e));
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                elementize_mask(ib->m_left, loop_var, logical_type, loc),
                ib->m_op,
                elementize_mask(ib->m_right, loop_var, logical_type, loc),
                int_elem_type, nullptr));
        } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
            ASR::IntrinsicElementalFunction_t *ief =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            Vec<ASR::expr_t*> new_args;
            new_args.reserve(al, ief->n_args);
            for (size_t i = 0; i < ief->n_args; i++) {
                new_args.push_back(al, ief->m_args[i]
                    ? elementize_mask(ief->m_args[i], loop_var,
                          logical_type, loc)
                    : nullptr);
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(e));
            return ASRUtils::EXPR(
                ASR::make_IntrinsicElementalFunction_t(al, loc,
                    ief->m_intrinsic_id, new_args.p, new_args.n,
                    ief->m_overload_id, elem_type, nullptr));
        }
        return e;
    }

    // Inline a single IntrinsicArrayFunction All into preamble statements
    // and return a Var expression referencing the result. Returns nullptr
    // if the All cannot be inlined.
    ASR::expr_t* inline_single_all(ASR::IntrinsicArrayFunction_t *iaf,
            const Location &loc, Vec<ASR::stmt_t*> &preamble) {
        if (iaf->n_args < 1 || !iaf->m_args[0]) return nullptr;
        ASR::expr_t *mask = iaf->m_args[0];

        ASR::expr_t *loop_start = nullptr;
        ASR::expr_t *loop_end = nullptr;
        find_array_section_bounds(mask, loop_start, loop_end);
        if (!loop_start || !loop_end) return nullptr;

        ASR::ttype_t *logical_type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, loc, 4));
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        // Create loop variable
        std::string loop_var_name = var_scope->get_unique_name("__gpu_all_i");
        ASR::symbol_t *loop_var_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, var_scope,
                s2c(al, loop_var_name), nullptr, 0,
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default,
                ASRUtils::duplicate_type(al, int_type),
                nullptr, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required, false));
        var_scope->add_symbol(loop_var_name, loop_var_sym);
        ASR::expr_t *loop_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, loop_var_sym));

        // Create result variable
        std::string res_var_name = var_scope->get_unique_name("__gpu_all_res");
        ASR::symbol_t *res_var_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, var_scope,
                s2c(al, res_var_name), nullptr, 0,
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default,
                ASRUtils::duplicate_type(al, logical_type),
                nullptr, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required, false));
        var_scope->add_symbol(res_var_name, res_var_sym);
        ASR::expr_t *res_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, res_var_sym));

        // __gpu_all_res = .true.
        preamble.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var,
                ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                    true, logical_type)),
                nullptr, false, false)));

        ASR::expr_t *elem_mask = elementize_mask(mask, loop_var,
            logical_type, loc);

        // Build loop body: if (.not. elem_mask) __gpu_all_res = .false.
        Vec<ASR::stmt_t*> loop_body;
        loop_body.reserve(al, 1);
        ASR::expr_t *not_mask = ASRUtils::EXPR(
            ASR::make_LogicalNot_t(al, loc, elem_mask, logical_type, nullptr));
        Vec<ASR::stmt_t*> if_body;
        if_body.reserve(al, 1);
        if_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var,
                ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                    false, logical_type)),
                nullptr, false, false)));
        Vec<ASR::stmt_t*> if_else;
        if_else.reserve(al, 0);
        loop_body.push_back(al, ASRUtils::STMT(
            ASR::make_If_t(al, loc, nullptr, not_mask,
                if_body.p, if_body.n, if_else.p, if_else.n)));

        // do __gpu_all_i = loop_start, loop_end
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = loop_var;
        head.m_start = loop_start;
        head.m_end = loop_end;
        head.m_increment = nullptr;
        preamble.push_back(al, ASRUtils::STMT(
            ASR::make_DoLoop_t(al, loc, nullptr,
                head, loop_body.p, loop_body.n, nullptr, 0)));

        return res_var;
    }

    // Check if an expression tree contains any IntrinsicArrayFunction All.
    bool contains_intrinsic_all(ASR::expr_t *e) {
        if (!e) return false;
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) {
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        == ASRUtils::IntrinsicArrayFunctions::All) {
                return true;
            }
        }
        if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            return contains_intrinsic_all(lb->m_left) ||
                   contains_intrinsic_all(lb->m_right);
        }
        if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
            return contains_intrinsic_all(
                ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg);
        }
        return false;
    }

    // Recursively replace IntrinsicArrayFunction All nodes in an expression
    // with temporary variables, emitting inline loops into preamble.
    ASR::expr_t* replace_all_in_expr(ASR::expr_t *e, const Location &loc,
            Vec<ASR::stmt_t*> &preamble) {
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) {
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        == ASRUtils::IntrinsicArrayFunctions::All) {
                ASR::expr_t *res = inline_single_all(iaf, loc, preamble);
                if (res) return res;
            }
            return e;
        }
        if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
            ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            ASR::expr_t *new_left = replace_all_in_expr(lb->m_left, loc,
                preamble);
            ASR::expr_t *new_right = replace_all_in_expr(lb->m_right, loc,
                preamble);
            if (new_left != lb->m_left || new_right != lb->m_right) {
                return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
                    new_left, lb->m_op, new_right, lb->m_type, nullptr));
            }
            return e;
        }
        if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
            ASR::LogicalNot_t *ln = ASR::down_cast<ASR::LogicalNot_t>(e);
            ASR::expr_t *new_arg = replace_all_in_expr(ln->m_arg, loc,
                preamble);
            if (new_arg != ln->m_arg) {
                return ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc,
                    new_arg, ln->m_type, nullptr));
            }
            return e;
        }
        return e;
    }

    // Inline IntrinsicArrayFunction All inside a DoConcurrentLoop body.
    // Replaces:
    //   eq(l) = all(a(:,l) == b(:,l))
    // or:
    //   eq(l) = all(a(1:l) > 0) .and. all(b(1:l) > 0)
    // With inlined loops that compute the All result into temporaries.
    // This avoids complex lowered code (Associate, Allocate, FunctionCall)
    // that the Metal backend cannot handle inside GPU kernels.
    void inline_intrinsic_all(ASR::DoConcurrentLoop_t &x) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, x.n_body * 3);
        bool changed = false;

        for (size_t si = 0; si < x.n_body; si++) {
            ASR::stmt_t *stmt = x.m_body[si];
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

            if (!contains_intrinsic_all(asgn->m_value)) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            Vec<ASR::stmt_t*> preamble;
            preamble.reserve(al, 8);

            ASR::expr_t *new_value = replace_all_in_expr(asgn->m_value,
                loc, preamble);

            if (preamble.n > 0) {
                changed = true;
                for (size_t pi = 0; pi < preamble.n; pi++) {
                    new_body.push_back(al, preamble[pi]);
                }
                new_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, asgn->m_target,
                        new_value, nullptr, false, false)));
            } else {
                new_body.push_back(al, stmt);
            }
        }

        if (changed) {
            x.m_body = new_body.p;
            x.n_body = new_body.n;
        }
    }

    // Inline IntrinsicArrayFunction MatMul inside a DoConcurrentLoop body.
    // Replaces:
    //   c = matmul(a, b)
    // With nested DoLoops that compute the matrix multiplication directly.
    // This avoids generating a call to _lcompilers_matmul which is not
    // available inside Metal GPU kernels.
    void inline_matmul_stmts(ASR::stmt_t** &body, size_t &n_body) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 4);
        bool changed = false;

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t &dl = *ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_matmul_stmts(dl.m_body, dl.n_body);
                new_body.push_back(al, stmt);
                continue;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t &bc = *ASR::down_cast<ASR::BlockCall_t>(stmt);
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc.m_m);
                inline_matmul_stmts(block->m_body, block->n_body);
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

            // Detect MatMul: either directly as the RHS, or inside a
            // RealBinOp (e.g., z = matmul(w, a) + b).
            ASR::IntrinsicArrayFunction_t *iaf = nullptr;
            ASR::expr_t *binop_other = nullptr;
            ASR::binopType binop_op = ASR::binopType::Add;
            bool matmul_is_left = true;

            if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
                iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(
                    asgn->m_value);
            } else if (ASR::is_a<ASR::RealBinOp_t>(*asgn->m_value)) {
                ASR::RealBinOp_t *rbop =
                    ASR::down_cast<ASR::RealBinOp_t>(asgn->m_value);
                ASR::expr_t *left = rbop->m_left;
                ASR::expr_t *right = rbop->m_right;
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*left))
                    left = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                        left)->m_arg;
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*right))
                    right = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                        right)->m_arg;
                if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*left)) {
                    auto *f =
                        ASR::down_cast<ASR::IntrinsicArrayFunction_t>(
                            left);
                    if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                            f->m_arr_intrinsic_id)
                                == ASRUtils::IntrinsicArrayFunctions::
                                    MatMul) {
                        iaf = f;
                        binop_other = rbop->m_right;
                        binop_op = rbop->m_op;
                        matmul_is_left = true;
                    }
                }
                if (!iaf &&
                        ASR::is_a<ASR::IntrinsicArrayFunction_t>(
                            *right)) {
                    auto *f =
                        ASR::down_cast<ASR::IntrinsicArrayFunction_t>(
                            right);
                    if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                            f->m_arr_intrinsic_id)
                                == ASRUtils::IntrinsicArrayFunctions::
                                    MatMul) {
                        iaf = f;
                        binop_other = rbop->m_left;
                        binop_op = rbop->m_op;
                        matmul_is_left = false;
                    }
                }
            }

            if (!iaf || static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::MatMul) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            // Strip ArrayPhysicalCast from arguments
            ASR::expr_t *arg_a = iaf->m_args[0];
            ASR::expr_t *arg_b = iaf->m_args[1];
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_a)) {
                arg_a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_a)->m_arg;
            }
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_b)) {
                arg_b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_b)->m_arg;
            }

            // Detect and unwrap Transpose on matmul arguments so the
            // inlined loops index into the original array with swapped
            // indices instead of calling _lcompilers_transpose (which
            // is unavailable inside Metal GPU kernels).
            bool transpose_a = false, transpose_b = false;
            if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*arg_a)) {
                auto *iaf_a = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(arg_a);
                if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                        iaf_a->m_arr_intrinsic_id)
                            == ASRUtils::IntrinsicArrayFunctions::Transpose) {
                    arg_a = iaf_a->m_args[0];
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_a)) {
                        arg_a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_a)->m_arg;
                    }
                    transpose_a = true;
                }
            }
            if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*arg_b)) {
                auto *iaf_b = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(arg_b);
                if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                        iaf_b->m_arr_intrinsic_id)
                            == ASRUtils::IntrinsicArrayFunctions::Transpose) {
                    arg_b = iaf_b->m_args[0];
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_b)) {
                        arg_b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_b)->m_arg;
                    }
                    transpose_b = true;
                }
            }

            ASR::ttype_t *type_a = ASRUtils::expr_type(arg_a);
            ASR::ttype_t *type_b = ASRUtils::expr_type(arg_b);
            ASR::dimension_t *dims_a = nullptr, *dims_b = nullptr;
            int rank_a = ASRUtils::extract_dimensions_from_ttype(type_a, dims_a);
            int rank_b = ASRUtils::extract_dimensions_from_ttype(type_b, dims_b);

            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(asgn->m_target));

            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            auto make_loop_var = [&](const std::string &prefix) -> ASR::expr_t* {
                std::string name = var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(name, sym);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            };

            auto make_array_item_1d = [&](ASR::expr_t *arr,
                    ASR::expr_t *idx) -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, 1);
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = nullptr;
                ai.m_right = idx;
                ai.m_step = nullptr;
                args.push_back(al, ai);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                    args.p, args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            };

            auto make_array_item_2d = [&](ASR::expr_t *arr,
                    ASR::expr_t *idx1, ASR::expr_t *idx2) -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, 2);
                ASR::array_index_t ai1;
                ai1.loc = loc;
                ai1.m_left = nullptr;
                ai1.m_right = idx1;
                ai1.m_step = nullptr;
                args.push_back(al, ai1);
                ASR::array_index_t ai2;
                ai2.loc = loc;
                ai2.m_left = nullptr;
                ai2.m_right = idx2;
                ai2.m_step = nullptr;
                args.push_back(al, ai2);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                    args.p, args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            };

            auto make_do_loop = [&](ASR::expr_t *var, ASR::expr_t *start,
                    ASR::expr_t *end, Vec<ASR::stmt_t*> &body) -> ASR::stmt_t* {
                ASR::do_loop_head_t head;
                head.loc = loc;
                head.m_v = var;
                head.m_start = start;
                head.m_end = end;
                head.m_increment = nullptr;
                return ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                    head, body.p, body.n, nullptr, 0));
            };

            // When an argument or target is an ArraySection (e.g. v(:,i)),
            // expand it into an ArrayItem on the base array by replacing
            // each range dimension with the corresponding loop variable
            // and keeping fixed dimensions as-is.
            // When the expression is an elemental FunctionCall with array
            // arguments (e.g. f(z(1:n))), elementize by converting each
            // array argument to a scalar indexed by the loop variable,
            // producing f(z(i)) instead of f(z(1:n))[i].
            std::function<ASR::expr_t*(ASR::expr_t*,
                std::vector<ASR::expr_t*>)> make_section_item;
            make_section_item = [&](ASR::expr_t *arr_expr,
                    std::vector<ASR::expr_t*> loop_vars) -> ASR::expr_t* {
                if (ASR::is_a<ASR::ArraySection_t>(*arr_expr)) {
                    ASR::ArraySection_t *sec =
                        ASR::down_cast<ASR::ArraySection_t>(arr_expr);
                    Vec<ASR::array_index_t> args;
                    args.reserve(al, sec->n_args);
                    size_t lv_idx = 0;
                    for (size_t d = 0; d < sec->n_args; d++) {
                        ASR::array_index_t ai;
                        ai.loc = loc;
                        if (sec->m_args[d].m_left != nullptr) {
                            ai.m_left = nullptr;
                            ai.m_right = loop_vars[lv_idx++];
                            ai.m_step = nullptr;
                        } else {
                            ai.m_left = nullptr;
                            ai.m_right = sec->m_args[d].m_right;
                            ai.m_step = nullptr;
                        }
                        args.push_back(al, ai);
                    }
                    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                        sec->m_v, args.p, args.n, elem_type,
                        ASR::arraystorageType::ColMajor, nullptr));
                }
                if (ASR::is_a<ASR::FunctionCall_t>(*arr_expr)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(arr_expr);
                    if (ASRUtils::is_elemental(fc->m_name)) {
                        Vec<ASR::call_arg_t> new_args;
                        new_args.reserve(al, fc->n_args);
                        for (size_t i = 0; i < fc->n_args; i++) {
                            ASR::call_arg_t arg;
                            arg.loc = fc->m_args[i].loc;
                            if (fc->m_args[i].m_value &&
                                    ASRUtils::is_array(
                                        ASRUtils::expr_type(
                                            fc->m_args[i].m_value))) {
                                arg.m_value = make_section_item(
                                    fc->m_args[i].m_value, loop_vars);
                            } else {
                                arg.m_value = fc->m_args[i].m_value;
                            }
                            new_args.push_back(al, arg);
                        }
                        ASR::ttype_t *ret_type = elem_type;
                        return ASRUtils::EXPR(
                            ASR::make_FunctionCall_t(al, fc->base.base.loc,
                                fc->m_name, fc->m_original_name,
                                new_args.p, new_args.n, ret_type,
                                nullptr, fc->m_dt));
                    }
                }
                if (loop_vars.size() == 1)
                    return make_array_item_1d(arr_expr, loop_vars[0]);
                return make_array_item_2d(arr_expr, loop_vars[0],
                    loop_vars[1]);
            };

            // When an argument is an ArraySection, extract loop bounds
            // from the section's range specs rather than from the type
            // dimensions (which may be null for section result types).
            auto get_loop_bounds = [&](ASR::expr_t *arg,
                    ASR::dimension_t *dims,
                    int dim_idx) -> std::pair<ASR::expr_t*, ASR::expr_t*> {
                if (ASR::is_a<ASR::ArraySection_t>(*arg)) {
                    ASR::ArraySection_t *sec =
                        ASR::down_cast<ASR::ArraySection_t>(arg);
                    int range_idx = 0;
                    for (size_t d = 0; d < sec->n_args; d++) {
                        if (sec->m_args[d].m_left != nullptr) {
                            if (range_idx == dim_idx) {
                                return {sec->m_args[d].m_left,
                                        sec->m_args[d].m_right};
                            }
                            range_idx++;
                        }
                    }
                }
                return {dims[dim_idx].m_start, dims[dim_idx].m_length};
            };

            ASR::expr_t *zero;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                    0.0, elem_type));
            } else {
                zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                    0, elem_type, ASR::integerbozType::Decimal));
            }

            int64_t overload_id = iaf->m_overload_id;

            if (overload_id == 2 && rank_a == 2 && rank_b == 1) {
                // c(i) = sum_k a(i,k) * b(k)
                // With transpose_a: c(i) = sum_k a(k,i) * b(k)
                ASR::expr_t *var_i = make_loop_var("__gpu_mm_i");
                ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

                ASR::expr_t *c_i = make_section_item(asgn->m_target, {var_i});
                ASR::expr_t *a_ik = transpose_a
                    ? make_section_item(arg_a, {var_k, var_i})
                    : make_section_item(arg_a, {var_i, var_k});
                ASR::expr_t *b_k = make_section_item(arg_b, {var_k});

                int i_dim = transpose_a ? 1 : 0;
                int k_dim = transpose_a ? 0 : 1;
                auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, k_dim);
                auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, i_dim);

                // k-loop body: c(i) = c(i) + a(i,k) * b(k)
                Vec<ASR::stmt_t*> k_body;
                k_body.reserve(al, 1);
                ASR::expr_t *prod = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, a_ik,
                        ASR::binopType::Mul, b_k, elem_type, nullptr));
                ASR::expr_t *sum = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, c_i,
                        ASR::binopType::Add, prod, elem_type, nullptr));
                k_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_i, sum,
                        nullptr, false, false)));

                // i-loop body: c(i) = 0; do k ...; [c(i) = c(i) OP other(i)]
                Vec<ASR::stmt_t*> i_body;
                i_body.reserve(al, binop_other ? 3 : 2);
                i_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_i, zero,
                        nullptr, false, false)));
                i_body.push_back(al,
                    make_do_loop(var_k, k_start, k_end, k_body));

                if (binop_other) {
                    ASR::expr_t *other_op = binop_other;
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*other_op))
                        other_op = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                            other_op)->m_arg;
                    ASR::expr_t *other_i = make_section_item(
                        other_op, {var_i});
                    ASR::expr_t *lhs = matmul_is_left ? c_i : other_i;
                    ASR::expr_t *rhs = matmul_is_left ? other_i : c_i;
                    ASR::expr_t *combined = ASRUtils::EXPR(
                        ASR::make_RealBinOp_t(al, loc, lhs, binop_op,
                            rhs, elem_type, nullptr));
                    i_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, c_i, combined,
                            nullptr, false, false)));
                }

                new_body.push_back(al,
                    make_do_loop(var_i, i_start, i_end, i_body));
            } else if (overload_id == 1 && rank_a == 1 && rank_b == 2) {
                // c(j) = sum_k a(k) * b(k, j)
                // With transpose_b: c(j) = sum_k a(k) * b(j, k)
                ASR::expr_t *var_j = make_loop_var("__gpu_mm_j");
                ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

                ASR::expr_t *c_j = make_section_item(asgn->m_target, {var_j});
                ASR::expr_t *a_k = make_section_item(arg_a, {var_k});
                ASR::expr_t *b_kj = transpose_b
                    ? make_section_item(arg_b, {var_j, var_k})
                    : make_section_item(arg_b, {var_k, var_j});

                int k_dim = transpose_b ? 1 : 0;
                int j_dim = transpose_b ? 0 : 1;
                auto [k_start, k_end] = get_loop_bounds(arg_b, dims_b, k_dim);
                auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, j_dim);

                Vec<ASR::stmt_t*> k_body;
                k_body.reserve(al, 1);
                ASR::expr_t *prod = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, a_k,
                        ASR::binopType::Mul, b_kj, elem_type, nullptr));
                ASR::expr_t *sum = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, c_j,
                        ASR::binopType::Add, prod, elem_type, nullptr));
                k_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_j, sum,
                        nullptr, false, false)));

                Vec<ASR::stmt_t*> j_body;
                j_body.reserve(al, binop_other ? 3 : 2);
                j_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_j, zero,
                        nullptr, false, false)));
                j_body.push_back(al,
                    make_do_loop(var_k, k_start, k_end, k_body));

                if (binop_other) {
                    ASR::expr_t *other_op = binop_other;
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*other_op))
                        other_op = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                            other_op)->m_arg;
                    ASR::expr_t *other_j = make_section_item(
                        other_op, {var_j});
                    ASR::expr_t *lhs = matmul_is_left ? c_j : other_j;
                    ASR::expr_t *rhs = matmul_is_left ? other_j : c_j;
                    ASR::expr_t *combined = ASRUtils::EXPR(
                        ASR::make_RealBinOp_t(al, loc, lhs, binop_op,
                            rhs, elem_type, nullptr));
                    j_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, c_j, combined,
                            nullptr, false, false)));
                }

                new_body.push_back(al,
                    make_do_loop(var_j, j_start, j_end, j_body));
            } else if (overload_id == 3 && rank_a == 2 && rank_b == 2) {
                // c(i,j) = sum_k a(i,k) * b(k,j)
                // With transpose_a: a(i,k) becomes a(k,i)
                // With transpose_b: b(k,j) becomes b(j,k)
                ASR::expr_t *var_i = make_loop_var("__gpu_mm_i");
                ASR::expr_t *var_j = make_loop_var("__gpu_mm_j");
                ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

                ASR::expr_t *c_ij = make_section_item(asgn->m_target,
                    {var_i, var_j});
                ASR::expr_t *a_ik = transpose_a
                    ? make_section_item(arg_a, {var_k, var_i})
                    : make_section_item(arg_a, {var_i, var_k});
                ASR::expr_t *b_kj = transpose_b
                    ? make_section_item(arg_b, {var_j, var_k})
                    : make_section_item(arg_b, {var_k, var_j});

                int a_k_dim = transpose_a ? 0 : 1;
                int a_i_dim = transpose_a ? 1 : 0;
                int b_j_dim = transpose_b ? 0 : 1;
                auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, a_k_dim);
                auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, b_j_dim);
                auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, a_i_dim);

                Vec<ASR::stmt_t*> k_body;
                k_body.reserve(al, 1);
                ASR::expr_t *prod = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, a_ik,
                        ASR::binopType::Mul, b_kj, elem_type, nullptr));
                ASR::expr_t *sum = ASRUtils::EXPR(
                    ASR::make_RealBinOp_t(al, loc, c_ij,
                        ASR::binopType::Add, prod, elem_type, nullptr));
                k_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_ij, sum,
                        nullptr, false, false)));

                Vec<ASR::stmt_t*> j_body;
                j_body.reserve(al, binop_other ? 3 : 2);
                j_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, c_ij, zero,
                        nullptr, false, false)));
                j_body.push_back(al,
                    make_do_loop(var_k, k_start, k_end, k_body));

                if (binop_other) {
                    ASR::expr_t *other_op = binop_other;
                    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*other_op))
                        other_op = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                            other_op)->m_arg;
                    ASR::expr_t *other_ij = make_section_item(
                        other_op, {var_i, var_j});
                    ASR::expr_t *lhs = matmul_is_left ? c_ij : other_ij;
                    ASR::expr_t *rhs = matmul_is_left ? other_ij : c_ij;
                    ASR::expr_t *combined = ASRUtils::EXPR(
                        ASR::make_RealBinOp_t(al, loc, lhs, binop_op,
                            rhs, elem_type, nullptr));
                    j_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, c_ij, combined,
                            nullptr, false, false)));
                }

                Vec<ASR::stmt_t*> i_body;
                i_body.reserve(al, 1);
                i_body.push_back(al,
                    make_do_loop(var_j, j_start, j_end, j_body));

                new_body.push_back(al,
                    make_do_loop(var_i, i_start, i_end, i_body));
            } else {
                new_body.push_back(al, stmt);
                continue;
            }
            changed = true;
        }

        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }

    void inline_intrinsic_matmul(ASR::DoConcurrentLoop_t &x) {
        inline_matmul_stmts(x.m_body, x.n_body);
    }

    // Distribute ArrayItem indexing through an array expression tree
    // to produce a scalar expression. For example:
    //   sum(a + b) with index k  -->  a(k) + b(k)
    // instead of the incorrect (a + b)[k] which would be pointer arithmetic.
    ASR::expr_t* index_array_expr(ASR::expr_t *expr,
            ASR::array_index_t *idx_p, size_t idx_n,
            ASR::ttype_t *elem_type, const Location &loc) {
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg;
        }
        if (!ASRUtils::is_array(ASRUtils::expr_type(expr))) {
            return expr;
        }
        if (ASR::is_a<ASR::Var_t>(*expr)) {
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, expr,
                idx_p, idx_n, elem_type,
                ASR::arraystorageType::ColMajor, nullptr));
        }
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            ASR::RealBinOp_t *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
            ASR::expr_t *left = index_array_expr(op->m_left,
                idx_p, idx_n, elem_type, loc);
            ASR::expr_t *right = index_array_expr(op->m_right,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                left, op->m_op, right, elem_type, nullptr));
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
            ASR::IntegerBinOp_t *op =
                ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            ASR::expr_t *left = index_array_expr(op->m_left,
                idx_p, idx_n, elem_type, loc);
            ASR::expr_t *right = index_array_expr(op->m_right,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                left, op->m_op, right, elem_type, nullptr));
        }
        if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
            ASR::RealUnaryMinus_t *u =
                ASR::down_cast<ASR::RealUnaryMinus_t>(expr);
            ASR::expr_t *arg = index_array_expr(u->m_arg,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
                arg, elem_type, nullptr));
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
            ASR::IntegerUnaryMinus_t *u =
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr);
            ASR::expr_t *arg = index_array_expr(u->m_arg,
                idx_p, idx_n, elem_type, loc);
            return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                arg, elem_type, nullptr));
        }
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, expr,
            idx_p, idx_n, elem_type,
            ASR::arraystorageType::ColMajor, nullptr));
    }

    // Inline IntrinsicArrayFunction Sum inside a DoConcurrentLoop body.
    // Replaces:
    //   results(i) = sum(a)
    // With:
    //   __gpu_sum_res = 0.0
    //   do __gpu_sum_k = 1, n
    //     __gpu_sum_res = __gpu_sum_res + a(__gpu_sum_k)
    //   end do
    //   results(i) = __gpu_sum_res
    // This avoids generating a call to _lcompilers_Sum which is not
    // available inside Metal GPU kernels.
    // Search an expression tree for an IntrinsicArrayFunction Sum node.
    ASR::IntrinsicArrayFunction_t* find_sum_in_expr(ASR::expr_t *expr) {
        if (!expr) return nullptr;
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
            auto *iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expr);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        == ASRUtils::IntrinsicArrayFunctions::Sum) {
                return iaf;
            }
        }
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
            auto *found = find_sum_in_expr(op->m_left);
            if (found) return found;
            return find_sum_in_expr(op->m_right);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            auto *found = find_sum_in_expr(op->m_left);
            if (found) return found;
            return find_sum_in_expr(op->m_right);
        }
        if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
            return find_sum_in_expr(
                ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_value);
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
            return find_sum_in_expr(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_value);
        }
        if (ASR::is_a<ASR::Cast_t>(*expr)) {
            return find_sum_in_expr(
                ASR::down_cast<ASR::Cast_t>(expr)->m_arg);
        }
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            return find_sum_in_expr(
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg);
        }
        return nullptr;
    }

    // Replace a specific Sum node in an expression tree with a replacement.
    bool replace_sum_in_expr(ASR::expr_t* &expr,
            ASR::IntrinsicArrayFunction_t *target,
            ASR::expr_t *replacement) {
        if (!expr) return false;
        if (expr == (ASR::expr_t*)target) {
            expr = replacement;
            return true;
        }
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
            if (replace_sum_in_expr(op->m_left, target, replacement))
                return true;
            return replace_sum_in_expr(op->m_right, target, replacement);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
            auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            if (replace_sum_in_expr(op->m_left, target, replacement))
                return true;
            return replace_sum_in_expr(op->m_right, target, replacement);
        }
        if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
            return replace_sum_in_expr(
                ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_value,
                target, replacement);
        }
        if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
            return replace_sum_in_expr(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_value,
                target, replacement);
        }
        if (ASR::is_a<ASR::Cast_t>(*expr)) {
            return replace_sum_in_expr(
                ASR::down_cast<ASR::Cast_t>(expr)->m_arg,
                target, replacement);
        }
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            return replace_sum_in_expr(
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg,
                target, replacement);
        }
        return false;
    }

    // Extract nested Sum calls from assignment values into separate
    // temporary assignments so the main Sum inlining logic can handle them.
    // E.g., "cost = cost + sum(a)" becomes:
    //   "__gpu_sum_tmp = sum(a)"
    //   "cost = cost + __gpu_sum_tmp"
    void extract_nested_sums(ASR::stmt_t** &stmts, size_t &n_stmts,
                             SymbolTable *scope) {
        Vec<ASR::stmt_t*> expanded;
        expanded.reserve(al, n_stmts * 2);
        bool changed = false;

        for (size_t i = 0; i < n_stmts; i++) {
            ASR::stmt_t *stmt = stmts[i];
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                expanded.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn =
                ASR::down_cast<ASR::Assignment_t>(stmt);

            // Skip if value is already a direct Sum
            if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
                expanded.push_back(al, stmt);
                continue;
            }

            ASR::IntrinsicArrayFunction_t *sum_node =
                find_sum_in_expr(asgn->m_value);
            if (!sum_node) {
                expanded.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *sum_type = sum_node->m_type;

            SymbolTable *var_scope = scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            std::string tmp_name =
                var_scope->get_unique_name("__gpu_sum_tmp");
            ASR::symbol_t *tmp_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, var_scope,
                    s2c(al, tmp_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, sum_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            var_scope->add_symbol(tmp_name, tmp_sym);
            ASR::expr_t *tmp_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, tmp_sym));

            // Create: __gpu_sum_tmp = sum(a)
            ASR::expr_t *sum_expr = (ASR::expr_t*)sum_node;
            expanded.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, tmp_var, sum_expr,
                    nullptr, false, false)));

            // Replace sum node in original expression with tmp_var
            replace_sum_in_expr(asgn->m_value, sum_node, tmp_var);

            // Add modified original assignment
            expanded.push_back(al, stmt);
            changed = true;
        }

        if (changed) {
            stmts = expanded.p;
            n_stmts = expanded.n;
        }
    }

    void inline_sum_in_stmts(ASR::stmt_t** &stmts, size_t &n_stmts,
                             SymbolTable *scope) {
        // Pre-pass: extract nested Sum calls into separate assignments
        extract_nested_sums(stmts, n_stmts, scope);

        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_stmts * 4);
        bool changed = false;

        for (size_t si = 0; si < n_stmts; si++) {
            ASR::stmt_t *stmt = stmts[si];

            // Recurse into DoLoop bodies
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_sum_in_stmts(dl->m_body, dl->n_body, scope);
                new_body.push_back(al, stmt);
                continue;
            }

            // Recurse into Block bodies
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    inline_sum_in_stmts(block->m_body, block->n_body,
                        block->m_symtab);
                }
                new_body.push_back(al, stmt);
                continue;
            }

            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(asgn->m_value);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::Sum) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            ASR::expr_t *arr_arg = iaf->m_args[0];
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arr_arg)) {
                arr_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    arr_arg)->m_arg;
            }

            ASR::ttype_t *elem_type = iaf->m_type;

            SymbolTable *var_scope = scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            auto make_var = [&](const std::string &prefix,
                    ASR::ttype_t *type) -> ASR::expr_t* {
                std::string name = var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(name, sym);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            };

            ASR::expr_t *res_var = make_var("__gpu_sum_res", elem_type);
            ASR::expr_t *zero;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                    0.0, elem_type));
            } else {
                zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                    0, elem_type, ASR::integerbozType::Decimal));
            }

            // __gpu_sum_res = 0
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, res_var, zero,
                    nullptr, false, false)));

            std::vector<ASR::expr_t*> loop_vars;
            std::vector<ASR::expr_t*> loop_starts;
            std::vector<ASR::expr_t*> loop_ends;
            std::vector<ASR::expr_t*> loop_steps;
            Vec<ASR::array_index_t> idx_args = {};
            ASR::expr_t *base_arr = nullptr;
            ASR::expr_t *arr_elem = nullptr;

            if (ASR::is_a<ASR::ArraySection_t>(*arr_arg)) {
                // ArraySection (e.g., x(:,i)): loop over range dimensions,
                // use scalar indices directly
                ASR::ArraySection_t *section =
                    ASR::down_cast<ASR::ArraySection_t>(arr_arg);
                base_arr = section->m_v;
                std::vector<size_t> range_dims;
                for (size_t d = 0; d < section->n_args; d++) {
                    if (section->m_args[d].m_left != nullptr) {
                        range_dims.push_back(d);
                    }
                }
                if (range_dims.empty()) {
                    new_body.push_back(al, stmt);
                    continue;
                }
                for (size_t ri = 0; ri < range_dims.size(); ri++) {
                    size_t d = range_dims[ri];
                    loop_vars.push_back(make_var("__gpu_sum_k", int_type));
                    loop_starts.push_back(section->m_args[d].m_left);
                    loop_ends.push_back(section->m_args[d].m_right);
                    loop_steps.push_back(section->m_args[d].m_step);
                }
                idx_args.reserve(al, section->n_args);
                size_t lv_idx = 0;
                for (size_t d = 0; d < section->n_args; d++) {
                    ASR::array_index_t ai;
                    ai.loc = loc;
                    ai.m_left = nullptr;
                    ai.m_step = nullptr;
                    if (section->m_args[d].m_left != nullptr) {
                        ai.m_right = loop_vars[lv_idx++];
                    } else {
                        ai.m_right = section->m_args[d].m_right;
                    }
                    idx_args.push_back(al, ai);
                }
            } else {
                // Check if arr_arg is an expression containing
                // ArraySection nodes (e.g., a(1:n) + b(1:n))
                ASR::expr_t *sec_start = nullptr, *sec_end = nullptr;
                find_array_section_bounds(arr_arg, sec_start, sec_end);
                if (sec_start && sec_end) {
                    loop_vars.push_back(
                        make_var("__gpu_sum_k", int_type));
                    loop_starts.push_back(sec_start);
                    loop_ends.push_back(sec_end);
                    loop_steps.push_back(nullptr);
                    arr_elem = elementize_mask(arr_arg, loop_vars[0],
                        elem_type, loc);
                } else {
                    // Whole array: loop over all dimensions
                    ASR::ttype_t *arr_type =
                        ASRUtils::expr_type(arr_arg);
                    ASR::dimension_t *dims = nullptr;
                    int rank =
                        ASRUtils::extract_dimensions_from_ttype(
                            arr_type, dims);
                    if (rank < 1) {
                        new_body.push_back(al, stmt);
                        continue;
                    }
                    base_arr = arr_arg;
                    for (int d = 0; d < rank; d++) {
                        loop_vars.push_back(
                            make_var("__gpu_sum_k", int_type));
                        if (dims[d].m_start && dims[d].m_length) {
                            loop_starts.push_back(dims[d].m_start);
                            loop_ends.push_back(dims[d].m_length);
                        } else if (ASR::is_a<ASR::FunctionCall_t>(
                                *arr_arg)) {
                            // FunctionCall returns allocatable with
                            // unknown dims. Extract allocation bounds
                            // from the function body to avoid emitting
                            // ArrayBound on a FunctionCall (unsupported
                            // by Metal codegen).
                            ASR::FunctionCall_t *fc =
                                ASR::down_cast<ASR::FunctionCall_t>(
                                    arr_arg);
                            ASR::symbol_t *fn_sym =
                                ASRUtils::symbol_get_past_external(
                                    fc->m_name);
                            bool found = false;
                            if (ASR::is_a<ASR::Function_t>(*fn_sym)) {
                                ASR::Function_t *fn =
                                    ASR::down_cast<ASR::Function_t>(
                                        fn_sym);
                                std::string ret_name;
                                if (fn->m_return_var &&
                                        ASR::is_a<ASR::Var_t>(
                                            *fn->m_return_var)) {
                                    ret_name =
                                        ASRUtils::symbol_name(
                                            ASR::down_cast<
                                                ASR::Var_t>(
                                                fn->m_return_var)
                                                ->m_v);
                                }
                                for (size_t bi = 0;
                                        bi < fn->n_body &&
                                        !ret_name.empty() && !found;
                                        bi++) {
                                    if (!ASR::is_a<ASR::Allocate_t>(
                                            *fn->m_body[bi]))
                                        continue;
                                    ASR::Allocate_t *al_stmt =
                                        ASR::down_cast<
                                            ASR::Allocate_t>(
                                                fn->m_body[bi]);
                                    for (size_t ai2 = 0;
                                            ai2 < al_stmt->n_args;
                                            ai2++) {
                                        if (!al_stmt->m_args[ai2].m_a
                                            || !ASR::is_a<ASR::Var_t>(
                                                *al_stmt->m_args[ai2]
                                                    .m_a))
                                            continue;
                                        std::string aname =
                                            ASRUtils::symbol_name(
                                                ASR::down_cast<
                                                    ASR::Var_t>(
                                                    al_stmt->m_args
                                                        [ai2].m_a)
                                                    ->m_v);
                                        if (aname != ret_name)
                                            continue;
                                        if ((size_t)d <
                                                al_stmt->m_args[ai2]
                                                    .n_dims) {
                                            ASR::dimension_t &adim =
                                                al_stmt->m_args[ai2]
                                                    .m_dims[d];
                                            if (adim.m_start) {
                                                loop_starts.push_back(
                                                    adim.m_start);
                                            } else {
                                                loop_starts.push_back(
                                                    ASRUtils::EXPR(
                                                        ASR::make_IntegerConstant_t(
                                                            al, loc,
                                                            1,
                                                            int_type,
                                                            ASR::integerbozType::Decimal)));
                                            }
                                            if (adim.m_length) {
                                                loop_ends.push_back(
                                                    adim.m_length);
                                            }
                                            found = true;
                                        }
                                        break;
                                    }
                                }
                            }
                            if (!found) {
                                // No Allocate found in the function
                                // body.  Fall back to the actual call
                                // arguments: use the first array
                                // actual argument's bounds (the
                                // return shape typically matches the
                                // input shape for element-wise
                                // functions like r = a).
                                for (size_t ai3 = 0;
                                        ai3 < fc->n_args && !found;
                                        ai3++) {
                                    if (!fc->m_args[ai3].m_value)
                                        continue;
                                    ASR::expr_t *actual =
                                        fc->m_args[ai3].m_value;
                                    if (ASR::is_a<
                                            ASR::ArrayPhysicalCast_t>(
                                                *actual)) {
                                        actual = ASR::down_cast<
                                            ASR::ArrayPhysicalCast_t>(
                                                actual)->m_arg;
                                    }
                                    ASR::ttype_t *atype =
                                        ASRUtils::type_get_past_allocatable_pointer(
                                            ASRUtils::expr_type(
                                                actual));
                                    ASR::dimension_t *adims = nullptr;
                                    int arank =
                                        ASRUtils::extract_dimensions_from_ttype(
                                            atype, adims);
                                    if (arank < 1 ||
                                            (size_t)d >= (size_t)arank)
                                        continue;
                                    if (adims[d].m_start &&
                                            adims[d].m_length) {
                                        loop_starts.push_back(
                                            adims[d].m_start);
                                        loop_ends.push_back(
                                            adims[d].m_length);
                                        found = true;
                                    }
                                }
                            }
                            if (!found) {
                                ASR::expr_t *dim_expr =
                                    ASRUtils::EXPR(
                                        ASR::make_IntegerConstant_t(
                                            al, loc, d + 1,
                                            int_type,
                                            ASR::integerbozType::Decimal));
                                loop_starts.push_back(ASRUtils::EXPR(
                                    ASR::make_ArrayBound_t(al, loc,
                                        arr_arg, dim_expr, int_type,
                                        ASR::arrayboundType::LBound,
                                        nullptr)));
                                loop_ends.push_back(ASRUtils::EXPR(
                                    ASR::make_ArrayBound_t(al, loc,
                                        arr_arg, dim_expr, int_type,
                                        ASR::arrayboundType::UBound,
                                        nullptr)));
                            }
                        } else {
                            ASR::expr_t *dim_expr = ASRUtils::EXPR(
                                ASR::make_IntegerConstant_t(al, loc,
                                    d + 1, int_type,
                                    ASR::integerbozType::Decimal));
                            loop_starts.push_back(ASRUtils::EXPR(
                                ASR::make_ArrayBound_t(al, loc,
                                    arr_arg, dim_expr, int_type,
                                    ASR::arrayboundType::LBound,
                                    nullptr)));
                            loop_ends.push_back(ASRUtils::EXPR(
                                ASR::make_ArrayBound_t(al, loc,
                                    arr_arg, dim_expr, int_type,
                                    ASR::arrayboundType::UBound,
                                    nullptr)));
                        }
                        loop_steps.push_back(nullptr);
                    }
                    idx_args.reserve(al, rank);
                    for (int d = 0; d < rank; d++) {
                        ASR::array_index_t ai;
                        ai.loc = loc;
                        ai.m_left = nullptr;
                        ai.m_right = loop_vars[d];
                        ai.m_step = nullptr;
                        idx_args.push_back(al, ai);
                    }
                }
            }

            if (!arr_elem) {
                arr_elem = index_array_expr(base_arr,
                        idx_args.p, idx_args.n, elem_type, loc);
            }

            // res = res + a(k1, k2, ...) or a(k1, i, ...)
            ASR::expr_t *add_expr;
            if (ASR::is_a<ASR::Real_t>(*elem_type)) {
                add_expr = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                    res_var, ASR::binopType::Add, arr_elem,
                    elem_type, nullptr));
            } else {
                add_expr = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    res_var, ASR::binopType::Add, arr_elem,
                    elem_type, nullptr));
            }
            ASR::stmt_t *accum_stmt = ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, res_var, add_expr,
                    nullptr, false, false));

            // Build nested loops from innermost to outermost
            int n_loops = (int)loop_vars.size();
            Vec<ASR::stmt_t*> innermost_body;
            innermost_body.reserve(al, 1);
            innermost_body.push_back(al, accum_stmt);

            ASR::stmt_t *loop_nest = nullptr;
            for (int d = n_loops - 1; d >= 0; d--) {
                ASR::do_loop_head_t head;
                head.loc = loc;
                head.m_v = loop_vars[d];
                head.m_start = loop_starts[d];
                head.m_end = loop_ends[d];
                head.m_increment = loop_steps[d];
                if (d == n_loops - 1) {
                    loop_nest = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
                        nullptr, head, innermost_body.p, innermost_body.n,
                        nullptr, 0));
                } else {
                    Vec<ASR::stmt_t*> outer_body;
                    outer_body.reserve(al, 1);
                    outer_body.push_back(al, loop_nest);
                    loop_nest = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
                        nullptr, head, outer_body.p, outer_body.n,
                        nullptr, 0));
                }
            }
            new_body.push_back(al, loop_nest);

            // target = __gpu_sum_res
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, asgn->m_target, res_var,
                    nullptr, false, false)));

            changed = true;
        }

        if (changed) {
            stmts = new_body.p;
            n_stmts = new_body.n;
        }
    }

    void inline_intrinsic_sum(ASR::DoConcurrentLoop_t &x) {
        inline_sum_in_stmts(x.m_body, x.n_body, current_scope);
    }

    // Inline IntrinsicArrayFunction Transpose inside a DoConcurrentLoop body.
    // Replaces:
    //   b = transpose(a)
    // With:
    //   do __gpu_tr_j = 1, n
    //     do __gpu_tr_i = 1, m
    //       b(__gpu_tr_i, __gpu_tr_j) = a(__gpu_tr_j, __gpu_tr_i)
    //     end do
    //   end do
    // This avoids generating a call to _lcompilers_transpose which is not
    // available inside Metal GPU kernels.
    void inline_intrinsic_transpose(ASR::DoConcurrentLoop_t &x) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, x.n_body * 4);
        bool changed = false;

        for (size_t si = 0; si < x.n_body; si++) {
            ASR::stmt_t *stmt = x.m_body[si];
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            ASR::expr_t *value = asgn->m_value;
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*value)) {
                value = ASR::down_cast<ASR::ArrayPhysicalCast_t>(value)->m_arg;
            }
            if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*value)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(value);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::Transpose) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            ASR::expr_t *arr_arg = iaf->m_args[0];
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arr_arg)) {
                arr_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    arr_arg)->m_arg;
            }

            ASR::ttype_t *arr_type = ASRUtils::expr_type(arr_arg);
            ASR::dimension_t *dims = nullptr;
            int rank = ASRUtils::extract_dimensions_from_ttype(arr_type, dims);
            if (rank != 2) {
                new_body.push_back(al, stmt);
                continue;
            }

            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(asgn->m_target));

            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            auto make_var = [&](const std::string &prefix) -> ASR::expr_t* {
                std::string name = var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(name, sym);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
            };

            auto make_array_item_2d = [&](ASR::expr_t *arr,
                    ASR::expr_t *idx1, ASR::expr_t *idx2) -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, 2);
                ASR::array_index_t ai1;
                ai1.loc = loc;
                ai1.m_left = nullptr;
                ai1.m_right = idx1;
                ai1.m_step = nullptr;
                args.push_back(al, ai1);
                ASR::array_index_t ai2;
                ai2.loc = loc;
                ai2.m_left = nullptr;
                ai2.m_right = idx2;
                ai2.m_step = nullptr;
                args.push_back(al, ai2);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                    args.p, args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));
            };

            // a is (m, n) => b = transpose(a) is (n, m)
            // b(i, j) = a(j, i) for i=1..n, j=1..m
            ASR::expr_t *var_i = make_var("__gpu_tr_i");
            ASR::expr_t *var_j = make_var("__gpu_tr_j");

            ASR::expr_t *b_ij = make_array_item_2d(asgn->m_target,
                var_i, var_j);
            ASR::expr_t *a_ji = make_array_item_2d(arr_arg,
                var_j, var_i);

            // Inner loop body: b(i, j) = a(j, i)
            Vec<ASR::stmt_t*> inner_body;
            inner_body.reserve(al, 1);
            inner_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, b_ij, a_ji,
                    nullptr, false, false)));

            // i loops over rows of b = columns of a (dim 1 of result)
            // j loops over columns of b = rows of a (dim 0 of result)
            // a(m, n): dims[0] = m, dims[1] = n
            // b(n, m): i = 1..n, j = 1..m
            ASR::expr_t *i_start = dims[1].m_start;
            ASR::expr_t *i_end = dims[1].m_length;
            ASR::expr_t *j_start = dims[0].m_start;
            ASR::expr_t *j_end = dims[0].m_length;

            ASR::do_loop_head_t inner_head;
            inner_head.loc = loc;
            inner_head.m_v = var_i;
            inner_head.m_start = i_start;
            inner_head.m_end = i_end;
            inner_head.m_increment = nullptr;
            ASR::stmt_t *inner_loop = ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr, inner_head,
                    inner_body.p, inner_body.n, nullptr, 0));

            Vec<ASR::stmt_t*> outer_body;
            outer_body.reserve(al, 1);
            outer_body.push_back(al, inner_loop);

            ASR::do_loop_head_t outer_head;
            outer_head.loc = loc;
            outer_head.m_v = var_j;
            outer_head.m_start = j_start;
            outer_head.m_end = j_end;
            outer_head.m_increment = nullptr;
            ASR::stmt_t *outer_loop = ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr, outer_head,
                    outer_body.p, outer_body.n, nullptr, 0));

            new_body.push_back(al, outer_loop);
            changed = true;
        }

        if (changed) {
            x.m_body = new_body.p;
            x.n_body = new_body.n;
        }
    }

    // Inline ArraySection assignments inside a DoConcurrentLoop body.
    // Replaces:
    //   b(1:n(l), l) = 1.0   (ArraySection = ArrayBroadcast)
    // With:
    //   do __gpu_sec_i = 1, n(l)
    //     b(__gpu_sec_i, l) = 1.0
    //   end do
    // This avoids complex lowered code (descriptor temps, ArrayBound)
    // that the Metal backend cannot handle inside GPU kernels.
    void inline_array_section_assignment(ASR::DoConcurrentLoop_t &x) {
        bool changed = false;
        inline_array_section_in_body(x.m_body, x.n_body, changed);
    }

    void inline_array_section_in_body(ASR::stmt_t** &body, size_t &n_body,
            bool &changed) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 2);

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            // Recurse into DoLoop bodies
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_array_section_in_body(dl->m_body, dl->n_body,
                    changed);
                new_body.push_back(al, stmt);
                continue;
            }
            // Recurse into BlockCall bodies
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    inline_array_section_in_body(block->m_body,
                        block->n_body, changed);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::ArraySection_t>(*asgn->m_target)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(
                asgn->m_target);

            // Collect all range dimensions (have m_left, m_right, m_step
            // set, meaning it's a slice like 1:n, not a scalar index)
            std::vector<int> range_dims;
            for (size_t i = 0; i < as->n_args; i++) {
                if (as->m_args[i].m_left && as->m_args[i].m_right
                        && as->m_args[i].m_step) {
                    range_dims.push_back((int)i);
                }
            }
            if (range_dims.empty()) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            // Create loop variable(s) in the containing function/program
            // scope, not in any enclosing AssociateBlock scope.
            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            // Create a loop variable for each range dimension
            std::vector<ASR::expr_t*> loop_vars(range_dims.size());
            for (size_t ri = 0; ri < range_dims.size(); ri++) {
                std::string loop_var_name = var_scope->get_unique_name(
                    "__gpu_sec_i");
                ASR::symbol_t *loop_var_sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, var_scope,
                        s2c(al, loop_var_name), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                var_scope->add_symbol(loop_var_name, loop_var_sym);
                loop_vars[ri] = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, loop_var_sym));
            }

            // Build ArrayItem: replace each range dim with its loop var,
            // keep scalar-index dims as-is
            Vec<ASR::array_index_t> new_args;
            new_args.reserve(al, as->n_args);
            for (size_t i = 0; i < as->n_args; i++) {
                ASR::array_index_t idx;
                idx.loc = as->m_args[i].loc;
                // Check if this dimension is a range dimension
                bool is_range = false;
                for (size_t ri = 0; ri < range_dims.size(); ri++) {
                    if ((int)i == range_dims[ri]) {
                        idx.m_left = nullptr;
                        idx.m_right = loop_vars[ri];
                        idx.m_step = nullptr;
                        is_range = true;
                        break;
                    }
                }
                if (!is_range) {
                    idx.m_left = as->m_args[i].m_left;
                    idx.m_right = as->m_args[i].m_right;
                    idx.m_step = as->m_args[i].m_step;
                }
                new_args.push_back(al, idx);
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(as->m_v));
            ASR::expr_t *array_item = ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, as->m_v,
                    new_args.p, new_args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));

            // Elementize: recursively replace ArraySection with
            // ArrayItem and unwrap ArrayBroadcast in the RHS
            std::function<ASR::expr_t*(ASR::expr_t*)> elementize_rhs =
                [&](ASR::expr_t *e) -> ASR::expr_t* {
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    ASR::ArraySection_t *rhs_as =
                        ASR::down_cast<ASR::ArraySection_t>(e);
                    Vec<ASR::array_index_t> rhs_new_args;
                    rhs_new_args.reserve(al, rhs_as->n_args);
                    size_t rv_idx = 0;
                    for (size_t i = 0; i < rhs_as->n_args; i++) {
                        ASR::array_index_t idx;
                        idx.loc = rhs_as->m_args[i].loc;
                        if (rhs_as->m_args[i].m_left &&
                                rhs_as->m_args[i].m_right &&
                                rhs_as->m_args[i].m_step) {
                            if (rv_idx < loop_vars.size()) {
                                idx.m_left = nullptr;
                                idx.m_right = loop_vars[rv_idx];
                                idx.m_step = nullptr;
                                rv_idx++;
                            } else {
                                idx = rhs_as->m_args[i];
                            }
                        } else {
                            idx.m_left = rhs_as->m_args[i].m_left;
                            idx.m_right = rhs_as->m_args[i].m_right;
                            idx.m_step = rhs_as->m_args[i].m_step;
                        }
                        rhs_new_args.push_back(al, idx);
                    }
                    ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                        ASRUtils::expr_type(rhs_as->m_v));
                    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                        rhs_as->m_v, rhs_new_args.p, rhs_new_args.n,
                        rhs_elem, ASR::arraystorageType::ColMajor,
                        nullptr));
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                    return ASR::down_cast<ASR::ArrayBroadcast_t>(
                        e)->m_array;
                } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    ASR::RealBinOp_t *rb =
                        ASR::down_cast<ASR::RealBinOp_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealBinOp_t(al,
                        loc, elementize_rhs(rb->m_left), rb->m_op,
                        elementize_rhs(rb->m_right), et, nullptr));
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    ASR::IntegerBinOp_t *ib =
                        ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
                        loc, elementize_rhs(ib->m_left), ib->m_op,
                        elementize_rhs(ib->m_right), et, nullptr));
                } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(
                        *e)) {
                    ASR::IntrinsicElementalFunction_t *f =
                        ASR::down_cast<
                            ASR::IntrinsicElementalFunction_t>(e);
                    Vec<ASR::expr_t*> new_fargs;
                    new_fargs.reserve(al, f->n_args);
                    for (size_t i = 0; i < f->n_args; i++) {
                        new_fargs.push_back(al,
                            f->m_args[i]
                                ? elementize_rhs(f->m_args[i])
                                : nullptr);
                    }
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_IntrinsicElementalFunction_t(al,
                            loc, f->m_intrinsic_id, new_fargs.p,
                            new_fargs.n, f->m_overload_id, et,
                            f->m_value));
                } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(e);
                    // Check if the function natively returns an array
                    // (not an elemental function with array args).
                    // In that case, keep the original return type and
                    // wrap with ArrayItem below.
                    ASR::ttype_t *fc_type = ASRUtils::type_get_past_allocatable(
                        fc->m_type);
                    ASR::Function_t *fn = ASRUtils::get_function(fc->m_name);
                    ASR::ttype_t *fn_ret = fn
                        ? ASRUtils::get_FunctionType(fn)->m_return_var_type
                        : nullptr;
                    bool fn_returns_array = fn_ret &&
                        ASR::is_a<ASR::Array_t>(
                            *ASRUtils::type_get_past_allocatable(fn_ret));
                    ASR::FunctionType_t *fn_type = fn
                        ? ASRUtils::get_FunctionType(fn) : nullptr;
                    Vec<ASR::call_arg_t> new_fargs;
                    new_fargs.reserve(al, fc->n_args);
                    for (size_t i = 0; i < fc->n_args; i++) {
                        ASR::call_arg_t arg;
                        arg.loc = fc->m_args[i].loc;
                        if (!fc->m_args[i].m_value) {
                            arg.m_value = nullptr;
                        } else if (fn_returns_array && fn_type
                                && i < fn_type->n_arg_types
                                && ASR::is_a<ASR::Array_t>(
                                    *ASRUtils::type_get_past_allocatable(
                                        fn_type->m_arg_types[i]))) {
                            // Keep array arguments as-is for functions
                            // that return arrays; elementizing would
                            // turn ArraySection into scalar ArrayItem,
                            // breaking the function's array contract.
                            arg.m_value = fc->m_args[i].m_value;
                        } else {
                            arg.m_value = elementize_rhs(
                                fc->m_args[i].m_value);
                        }
                        new_fargs.push_back(al, arg);
                    }
                    if (fn_returns_array && ASR::is_a<ASR::Array_t>(*fc_type)) {
                        ASR::expr_t *new_fc = ASRUtils::EXPR(
                            ASR::make_FunctionCall_t(al, loc,
                                fc->m_name, fc->m_original_name,
                                new_fargs.p, new_fargs.n, fc->m_type,
                                fc->m_value, fc->m_dt));
                        Vec<ASR::array_index_t> rhs_args;
                        rhs_args.reserve(al, range_dims.size());
                        for (size_t ri = 0; ri < range_dims.size(); ri++) {
                            ASR::array_index_t idx;
                            idx.loc = loc;
                            idx.m_left = nullptr;
                            idx.m_right = loop_vars[ri];
                            idx.m_step = nullptr;
                            rhs_args.push_back(al, idx);
                        }
                        ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                            fc->m_type);
                        return ASRUtils::EXPR(
                            ASR::make_ArrayItem_t(al, loc, new_fc,
                                rhs_args.p, rhs_args.n, rhs_elem,
                                ASR::arraystorageType::ColMajor, nullptr));
                    }
                    ASR::ttype_t *et = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_FunctionCall_t(al, loc,
                            fc->m_name, fc->m_original_name,
                            new_fargs.p, new_fargs.n, et,
                            fc->m_value, fc->m_dt));
                } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                    return elementize_rhs(
                        ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                            e)->m_arg);
                } else if (ASR::is_a<ASR::Cast_t>(*e)) {
                    ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
                    ASR::ttype_t *ct = c->m_type;
                    if (ASR::is_a<ASR::Array_t>(*ct)) {
                        ct = ASRUtils::extract_type(ct);
                    }
                    return ASRUtils::EXPR(ASR::make_Cast_t(al, loc,
                        elementize_rhs(c->m_arg), c->m_kind, ct,
                        c->m_value, nullptr));
                }
                // Fallback: if still array-typed, wrap with ArrayItem
                ASR::ttype_t *e_type = ASRUtils::expr_type(e);
                if (ASR::is_a<ASR::Array_t>(*e_type)) {
                    Vec<ASR::array_index_t> rhs_args;
                    rhs_args.reserve(al, range_dims.size());
                    for (size_t ri = 0; ri < range_dims.size(); ri++) {
                        ASR::array_index_t idx;
                        idx.loc = loc;
                        idx.m_left = nullptr;
                        idx.m_right = loop_vars[ri];
                        idx.m_step = nullptr;
                        rhs_args.push_back(al, idx);
                    }
                    ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                        e_type);
                    return ASRUtils::EXPR(
                        ASR::make_ArrayItem_t(al, loc, e,
                            rhs_args.p, rhs_args.n, rhs_elem,
                            ASR::arraystorageType::ColMajor, nullptr));
                }
                return e;
            };
            ASR::expr_t *scalar_value = elementize_rhs(asgn->m_value);

            // Build innermost loop body: array_item = scalar_value
            Vec<ASR::stmt_t*> inner_body;
            inner_body.reserve(al, 1);
            inner_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, array_item, scalar_value,
                    nullptr, false, false)));

            // Build nested DoLoops from innermost to outermost
            ASR::stmt_t *loop_stmt = nullptr;
            for (int ri = (int)range_dims.size() - 1; ri >= 0; ri--) {
                int dim = range_dims[ri];
                ASR::do_loop_head_t head;
                head.loc = loc;
                head.m_v = loop_vars[ri];
                head.m_start = as->m_args[dim].m_left;
                head.m_end = as->m_args[dim].m_right;
                head.m_increment = nullptr;

                Vec<ASR::stmt_t*> body;
                body.reserve(al, 1);
                if (loop_stmt) {
                    body.push_back(al, loop_stmt);
                } else {
                    body.push_back(al, inner_body[0]);
                }
                loop_stmt = ASRUtils::STMT(
                    ASR::make_DoLoop_t(al, loc, nullptr,
                        head, body.p, body.n, nullptr, 0));
            }
            new_body.push_back(al, loop_stmt);

            changed = true;
        }

        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }

    // Inline whole-array assignments whose RHS contains ArraySection
    // wrapped in elemental operations (e.g., b = abs(a(:,l))).
    // Replaces:
    //   b = abs(a(:,l))
    // With:
    //   do __gpu_elem_i = lbound(a,1), ubound(a,1)
    //     b(__gpu_elem_i) = abs(a(__gpu_elem_i, l))
    //   end do
    void inline_elemental_array_var_assignment(ASR::DoConcurrentLoop_t &x) {
        bool changed = false;
        inline_elemental_array_var_in_body(x.m_body, x.n_body, changed);
    }

    void inline_elemental_array_var_in_body(ASR::stmt_t** &body,
            size_t &n_body, bool &changed) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body * 2);

        for (size_t si = 0; si < n_body; si++) {
            ASR::stmt_t *stmt = body[si];
            // Recurse into DoLoop bodies
            if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                inline_elemental_array_var_in_body(dl->m_body, dl->n_body,
                    changed);
                new_body.push_back(al, stmt);
                continue;
            }
            // Recurse into BlockCall bodies
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    inline_elemental_array_var_in_body(block->m_body,
                        block->n_body, changed);
                }
                new_body.push_back(al, stmt);
                continue;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

            // Only handle Var targets with array type
            if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::ttype_t *target_type = ASRUtils::type_get_past_allocatable(
                ASRUtils::expr_type(asgn->m_target));
            if (!ASR::is_a<ASR::Array_t>(*target_type)) {
                new_body.push_back(al, stmt);
                continue;
            }

            // Walk the RHS to find the first ArraySection
            ASR::ArraySection_t *first_as = nullptr;
            std::function<void(ASR::expr_t*)> find_array_section =
                [&](ASR::expr_t *e) {
                if (first_as) return;
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    first_as = ASR::down_cast<ASR::ArraySection_t>(e);
                } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
                    ASR::IntrinsicElementalFunction_t *f =
                        ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                    for (size_t i = 0; i < f->n_args; i++) {
                        if (f->m_args[i]) find_array_section(f->m_args[i]);
                    }
                } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(e);
                    for (size_t i = 0; i < fc->n_args; i++) {
                        if (fc->m_args[i].m_value)
                            find_array_section(fc->m_args[i].m_value);
                    }
                } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
                    find_array_section(rb->m_left);
                    find_array_section(rb->m_right);
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    find_array_section(ib->m_left);
                    find_array_section(ib->m_right);
                } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                    find_array_section(
                        ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg);
                } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                    find_array_section(
                        ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                    ASR::ArrayBroadcast_t *ab = ASR::down_cast<ASR::ArrayBroadcast_t>(e);
                    find_array_section(ab->m_array);
                } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                    ASR::ArrayPhysicalCast_t *apc =
                        ASR::down_cast<ASR::ArrayPhysicalCast_t>(e);
                    find_array_section(apc->m_arg);
                }
            };
            find_array_section(asgn->m_value);

            if (!first_as) {
                if (ASR::is_a<ASR::ArrayBroadcast_t>(*asgn->m_value)) {
                    // Handle whole-array broadcast assignment:
                    //   x = 1.0  (Var_array = ArrayBroadcast(scalar))
                    // Convert to: do i = 1, size(x); x(i) = 1.0; end do
                    ASR::ArrayBroadcast_t *ab =
                        ASR::down_cast<ASR::ArrayBroadcast_t>(asgn->m_value);
                    ASR::expr_t *scalar_value = ab->m_array;

                    Location loc = stmt->base.loc;
                    ASR::ttype_t *int_type = ASRUtils::TYPE(
                        ASR::make_Integer_t(al, loc, 4));
                    ASR::ttype_t *elem_type =
                        ASRUtils::extract_type(target_type);

                    ASR::Array_t *arr =
                        ASR::down_cast<ASR::Array_t>(target_type);
                    ASR::dimension_t *dims = arr->m_dims;
                    size_t n_dims = arr->n_dims;

                    SymbolTable *var_scope = current_scope;
                    while (var_scope && var_scope->asr_owner &&
                           var_scope->asr_owner->type ==
                               ASR::asrType::symbol &&
                           ASR::is_a<ASR::AssociateBlock_t>(
                               *ASR::down_cast<ASR::symbol_t>(
                                   var_scope->asr_owner))) {
                        var_scope = var_scope->parent;
                    }

                    auto make_bc_loop_var =
                        [&](const std::string &prefix) -> ASR::expr_t* {
                        std::string name =
                            var_scope->get_unique_name(prefix);
                        ASR::symbol_t *sym =
                            ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                var_scope, s2c(al, name), nullptr, 0,
                                ASR::intentType::Local, nullptr, nullptr,
                                ASR::storage_typeType::Default,
                                ASRUtils::duplicate_type(al, int_type),
                                nullptr, ASR::abiType::Source,
                                ASR::accessType::Public,
                                ASR::presenceType::Required, false));
                        var_scope->add_symbol(name, sym);
                        return ASRUtils::EXPR(
                            ASR::make_Var_t(al, loc, sym));
                    };

                    std::vector<ASR::expr_t*> loop_vars;
                    for (size_t d = 0; d < n_dims; d++) {
                        loop_vars.push_back(
                            make_bc_loop_var("__gpu_bc_i"));
                    }

                    Vec<ASR::array_index_t> lhs_args;
                    lhs_args.reserve(al, n_dims);
                    for (size_t d = 0; d < n_dims; d++) {
                        ASR::array_index_t idx;
                        idx.loc = loc;
                        idx.m_left = nullptr;
                        idx.m_right = loop_vars[d];
                        idx.m_step = nullptr;
                        lhs_args.push_back(al, idx);
                    }
                    ASR::expr_t *lhs_item = ASRUtils::EXPR(
                        ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                            lhs_args.p, lhs_args.n, elem_type,
                            ASR::arraystorageType::ColMajor, nullptr));

                    Vec<ASR::stmt_t*> innermost_body;
                    innermost_body.reserve(al, 1);
                    innermost_body.push_back(al, ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, lhs_item,
                            scalar_value, nullptr, false, false)));

                    ASR::stmt_t *loop_nest = nullptr;
                    for (int d = (int)n_dims - 1; d >= 0; d--) {
                        ASR::do_loop_head_t head;
                        head.loc = loc;
                        head.m_v = loop_vars[d];
                        head.m_start = dims[d].m_start;
                        head.m_end = dims[d].m_length;
                        head.m_increment = nullptr;
                        if (loop_nest == nullptr) {
                            loop_nest = ASRUtils::STMT(
                                ASR::make_DoLoop_t(al, loc, nullptr,
                                    head, innermost_body.p,
                                    innermost_body.n, nullptr, 0));
                        } else {
                            Vec<ASR::stmt_t*> outer_body;
                            outer_body.reserve(al, 1);
                            outer_body.push_back(al, loop_nest);
                            loop_nest = ASRUtils::STMT(
                                ASR::make_DoLoop_t(al, loc, nullptr,
                                    head, outer_body.p, outer_body.n,
                                    nullptr, 0));
                        }
                    }
                    new_body.push_back(al, loop_nest);
                    changed = true;
                    continue;
                }

                // Handle whole-array elemental assignment without
                // ArraySection (e.g., a = obj%eval(z) where eval is
                // elemental and z is a whole-array Var).
                // Convert to:
                //   do i = 1, size(a); a(i) = obj%eval(z(i)); end do
                ASR::ttype_t *rhs_type =
                    ASRUtils::type_get_past_allocatable(
                        ASRUtils::expr_type(asgn->m_value));
                if (!ASR::is_a<ASR::Array_t>(*rhs_type)) {
                    new_body.push_back(al, stmt);
                    continue;
                }

                // Skip decomposition for non-elemental FunctionCalls
                // that return arrays (e.g., a = f() where f returns
                // a whole array). Only elemental operations can be
                // safely decomposed into element-wise loops.
                if (ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(
                            asgn->m_value);
                    if (!ASRUtils::is_elemental(fc->m_name)) {
                        new_body.push_back(al, stmt);
                        continue;
                    }
                }

                ASR::Array_t *target_arr =
                    ASR::down_cast<ASR::Array_t>(target_type);

                Location loc = stmt->base.loc;
                ASR::ttype_t *int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4));
                ASR::ttype_t *elem_type =
                    ASRUtils::extract_type(target_type);
                ASR::dimension_t *dims = target_arr->m_dims;
                size_t n_dims = target_arr->n_dims;

                SymbolTable *var_scope = current_scope;
                while (var_scope && var_scope->asr_owner &&
                       var_scope->asr_owner->type ==
                           ASR::asrType::symbol &&
                       ASR::is_a<ASR::AssociateBlock_t>(
                           *ASR::down_cast<ASR::symbol_t>(
                               var_scope->asr_owner))) {
                    var_scope = var_scope->parent;
                }

                auto make_elem_loop_var =
                    [&](const std::string &prefix) -> ASR::expr_t* {
                    std::string name =
                        var_scope->get_unique_name(prefix);
                    ASR::symbol_t *sym =
                        ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::make_Variable_t_util(al, loc,
                            var_scope, s2c(al, name), nullptr, 0,
                            ASR::intentType::Local, nullptr, nullptr,
                            ASR::storage_typeType::Default,
                            ASRUtils::duplicate_type(al, int_type),
                            nullptr, ASR::abiType::Source,
                            ASR::accessType::Public,
                            ASR::presenceType::Required, false));
                    var_scope->add_symbol(name, sym);
                    return ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, sym));
                };

                std::vector<ASR::expr_t*> loop_vars;
                for (size_t d = 0; d < n_dims; d++) {
                    loop_vars.push_back(
                        make_elem_loop_var("__gpu_elem_i"));
                }

                // Elementize: replace array-typed Vars with ArrayItem
                std::function<ASR::expr_t*(ASR::expr_t*)> elementize =
                    [&](ASR::expr_t *e) -> ASR::expr_t* {
                    if (ASR::is_a<ASR::Var_t>(*e)) {
                        ASR::ttype_t *vtype =
                            ASRUtils::type_get_past_allocatable(
                                ASRUtils::expr_type(e));
                        if (ASR::is_a<ASR::Array_t>(*vtype)) {
                            ASR::ttype_t *velem =
                                ASRUtils::extract_type(vtype);
                            ASR::Array_t *va =
                                ASR::down_cast<ASR::Array_t>(vtype);
                            Vec<ASR::array_index_t> idx_args;
                            idx_args.reserve(al, va->n_dims);
                            for (size_t d = 0; d < va->n_dims; d++) {
                                ASR::array_index_t idx;
                                idx.loc = loc;
                                idx.m_left = nullptr;
                                idx.m_right = loop_vars[
                                    d < loop_vars.size() ? d : 0];
                                idx.m_step = nullptr;
                                idx_args.push_back(al, idx);
                            }
                            return ASRUtils::EXPR(
                                ASR::make_ArrayItem_t(al, loc, e,
                                    idx_args.p, idx_args.n, velem,
                                    ASR::arraystorageType::ColMajor,
                                    nullptr));
                        }
                        return e;
                    } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                        ASR::FunctionCall_t *fc =
                            ASR::down_cast<ASR::FunctionCall_t>(e);
                        Vec<ASR::call_arg_t> new_args;
                        new_args.reserve(al, fc->n_args);
                        for (size_t i = 0; i < fc->n_args; i++) {
                            ASR::call_arg_t arg;
                            arg.loc = fc->m_args[i].loc;
                            arg.m_value = fc->m_args[i].m_value
                                ? elementize(fc->m_args[i].m_value)
                                : nullptr;
                            new_args.push_back(al, arg);
                        }
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_FunctionCall_t(al, loc,
                                fc->m_name, fc->m_original_name,
                                new_args.p, new_args.n, ret_type,
                                fc->m_value, fc->m_dt));
                    } else if (ASR::is_a<
                            ASR::IntrinsicElementalFunction_t>(*e)) {
                        ASR::IntrinsicElementalFunction_t *f =
                            ASR::down_cast<
                                ASR::IntrinsicElementalFunction_t>(e);
                        Vec<ASR::expr_t*> new_args;
                        new_args.reserve(al, f->n_args);
                        for (size_t i = 0; i < f->n_args; i++) {
                            new_args.push_back(al,
                                f->m_args[i]
                                    ? elementize(f->m_args[i])
                                    : nullptr);
                        }
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_IntrinsicElementalFunction_t(
                                al, loc, f->m_intrinsic_id,
                                new_args.p, new_args.n,
                                f->m_overload_id, ret_type,
                                f->m_value));
                    } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                        ASR::RealBinOp_t *rb =
                            ASR::down_cast<ASR::RealBinOp_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_RealBinOp_t(al, loc,
                                elementize(rb->m_left), rb->m_op,
                                elementize(rb->m_right), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                        ASR::IntegerBinOp_t *ib =
                            ASR::down_cast<ASR::IntegerBinOp_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_IntegerBinOp_t(al, loc,
                                elementize(ib->m_left), ib->m_op,
                                elementize(ib->m_right), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                        ASR::RealUnaryMinus_t *u =
                            ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_RealUnaryMinus_t(al, loc,
                                elementize(u->m_arg), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                        ASR::IntegerUnaryMinus_t *u =
                            ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                        ASR::ttype_t *ret_type =
                            ASRUtils::extract_type(
                                ASRUtils::expr_type(e));
                        return ASRUtils::EXPR(
                            ASR::make_IntegerUnaryMinus_t(al, loc,
                                elementize(u->m_arg), ret_type,
                                nullptr));
                    } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                        return ASR::down_cast<ASR::ArrayBroadcast_t>(
                            e)->m_array;
                    } else if (ASR::is_a<
                            ASR::ArrayPhysicalCast_t>(*e)) {
                        return elementize(
                            ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                                e)->m_arg);
                    } else if (ASR::is_a<
                            ASR::StructInstanceMember_t>(*e)) {
                        ASR::ttype_t *mtype =
                            ASRUtils::type_get_past_allocatable(
                                ASRUtils::expr_type(e));
                        if (ASR::is_a<ASR::Array_t>(*mtype)) {
                            ASR::ttype_t *melem =
                                ASRUtils::extract_type(mtype);
                            ASR::Array_t *ma =
                                ASR::down_cast<ASR::Array_t>(mtype);
                            Vec<ASR::array_index_t> idx_args;
                            idx_args.reserve(al, ma->n_dims);
                            for (size_t d = 0; d < ma->n_dims; d++) {
                                ASR::array_index_t idx;
                                idx.loc = loc;
                                idx.m_left = nullptr;
                                idx.m_right = loop_vars[
                                    d < loop_vars.size() ? d : 0];
                                idx.m_step = nullptr;
                                idx_args.push_back(al, idx);
                            }
                            return ASRUtils::EXPR(
                                ASR::make_ArrayItem_t(al, loc, e,
                                    idx_args.p, idx_args.n, melem,
                                    ASR::arraystorageType::ColMajor,
                                    nullptr));
                        }
                        return e;
                    }
                    return e;
                };

                Vec<ASR::array_index_t> lhs_args;
                lhs_args.reserve(al, n_dims);
                for (size_t d = 0; d < n_dims; d++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = nullptr;
                    idx.m_right = loop_vars[d];
                    idx.m_step = nullptr;
                    lhs_args.push_back(al, idx);
                }
                ASR::expr_t *lhs_item = ASRUtils::EXPR(
                    ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                        lhs_args.p, lhs_args.n, elem_type,
                        ASR::arraystorageType::ColMajor, nullptr));

                ASR::expr_t *rhs_item = elementize(asgn->m_value);

                Vec<ASR::stmt_t*> innermost_body;
                innermost_body.reserve(al, 1);
                innermost_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, lhs_item,
                        rhs_item, nullptr, false, false)));

                ASR::stmt_t *loop_nest = nullptr;
                for (int d = (int)n_dims - 1; d >= 0; d--) {
                    ASR::do_loop_head_t head;
                    head.loc = loc;
                    head.m_v = loop_vars[d];
                    head.m_start = dims[d].m_start;
                    head.m_end = dims[d].m_length;
                    head.m_increment = nullptr;
                    if (loop_nest == nullptr) {
                        loop_nest = ASRUtils::STMT(
                            ASR::make_DoLoop_t(al, loc, nullptr,
                                head, innermost_body.p,
                                innermost_body.n, nullptr, 0));
                    } else {
                        Vec<ASR::stmt_t*> outer_body;
                        outer_body.reserve(al, 1);
                        outer_body.push_back(al, loop_nest);
                        loop_nest = ASRUtils::STMT(
                            ASR::make_DoLoop_t(al, loc, nullptr,
                                head, outer_body.p, outer_body.n,
                                nullptr, 0));
                    }
                }
                new_body.push_back(al, loop_nest);
                changed = true;
                continue;
            }

            // Find the range dimension
            int range_dim = -1;
            for (size_t i = 0; i < first_as->n_args; i++) {
                if (first_as->m_args[i].m_left && first_as->m_args[i].m_right
                        && first_as->m_args[i].m_step) {
                    if (range_dim != -1) {
                        range_dim = -1;
                        break;
                    }
                    range_dim = (int)i;
                }
            }
            if (range_dim == -1) {
                new_body.push_back(al, stmt);
                continue;
            }

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            ASR::expr_t *loop_start = first_as->m_args[range_dim].m_left;
            ASR::expr_t *loop_end = first_as->m_args[range_dim].m_right;

            // Create loop variable in the containing function/program scope
            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type == ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }
            std::string loop_var_name = var_scope->get_unique_name(
                "__gpu_elem_i");
            ASR::symbol_t *loop_var_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, var_scope,
                    s2c(al, loop_var_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, int_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            var_scope->add_symbol(loop_var_name, loop_var_sym);
            ASR::expr_t *loop_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, loop_var_sym));

            // Elementize: replace ArraySection with ArrayItem, recurse
            // into elemental wrappers
            std::function<ASR::expr_t*(ASR::expr_t*)> elementize =
                [&](ASR::expr_t *e) -> ASR::expr_t* {
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    ASR::ArraySection_t *as =
                        ASR::down_cast<ASR::ArraySection_t>(e);
                    Vec<ASR::array_index_t> new_args;
                    new_args.reserve(al, as->n_args);
                    for (size_t i = 0; i < as->n_args; i++) {
                        ASR::array_index_t idx;
                        idx.loc = as->m_args[i].loc;
                        if (as->m_args[i].m_left && as->m_args[i].m_right
                                && as->m_args[i].m_step) {
                            idx.m_left = nullptr;
                            idx.m_right = loop_var;
                            idx.m_step = nullptr;
                        } else {
                            idx.m_left = as->m_args[i].m_left;
                            idx.m_right = as->m_args[i].m_right;
                            idx.m_step = as->m_args[i].m_step;
                        }
                        new_args.push_back(al, idx);
                    }
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(as->m_v));
                    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                        as->m_v, new_args.p, new_args.n,
                        elem_type, ASR::arraystorageType::ColMajor, nullptr));
                } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
                    ASR::IntrinsicElementalFunction_t *f =
                        ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                    Vec<ASR::expr_t*> new_args;
                    new_args.reserve(al, f->n_args);
                    for (size_t i = 0; i < f->n_args; i++) {
                        new_args.push_back(al,
                            f->m_args[i] ? elementize(f->m_args[i])
                                         : nullptr);
                    }
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_IntrinsicElementalFunction_t(al, loc,
                            f->m_intrinsic_id, new_args.p, new_args.n,
                            f->m_overload_id, elem_type, f->m_value));
                } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    ASR::RealBinOp_t *rb =
                        ASR::down_cast<ASR::RealBinOp_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                        elementize(rb->m_left), rb->m_op,
                        elementize(rb->m_right), elem_type, nullptr));
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    ASR::IntegerBinOp_t *ib =
                        ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        elementize(ib->m_left), ib->m_op,
                        elementize(ib->m_right), elem_type, nullptr));
                } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                    ASR::RealUnaryMinus_t *u =
                        ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
                        elementize(u->m_arg), elem_type, nullptr));
                } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                    ASR::IntegerUnaryMinus_t *u =
                        ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                        elementize(u->m_arg), elem_type, nullptr));
                } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(e);
                    Vec<ASR::call_arg_t> new_args;
                    new_args.reserve(al, fc->n_args);
                    for (size_t i = 0; i < fc->n_args; i++) {
                        ASR::call_arg_t arg;
                        arg.loc = fc->m_args[i].loc;
                        arg.m_value = fc->m_args[i].m_value
                            ? elementize(fc->m_args[i].m_value)
                            : nullptr;
                        new_args.push_back(al, arg);
                    }
                    ASR::ttype_t *elem_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_FunctionCall_t(al, loc,
                            fc->m_name, fc->m_original_name,
                            new_args.p, new_args.n, elem_type,
                            fc->m_value, fc->m_dt));
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                    return ASR::down_cast<ASR::ArrayBroadcast_t>(e)->m_array;
                } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                    return elementize(
                        ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
                }
                return e;
            };

            // Build LHS ArrayItem: b(loop_var)
            ASR::ttype_t *elem_type = ASRUtils::extract_type(target_type);
            Vec<ASR::array_index_t> lhs_args;
            lhs_args.reserve(al, 1);
            ASR::array_index_t lhs_idx;
            lhs_idx.loc = loc;
            lhs_idx.m_left = nullptr;
            lhs_idx.m_right = loop_var;
            lhs_idx.m_step = nullptr;
            lhs_args.push_back(al, lhs_idx);
            ASR::expr_t *lhs_item = ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                    lhs_args.p, lhs_args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));

            // Build RHS: elementize the value expression
            ASR::expr_t *rhs_item = elementize(asgn->m_value);

            // Build loop body: lhs_item = rhs_item
            Vec<ASR::stmt_t*> loop_body;
            loop_body.reserve(al, 1);
            loop_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, lhs_item, rhs_item,
                    nullptr, false, false)));

            // Build DoLoop
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = loop_var;
            head.m_start = loop_start;
            head.m_end = loop_end;
            head.m_increment = nullptr;
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr,
                    head, loop_body.p, loop_body.n, nullptr, 0)));

            changed = true;
        }

        if (changed) {
            body = new_body.p;
            n_body = new_body.n;
        }
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (!pass_options.gpu_offload_metal && !pass_options.gpu_offload_cuda) return;

        // Skip loops with reduce clause (let do_loops handle as regular loop)
        if (x.n_reduction > 0) return;

        Location loc = x.base.base.loc;
        size_t n_dims = x.n_head;
        if (n_dims == 0 || n_dims > 3) return;

        for (size_t d = 0; d < n_dims; d++) {
            if (!x.m_head[d].m_v || !x.m_head[d].m_start || !x.m_head[d].m_end) return;
        }

        // Resolve associate variables to their original targets if this
        // DoConcurrentLoop is inside one or more nested AssociateBlocks.
        // The kernel function lives at the translation-unit level and
        // cannot reference symbols from any AssociateBlock's scope, so
        // we walk up through all enclosing AssociateBlock ancestors and
        // collect all their associate mappings.
        // The map is declared outside the block so it is available later
        // when resolving inner AssociateBlockCalls in the loop body.
        std::map<ASR::symbol_t*, ASR::expr_t*> enclosing_assoc_map;
        {
            std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map = enclosing_assoc_map;
            SymbolTable *scope = current_scope;
            while (scope && scope->asr_owner &&
                   scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
                    scope->asr_owner);
                if (is_a<ASR::Block_t>(*owner_sym)) {
                    scope = scope->parent;
                    continue;
                }
                if (!is_a<ASR::AssociateBlock_t>(*owner_sym)) break;
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast2<ASR::AssociateBlock_t>(scope->asr_owner);
                for (size_t i = 0; i < ab->n_body; i++) {
                    if (is_a<ASR::Associate_t>(*ab->m_body[i])) {
                        ASR::Associate_t *assoc = down_cast<ASR::Associate_t>(
                            ab->m_body[i]);
                        if (is_a<ASR::Var_t>(*assoc->m_target)) {
                            ASR::symbol_t *assoc_sym =
                                down_cast<ASR::Var_t>(assoc->m_target)->m_v;
                            assoc_map[assoc_sym] = assoc->m_value;
                        }
                    } else if (is_a<ASR::Assignment_t>(*ab->m_body[i])) {
                        // associate(n => constant_expr) generates an
                        // Assignment instead of Associate. Capture the
                        // initial value for variables owned by this
                        // AssociateBlock so they can be resolved.
                        // Only add if the symbol isn't already mapped
                        // (e.g., from a prior Associate node); otherwise
                        // we would overwrite the real alias with a
                        // regular assignment like `v = 0.`, whose RHS
                        // may reference `v` itself and cause infinite
                        // recursion during resolution.
                        ASR::Assignment_t *asgn = down_cast<ASR::Assignment_t>(
                            ab->m_body[i]);
                        if (is_a<ASR::Var_t>(*asgn->m_target)) {
                            ASR::symbol_t *sym =
                                down_cast<ASR::Var_t>(asgn->m_target)->m_v;
                            if (is_a<ASR::Variable_t>(*sym) &&
                                down_cast<ASR::Variable_t>(sym)->m_parent_symtab
                                    == ab->m_symtab &&
                                assoc_map.find(sym) == assoc_map.end()) {
                                assoc_map[sym] = asgn->m_value;
                            }
                        }
                    }
                }
                scope = scope->parent;
            }
            if (!assoc_map.empty()) {
                AssociateVarResolver resolver(al, assoc_map);
                ASR::DoConcurrentLoop_t &xx =
                    const_cast<ASR::DoConcurrentLoop_t&>(x);
                for (size_t d = 0; d < n_dims; d++) {
                    if (xx.m_head[d].m_start) {
                        resolver.current_expr = &(xx.m_head[d].m_start);
                        resolver.replace_expr(xx.m_head[d].m_start);
                    }
                    if (xx.m_head[d].m_end) {
                        resolver.current_expr = &(xx.m_head[d].m_end);
                        resolver.replace_expr(xx.m_head[d].m_end);
                    }
                    if (xx.m_head[d].m_increment) {
                        resolver.current_expr = &(xx.m_head[d].m_increment);
                        resolver.replace_expr(xx.m_head[d].m_increment);
                    }
                }
                AssociateVarResolverVisitor resolver_visitor(al, assoc_map);
                for (size_t i = 0; i < x.n_body; i++) {
                    resolver_visitor.visit_stmt(*x.m_body[i]);
                }
                // The statement visitor above does not descend into
                // BlockCall targets (Blocks have their own scope), so
                // resolve associate aliases in both block body statements
                // and block-local type expressions (e.g., `real a(n)` where
                // `n` is an associate alias from an enclosing associate).
                // This must be recursive to handle nested blocks.
                std::function<void(ASR::stmt_t**, size_t)>
                    resolve_assoc_in_blocks = [&](ASR::stmt_t **stmts,
                                                  size_t n_stmts) {
                    for (size_t i = 0; i < n_stmts; i++) {
                        if (!ASR::is_a<ASR::BlockCall_t>(*stmts[i])) continue;
                        ASR::BlockCall_t *bc =
                            ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                        if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                        ASR::Block_t *block =
                            ASR::down_cast<ASR::Block_t>(bc->m_m);
                        // Resolve in block body statements
                        for (size_t j = 0; j < block->n_body; j++) {
                            resolver_visitor.visit_stmt(*block->m_body[j]);
                        }
                        // Recurse into nested blocks
                        resolve_assoc_in_blocks(block->m_body,
                                                block->n_body);
                        // Resolve in block-local array dimension expressions
                        AssociateVarResolver type_resolver(al, assoc_map);
                        for (auto &item : block->m_symtab->get_scope()) {
                            if (!ASR::is_a<ASR::Variable_t>(*item.second))
                                continue;
                            ASR::Variable_t *var =
                                ASR::down_cast<ASR::Variable_t>(item.second);
                            if (!ASR::is_a<ASR::Array_t>(*var->m_type))
                                continue;
                            ASR::Array_t *arr =
                                ASR::down_cast<ASR::Array_t>(var->m_type);
                            for (size_t d = 0; d < arr->n_dims; d++) {
                                if (arr->m_dims[d].m_start) {
                                    type_resolver.current_expr =
                                        &(arr->m_dims[d].m_start);
                                    type_resolver.replace_expr(
                                        arr->m_dims[d].m_start);
                                }
                                if (arr->m_dims[d].m_length) {
                                    type_resolver.current_expr =
                                        &(arr->m_dims[d].m_length);
                                    type_resolver.replace_expr(
                                        arr->m_dims[d].m_length);
                                }
                            }
                        }
                    }
                };
                resolve_assoc_in_blocks(x.m_body, x.n_body);
                // Resolve associate aliases in enclosing Block scopes'
                // variable type expressions. When a do concurrent is
                // inside a Block that is inside an AssociateBlock, the
                // block-local arrays may use associate variables in
                // their dimension expressions (e.g., `real r(size(n))`
                // where `n` is an associate alias). These must be
                // resolved before kernel extraction moves the block
                // into the kernel scope where the AssociateBlock's
                // symtab is no longer reachable.
                {
                    SymbolTable *bs = current_scope;
                    while (bs && bs->asr_owner &&
                           bs->asr_owner->type == ASR::asrType::symbol) {
                        ASR::symbol_t *owner = down_cast<ASR::symbol_t>(
                            bs->asr_owner);
                        if (is_a<ASR::Block_t>(*owner)) {
                            AssociateVarResolver type_resolver(al,
                                assoc_map);
                            for (auto &item : bs->get_scope()) {
                                if (!ASR::is_a<ASR::Variable_t>(
                                        *item.second))
                                    continue;
                                ASR::Variable_t *var =
                                    ASR::down_cast<ASR::Variable_t>(
                                        item.second);
                                if (!ASR::is_a<ASR::Array_t>(
                                        *var->m_type))
                                    continue;
                                ASR::Array_t *arr =
                                    ASR::down_cast<ASR::Array_t>(
                                        var->m_type);
                                for (size_t d = 0; d < arr->n_dims;
                                     d++) {
                                    if (arr->m_dims[d].m_start) {
                                        type_resolver.current_expr =
                                            &(arr->m_dims[d].m_start);
                                        type_resolver.replace_expr(
                                            arr->m_dims[d].m_start);
                                    }
                                    if (arr->m_dims[d].m_length) {
                                        type_resolver.current_expr =
                                            &(arr->m_dims[d].m_length);
                                        type_resolver.replace_expr(
                                            arr->m_dims[d].m_length);
                                    }
                                }
                            }
                            bs = bs->parent;
                        } else if (is_a<ASR::AssociateBlock_t>(*owner)) {
                            bs = bs->parent;
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        // Inline IntrinsicArrayFunction All before kernel extraction
        inline_intrinsic_all(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline IntrinsicArrayFunction MatMul before kernel extraction
        inline_intrinsic_matmul(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline IntrinsicArrayFunction Sum before kernel extraction
        inline_intrinsic_sum(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline IntrinsicArrayFunction Transpose before kernel extraction
        inline_intrinsic_transpose(const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline ArraySection assignments before kernel extraction
        inline_array_section_assignment(
            const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Inline whole-array elemental assignments (e.g., b = abs(a(:,l)))
        inline_elemental_array_var_assignment(
            const_cast<ASR::DoConcurrentLoop_t&>(x));

        // Recursive helper to inline an AssociateBlock's body.
        // Collects Associate mappings into assoc_map and non-Associate
        // statements into resolved_stmts. Handles nested
        // AssociateBlockCalls by recursing into inner blocks.
        std::function<void(ASR::AssociateBlock_t*,
                           std::map<ASR::symbol_t*, ASR::expr_t*>&,
                           Vec<ASR::stmt_t*>&)>
            inline_assoc_body = [&](ASR::AssociateBlock_t *ab,
                                    std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map,
                                    Vec<ASR::stmt_t*> &resolved_stmts) {
            for (size_t ai = 0; ai < ab->n_body; ai++) {
                if (ASR::is_a<ASR::Associate_t>(*ab->m_body[ai])) {
                    ASR::Associate_t *assoc =
                        ASR::down_cast<ASR::Associate_t>(
                            ab->m_body[ai]);
                    if (ASR::is_a<ASR::Var_t>(*assoc->m_target)) {
                        ASR::symbol_t *sym =
                            ASR::down_cast<ASR::Var_t>(
                                assoc->m_target)->m_v;
                        ASRUtils::ExprStmtDuplicator dup(al);
                        dup.success = true;
                        ASR::expr_t *value =
                            dup.duplicate_expr(assoc->m_value);
                        if (!assoc_map.empty()) {
                            AssociateVarResolver resolver(al, assoc_map);
                            resolver.current_expr = &value;
                            resolver.replace_expr(value);
                        }
                        assoc_map[sym] = value;
                    }
                } else if (ASR::is_a<ASR::AssociateBlockCall_t>(
                               *ab->m_body[ai])) {
                    ASR::AssociateBlockCall_t *inner_abc =
                        ASR::down_cast<ASR::AssociateBlockCall_t>(
                            ab->m_body[ai]);
                    if (ASR::is_a<ASR::AssociateBlock_t>(
                            *inner_abc->m_m)) {
                        ASR::AssociateBlock_t *inner_ab =
                            ASR::down_cast<ASR::AssociateBlock_t>(
                                inner_abc->m_m);
                        inline_assoc_body(inner_ab, assoc_map,
                            resolved_stmts);
                        for (auto &es_item :
                                 inner_ab->m_symtab->get_scope()) {
                            if (!ASR::is_a<ASR::ExternalSymbol_t>(
                                    *es_item.second)) continue;
                            if (!current_scope->get_symbol(
                                    es_item.first)) {
                                ASR::down_cast<ASR::ExternalSymbol_t>(
                                    es_item.second)->m_parent_symtab =
                                        current_scope;
                                current_scope->add_symbol(
                                    es_item.first, es_item.second);
                            }
                        }
                        std::string inner_name = inner_ab->m_name;
                        ab->m_symtab->erase_symbol(inner_name);
                    } else {
                        resolved_stmts.push_back(al,
                            ab->m_body[ai]);
                    }
                } else if (ASR::is_a<ASR::Assignment_t>(
                               *ab->m_body[ai])) {
                    ASR::Assignment_t *asgn =
                        ASR::down_cast<ASR::Assignment_t>(
                            ab->m_body[ai]);
                    if (ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                        ASR::symbol_t *sym =
                            ASR::down_cast<ASR::Var_t>(
                                asgn->m_target)->m_v;
                        if (ASR::is_a<ASR::Variable_t>(*sym) &&
                            ASR::down_cast<ASR::Variable_t>(sym)
                                ->m_parent_symtab == ab->m_symtab &&
                            assoc_map.find(sym) == assoc_map.end()) {
                            assoc_map[sym] = asgn->m_value;
                        } else {
                            resolved_stmts.push_back(al,
                                ab->m_body[ai]);
                        }
                    } else {
                        resolved_stmts.push_back(al, ab->m_body[ai]);
                    }
                } else {
                    resolved_stmts.push_back(al, ab->m_body[ai]);
                }
            }
        };

        // Resolve AssociateBlocks inside the do concurrent body (e.g.,
        // block { associate(nh => n) ... } within the loop). GPU kernels
        // cannot use Pointer-based associate aliases, so we inline the
        // associate targets and replace the AssociateBlockCall with the
        // resolved statements.
        for (size_t bi = 0; bi < x.n_body; bi++) {
            if (!ASR::is_a<ASR::BlockCall_t>(*x.m_body[bi])) continue;
            ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
                x.m_body[bi]);
            if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
            Vec<ASR::stmt_t*> new_block_body;
            new_block_body.reserve(al, block->n_body);
            bool changed = false;
            for (size_t si = 0; si < block->n_body; si++) {
                if (!ASR::is_a<ASR::AssociateBlockCall_t>(
                        *block->m_body[si])) {
                    new_block_body.push_back(al, block->m_body[si]);
                    continue;
                }
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        block->m_body[si]);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*abc->m_m)) {
                    new_block_body.push_back(al, block->m_body[si]);
                    continue;
                }
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map(
                    enclosing_assoc_map);
                Vec<ASR::stmt_t*> resolved_stmts;
                resolved_stmts.reserve(al, ab->n_body);
                inline_assoc_body(ab, assoc_map, resolved_stmts);
                if (!assoc_map.empty()) {
                    AssociateVarResolverVisitor resolver(al, assoc_map);
                    for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                        resolver.visit_stmt(*resolved_stmts.p[ri]);
                    }
                }
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    new_block_body.push_back(al, resolved_stmts.p[ri]);
                }
                // Migrate ExternalSymbol entries (e.g., type-bound
                // procedure references like `1_t_f`) from the
                // AssociateBlock's symtab to the enclosing scope
                // before erasing it. These symbols are still
                // referenced by FunctionCall/SubroutineCall nodes
                // in the resolved statements and must remain
                // reachable for import_struct_def.
                for (auto &es_item : ab->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::ExternalSymbol_t>(
                            *es_item.second)) continue;
                    if (!current_scope->get_symbol(es_item.first)) {
                        ASR::down_cast<ASR::ExternalSymbol_t>(
                            es_item.second)->m_parent_symtab =
                                current_scope;
                        current_scope->add_symbol(es_item.first,
                            es_item.second);
                    }
                }
                std::string ab_name = ab->m_name;
                block->m_symtab->erase_symbol(ab_name);
                changed = true;
            }
            if (changed) {
                block->m_body = new_block_body.p;
                block->n_body = new_block_body.n;
            }
        }

        // Resolve bare AssociateBlockCall statements directly in the
        // do concurrent body (not wrapped in a BlockCall). GPU kernels
        // cannot use Pointer-based associate aliases, so we inline the
        // associate targets and replace each AssociateBlockCall with
        // the resolved statements.
        {
            Vec<ASR::stmt_t*> new_dc_body;
            new_dc_body.reserve(al, x.n_body);
            bool dc_changed = false;
            for (size_t bi = 0; bi < x.n_body; bi++) {
                if (!ASR::is_a<ASR::AssociateBlockCall_t>(*x.m_body[bi])) {
                    new_dc_body.push_back(al, x.m_body[bi]);
                    continue;
                }
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        x.m_body[bi]);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*abc->m_m)) {
                    new_dc_body.push_back(al, x.m_body[bi]);
                    continue;
                }
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                // Start with mappings from enclosing AssociateBlocks so
                // that references to outer associate variables (e.g., `m`
                // from an outer `associate(m => n)`) are resolved even
                // when they appear inside an inner associate block.
                std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map(
                    enclosing_assoc_map);
                Vec<ASR::stmt_t*> resolved_stmts;
                resolved_stmts.reserve(al, ab->n_body);
                inline_assoc_body(ab, assoc_map, resolved_stmts);
                if (!assoc_map.empty()) {
                    AssociateVarResolverVisitor resolver(al, assoc_map);
                    for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                        resolver.visit_stmt(*resolved_stmts.p[ri]);
                    }
                }
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    new_dc_body.push_back(al, resolved_stmts.p[ri]);
                }
                // Migrate ExternalSymbol entries from the
                // AssociateBlock's symtab to the enclosing scope
                // before erasing it (same as above for BlockCall).
                for (auto &es_item : ab->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::ExternalSymbol_t>(
                            *es_item.second)) continue;
                    if (!current_scope->get_symbol(es_item.first)) {
                        ASR::down_cast<ASR::ExternalSymbol_t>(
                            es_item.second)->m_parent_symtab =
                                current_scope;
                        current_scope->add_symbol(es_item.first,
                            es_item.second);
                    }
                }
                std::string ab_name = ab->m_name;
                current_scope->erase_symbol(ab_name);
                dc_changed = true;
            }
            if (dc_changed) {
                ASR::DoConcurrentLoop_t &xx =
                    const_cast<ASR::DoConcurrentLoop_t&>(x);
                xx.m_body = new_dc_body.p;
                xx.n_body = new_dc_body.n;
            }
        }

        // Detect if the do concurrent is inside a Block scope. If so,
        // block-local variables need to be collected as kernel parameters
        // rather than skipped. Walk up through AssociateBlock and Block
        // parents to find ALL enclosing Block scopes (e.g., do concurrent
        // inside a nested Block that accesses variables from outer Blocks).
        std::set<SymbolTable*> enclosing_block_scopes;
        {
            SymbolTable *scope = current_scope;
            while (scope && scope->asr_owner &&
                   scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
                    scope->asr_owner);
                if (is_a<ASR::Block_t>(*owner_sym)) {
                    enclosing_block_scopes.insert(scope);
                    scope = scope->parent;
                } else if (is_a<ASR::AssociateBlock_t>(*owner_sym)) {
                    scope = scope->parent;
                } else {
                    break;
                }
            }
        }

        // 1. Collect all symbols from body AND head expressions
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> involved_syms;
        GpuSymbolCollector collector(al, involved_syms, enclosing_block_scopes);
        collector.visit_DoConcurrentLoop(x);

        // Also collect symbols referenced in the type expressions (array
        // dimensions) of already-collected symbols. For example, if
        // `tmp(size(b))` is used in the body, `tmp` is collected but `b`
        // only appears in tmp's type — we must also pull `b` into
        // involved_syms so it becomes a kernel parameter.
        {
            bool added = true;
            while (added) {
                added = false;
                std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> extra_syms;
                GpuSymbolCollector type_collector(al, extra_syms, enclosing_block_scopes);
                for (auto &[sym_name, sym_info] : involved_syms) {
                    ASR::symbol_t *sym = current_scope->resolve_symbol(sym_name);
                    if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
                    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                    if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        if (arr->m_dims[d].m_start)
                            type_collector.visit_expr(*arr->m_dims[d].m_start);
                        if (arr->m_dims[d].m_length)
                            type_collector.visit_expr(*arr->m_dims[d].m_length);
                    }
                }
                for (auto &[name, info] : extra_syms) {
                    if (involved_syms.find(name) == involved_syms.end()) {
                        involved_syms[name] = info;
                        added = true;
                    }
                }
            }
        }

        // Skip loops containing real(8)/integer(8) types — Metal has no double/int64 support
        for (auto &sym : involved_syms) {
            ASR::ttype_t *t = sym.second.first;
            ASR::ttype_t *base_t = ASRUtils::type_get_past_array(t);
            if (base_t->type == ASR::ttypeType::Real &&
                ASR::down_cast<ASR::Real_t>(base_t)->m_kind == 8) return;
            if (base_t->type == ASR::ttypeType::Integer &&
                ASR::down_cast<ASR::Integer_t>(base_t)->m_kind == 8) return;
        }

        // Collect loop variable names
        std::vector<std::string> loop_var_names;
        for (size_t d = 0; d < n_dims; d++) {
            ASR::Var_t *lv = down_cast<ASR::Var_t>(x.m_head[d].m_v);
            loop_var_names.push_back(ASRUtils::symbol_name(lv->m_v));
        }

        // Find local scalar temporaries (assigned but not arrays, not loop vars)
        std::set<std::string> local_vars, assigned_vars;
        GpuLocalVarCollector lv_collector(local_vars, assigned_vars, enclosing_block_scopes);
        for (size_t i = 0; i < x.n_body; i++) {
            lv_collector.visit_stmt(*x.m_body[i]);
        }

        // Separate into kernel params vs local vars
        // Params: arrays + scalars that are read but NOT assigned in loop body
        // (unless they're also read from arrays, in which case they're params)
        // Local: scalars that are assigned in the loop body and not arrays
        std::set<std::string> loop_var_set(loop_var_names.begin(), loop_var_names.end());

        // Remove loop variables from involved_syms (kernel computes them)
        for (auto &lvn : loop_var_names) {
            involved_syms.erase(lvn);
        }

        // Identify which symbols are local temporaries (assigned scalar, non-array)
        // vs kernel parameters (arrays or read-only scalars)
        std::set<std::string> local_scalar_names;
        for (auto &name : assigned_vars) {
            if (loop_var_set.count(name)) continue;
            auto it = involved_syms.find(name);
            if (it != involved_syms.end()) {
                ASR::ttype_t *type = it->second.first;
                if (!ASRUtils::is_array(type)) {
                    // Check if this variable is used as an array argument too
                    // (e.g., a scalar that's also passed). For simplicity, only
                    // treat as local if it's a simple scalar temp.
                    local_scalar_names.insert(name);
                }
            }
        }

        // Remove local scalars from involved_syms (they become kernel locals)
        for (auto &name : local_scalar_names) {
            involved_syms.erase(name);
        }

        // Decompose struct variables with allocatable array members.
        // Metal cannot represent allocatable descriptors inside structs,
        // so we extract each allocatable array member into a separate
        // kernel buffer parameter and replace StructInstanceMember
        // references in the body with the new flat-array Var.
        GpuAllocStructMemberCollector alloc_collector;
        for (size_t i = 0; i < x.n_body; i++) {
            alloc_collector.visit_stmt(*x.m_body[i]);
        }
        // Maps (struct_name, member_name) -> decomposed parameter name
        std::map<std::pair<std::string, std::string>, std::string>
            decomp_map;
        // Info for creating host-side call arguments later
        struct DecompInfo {
            std::string struct_name;
            std::string member_name;
            std::string param_name;
            ASR::symbol_t *orig_mem_sym;
            ASR::ttype_t *alloc_type;
        };
        std::vector<DecompInfo> decomp_infos;
        for (auto &[struct_name, members] :
                alloc_collector.alloc_members) {
            if (involved_syms.find(struct_name) == involved_syms.end())
                continue;
            for (auto &[mem_name, mem_info] : members) {
                std::string param_name = struct_name + "__" + mem_name;
                decomp_map[{struct_name, mem_name}] = param_name;
                decomp_infos.push_back({struct_name, mem_name,
                    param_name, mem_info.first, mem_info.second});
            }
            // If struct only accessed through allocatable members,
            // remove from involved_syms (it won't be passed as a
            // kernel parameter)
            if (alloc_collector.has_non_alloc_access.find(struct_name)
                    == alloc_collector.has_non_alloc_access.end()) {
                involved_syms.erase(struct_name);
            }
        }

        // 2. Create kernel scope and parameters
        SymbolTable *tu_symtab = tu.m_symtab;
        std::string kernel_name = tu_symtab->get_unique_name(
            "__lfortran_gpu_kernel_" + std::to_string(gpu_kernel_counter++));
        SymbolTable *kernel_scope = al.make_new<SymbolTable>(tu_symtab);

        Vec<ASR::expr_t*> kernel_args;
        kernel_args.reserve(al, involved_syms.size());
        Vec<ASR::call_arg_t> call_args;
        call_args.reserve(al, involved_syms.size());

        SymbolTable *orig_scope = this->current_scope;

        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *type = sym_info.first;

            // For struct-typed variables, import the Struct into kernel scope
            ASR::symbol_t *type_decl = nullptr;
            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            if (orig_sym) {
                type_decl = import_struct_type(orig_sym,
                    orig_scope, kernel_scope, loc);
            }

            // Strip Allocatable wrapper: GPU kernel parameters receive
            // raw array data, not allocatable descriptors
            ASR::ttype_t *dup_type = ASRUtils::duplicate_type(al,
                ASRUtils::type_get_past_allocatable(type));

            // Recompute dependencies from the type alone (symbolic_value
            // and value are nullptr for kernel parameters)
            SetChar deps_vec;
            deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(
                al, deps_vec, dup_type, nullptr, nullptr, sym_name);

            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, sym_name), deps_vec.p, deps_vec.size(),
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default, dup_type,
                    type_decl, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(sym_name, param);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));

            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, orig_sym));
            call_args.push_back(al, carg);
        }

        // Create kernel parameters for decomposed allocatable struct
        // members. Each allocatable array member becomes a separate
        // flat-array buffer parameter.
        for (auto &di : decomp_infos) {
            ASR::ttype_t *flat_type = ASRUtils::duplicate_type(al,
                ASRUtils::type_get_past_allocatable(di.alloc_type));

            ASR::symbol_t *flat_type_decl = nullptr;
            if (ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(flat_type))) {
                ASR::symbol_t *mem_resolved =
                    ASRUtils::symbol_get_past_external(di.orig_mem_sym);
                if (is_a<ASR::Variable_t>(*mem_resolved)) {
                    ASR::Variable_t *mv =
                        down_cast<ASR::Variable_t>(mem_resolved);
                    if (mv->m_type_declaration) {
                        ASR::symbol_t *inner_struct_sym =
                            ASRUtils::symbol_get_past_external(
                                mv->m_type_declaration);
                        if (is_a<ASR::Struct_t>(*inner_struct_sym)) {
                            flat_type_decl = import_struct_def(
                                down_cast<ASR::Struct_t>(inner_struct_sym),
                                orig_scope, kernel_scope, loc);
                        }
                    }
                }
            }

            SetChar deps_vec;
            deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(
                al, deps_vec, flat_type, nullptr, nullptr, di.param_name);

            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, di.param_name), deps_vec.p, deps_vec.size(),
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default, flat_type,
                    flat_type_decl, ASR::abiType::Source,
                    ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            kernel_scope->add_symbol(di.param_name, param);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));

            // Host-side: pass StructInstanceMember(Var(x), member)
            ASR::symbol_t *orig_struct_sym =
                orig_scope->resolve_symbol(di.struct_name);
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        orig_struct_sym)),
                    di.orig_mem_sym, di.alloc_type, nullptr));
            call_args.push_back(al, carg);
        }

        // Pass dimension sizes for decomposed allocatable struct
        // members so the kernel can compute ArraySize and strides.
        {
            ASR::ttype_t *int_type_dim = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            for (auto &di : decomp_infos) {
                ASR::ttype_t *inner =
                    ASRUtils::type_get_past_allocatable(di.alloc_type);
                if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);

                ASR::symbol_t *k_sym =
                    kernel_scope->get_symbol(di.param_name);
                LCOMPILERS_ASSERT(k_sym);
                ASR::Variable_t *k_var =
                    ASR::down_cast<ASR::Variable_t>(k_sym);
                ASR::Array_t *k_arr = ASR::down_cast<ASR::Array_t>(
                    ASRUtils::type_get_past_allocatable(k_var->m_type));

                ASR::symbol_t *orig_struct_sym =
                    orig_scope->resolve_symbol(di.struct_name);
                ASR::expr_t *host_member_expr = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_struct_sym)),
                        di.orig_mem_sym, di.alloc_type, nullptr));

                for (size_t d = 0; d < arr->n_dims; d++) {
                    std::string dim_name = "__dim_" + di.param_name
                        + "_" + std::to_string(d);
                    ASR::symbol_t *dim_sym =
                        ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                kernel_scope, s2c(al, dim_name),
                                nullptr, 0,
                                ASR::intentType::InOut, nullptr,
                                nullptr,
                                ASR::storage_typeType::Default,
                                ASRUtils::duplicate_type(al,
                                    int_type_dim),
                                nullptr, ASR::abiType::Source,
                                ASR::accessType::Public,
                                ASR::presenceType::Required, false));
                    kernel_scope->add_symbol(dim_name, dim_sym);
                    kernel_args.push_back(al,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            dim_sym)));

                    ASR::expr_t *dim_expr = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc,
                            (int64_t)(d + 1), int_type_dim,
                            ASR::integerbozType::Decimal));
                    ASR::expr_t *host_size = ASRUtils::EXPR(
                        ASR::make_ArraySize_t(al, loc,
                            host_member_expr, dim_expr,
                            int_type_dim, nullptr));
                    ASR::call_arg_t carg;
                    carg.loc = loc;
                    carg.m_value = host_size;
                    call_args.push_back(al, carg);

                    k_arr->m_dims[d].m_length = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, dim_sym));
                    if (!k_arr->m_dims[d].m_start) {
                        k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                            ASR::make_IntegerConstant_t(al, loc, 1,
                                int_type_dim,
                                ASR::integerbozType::Decimal));
                    }
                }
            }
        }
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *orig_type = sym_info.first;
            if (!ASRUtils::is_allocatable(orig_type)) continue;
            ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(orig_type);
            if (!ASR::is_a<ASR::Array_t>(*inner)) continue;

            // Locate the kernel-scope Variable whose type we must update
            ASR::symbol_t *k_sym = kernel_scope->get_symbol(sym_name);
            LCOMPILERS_ASSERT(k_sym);
            ASR::Variable_t *k_var = ASR::down_cast<ASR::Variable_t>(k_sym);
            ASR::ttype_t *k_type = k_var->m_type;
            ASR::Array_t *k_arr = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_allocatable(k_type));

            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            ASR::ttype_t *int_type_dim = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            for (size_t d = 0; d < k_arr->n_dims; d++) {
                std::string dim_name = "__dim_" + sym_name + "_"
                    + std::to_string(d);
                ASR::symbol_t *dim_sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                        s2c(al, dim_name), nullptr, 0,
                        ASR::intentType::InOut, nullptr, nullptr,
                        ASR::storage_typeType::Default,
                        ASRUtils::duplicate_type(al, int_type_dim),
                        nullptr, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
                kernel_scope->add_symbol(dim_name, dim_sym);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, dim_sym)));

                // Host-side value: size(arr, dim=d+1)
                ASR::expr_t *dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                        int_type_dim, ASR::integerbozType::Decimal));
                ASR::expr_t *host_size = ASRUtils::EXPR(
                    ASR::make_ArraySize_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, orig_sym)),
                        dim_expr, int_type_dim, nullptr));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = host_size;
                call_args.push_back(al, carg);

                // Set dimension length in kernel-scope array type
                k_arr->m_dims[d].m_length = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, dim_sym));
                if (!k_arr->m_dims[d].m_start) {
                    k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc, 1,
                            int_type_dim, ASR::integerbozType::Decimal));
                }
            }
        }

        // Add total-size kernel parameters for allocatable array members
        // of struct-typed kernel parameters. These sizes are needed by
        // Metal inline functions that call size() on struct members.
        // Skip array-of-structs variables — StructInstanceMember requires
        // a scalar struct base, not an array of structs.
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *type = sym_info.first;
            ASR::ttype_t *inner_t = ASRUtils::type_get_past_allocatable(type);
            if (ASRUtils::is_array(inner_t)) continue;
            if (!ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(inner_t)))
                continue;
            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            if (!orig_sym || !is_a<ASR::Variable_t>(*orig_sym)) continue;
            ASR::Variable_t *orig_var =
                down_cast<ASR::Variable_t>(orig_sym);
            if (!orig_var->m_type_declaration) continue;
            ASR::symbol_t *struct_sym =
                ASRUtils::symbol_get_past_external(
                    orig_var->m_type_declaration);
            if (!is_a<ASR::Struct_t>(*struct_sym)) continue;
            ASR::Struct_t *st = down_cast<ASR::Struct_t>(struct_sym);
            ASR::ttype_t *int_type_sz = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            for (size_t m = 0; m < st->n_members; m++) {
                ASR::symbol_t *mem_sym =
                    st->m_symtab->get_symbol(st->m_members[m]);
                if (!mem_sym || !is_a<ASR::Variable_t>(*mem_sym))
                    continue;
                ASR::Variable_t *mv =
                    down_cast<ASR::Variable_t>(mem_sym);
                if (!ASRUtils::is_allocatable(mv->m_type)) continue;
                ASR::ttype_t *mem_inner =
                    ASRUtils::type_get_past_allocatable(mv->m_type);
                if (!ASR::is_a<ASR::Array_t>(*mem_inner)) continue;
                std::string size_name = "__size_" + sym_name + "_"
                    + std::string(st->m_members[m]);
                ASR::symbol_t *size_sym =
                    ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::make_Variable_t_util(al, loc,
                            kernel_scope, s2c(al, size_name),
                            nullptr, 0,
                            ASR::intentType::InOut, nullptr,
                            nullptr,
                            ASR::storage_typeType::Default,
                            ASRUtils::duplicate_type(al, int_type_sz),
                            nullptr, ASR::abiType::Source,
                            ASR::accessType::Public,
                            ASR::presenceType::Required, false));
                kernel_scope->add_symbol(size_name, size_sym);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        size_sym)));
                // Host-side: size(struct%member) (total size)
                // Look up the member symbol in the original struct's
                // scope for the ExternalSymbol reference used in the
                // program scope (needed for StructInstanceMember).
                ASR::symbol_t *orig_mem_ref = nullptr;
                for (auto &scope_item :
                        orig_scope->get_scope()) {
                    if (!is_a<ASR::ExternalSymbol_t>(
                            *scope_item.second)) continue;
                    ASR::ExternalSymbol_t *es =
                        down_cast<ASR::ExternalSymbol_t>(
                            scope_item.second);
                    ASR::symbol_t *resolved =
                        ASRUtils::symbol_get_past_external(
                            es->m_external);
                    if (resolved == mem_sym) {
                        orig_mem_ref = scope_item.second;
                        break;
                    }
                }
                if (!orig_mem_ref) orig_mem_ref = mem_sym;
                ASR::expr_t *host_member = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_sym)),
                        orig_mem_ref, mv->m_type, nullptr));
                ASR::expr_t *host_size = ASRUtils::EXPR(
                    ASR::make_ArraySize_t(al, loc,
                        host_member, nullptr, int_type_sz,
                        nullptr));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = host_size;
                call_args.push_back(al, carg);
            }
        }

        // Add allocatable-member data kernel parameters for struct-typed
        // kernel parameters that were NOT fully decomposed. These provide
        // the actual array data as separate device buffers so that Metal
        // inline functions can index into allocatable members.
        // Skip array-of-structs variables — StructInstanceMember requires
        // a scalar struct base, not an array of structs.
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::ttype_t *type = sym_info.first;
            ASR::ttype_t *inner_t = ASRUtils::type_get_past_allocatable(type);
            if (ASRUtils::is_array(inner_t)) continue;
            if (!ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(inner_t)))
                continue;
            ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
            if (!orig_sym || !is_a<ASR::Variable_t>(*orig_sym)) continue;
            ASR::Variable_t *orig_var =
                down_cast<ASR::Variable_t>(orig_sym);
            if (!orig_var->m_type_declaration) continue;
            ASR::symbol_t *struct_sym =
                ASRUtils::symbol_get_past_external(
                    orig_var->m_type_declaration);
            if (!is_a<ASR::Struct_t>(*struct_sym)) continue;
            ASR::Struct_t *st = down_cast<ASR::Struct_t>(struct_sym);
            for (size_t m = 0; m < st->n_members; m++) {
                ASR::symbol_t *mem_sym =
                    st->m_symtab->get_symbol(st->m_members[m]);
                if (!mem_sym || !is_a<ASR::Variable_t>(*mem_sym))
                    continue;
                ASR::Variable_t *mv =
                    down_cast<ASR::Variable_t>(mem_sym);
                if (!ASRUtils::is_allocatable(mv->m_type)) continue;
                ASR::ttype_t *mem_inner =
                    ASRUtils::type_get_past_allocatable(mv->m_type);
                if (!ASR::is_a<ASR::Array_t>(*mem_inner)) continue;
                std::string data_name = "__data_" + sym_name + "_"
                    + std::string(st->m_members[m]);
                ASR::ttype_t *data_type =
                    ASRUtils::duplicate_type(al, mem_inner);
                ASR::symbol_t *data_type_decl = nullptr;
                if (ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::extract_type(data_type)) &&
                        mv->m_type_declaration) {
                    ASR::symbol_t *inner_struct_sym =
                        ASRUtils::symbol_get_past_external(
                            mv->m_type_declaration);
                    if (is_a<ASR::Struct_t>(*inner_struct_sym)) {
                        data_type_decl = import_struct_def(
                            down_cast<ASR::Struct_t>(inner_struct_sym),
                            orig_scope, kernel_scope, loc);
                    }
                }
                SetChar deps_vec;
                deps_vec.reserve(al, 1);
                ASRUtils::collect_variable_dependencies(
                    al, deps_vec, data_type, nullptr, nullptr,
                    data_name);
                ASR::symbol_t *data_sym =
                    ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::make_Variable_t_util(al, loc,
                            kernel_scope, s2c(al, data_name),
                            deps_vec.p, deps_vec.size(),
                            ASR::intentType::InOut, nullptr,
                            nullptr,
                            ASR::storage_typeType::Default,
                            data_type,
                            data_type_decl, ASR::abiType::Source,
                            ASR::accessType::Public,
                            ASR::presenceType::Required, false));
                kernel_scope->add_symbol(data_name, data_sym);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        data_sym)));
                ASR::symbol_t *orig_mem_ref = nullptr;
                for (auto &scope_item :
                        orig_scope->get_scope()) {
                    if (!is_a<ASR::ExternalSymbol_t>(
                            *scope_item.second)) continue;
                    ASR::ExternalSymbol_t *es =
                        down_cast<ASR::ExternalSymbol_t>(
                            scope_item.second);
                    ASR::symbol_t *resolved =
                        ASRUtils::symbol_get_past_external(
                            es->m_external);
                    if (resolved == mem_sym) {
                        orig_mem_ref = scope_item.second;
                        break;
                    }
                }
                if (!orig_mem_ref) orig_mem_ref = mem_sym;
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_sym)),
                        orig_mem_ref, mv->m_type, nullptr));
                call_args.push_back(al, carg);
            }
        }

        // Create loop variables in kernel scope (local, not parameters)
        for (size_t d = 0; d < n_dims; d++) {
            ASR::Var_t *lv = down_cast<ASR::Var_t>(x.m_head[d].m_v);
            ASR::ttype_t *loop_var_type = ASRUtils::symbol_type(lv->m_v);
            std::string lvn = loop_var_names[d];
            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, lvn), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, loop_var_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(lvn, param);
        }

        // Create local scalar temporaries in kernel scope
        for (auto &name : local_scalar_names) {
            auto it_orig = orig_scope->resolve_symbol(name);
            if (!it_orig) continue;
            ASR::ttype_t *type = ASRUtils::symbol_type(it_orig);
            ASR::symbol_t *type_decl = import_struct_type(it_orig,
                orig_scope, kernel_scope, loc);
            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, type),
                    type_decl, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(name, param);
        }

        // Import functions/subroutines called in the do concurrent body
        // into the kernel scope so FunctionCall/SubroutineCall nodes
        // can reference them after symbol remapping.
        // Collect transitively: if f() calls g(), both must be imported.
        {
            GpuFunctionCollector func_collector;
            for (size_t i = 0; i < x.n_body; i++) {
                func_collector.visit_stmt(*x.m_body[i]);
            }
            {
                bool added = true;
                while (added) {
                    added = false;
                    GpuFunctionCollector transitive_collector;
                    for (auto &[fn_name, fn_sym] : func_collector.functions) {
                        ASR::symbol_t *fn_resolved =
                            ASRUtils::symbol_get_past_external(fn_sym);
                        ASR::Function_t *fn = nullptr;
                        if (ASR::is_a<ASR::Function_t>(*fn_resolved)) {
                            fn = ASR::down_cast<ASR::Function_t>(fn_resolved);
                        } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                                *fn_resolved)) {
                            ASR::StructMethodDeclaration_t *smd =
                                ASR::down_cast<ASR::StructMethodDeclaration_t>(
                                    fn_resolved);
                            ASR::symbol_t *proc =
                                ASRUtils::symbol_get_past_external(smd->m_proc);
                            if (ASR::is_a<ASR::Function_t>(*proc)) {
                                fn = ASR::down_cast<ASR::Function_t>(proc);
                            }
                        }
                        if (fn) {
                            ASR::Function_t *fn_impl = fn;
                            ASR::FunctionType_t *fn_ft =
                                ASR::down_cast<ASR::FunctionType_t>(
                                    fn->m_function_signature);
                            if (fn_ft->m_deftype ==
                                    ASR::deftypeType::Interface) {
                                std::string pname = fn->m_name;
                                bool found_impl = false;
                                for (auto &tu_item :
                                        tu.m_symtab->get_scope()) {
                                    if (!ASR::is_a<ASR::Module_t>(
                                            *tu_item.second)) continue;
                                    ASR::Module_t *mod =
                                        ASR::down_cast<ASR::Module_t>(
                                            tu_item.second);
                                    ASR::symbol_t *impl_sym =
                                        mod->m_symtab->get_symbol(pname);
                                    if (!impl_sym ||
                                        !ASR::is_a<ASR::Function_t>(
                                            *impl_sym)) continue;
                                    ASR::Function_t *impl_func =
                                        ASR::down_cast<ASR::Function_t>(
                                            impl_sym);
                                    ASR::FunctionType_t *impl_ft =
                                        ASR::down_cast<ASR::FunctionType_t>(
                                            impl_func
                                                ->m_function_signature);
                                    if (impl_ft->m_deftype ==
                                            ASR::deftypeType::Implementation) {
                                        fn_impl = impl_func;
                                        found_impl = true;
                                        break;
                                    }
                                }
                                if (!found_impl) {
                                    // Load submodule from .smod file
                                    // on disk (--separate-compilation).
                                    SymbolTable *parent_st =
                                        fn->m_symtab->parent;
                                    if (parent_st->asr_owner &&
                                            parent_st->asr_owner->type ==
                                                ASR::asrType::symbol &&
                                            ASR::is_a<ASR::Module_t>(
                                                *ASR::down_cast<
                                                    ASR::symbol_t>(
                                                    parent_st
                                                        ->asr_owner))) {
                                        std::string parent_mod =
                                            ASR::down_cast<ASR::Module_t>(
                                                ASR::down_cast<
                                                    ASR::symbol_t>(
                                                    parent_st
                                                        ->asr_owner))
                                                ->m_name;
                                        std::string smod_prefix =
                                            parent_mod + "@";
                                        std::vector<
                                            std::filesystem::path>
                                                mod_dirs;
                                        mod_dirs.push_back(
                                            pass_options
                                                .runtime_library_dir);
                                        mod_dirs.push_back(
                                            pass_options.mod_files_dir);
                                        mod_dirs.insert(mod_dirs.end(),
                                            pass_options.include_dirs
                                                .begin(),
                                            pass_options.include_dirs
                                                .end());
                                        for (auto &dir : mod_dirs) {
                                            if (dir.empty())
                                                dir = ".";
                                            if (!std::filesystem::
                                                    is_directory(dir))
                                                continue;
                                            for (auto &file :
                                                    std::filesystem::
                                                        directory_iterator(
                                                            dir)) {
                                                std::string fname =
                                                    file.path()
                                                        .filename()
                                                        .string();
                                                if (!startswith(fname,
                                                        smod_prefix) ||
                                                    !endswith(fname,
                                                        ".smod"))
                                                    continue;
                                                std::string content;
                                                if (!read_file(
                                                        file.path()
                                                            .string(),
                                                        content) ||
                                                    content.empty())
                                                    continue;
                                                LocationManager
                                                    lm_tmp;
                                                auto res =
                                                    load_modfile(
                                                        al, content,
                                                        false,
                                                        *tu.m_symtab,
                                                        lm_tmp);
                                                if (!res.ok) continue;
                                                load_submodule_deps(
                                                    *res.result);
                                                fix_external_symbols(
                                                    *res.result,
                                                    *tu.m_symtab);
                                                ASR::Module_t *submod =
                                                    ASRUtils::
                                                        extract_module(
                                                            *res.result);
                                                ASR::symbol_t
                                                    *impl_sym =
                                                    submod->m_symtab
                                                        ->get_symbol(
                                                            pname);
                                                if (!impl_sym ||
                                                    !ASR::is_a<
                                                        ASR::Function_t
                                                            >(*impl_sym))
                                                    continue;
                                                ASR::Function_t
                                                    *impl_func =
                                                    ASR::down_cast<
                                                        ASR::Function_t>(
                                                            impl_sym);
                                                ASR::FunctionType_t
                                                    *impl_ft =
                                                    ASR::down_cast<
                                                        ASR::FunctionType_t>(
                                                        impl_func
                                                        ->m_function_signature);
                                                if (impl_ft->m_deftype
                                                        != ASR::
                                                        deftypeType::
                                                        Implementation)
                                                    continue;
                                                fn_impl = impl_func;
                                                found_impl = true;
                                                break;
                                            }
                                            if (found_impl) break;
                                        }
                                    }
                                }
                            }
                            for (size_t i = 0; i < fn_impl->n_body; i++) {
                                transitive_collector.visit_stmt(
                                    *fn_impl->m_body[i]);
                            }
                        }
                    }
                    for (auto &[name, sym] : transitive_collector.functions) {
                        if (func_collector.functions.find(name) ==
                                func_collector.functions.end()) {
                            func_collector.functions[name] = sym;
                            added = true;
                        }
                    }
                }
            }
            ASRUtils::SymbolDuplicator sym_dup(al);
            for (auto &[func_name, func_sym] : func_collector.functions) {
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(func_sym);
                if (kernel_scope->get_symbol(func_name)) {
                    // ExternalSymbol already created (e.g., by
                    // import_struct_def). Still need to import the
                    // function body for StructMethodDeclaration calls
                    // so the Metal backend can generate shader code.
                } else if (ASR::is_a<ASR::ExternalSymbol_t>(*func_sym) &&
                           ASR::is_a<ASR::Function_t>(*resolved)) {
                    // The function is accessed via use-association
                    // (ExternalSymbol). Duplicate the underlying function
                    // body into the kernel scope so its types reference
                    // the kernel's struct copies (not the module's).
                    ASR::Function_t *resolved_func =
                        ASR::down_cast<ASR::Function_t>(resolved);
                    ASR::FunctionType_t *resolved_ftype =
                        ASR::down_cast<ASR::FunctionType_t>(
                            resolved_func->m_function_signature);
                    if (resolved_ftype->m_deftype ==
                            ASR::deftypeType::Interface) {
                        // Submodule interface: find the Implementation
                        // in a submodule already in the TU, or load it
                        // from disk (needed for --separate-compilation).
                        std::string pname =
                            ASRUtils::symbol_name(resolved);
                        bool found = false;
                        for (auto &tu_item :
                                tu.m_symtab->get_scope()) {
                            if (!ASR::is_a<ASR::Module_t>(
                                    *tu_item.second)) continue;
                            ASR::Module_t *mod =
                                ASR::down_cast<ASR::Module_t>(
                                    tu_item.second);
                            ASR::symbol_t *impl_sym =
                                mod->m_symtab->get_symbol(pname);
                            if (!impl_sym ||
                                !ASR::is_a<ASR::Function_t>(
                                    *impl_sym)) continue;
                            ASR::Function_t *impl_func =
                                ASR::down_cast<ASR::Function_t>(
                                    impl_sym);
                            ASR::FunctionType_t *impl_ft =
                                ASR::down_cast<ASR::FunctionType_t>(
                                    impl_func
                                        ->m_function_signature);
                            if (impl_ft->m_deftype !=
                                    ASR::deftypeType::Implementation)
                                continue;
                            resolved = impl_sym;
                            found = true;
                            break;
                        }
                        if (!found) {
                            // Load submodule from smod file on disk.
                            SymbolTable *parent_st =
                                ASRUtils::symbol_parent_symtab(
                                    resolved);
                            if (parent_st->asr_owner &&
                                    parent_st->asr_owner->type ==
                                        ASR::asrType::symbol &&
                                    ASR::is_a<ASR::Module_t>(
                                        *ASR::down_cast<ASR::symbol_t>(
                                            parent_st->asr_owner))) {
                                std::string parent_mod =
                                    ASR::down_cast<ASR::Module_t>(
                                        ASR::down_cast<ASR::symbol_t>(
                                            parent_st->asr_owner))
                                        ->m_name;
                                std::string smod_prefix =
                                    parent_mod + "@";
                                std::vector<std::filesystem::path>
                                    mod_dirs;
                                mod_dirs.push_back(
                                    pass_options.runtime_library_dir);
                                mod_dirs.push_back(
                                    pass_options.mod_files_dir);
                                mod_dirs.insert(mod_dirs.end(),
                                    pass_options.include_dirs.begin(),
                                    pass_options.include_dirs.end());
                                for (auto &dir : mod_dirs) {
                                    if (dir.empty())
                                        dir = ".";
                                    if (!std::filesystem::is_directory(
                                            dir)) continue;
                                    for (auto &file :
                                            std::filesystem::
                                                directory_iterator(
                                                    dir)) {
                                        std::string fname =
                                            file.path().filename()
                                                .string();
                                        if (!startswith(fname,
                                                smod_prefix) ||
                                            !endswith(fname, ".smod"))
                                            continue;
                                        std::string content;
                                        if (!read_file(
                                                file.path().string(),
                                                content) ||
                                            content.empty())
                                            continue;
                                        LocationManager lm_tmp;
                                        auto res = load_modfile(
                                            al, content, false,
                                            *tu.m_symtab, lm_tmp);
                                        if (!res.ok) continue;
                                        load_submodule_deps(
                                            *res.result);
                                        fix_external_symbols(
                                            *res.result,
                                            *tu.m_symtab);
                                        ASR::Module_t *submod =
                                            ASRUtils::extract_module(
                                                *res.result);
                                        ASR::symbol_t *impl_sym =
                                            submod->m_symtab
                                                ->get_symbol(pname);
                                        if (!impl_sym ||
                                            !ASR::is_a<ASR::Function_t>(
                                                *impl_sym)) continue;
                                        ASR::Function_t *impl_func =
                                            ASR::down_cast<
                                                ASR::Function_t>(
                                                    impl_sym);
                                        ASR::FunctionType_t *impl_ft =
                                            ASR::down_cast<
                                                ASR::FunctionType_t>(
                                                    impl_func
                                                    ->m_function_signature);
                                        if (impl_ft->m_deftype !=
                                                ASR::deftypeType::
                                                    Implementation)
                                            continue;
                                        resolved = impl_sym;
                                        found = true;
                                        break;
                                    }
                                    if (found) break;
                                }
                            }
                        }
                    }
                    if (ASR::is_a<ASR::Function_t>(*resolved)) {
                        ASR::Function_t *rf =
                            ASR::down_cast<ASR::Function_t>(resolved);
                        ASR::FunctionType_t *rft =
                            ASR::down_cast<ASR::FunctionType_t>(
                                rf->m_function_signature);
                        if (rft->m_deftype ==
                                ASR::deftypeType::Interface) {
                            // Still an interface after searching TU and
                            // .smod files — the submodule body is
                            // unavailable (e.g., parallel build race).
                            // Error out instead of generating an empty
                            // function in the Metal shader.
                            throw LCompilersException(
                                "GPU Metal offload: cannot find "
                                "submodule implementation for '" +
                                std::string(rf->m_name) + "'; "
                                "ensure the submodule is compiled "
                                "before the file that uses it");
                        }
                    }
                    std::string real_name =
                        ASRUtils::symbol_name(resolved);
                    // When two modules define functions with the same
                    // name (e.g., both have "my_construct"), the first
                    // gets added under real_name. For subsequent
                    // collisions, sanitize the ExternalSymbol name to
                    // a valid C identifier to disambiguate.
                    std::string dup_name = real_name;
                    if (kernel_scope->get_symbol(real_name)) {
                        dup_name = func_name;
                        for (char &c : dup_name) {
                            if (c == '~' || c == '@') c = '_';
                        }
                    }
                    if (!kernel_scope->get_symbol(dup_name)) {
                        ASR::symbol_t *dup =
                            sym_dup.duplicate_Function(
                                ASR::down_cast<ASR::Function_t>(
                                    resolved),
                                kernel_scope);
                        if (dup) {
                            ASR::down_cast<ASR::Function_t>(dup)
                                ->m_name = s2c(al, dup_name);
                            kernel_scope->add_symbol(dup_name, dup);
                            // The duplicated function still references
                            // the module's struct definitions. Remap
                            // ExternalSymbol targets and Variable
                            // m_type_declarations to point to the
                            // kernel's struct copies instead.
                            fixup_struct_refs_in_scope(
                                ASR::down_cast<ASR::Function_t>(dup)
                                    ->m_symtab,
                                kernel_scope);
                        }
                    }
                } else if (ASR::is_a<ASR::ExternalSymbol_t>(*func_sym) &&
                           !ASR::is_a<ASR::StructMethodDeclaration_t>(
                               *resolved)) {
                    // Non-function, non-method ExternalSymbol (e.g.,
                    // GenericProcedure from m_original_name). Create a
                    // matching ExternalSymbol in the kernel scope.
                    ASR::ExternalSymbol_t *es =
                        ASR::down_cast<ASR::ExternalSymbol_t>(func_sym);
                    ASR::asr_t *new_es = ASR::make_ExternalSymbol_t(
                        al, loc, kernel_scope, s2c(al, func_name),
                        es->m_external, es->m_module_name,
                        nullptr, 0, es->m_original_name,
                        es->m_access);
                    kernel_scope->add_symbol(func_name,
                        ASR::down_cast<ASR::symbol_t>(new_es));
                } else if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    // Skip functions that are already accessible through
                    // the kernel scope's parent chain (e.g., TU-scope
                    // generated helpers from the
                    // function_call_in_declaration pass).
                    if (kernel_scope->parent &&
                            kernel_scope->parent->resolve_symbol(
                                ASRUtils::symbol_name(resolved))) {
                        if (!ASR::is_a<ASR::StructMethodDeclaration_t>(
                                *resolved)) {
                            continue;
                        }
                    }
                    ASR::symbol_t *dup = sym_dup.duplicate_Function(
                        ASR::down_cast<ASR::Function_t>(resolved),
                        kernel_scope);
                    if (dup) {
                        kernel_scope->add_symbol(func_name, dup);
                    }
                } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                               *resolved)) {
                    // Type-bound procedure call: the resolved symbol is
                    // a StructMethodDeclaration inside a Struct's symtab.
                    // Create an ExternalSymbol in the kernel scope that
                    // points to the corresponding method declaration in
                    // the kernel's copy of the struct (imported earlier
                    // by import_struct_def for the struct-typed variable).
                    SymbolTable *method_st =
                        ASRUtils::symbol_parent_symtab(resolved);
                    if (method_st->asr_owner &&
                            method_st->asr_owner->type ==
                                ASR::asrType::symbol) {
                        ASR::symbol_t *struct_owner =
                            down_cast<ASR::symbol_t>(method_st->asr_owner);
                        if (is_a<ASR::Struct_t>(*struct_owner)) {
                            std::string struct_name =
                                down_cast<ASR::Struct_t>(struct_owner)
                                    ->m_name;
                            std::string orig_name =
                                ASRUtils::symbol_name(resolved);
                            ASR::symbol_t *kernel_struct =
                                find_kernel_struct(kernel_scope,
                                    struct_name, orig_name);
                            if (kernel_struct &&
                                    is_a<ASR::Struct_t>(*kernel_struct)) {
                                struct_name = down_cast<ASR::Struct_t>(
                                    kernel_struct)->m_name;
                                ASR::Struct_t *ks =
                                    down_cast<ASR::Struct_t>(kernel_struct);
                                ASR::symbol_t *kernel_method =
                                    ks->m_symtab->get_symbol(orig_name);
                                if (kernel_method) {
                                    ASR::asr_t *new_es =
                                        ASR::make_ExternalSymbol_t(al, loc,
                                            kernel_scope,
                                            s2c(al, func_name),
                                            kernel_method,
                                            s2c(al, struct_name),
                                            nullptr, 0,
                                            s2c(al, orig_name),
                                            ASR::accessType::Public);
                                    kernel_scope->add_symbol(func_name,
                                        down_cast<ASR::symbol_t>(new_es));
                                }
                            }
                        }
                    }
                }
                // For type-bound procedure calls, also import the
                // underlying Function body into the kernel scope so
                // the Metal backend can generate shader code.
                // For submodule procedures, the module-scope Function
                // is just an interface (no body); find and import the
                // submodule implementation instead.
                if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                        *resolved)) {
                    ASR::StructMethodDeclaration_t *smd =
                        ASR::down_cast<ASR::StructMethodDeclaration_t>(
                            resolved);
                    ASR::symbol_t *proc_sym =
                        ASRUtils::symbol_get_past_external(smd->m_proc);
                    if (ASR::is_a<ASR::Function_t>(*proc_sym)) {
                        ASR::Function_t *proc_func =
                            ASR::down_cast<ASR::Function_t>(proc_sym);
                        std::string pname =
                            ASRUtils::symbol_name(proc_sym);
                        ASR::symbol_t *existing =
                            kernel_scope->get_symbol(pname);
                        bool already_has_body = false;
                        if (existing &&
                                ASR::is_a<ASR::Function_t>(*existing)) {
                            ASR::FunctionType_t *eft =
                                ASR::down_cast<ASR::FunctionType_t>(
                                    ASR::down_cast<ASR::Function_t>(
                                        existing)
                                        ->m_function_signature);
                            if (eft->m_deftype ==
                                    ASR::deftypeType::Implementation) {
                                already_has_body = true;
                            }
                        }
                        if (!already_has_body) {
                            if (existing) {
                                kernel_scope->erase_symbol(pname);
                            }
                            ASR::FunctionType_t *ftype =
                                ASR::down_cast<ASR::FunctionType_t>(
                                    proc_func->m_function_signature);
                            if (ftype->m_deftype ==
                                    ASR::deftypeType::Interface) {
                                // Submodule interface: find the
                                // Implementation in a submodule
                                // already in the TU, or load it from
                                // disk (--separate-compilation).
                                bool found = false;
                                for (auto &tu_item :
                                        tu.m_symtab->get_scope()) {
                                    if (!ASR::is_a<ASR::Module_t>(
                                            *tu_item.second)) continue;
                                    ASR::Module_t *mod =
                                        ASR::down_cast<ASR::Module_t>(
                                            tu_item.second);
                                    ASR::symbol_t *impl_sym =
                                        mod->m_symtab->get_symbol(pname);
                                    if (!impl_sym ||
                                        !ASR::is_a<ASR::Function_t>(
                                            *impl_sym)) continue;
                                    ASR::Function_t *impl_func =
                                        ASR::down_cast<ASR::Function_t>(
                                            impl_sym);
                                    ASR::FunctionType_t *impl_ft =
                                        ASR::down_cast<ASR::FunctionType_t>(
                                            impl_func
                                                ->m_function_signature);
                                    if (impl_ft->m_deftype !=
                                            ASR::deftypeType::Implementation)
                                        continue;
                                    ASR::symbol_t *dup =
                                        sym_dup.duplicate_Function(
                                            impl_func, kernel_scope);
                                    if (dup) {
                                        kernel_scope->add_symbol(
                                            pname, dup);
                                    }
                                    found = true;
                                    break;
                                }
                                if (!found) {
                                    // Load submodule from smod file.
                                    SymbolTable *parent_st =
                                        ASRUtils::symbol_parent_symtab(
                                            proc_sym);
                                    if (parent_st->asr_owner &&
                                            parent_st->asr_owner->type ==
                                                ASR::asrType::symbol &&
                                            ASR::is_a<ASR::Module_t>(
                                                *ASR::down_cast<
                                                    ASR::symbol_t>(
                                                    parent_st
                                                        ->asr_owner))) {
                                        std::string parent_mod =
                                            ASR::down_cast<ASR::Module_t>(
                                                ASR::down_cast<
                                                    ASR::symbol_t>(
                                                    parent_st
                                                        ->asr_owner))
                                                ->m_name;
                                        std::string smod_prefix =
                                            parent_mod + "@";
                                        std::vector<
                                            std::filesystem::path>
                                                mod_dirs;
                                        mod_dirs.push_back(
                                            pass_options
                                                .runtime_library_dir);
                                        mod_dirs.push_back(
                                            pass_options.mod_files_dir);
                                        mod_dirs.insert(mod_dirs.end(),
                                            pass_options.include_dirs
                                                .begin(),
                                            pass_options.include_dirs
                                                .end());
                                        for (auto &dir : mod_dirs) {
                                            if (dir.empty())
                                                dir = ".";
                                            if (!std::filesystem::
                                                    is_directory(dir))
                                                continue;
                                            for (auto &file :
                                                    std::filesystem::
                                                        directory_iterator(
                                                            dir)) {
                                                std::string fname =
                                                    file.path()
                                                        .filename()
                                                        .string();
                                                if (!startswith(fname,
                                                        smod_prefix) ||
                                                    !endswith(fname,
                                                        ".smod"))
                                                    continue;
                                                std::string content;
                                                if (!read_file(
                                                        file.path()
                                                            .string(),
                                                        content) ||
                                                    content.empty())
                                                    continue;
                                                LocationManager
                                                    lm_tmp;
                                                auto res =
                                                    load_modfile(
                                                        al, content,
                                                        false,
                                                        *tu.m_symtab,
                                                        lm_tmp);
                                                if (!res.ok) continue;
                                                load_submodule_deps(
                                                    *res.result);
                                                fix_external_symbols(
                                                    *res.result,
                                                    *tu.m_symtab);
                                                ASR::Module_t *submod =
                                                    ASRUtils::
                                                        extract_module(
                                                            *res.result);
                                                ASR::symbol_t
                                                    *impl_sym =
                                                    submod->m_symtab
                                                        ->get_symbol(
                                                            pname);
                                                if (!impl_sym ||
                                                    !ASR::is_a<
                                                        ASR::Function_t
                                                            >(*impl_sym))
                                                    continue;
                                                ASR::Function_t
                                                    *impl_func =
                                                    ASR::down_cast<
                                                        ASR::Function_t>(
                                                            impl_sym);
                                                ASR::FunctionType_t
                                                    *impl_ft =
                                                    ASR::down_cast<
                                                        ASR::FunctionType_t>(
                                                        impl_func
                                                        ->m_function_signature);
                                                if (impl_ft->m_deftype
                                                        != ASR::
                                                        deftypeType::
                                                        Implementation)
                                                    continue;
                                                ASR::symbol_t *dup =
                                                    sym_dup
                                                        .duplicate_Function(
                                                        impl_func,
                                                        kernel_scope);
                                                if (dup) {
                                                    kernel_scope
                                                        ->add_symbol(
                                                            pname, dup);
                                                }
                                                found = true;
                                                break;
                                            }
                                            if (found) break;
                                        }
                                    }
                                }
                                if (!found) {
                                    throw LCompilersException(
                                        "GPU Metal offload: cannot find "
                                        "submodule implementation for '"
                                        + pname + "'; ensure the "
                                        "submodule is compiled before "
                                        "the file that uses it");
                                }
                            } else {
                                // Non-submodule: function has a body.
                                ASR::symbol_t *dup =
                                    sym_dup.duplicate_Function(
                                        proc_func, kernel_scope);
                                if (dup) {
                                    kernel_scope->add_symbol(pname, dup);
                                }
                            }
                        }
                        // Update the StructMethodDeclaration in the
                        // kernel's struct to point to the kernel-scope
                        // function copy instead of the original module
                        // interface (which may have no body).
                        ASR::symbol_t *kernel_func =
                            kernel_scope->get_symbol(pname);
                        if (kernel_func) {
                            SymbolTable *method_st =
                                ASRUtils::symbol_parent_symtab(resolved);
                            if (method_st->asr_owner &&
                                    method_st->asr_owner->type ==
                                        ASR::asrType::symbol) {
                                ASR::symbol_t *struct_owner =
                                    down_cast<ASR::symbol_t>(
                                        method_st->asr_owner);
                                if (is_a<ASR::Struct_t>(*struct_owner)) {
                                    std::string sname =
                                        down_cast<ASR::Struct_t>(
                                            struct_owner)->m_name;
                                    std::string mname =
                                        ASRUtils::symbol_name(
                                            resolved);
                                    ASR::symbol_t *ks =
                                        find_kernel_struct(kernel_scope,
                                            sname, mname);
                                    if (ks &&
                                            is_a<ASR::Struct_t>(*ks)) {
                                        ASR::symbol_t *km =
                                            down_cast<ASR::Struct_t>(ks)
                                                ->m_symtab
                                                ->get_symbol(mname);
                                        if (km && is_a<
                                            ASR::StructMethodDeclaration_t
                                                >(*km)) {
                                            down_cast<ASR::
                                                StructMethodDeclaration_t
                                                    >(km)->m_proc =
                                                        kernel_func;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Fix up struct references in ALL duplicated kernel functions.
        // After duplication, ExternalSymbol targets and Variable
        // m_type_declarations may still reference the original module's
        // struct definitions. Remap them to the kernel's copies.
        // This recurses into nested scopes (Block, AssociateBlock, etc.).
        {
            for (auto &item : kernel_scope->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *dfunc = ASR::down_cast<ASR::Function_t>(
                    item.second);
                fixup_struct_refs_in_scope(dfunc->m_symtab,
                    kernel_scope);
            }
        }

        // Fix dangling variable references in duplicated kernel functions.
        // When a contained function references variables from the original
        // enclosing scope (e.g., a program-scope Parameter used by a
        // contained function), the duplicated function body retains the
        // original Var references which are unreachable from the kernel.
        // For Parameter variables, clone them into the function's scope.
        // For other variables, add them as extra kernel parameters.
        {
            for (auto &item : kernel_scope->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(
                    item.second);
                DanglingVarCollector dvc(func->m_symtab);
                for (size_t bi = 0; bi < func->n_body; bi++) {
                    dvc.visit_stmt(*func->m_body[bi]);
                }
                if (dvc.dangling.empty()) continue;

                std::set<std::string> fixed_names;
                for (auto &[name, orig_sym] : dvc.dangling) {
                    ASR::symbol_t *resolved_sym =
                        ASRUtils::symbol_get_past_external(orig_sym);
                    if (!ASR::is_a<ASR::Variable_t>(*resolved_sym)) continue;
                    ASR::Variable_t *orig_var =
                        ASR::down_cast<ASR::Variable_t>(resolved_sym);
                    if (orig_var->m_storage ==
                            ASR::storage_typeType::Parameter) {
                        ASR::symbol_t *new_var =
                            ASR::down_cast<ASR::symbol_t>(
                                ASRUtils::make_Variable_t_util(al, loc,
                                    func->m_symtab, s2c(al, name),
                                    nullptr, 0,
                                    ASR::intentType::Local,
                                    orig_var->m_symbolic_value,
                                    orig_var->m_value,
                                    ASR::storage_typeType::Parameter,
                                    ASRUtils::duplicate_type(al,
                                        orig_var->m_type),
                                    nullptr, orig_var->m_abi,
                                    orig_var->m_access,
                                    ASR::presenceType::Required, false));
                        func->m_symtab->add_symbol(name, new_var);
                        fixed_names.insert(name);
                    } else {
                        if (!kernel_scope->get_symbol(name)) {
                            ASR::ttype_t *dup_type =
                                ASRUtils::duplicate_type(al,
                                    ASRUtils::type_get_past_allocatable(
                                        orig_var->m_type));
                            SetChar deps_vec;
                            deps_vec.reserve(al, 1);
                            ASRUtils::collect_variable_dependencies(
                                al, deps_vec, dup_type, nullptr,
                                nullptr, name);
                            ASR::symbol_t *param =
                                ASR::down_cast<ASR::symbol_t>(
                                    ASRUtils::make_Variable_t_util(al,
                                        loc, kernel_scope,
                                        s2c(al, name),
                                        deps_vec.p, deps_vec.size(),
                                        ASR::intentType::InOut,
                                        nullptr, nullptr,
                                        ASR::storage_typeType::Default,
                                        dup_type, nullptr,
                                        ASR::abiType::Source,
                                        ASR::accessType::Public,
                                        ASR::presenceType::Required,
                                        false));
                            kernel_scope->add_symbol(name, param);
                            kernel_args.push_back(al,
                                ASRUtils::EXPR(ASR::make_Var_t(
                                    al, loc, param)));
                            ASR::symbol_t *host_sym =
                                orig_scope->resolve_symbol(name);
                            ASR::call_arg_t carg;
                            carg.loc = loc;
                            carg.m_value = ASRUtils::EXPR(
                                ASR::make_Var_t(al, loc,
                                    host_sym ? host_sym : orig_sym));
                            call_args.push_back(al, carg);
                        }
                        fixed_names.insert(name);
                    }
                }
                if (!fixed_names.empty()) {
                    DanglingVarFixer fixer(func->m_symtab, fixed_names);
                    for (size_t bi = 0; bi < func->n_body; bi++) {
                        fixer.visit_stmt(*func->m_body[bi]);
                    }
                }
            }
        }

        // Remap FunctionCall/SubroutineCall references inside duplicated
        // kernel functions. When function f() calls g() and both are
        // duplicated into the kernel scope, f's body still references the
        // original g from the program scope. Fix those up.
        // Also descend into AssociateBlock and Block bodies within
        // duplicated functions — the statement visitor does not enter
        // these sub-scopes, so FunctionCall m_name references inside
        // them (e.g., type-bound procedure calls in associate blocks)
        // still point to the original scope after duplication.
        {
            for (auto &item : kernel_scope->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(
                    item.second);
                GpuReplaceSymbolsVisitor fn_replacer(*kernel_scope);
                fn_replacer.replacer.skip_scopes.insert(func->m_symtab);
                for (size_t bi = 0; bi < func->n_body; bi++) {
                    fn_replacer.visit_stmt(*func->m_body[bi]);
                }
                for (auto &fn_item : func->m_symtab->get_scope()) {
                    if (ASR::is_a<ASR::AssociateBlock_t>(
                            *fn_item.second)) {
                        ASR::AssociateBlock_t *ab =
                            ASR::down_cast<ASR::AssociateBlock_t>(
                                fn_item.second);
                        for (size_t bi = 0; bi < ab->n_body; bi++) {
                            fn_replacer.visit_stmt(*ab->m_body[bi]);
                        }
                    } else if (ASR::is_a<ASR::Block_t>(
                                   *fn_item.second)) {
                        ASR::Block_t *block =
                            ASR::down_cast<ASR::Block_t>(fn_item.second);
                        for (size_t bi = 0; bi < block->n_body; bi++) {
                            fn_replacer.visit_stmt(*block->m_body[bi]);
                        }
                    }
                }
            }
        }

        // Decompose StructInstanceMember references in kernel variable
        // type expressions (e.g., ArraySize(StructInstanceMember(Var(x),
        // nodes)) in VLA dimensions). When a struct variable is fully
        // decomposed into flat-array parameters, it is removed from the
        // kernel scope, but other variables' VLA dimensions may still
        // reference it through StructInstanceMember. Replace those with
        // the decomposed flat-array parameter Var before general symbol
        // remapping.
        if (!decomp_map.empty()) {
            GpuDecomposeStructReplacer type_decomp(al, kernel_scope,
                decomp_map);
            for (auto &item : kernel_scope->get_scope()) {
                if (!is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = down_cast<ASR::Variable_t>(
                    item.second);
                if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                    var->m_type);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_start) {
                        type_decomp.current_expr =
                            &(arr->m_dims[d].m_start);
                        type_decomp.replace_expr(
                            arr->m_dims[d].m_start);
                    }
                    if (arr->m_dims[d].m_length) {
                        type_decomp.current_expr =
                            &(arr->m_dims[d].m_length);
                        type_decomp.replace_expr(
                            arr->m_dims[d].m_length);
                    }
                }
            }
        }

        // Remap symbol references in kernel parameter types (e.g., array
        // dimension expressions like s(x%n) that still point to the
        // original scope after duplicate_type).
        {
            GpuReplaceSymbols type_replacer(*kernel_scope);
            for (auto &item : kernel_scope->get_scope()) {
                if (!is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = down_cast<ASR::Variable_t>(item.second);
                ASR::ttype_t *type = var->m_type;
                if (ASR::is_a<ASR::Array_t>(*type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                    for (size_t i = 0; i < arr->n_dims; i++) {
                        if (arr->m_dims[i].m_start) {
                            type_replacer.current_expr = &(arr->m_dims[i].m_start);
                            type_replacer.replace_expr(arr->m_dims[i].m_start);
                        }
                        if (arr->m_dims[i].m_length) {
                            type_replacer.current_expr = &(arr->m_dims[i].m_length);
                            type_replacer.replace_expr(arr->m_dims[i].m_length);
                        }
                    }
                }
            }
        }

        // Save host-side head expressions BEFORE in-place replacement
        struct DimInfo {
            ASR::expr_t *host_start;
            ASR::expr_t *host_end;
        };
        std::vector<DimInfo> dim_info;
        for (size_t d = 0; d < n_dims; d++) {
            dim_info.push_back({x.m_head[d].m_start, x.m_head[d].m_end});
        }

        // Deep-copy the body statements so that in-place symbol remapping
        // does not corrupt types shared with the original function scope
        // (e.g., ArrayBroadcast type sharing the same Array dimension Var
        // nodes as the original variable's type).
        ASRUtils::ExprStmtDuplicator body_dup(al);
        body_dup.success = true;
        Vec<ASR::stmt_t*> body_copy;
        body_copy.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            ASR::stmt_t *copy = body_dup.duplicate_stmt(x.m_body[i]);
            LCOMPILERS_ASSERT(copy);
            body_copy.push_back(al, copy);
        }

        // Replace StructInstanceMember references to decomposed
        // allocatable members with Var references to the new
        // flat-array kernel parameters, before general symbol remapping.
        if (!decomp_map.empty()) {
            GpuDecomposeStructVisitor decomp_visitor(al, kernel_scope,
                decomp_map);
            for (size_t i = 0; i < body_copy.n; i++) {
                decomp_visitor.visit_stmt(*body_copy.p[i]);
            }
        }

        // 3. Replace Var references in copied body to point to kernel scope
        GpuReplaceSymbolsVisitor sym_replacer(*kernel_scope);
        for (size_t i = 0; i < body_copy.n; i++) {
            sym_replacer.visit_stmt(*body_copy.p[i]);
        }

        // 4. Build kernel body
        Vec<ASR::stmt_t*> kernel_body;
        kernel_body.reserve(al, x.n_body + 2 * n_dims + 1);

        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        ASR::expr_t *thread_idx = ASRUtils::EXPR(
            ASR::make_GpuThreadIndex_t(al, loc, 0, int_type, nullptr));
        ASR::expr_t *block_idx = ASRUtils::EXPR(
            ASR::make_GpuBlockIndex_t(al, loc, 0, int_type, nullptr));
        ASR::expr_t *block_sz = ASRUtils::EXPR(
            ASR::make_GpuBlockSize_t(al, loc, 0, int_type, nullptr));

        // flat_idx = block_idx * block_size + thread_idx
        ASR::expr_t *flat_idx = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc,
                ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    block_idx, ASR::binopType::Mul, block_sz, int_type, nullptr)),
                ASR::binopType::Add, thread_idx, int_type, nullptr));

        // For multi-dimensional: linearize index
        // For do concurrent (i=1:m, j=1:n, k=1:p):
        //   flat = flat_idx
        //   i = flat % m + 1;  flat = flat / m
        //   j = flat % n + 1;  flat = flat / n
        //   k = flat + 1  (last dim)
        //   guard: flat_idx >= m*n*k → return

        // Create kernel-scope versions of start/end for each dimension
        std::vector<ASR::expr_t*> kernel_starts, kernel_ends;
        for (size_t d = 0; d < n_dims; d++) {
            kernel_starts.push_back(dup_expr_to_scope(dim_info[d].host_start, kernel_scope));
            kernel_ends.push_back(dup_expr_to_scope(dim_info[d].host_end, kernel_scope));
        }

        // Decompose StructInstanceMember references in kernel head
        // expressions. After associate resolution, head bounds may
        // contain e.g. ArraySize(StructInstanceMember(Var(arg), nodes))
        // where arg was decomposed and removed from involved_syms.
        // Replace these with Var(arg__nodes) to match the kernel params.
        if (!decomp_map.empty()) {
            GpuDecomposeStructReplacer head_decomp(al, kernel_scope,
                decomp_map);
            for (size_t d = 0; d < n_dims; d++) {
                if (kernel_starts[d]) {
                    head_decomp.current_expr = &kernel_starts[d];
                    head_decomp.replace_expr(kernel_starts[d]);
                }
                if (kernel_ends[d]) {
                    head_decomp.current_expr = &kernel_ends[d];
                    head_decomp.replace_expr(kernel_ends[d]);
                }
            }
        }

        // Compute total_elements for host-side grid size
        // Also compute per-dim range: range_d = end_d - start_d + 1
        // For kernel: dim_size_d = end_d - start_d + 1
        ASR::expr_t *one_const = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));

        // Compute total flat size for guard
        ASR::expr_t *total_size_kernel = nullptr;
        for (size_t d = 0; d < n_dims; d++) {
            // dim_range = kernel_end - kernel_start + 1
            ASR::expr_t *dim_range = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        kernel_ends[d], ASR::binopType::Sub,
                        kernel_starts[d], int_type, nullptr)),
                    ASR::binopType::Add, one_const, int_type, nullptr));
            if (total_size_kernel == nullptr) {
                total_size_kernel = dim_range;
            } else {
                total_size_kernel = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        total_size_kernel, ASR::binopType::Mul,
                        dim_range, int_type, nullptr));
            }
        }

        // Guard: if (flat_idx >= total_size) return
        ASR::expr_t *guard = ASRUtils::EXPR(
            ASR::make_IntegerCompare_t(al, loc, flat_idx,
                ASR::cmpopType::GtE, total_size_kernel,
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
        Vec<ASR::stmt_t*> guard_body;
        guard_body.reserve(al, 1);
        guard_body.push_back(al, ASRUtils::STMT(ASR::make_Return_t(al, loc)));
        Vec<ASR::stmt_t*> guard_else;
        guard_else.reserve(al, 0);
        kernel_body.push_back(al, ASRUtils::STMT(
            ASR::make_If_t(al, loc, nullptr, guard,
                guard_body.p, guard_body.n,
                guard_else.p, guard_else.n)));

        // Compute per-dim loop variable from flat_idx
        // We need a "remaining" variable in kernel scope
        std::string remain_name = "__flat_idx";
        {
            ASR::symbol_t *remain_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, remain_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, int_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            kernel_scope->add_symbol(remain_name, remain_sym);
        }
        ASR::expr_t *remain_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, kernel_scope->get_symbol(remain_name)));

        // __flat_idx = flat_idx (the raw thread index)
        kernel_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, remain_var, flat_idx, nullptr, false, false)));

        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *dim_range = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        kernel_ends[d], ASR::binopType::Sub,
                        kernel_starts[d], int_type, nullptr)),
                    ASR::binopType::Add, one_const, int_type, nullptr));

            ASR::symbol_t *kvar = kernel_scope->get_symbol(loop_var_names[d]);
            ASR::expr_t *kvar_expr = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, kvar));

            if (d < n_dims - 1) {
                // loop_var = __flat_idx % dim_range + start
                // Since ASR has no Mod binop, compute as: a - (a/b)*b
                ASR::expr_t *div_part = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Div,
                        dim_range, int_type, nullptr));
                ASR::expr_t *mul_part = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        div_part, ASR::binopType::Mul,
                        dim_range, int_type, nullptr));
                ASR::expr_t *mod_val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Sub,
                        mul_part, int_type, nullptr));
                ASR::expr_t *val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        mod_val, ASR::binopType::Add,
                        kernel_starts[d], int_type, nullptr));
                kernel_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, kvar_expr, val, nullptr, false, false)));

                // __flat_idx = __flat_idx / dim_range
                ASR::expr_t *div_val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Div,
                        dim_range, int_type, nullptr));
                kernel_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, remain_var, div_val, nullptr, false, false)));
            } else {
                // Last dim: loop_var = __flat_idx + start
                ASR::expr_t *val = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        remain_var, ASR::binopType::Add,
                        kernel_starts[d], int_type, nullptr));
                kernel_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, kvar_expr, val, nullptr, false, false)));
            }
        }

        // Move Block symbols referenced by BlockCall into kernel scope.
        // This helper processes a block and recursively handles any nested
        // BlockCall statements, since GpuReplaceSymbolsVisitor does not
        // descend into BlockCall/AssociateBlockCall automatically.
        // `reparent` is true only for the top-level block; nested blocks
        // keep their existing parent (the enclosing block's symtab).
        std::function<void(ASR::Block_t*, bool)> process_block_for_kernel =
            [&](ASR::Block_t *block, bool reparent) {
            if (reparent) {
                block->m_symtab->parent = kernel_scope;
            }
            // Pre-compute VLA dimension expressions that contain
            // FunctionCall nodes on the host side and pass the
            // results as scalar kernel parameters, because GPU
            // kernels cannot call arbitrary host-side functions.
            // This must happen BEFORE body remapping: the variable
            // type and body expression types (e.g. ArrayBroadcast
            // m_type) may share the same Array_t pointer, so body
            // remapping would change Var references in the shared
            // dimension to point to kernel-scope symbols. The
            // host_expr duplicate must capture the original
            // (caller-scope) references for the host-side call args.
            // Track old→new expression replacements so that DoLoop
            // bounds created by the ArrayBroadcast lowering (which
            // copied the old dimension pointers) can be updated too.
            std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>>
                dim_replacements;
            {
                ASRUtils::ExprStmtDuplicator dim_dup(al);
                dim_dup.success = true;
                for (auto &item : block->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second))
                        continue;
                    ASR::Variable_t *bvar =
                        ASR::down_cast<ASR::Variable_t>(item.second);
                    if (!ASR::is_a<ASR::Array_t>(*bvar->m_type))
                        continue;
                    ASR::Array_t *arr =
                        ASR::down_cast<ASR::Array_t>(bvar->m_type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        ASR::expr_t **dim_ptrs[2] = {
                            &arr->m_dims[d].m_start,
                            &arr->m_dims[d].m_length};
                        for (int e = 0; e < 2; e++) {
                            if (!*dim_ptrs[e]) continue;
                            if (!expr_has_function_call(
                                    *dim_ptrs[e]))
                                continue;
                            ASR::expr_t *old_dim_expr = *dim_ptrs[e];
                            ASR::expr_t *host_expr =
                                dim_dup.duplicate_expr(
                                    *dim_ptrs[e]);
                            std::string pname =
                                kernel_scope->get_unique_name(
                                    "__lfortran_gpu_dim_", false);
                            ASR::ttype_t *ptype =
                                ASRUtils::duplicate_type(al,
                                    ASRUtils::expr_type(
                                        *dim_ptrs[e]));
                            ASR::symbol_t *psym =
                                ASR::down_cast<ASR::symbol_t>(
                                    ASRUtils::make_Variable_t_util(
                                        al, loc, kernel_scope,
                                        s2c(al, pname),
                                        nullptr, 0,
                                        ASR::intentType::InOut,
                                        nullptr, nullptr,
                                        ASR::storage_typeType::Default,
                                        ptype, nullptr,
                                        ASR::abiType::Source,
                                        ASR::accessType::Public,
                                        ASR::presenceType::Required,
                                        false));
                            kernel_scope->add_symbol(pname, psym);
                            kernel_args.push_back(al,
                                ASRUtils::EXPR(ASR::make_Var_t(
                                    al, loc, psym)));
                            ASR::call_arg_t carg;
                            carg.loc = loc;
                            carg.m_value = host_expr;
                            call_args.push_back(al, carg);
                            ASR::expr_t *new_dim_expr = ASRUtils::EXPR(
                                ASR::make_Var_t(al, loc, psym));
                            *dim_ptrs[e] = new_dim_expr;
                            dim_replacements.push_back(
                                {old_dim_expr, new_dim_expr});
                        }
                    }
                }
            }
            // The ArrayBroadcast lowering (inline_elemental_array_var_
            // in_body) may have created DoLoop statements whose bounds
            // copied the old VLA dimension expression pointers before
            // the pre-computation above replaced them. Walk the block
            // body and patch any DoLoop bounds that still reference the
            // old expressions.
            if (!dim_replacements.empty()) {
                std::function<void(ASR::stmt_t**, size_t)>
                    patch_do_loop_bounds = [&](ASR::stmt_t **stmts,
                                               size_t n_stmts) {
                    for (size_t si = 0; si < n_stmts; si++) {
                        if (ASR::is_a<ASR::DoLoop_t>(*stmts[si])) {
                            ASR::DoLoop_t *dl =
                                ASR::down_cast<ASR::DoLoop_t>(
                                    stmts[si]);
                            for (auto &[old_e, new_e] :
                                    dim_replacements) {
                                if (dl->m_head.m_start == old_e)
                                    dl->m_head.m_start = new_e;
                                if (dl->m_head.m_end == old_e)
                                    dl->m_head.m_end = new_e;
                            }
                            patch_do_loop_bounds(dl->m_body,
                                dl->n_body);
                        }
                    }
                };
                patch_do_loop_bounds(block->m_body, block->n_body);
            }
            // Remap Var references inside the block body
            GpuReplaceSymbolsVisitor block_replacer(*kernel_scope);
            for (size_t j = 0; j < block->n_body; j++) {
                block_replacer.visit_stmt(*block->m_body[j]);
            }
            // Also remap Var references inside AssociateBlock bodies
            // within this Block, since the visitor does not descend
            // into AssociateBlockCall targets automatically.
            for (auto &item : block->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::AssociateBlock_t>(*item.second))
                    continue;
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(
                        item.second);
                for (size_t j = 0; j < ab->n_body; j++) {
                    block_replacer.visit_stmt(*ab->m_body[j]);
                }
            }
            // Replace StructInstanceMember references to decomposed
            // allocatable members inside the block body.
            if (!decomp_map.empty()) {
                GpuDecomposeStructVisitor block_decomp(al, kernel_scope,
                    decomp_map);
                for (size_t j = 0; j < block->n_body; j++) {
                    block_decomp.visit_stmt(*block->m_body[j]);
                }
            }
            // Recursively process nested BlockCall statements
            for (size_t j = 0; j < block->n_body; j++) {
                if (ASR::is_a<ASR::BlockCall_t>(*block->m_body[j])) {
                    ASR::BlockCall_t *inner_bc =
                        ASR::down_cast<ASR::BlockCall_t>(block->m_body[j]);
                    if (ASR::is_a<ASR::Block_t>(*inner_bc->m_m)) {
                        process_block_for_kernel(
                            ASR::down_cast<ASR::Block_t>(inner_bc->m_m),
                            false);
                    }
                }
            }
            // Remap type expressions of block-local variables
            // (e.g., VLA dimensions like n(i) in real :: a(n(i)))
            GpuReplaceSymbols block_type_replacer(*kernel_scope);
            for (auto &item : block->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                ASR::ttype_t *type = var->m_type;
                if (ASR::is_a<ASR::Array_t>(*type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        if (arr->m_dims[d].m_start) {
                            block_type_replacer.current_expr =
                                &(arr->m_dims[d].m_start);
                            block_type_replacer.replace_expr(
                                arr->m_dims[d].m_start);
                        }
                        if (arr->m_dims[d].m_length) {
                            block_type_replacer.current_expr =
                                &(arr->m_dims[d].m_length);
                            block_type_replacer.replace_expr(
                                arr->m_dims[d].m_length);
                        }
                    }
                }
            }
        };
        // Recursively find and move all BlockCall targets from any
        // nesting depth (e.g., BlockCall inside a DoLoop inside the
        // do concurrent body) into the kernel scope.
        std::function<void(ASR::stmt_t**, size_t)>
            move_blocks_to_kernel = [&](ASR::stmt_t **stmts,
                                        size_t n_stmts) {
            for (size_t i = 0; i < n_stmts; i++) {
                if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
                    ASR::BlockCall_t *bc =
                        ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                    if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                        ASR::Block_t *block =
                            ASR::down_cast<ASR::Block_t>(bc->m_m);
                        std::string block_name = block->m_name;
                        process_block_for_kernel(block, true);
                        if (orig_scope->get_symbol(block_name)) {
                            orig_scope->erase_symbol(block_name);
                        }
                        if (!kernel_scope->get_symbol(block_name)) {
                            kernel_scope->add_symbol(block_name, bc->m_m);
                        }
                    }
                } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
                    ASR::DoLoop_t *dl =
                        ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                    move_blocks_to_kernel(dl->m_body, dl->n_body);
                } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                    ASR::If_t *ifs =
                        ASR::down_cast<ASR::If_t>(stmts[i]);
                    move_blocks_to_kernel(ifs->m_body, ifs->n_body);
                    move_blocks_to_kernel(ifs->m_orelse, ifs->n_orelse);
                } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
                    ASR::WhileLoop_t *wl =
                        ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                    move_blocks_to_kernel(wl->m_body, wl->n_body);
                }
            }
        };
        move_blocks_to_kernel(body_copy.p, body_copy.n);

        // Add copied loop body (already remapped)
        for (size_t i = 0; i < body_copy.n; i++) {
            kernel_body.push_back(al, body_copy.p[i]);
        }

        // 5. Build function signature
        // FunctionType arg_types must not contain scope-bound expressions,
        // so strip dimension expressions that reference variables.
        Vec<ASR::ttype_t*> arg_types;
        arg_types.reserve(al, kernel_args.n);
        for (size_t i = 0; i < kernel_args.n; i++) {
            ASR::Var_t *v = down_cast<ASR::Var_t>(kernel_args.p[i]);
            ASR::ttype_t *t = ASRUtils::symbol_type(v->m_v);
            if (ASR::is_a<ASR::Array_t>(*t)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
                ASR::dimension_t *new_dims = al.allocate<ASR::dimension_t>(arr->n_dims);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    new_dims[d].loc = arr->m_dims[d].loc;
                    new_dims[d].m_start = nullptr;
                    new_dims[d].m_length = nullptr;
                }
                t = ASRUtils::TYPE(ASR::make_Array_t(al, arr->base.base.loc,
                    arr->m_type, new_dims, arr->n_dims,
                    arr->m_physical_type));
            }
            arg_types.push_back(al, t);
        }
        ASR::ttype_t *fn_sig = ASRUtils::TYPE(
            ASR::make_FunctionType_t(al, loc,
                arg_types.p, arg_types.n, nullptr,
                ASR::abiType::Source, ASR::deftypeType::Implementation,
                nullptr, false, false, false, false, false, nullptr, 0, false));

        // 6. Create GpuKernelFunction
        ASR::asr_t *kernel_func = ASR::make_GpuKernelFunction_t(al, loc,
            kernel_scope, s2c(al, kernel_name), fn_sig,
            nullptr, 0,
            kernel_args.p, kernel_args.n,
            kernel_body.p, kernel_body.n,
            ASR::accessType::Public);
        tu_symtab->add_symbol(kernel_name,
            ASR::down_cast<ASR::symbol_t>(kernel_func));

        // Pre-allocate host-side allocatable arrays that are assigned
        // from a FunctionCall inside the do concurrent body. The GPU
        // kernel receives the buffer pointer at launch time, so the
        // array must already be allocated on the host before dispatch.
        Vec<ASR::stmt_t*> pre_launch_stmts;
        pre_launch_stmts.reserve(al, 4);
        for (size_t si = 0; si < x.n_body; si++) {
            ASR::stmt_t *stmt = x.m_body[si];
            // Unwrap BlockCall to inspect block body statements
            ASR::stmt_t **stmts_to_scan = &stmt;
            size_t n_stmts_to_scan = 1;
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmt);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *blk =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    stmts_to_scan = blk->m_body;
                    n_stmts_to_scan = blk->n_body;
                }
            }
            for (size_t sj = 0; sj < n_stmts_to_scan; sj++) {
                if (!ASR::is_a<ASR::Assignment_t>(*stmts_to_scan[sj]))
                    continue;
                ASR::Assignment_t *asgn =
                    ASR::down_cast<ASR::Assignment_t>(stmts_to_scan[sj]);
                if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) continue;
                if (!ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value))
                    continue;

                ASR::Var_t *target_var =
                    ASR::down_cast<ASR::Var_t>(asgn->m_target);
                ASR::symbol_t *orig_sym =
                    ASRUtils::symbol_get_past_external(target_var->m_v);
                if (!ASR::is_a<ASR::Variable_t>(*orig_sym)) continue;
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(orig_sym);
                if (!ASRUtils::is_allocatable(var->m_type)) continue;

                ASR::FunctionCall_t *fc =
                    ASR::down_cast<ASR::FunctionCall_t>(asgn->m_value);
                ASR::symbol_t *fn_sym =
                    ASRUtils::symbol_get_past_external(fc->m_name);
                if (!ASR::is_a<ASR::Function_t>(*fn_sym)) continue;

                ASR::Function_t *fn =
                    ASR::down_cast<ASR::Function_t>(fn_sym);
                std::string ret_name;
                if (fn->m_return_var &&
                        ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                    ret_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            fn->m_return_var)->m_v);
                }
                if (ret_name.empty()) continue;

                // Find the Allocate statement for the return variable
                // in the function body and use its dimensions.
                bool alloc_found = false;
                for (size_t bi = 0;
                        bi < fn->n_body && !alloc_found; bi++) {
                    if (!ASR::is_a<ASR::Allocate_t>(*fn->m_body[bi]))
                        continue;
                    ASR::Allocate_t *fn_alloc =
                        ASR::down_cast<ASR::Allocate_t>(fn->m_body[bi]);
                    for (size_t ai = 0; ai < fn_alloc->n_args; ai++) {
                        if (!fn_alloc->m_args[ai].m_a ||
                                !ASR::is_a<ASR::Var_t>(
                                    *fn_alloc->m_args[ai].m_a))
                            continue;
                        std::string aname = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                fn_alloc->m_args[ai].m_a)->m_v);
                        if (aname != ret_name) continue;

                        ASRUtils::ExprStmtDuplicator dup(al);
                        dup.success = true;
                        ASR::alloc_arg_t host_arg;
                        host_arg.loc = loc;
                        host_arg.m_a = asgn->m_target;
                        host_arg.n_dims =
                            fn_alloc->m_args[ai].n_dims;
                        host_arg.m_dims =
                            al.allocate<ASR::dimension_t>(
                                host_arg.n_dims);
                        for (size_t d = 0; d < host_arg.n_dims; d++) {
                            host_arg.m_dims[d].loc = loc;
                            host_arg.m_dims[d].m_start =
                                fn_alloc->m_args[ai].m_dims[d].m_start
                                ? dup.duplicate_expr(
                                    fn_alloc->m_args[ai]
                                        .m_dims[d].m_start)
                                : nullptr;
                            host_arg.m_dims[d].m_length =
                                fn_alloc->m_args[ai].m_dims[d].m_length
                                ? dup.duplicate_expr(
                                    fn_alloc->m_args[ai]
                                        .m_dims[d].m_length)
                                : nullptr;
                        }
                        host_arg.m_len_expr = nullptr;
                        host_arg.m_sym_subclass = nullptr;
                        host_arg.m_type = nullptr;

                        Vec<ASR::alloc_arg_t> alloc_vec;
                        alloc_vec.reserve(al, 1);
                        alloc_vec.push_back(al, host_arg);
                        pre_launch_stmts.push_back(al,
                            ASRUtils::STMT(ASR::make_Allocate_t(
                                al, loc, alloc_vec.p, alloc_vec.n,
                                nullptr, nullptr, nullptr)));
                        alloc_found = true;
                        break;
                    }
                }
            }
        }

        // 7. Replace DoConcurrentLoop with GpuKernelLaunch + GpuSync
        pass_result.reserve(al, pre_launch_stmts.n + 2);
        for (size_t pi = 0; pi < pre_launch_stmts.n; pi++) {
            pass_result.push_back(al, pre_launch_stmts.p[pi]);
        }

        ASR::expr_t *block_size_const = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 256, int_type,
                ASR::integerbozType::Decimal));

        // Compute host-side total_elements = product of (end_d - start_d + 1)
        ASR::expr_t *host_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));
        ASR::expr_t *host_total = nullptr;
        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *dim_range = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        dim_info[d].host_end, ASR::binopType::Sub,
                        dim_info[d].host_start, int_type, nullptr)),
                    ASR::binopType::Add, host_one, int_type, nullptr));
            if (host_total == nullptr) {
                host_total = dim_range;
            } else {
                host_total = ASRUtils::EXPR(
                    ASR::make_IntegerBinOp_t(al, loc,
                        host_total, ASR::binopType::Mul,
                        dim_range, int_type, nullptr));
            }
        }

        // grid_size = (total + 255) / 256
        ASR::expr_t *grid_padded = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc, host_total, ASR::binopType::Add,
                ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 255, int_type,
                    ASR::integerbozType::Decimal)),
                int_type, nullptr));
        ASR::expr_t *grid_size = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc, grid_padded, ASR::binopType::Div,
                block_size_const, int_type, nullptr));

        pass_result.push_back(al, ASRUtils::STMT(
            ASR::make_GpuKernelLaunch_t(al, loc,
                ASR::down_cast<ASR::symbol_t>(kernel_func),
                grid_size, block_size_const,
                call_args.p, call_args.n)));

        pass_result.push_back(al, ASRUtils::STMT(
            ASR::make_GpuSync_t(al, loc)));
    }
};

void pass_replace_gpu_offload(Allocator &al, ASR::TranslationUnit_t &unit,
                              const LCompilers::PassOptions& pass_options) {
    GpuOffloadVisitor v(al, pass_options, unit);
    v.asr_changed = true;
    while (v.asr_changed) {
        v.asr_changed = false;
        v.visit_TranslationUnit(unit);
    }
    // Kernel extraction moves Block symbols out of their enclosing
    // function, which can leave stale entries in that function's
    // dependency list. Recompute all dependencies to fix this.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
