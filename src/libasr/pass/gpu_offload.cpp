#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_gpu_offload.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/stmt_walk_visitor.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/string_utils.h>

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
    SymbolTable *enclosing_scope;

    GpuSymbolCollector(Allocator &al_,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &syms,
        SymbolTable *scope = nullptr)
        : al(al_), symbols(syms), enclosing_scope(scope) {}

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
        // Skip variables local to Block scopes, except those in the
        // enclosing Block scope (which need to become kernel parameters
        // when a do concurrent is directly inside a Block)
        if (ASR::is_a<ASR::Variable_t>(*x.m_v)) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(x.m_v);
            if (var->m_parent_symtab->asr_owner &&
                ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(
                        var->m_parent_symtab->asr_owner))) {
                if (var->m_parent_symtab != enclosing_scope) {
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
    GpuReplaceSymbols(SymbolTable &scope) : kernel_scope(scope) {}

    void replace_Var(ASR::Var_t *x) {
        std::string name = ASRUtils::symbol_name(x->m_v);
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
    SymbolTable *enclosing_scope;

    GpuLocalVarCollector(std::set<std::string> &lv, std::set<std::string> &av,
        SymbolTable *scope = nullptr)
        : local_vars(lv), assigned_vars(av), enclosing_scope(scope) {}

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
                        if (var->m_parent_symtab != enclosing_scope) {
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

class GpuOffloadVisitor : public ASR::StatementWalkVisitor<GpuOffloadVisitor>
{
public:
    PassOptions pass_options;
    ASR::TranslationUnit_t &tu;

    GpuOffloadVisitor(Allocator &al, PassOptions pass_options_,
                      ASR::TranslationUnit_t &tu_)
        : StatementWalkVisitor(al), pass_options(pass_options_), tu(tu_) {}

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
        for (auto &item : orig_scope->get_scope()) {
            if (!is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
            ASR::ExternalSymbol_t *es = down_cast<ASR::ExternalSymbol_t>(item.second);
            ASR::symbol_t *es_external = ASRUtils::symbol_get_past_external(es->m_external);
            // Check if this ExternalSymbol refers to a member of our struct
            if (ASRUtils::symbol_parent_symtab(es_external) == orig_struct->m_symtab) {
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

        return kernel_struct;
    }

    // For a struct-typed variable, get the type_declaration symbol
    // from the original scope and ensure the Struct (and its member
    // ExternalSymbols) exist in the kernel scope.
    ASR::symbol_t* import_struct_type(ASR::symbol_t *orig_sym,
            SymbolTable * /*orig_scope*/, SymbolTable *kernel_scope,
            const Location &loc) {
        if (!is_a<ASR::Variable_t>(*orig_sym)) return nullptr;
        ASR::Variable_t *var = down_cast<ASR::Variable_t>(orig_sym);
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(var->m_type))) return nullptr;
        ASR::symbol_t *type_decl = var->m_type_declaration;
        if (!type_decl) return nullptr;
        ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(type_decl);
        if (!is_a<ASR::Struct_t>(*struct_sym)) return nullptr;
        // Use the variable's declaring scope (where ExternalSymbols for
        // struct members live), not the possibly-nested current scope.
        SymbolTable *var_scope = var->m_parent_symtab;
        return import_struct_def(down_cast<ASR::Struct_t>(struct_sym),
            var_scope, kernel_scope, loc);
    }

    // Inline IntrinsicArrayFunction All inside a DoConcurrentLoop body.
    // Replaces:
    //   eq(l) = all(a(:,l) == b(:,l))
    // With:
    //   __gpu_all_res = .true.
    //   do __gpu_all_i = lbound(a,1), ubound(a,1)
    //     if (.not. mask_element(__gpu_all_i)) __gpu_all_res = .false.
    //   end do
    //   eq(l) = __gpu_all_res
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
            if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::IntrinsicArrayFunction_t *iaf =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(asgn->m_value);
            // Only handle All (intrinsic_id == 0 for All)
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(iaf->m_arr_intrinsic_id)
                    != ASRUtils::IntrinsicArrayFunctions::All) {
                new_body.push_back(al, stmt);
                continue;
            }
            if (iaf->n_args < 1 || !iaf->m_args[0]) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::expr_t *mask = iaf->m_args[0];
            Location loc = stmt->base.loc;

            // The mask should be an array expression (e.g., RealCompare on arrays).
            // We need to find the array dimension to loop over.
            // Extract the first ArraySection to determine loop bounds.
            ASR::expr_t *loop_start = nullptr;
            ASR::expr_t *loop_end = nullptr;
            // Walk the mask expression to find ArraySection
            std::function<void(ASR::expr_t*)> find_bounds = [&](ASR::expr_t *e) {
                if (loop_start) return; // already found
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
                    if (as->n_args > 0 && as->m_args[0].m_left && as->m_args[0].m_right) {
                        loop_start = as->m_args[0].m_left;
                        loop_end = as->m_args[0].m_right;
                    }
                } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                    ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
                    find_bounds(rc->m_left);
                    find_bounds(rc->m_right);
                } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                    ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
                    find_bounds(ic->m_left);
                    find_bounds(ic->m_right);
                } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
                    ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
                    find_bounds(lb->m_left);
                    find_bounds(lb->m_right);
                }
            };
            find_bounds(mask);

            if (!loop_start || !loop_end) {
                new_body.push_back(al, stmt);
                continue;
            }

            changed = true;

            ASR::ttype_t *logical_type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, loc, 4));
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));

            // Create loop variable __gpu_all_i
            std::string loop_var_name = current_scope->get_unique_name("__gpu_all_i");
            ASR::symbol_t *loop_var_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, current_scope,
                    s2c(al, loop_var_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, int_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            current_scope->add_symbol(loop_var_name, loop_var_sym);
            ASR::expr_t *loop_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, loop_var_sym));

            // Create result variable __gpu_all_res
            std::string res_var_name = current_scope->get_unique_name("__gpu_all_res");
            ASR::symbol_t *res_var_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, current_scope,
                    s2c(al, res_var_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, logical_type),
                    nullptr, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required, false));
            current_scope->add_symbol(res_var_name, res_var_sym);
            ASR::expr_t *res_var = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, res_var_sym));

            // __gpu_all_res = .true.
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, res_var,
                    ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                        true, logical_type)),
                    nullptr, false, false)));

            // Build element-wise mask expression by replacing ArraySection
            // with ArrayItem indexed by loop_var
            std::function<ASR::expr_t*(ASR::expr_t*)> elementize =
                [&](ASR::expr_t *e) -> ASR::expr_t* {
                if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
                    // Replace the range dimension with the loop variable
                    Vec<ASR::array_index_t> new_args;
                    new_args.reserve(al, as->n_args);
                    for (size_t i = 0; i < as->n_args; i++) {
                        ASR::array_index_t idx;
                        idx.loc = as->m_args[i].loc;
                        if (as->m_args[i].m_left && as->m_args[i].m_right) {
                            // This is a range — replace with loop variable
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
                    ASR::ttype_t *elem_type = ASRUtils::type_get_past_array(
                        ASRUtils::expr_type(as->m_v));
                    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                        as->m_v, new_args.p, new_args.n,
                        elem_type, ASR::arraystorageType::ColMajor, nullptr));
                } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                    ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
                    return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
                        elementize(rc->m_left), rc->m_op, elementize(rc->m_right),
                        logical_type, nullptr));
                } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                    ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
                    return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                        elementize(ic->m_left), ic->m_op, elementize(ic->m_right),
                        logical_type, nullptr));
                } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
                    ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
                    return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
                        elementize(lb->m_left), lb->m_op, elementize(lb->m_right),
                        logical_type, nullptr));
                }
                return e;
            };

            ASR::expr_t *elem_mask = elementize(mask);

            // Build loop body:
            //   if (.not. elem_mask) __gpu_all_res = .false.
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
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr,
                    head, loop_body.p, loop_body.n, nullptr, 0)));

            // eq(l) = __gpu_all_res
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, asgn->m_target, res_var,
                    nullptr, false, false)));
        }

        if (changed) {
            x.m_body = new_body.p;
            x.n_body = new_body.n;
        }
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (!pass_options.gpu_offload_metal) return;

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
        {
            std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map;
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
                for (size_t i = 0; i < x.n_body; i++) {
                    if (!ASR::is_a<ASR::BlockCall_t>(*x.m_body[i])) continue;
                    ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
                        x.m_body[i]);
                    if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                    ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                        bc->m_m);
                    // Resolve in block body statements
                    for (size_t j = 0; j < block->n_body; j++) {
                        resolver_visitor.visit_stmt(*block->m_body[j]);
                    }
                    // Resolve in block-local array dimension expressions
                    AssociateVarResolver type_resolver(al, assoc_map);
                    for (auto &item : block->m_symtab->get_scope()) {
                        if (!ASR::is_a<ASR::Variable_t>(*item.second))
                            continue;
                        ASR::Variable_t *var =
                            ASR::down_cast<ASR::Variable_t>(item.second);
                        if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
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
            }
        }

        // Inline IntrinsicArrayFunction All before kernel extraction
        inline_intrinsic_all(const_cast<ASR::DoConcurrentLoop_t&>(x));

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
                std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map;
                Vec<ASR::stmt_t*> resolved_stmts;
                resolved_stmts.reserve(al, ab->n_body);
                for (size_t ai = 0; ai < ab->n_body; ai++) {
                    if (ASR::is_a<ASR::Associate_t>(*ab->m_body[ai])) {
                        ASR::Associate_t *assoc =
                            ASR::down_cast<ASR::Associate_t>(
                                ab->m_body[ai]);
                        if (ASR::is_a<ASR::Var_t>(*assoc->m_target)) {
                            ASR::symbol_t *sym =
                                ASR::down_cast<ASR::Var_t>(
                                    assoc->m_target)->m_v;
                            assoc_map[sym] = assoc->m_value;
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
                if (!assoc_map.empty()) {
                    AssociateVarResolverVisitor resolver(al, assoc_map);
                    for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                        resolver.visit_stmt(*resolved_stmts.p[ri]);
                    }
                }
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    new_block_body.push_back(al, resolved_stmts.p[ri]);
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

        // Detect if the do concurrent is inside a Block scope. If so,
        // block-local variables need to be collected as kernel parameters
        // rather than skipped. Walk up through AssociateBlock parents to
        // find the enclosing Block (e.g., DoConcurrentLoop inside
        // AssociateBlock inside Block).
        SymbolTable *enclosing_block_scope = nullptr;
        {
            SymbolTable *scope = current_scope;
            while (scope && scope->asr_owner &&
                   scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
                    scope->asr_owner);
                if (is_a<ASR::Block_t>(*owner_sym)) {
                    enclosing_block_scope = scope;
                    break;
                } else if (is_a<ASR::AssociateBlock_t>(*owner_sym)) {
                    scope = scope->parent;
                } else {
                    break;
                }
            }
        }

        // 1. Collect all symbols from body AND head expressions
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> involved_syms;
        GpuSymbolCollector collector(al, involved_syms, enclosing_block_scope);
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
                GpuSymbolCollector type_collector(al, extra_syms, enclosing_block_scope);
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
        GpuLocalVarCollector lv_collector(local_vars, assigned_vars, enclosing_block_scope);
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
                            *dim_ptrs[e] = ASRUtils::EXPR(
                                ASR::make_Var_t(al, loc, psym));
                        }
                    }
                }
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
        for (size_t i = 0; i < body_copy.n; i++) {
            if (ASR::is_a<ASR::BlockCall_t>(*body_copy.p[i])) {
                ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
                    body_copy.p[i]);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                        bc->m_m);
                    std::string block_name = block->m_name;
                    process_block_for_kernel(block, true);
                    // Move Block from original scope to kernel scope
                    orig_scope->erase_symbol(block_name);
                    kernel_scope->add_symbol(block_name, bc->m_m);
                }
            }
        }

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

        // 7. Replace DoConcurrentLoop with GpuKernelLaunch + GpuSync
        pass_result.reserve(al, 2);

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
