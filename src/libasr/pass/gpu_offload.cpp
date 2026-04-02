#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_gpu_offload.h>
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

    GpuSymbolCollector(Allocator &al_,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &syms)
        : al(al_), symbols(syms) {}

    void visit_Var(const ASR::Var_t &x) {
        std::string name = to_lower(ASRUtils::symbol_name(x.m_v));
        if (symbols.find(name) == symbols.end()) {
            symbols[name] = {ASRUtils::symbol_type(x.m_v),
                ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, x.m_v))};
        }
    }
};

// Replaces all Var references in-place to point to the kernel scope symbols
class GpuReplaceSymbols : public ASR::BaseExprReplacer<GpuReplaceSymbols> {
public:
    SymbolTable &kernel_scope;
    GpuReplaceSymbols(SymbolTable &scope) : kernel_scope(scope) {}

    void replace_Var(ASR::Var_t *x) {
        std::string name = to_lower(ASRUtils::symbol_name(x->m_v));
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
        std::string mem_name = to_lower(ASRUtils::symbol_name(x->m_m));
        ASR::symbol_t *new_mem = kernel_scope.get_symbol(mem_name);
        if (new_mem) {
            x->m_m = new_mem;
        }
    }

    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        // Remap m_name to kernel scope symbol
        std::string name = to_lower(ASRUtils::symbol_name(x->m_name));
        ASR::symbol_t *new_sym = kernel_scope.get_symbol(name);
        if (new_sym) {
            x->m_name = new_sym;
        }
        if (x->m_original_name) {
            std::string orig_name = to_lower(ASRUtils::symbol_name(x->m_original_name));
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
// (associated with `n`) must be resolved to `n` before kernel extraction,
// because the kernel scope cannot access the AssociateBlock's symbol table.
class AssociateVarResolver : public ASR::BaseExprReplacer<AssociateVarResolver> {
public:
    std::map<ASR::symbol_t*, ASR::symbol_t*> &assoc_map;
    AssociateVarResolver(std::map<ASR::symbol_t*, ASR::symbol_t*> &map)
        : assoc_map(map) {}

    void replace_Var(ASR::Var_t *x) {
        auto it = assoc_map.find(x->m_v);
        if (it != assoc_map.end()) {
            x->m_v = it->second;
        }
    }
};

class AssociateVarResolverVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<AssociateVarResolverVisitor> {
public:
    AssociateVarResolver replacer;
    AssociateVarResolverVisitor(std::map<ASR::symbol_t*, ASR::symbol_t*> &map)
        : replacer(map) {}

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

    GpuLocalVarCollector(std::set<std::string> &lv, std::set<std::string> &av)
        : local_vars(lv), assigned_vars(av) {}

    void visit_Assignment(const ASR::Assignment_t &x) {
        // Check if target is a simple Var (not ArrayItem)
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_target);
            std::string name = to_lower(ASRUtils::symbol_name(v->m_v));
            ASR::ttype_t *type = ASRUtils::symbol_type(v->m_v);
            if (!ASRUtils::is_array(type)) {
                assigned_vars.insert(name);
            }
        }
        // Check if target is a StructInstanceMember (e.g., x%v = ...)
        if (ASR::is_a<ASR::StructInstanceMember_t>(*x.m_target)) {
            ASR::StructInstanceMember_t *sm =
                ASR::down_cast<ASR::StructInstanceMember_t>(x.m_target);
            if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(sm->m_v);
                std::string name = to_lower(ASRUtils::symbol_name(v->m_v));
                assigned_vars.insert(name);
            }
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_Assignment(x);
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
        return import_struct_def(down_cast<ASR::Struct_t>(struct_sym),
            orig_scope, kernel_scope, loc);
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
        // DoConcurrentLoop is inside an AssociateBlock. The kernel function
        // lives at the translation-unit level and cannot reference symbols
        // from the AssociateBlock's scope.
        if (current_scope->asr_owner &&
            current_scope->asr_owner->type == ASR::asrType::symbol &&
            is_a<ASR::AssociateBlock_t>(
                *down_cast<ASR::symbol_t>(current_scope->asr_owner))) {
            ASR::AssociateBlock_t *ab = ASR::down_cast2<ASR::AssociateBlock_t>(
                current_scope->asr_owner);
            std::map<ASR::symbol_t*, ASR::symbol_t*> assoc_map;
            for (size_t i = 0; i < ab->n_body; i++) {
                if (is_a<ASR::Associate_t>(*ab->m_body[i])) {
                    ASR::Associate_t *assoc = down_cast<ASR::Associate_t>(
                        ab->m_body[i]);
                    if (is_a<ASR::Var_t>(*assoc->m_target)) {
                        ASR::symbol_t *assoc_sym =
                            down_cast<ASR::Var_t>(assoc->m_target)->m_v;
                        ASR::expr_t *val = assoc->m_value;
                        while (is_a<ASR::ArrayPhysicalCast_t>(*val)) {
                            val = down_cast<ASR::ArrayPhysicalCast_t>(val)->m_arg;
                        }
                        if (is_a<ASR::Var_t>(*val)) {
                            assoc_map[assoc_sym] =
                                down_cast<ASR::Var_t>(val)->m_v;
                        }
                    }
                }
            }
            if (!assoc_map.empty()) {
                AssociateVarResolver resolver(assoc_map);
                for (size_t d = 0; d < n_dims; d++) {
                    ASR::expr_t *e;
                    e = x.m_head[d].m_start;
                    if (e) { resolver.current_expr = &e; resolver.replace_expr(e); }
                    e = x.m_head[d].m_end;
                    if (e) { resolver.current_expr = &e; resolver.replace_expr(e); }
                    e = x.m_head[d].m_increment;
                    if (e) { resolver.current_expr = &e; resolver.replace_expr(e); }
                }
                AssociateVarResolverVisitor resolver_visitor(assoc_map);
                for (size_t i = 0; i < x.n_body; i++) {
                    resolver_visitor.visit_stmt(*x.m_body[i]);
                }
            }
        }

        // 1. Collect all symbols from body AND head expressions
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> involved_syms;
        GpuSymbolCollector collector(al, involved_syms);
        collector.visit_DoConcurrentLoop(x);

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
            loop_var_names.push_back(to_lower(ASRUtils::symbol_name(lv->m_v)));
        }

        // Find local scalar temporaries (assigned but not arrays, not loop vars)
        std::set<std::string> local_vars, assigned_vars;
        GpuLocalVarCollector lv_collector(local_vars, assigned_vars);
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

            // Copy dependencies from the original variable
            char **deps = nullptr;
            size_t n_deps = 0;
            if (orig_sym && is_a<ASR::Variable_t>(*orig_sym)) {
                ASR::Variable_t *orig_var = down_cast<ASR::Variable_t>(orig_sym);
                n_deps = orig_var->n_dependencies;
                if (n_deps > 0) {
                    deps = al.allocate<char*>(n_deps);
                    for (size_t di = 0; di < n_deps; di++) {
                        deps[di] = orig_var->m_dependencies[di];
                    }
                }
            }

            ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                    s2c(al, sym_name), deps, n_deps,
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default,
                    ASRUtils::duplicate_type(al, type),
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

        // 3. Replace Var references in body to point to kernel scope
        GpuReplaceSymbolsVisitor sym_replacer(*kernel_scope);
        for (size_t i = 0; i < x.n_body; i++) {
            sym_replacer.visit_stmt(*x.m_body[i]);
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

        // Add original loop body (already remapped in-place)
        for (size_t i = 0; i < x.n_body; i++) {
            kernel_body.push_back(al, x.m_body[i]);
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
}

} // namespace LCompilers
