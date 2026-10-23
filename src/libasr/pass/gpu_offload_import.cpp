#include <filesystem>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/string_utils.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/replace_implied_do_loops.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

void GpuOffloadVisitor::load_submodule_deps(ASR::TranslationUnit_t &sub_tu) {
    implementations.load_submodule_deps(sub_tu);
}

void GpuOffloadVisitor::lower_loaded_implied_do_loops(
        ASR::TranslationUnit_t &sub_tu) {
    implementations.lower_loaded_implied_do_loops(sub_tu);
}

// Duplicate an expression, remapping all Var references to point to the
// given scope. Used to create kernel-scope copies of head expressions.
ASR::expr_t* GpuOffloadVisitor::dup_expr_to_scope(
        ASR::expr_t *expr, SymbolTable *scope) {
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
// original module definitions.  Also redirect ExternalSymbols that
// point to functions already duplicated into the kernel scope.
void GpuOffloadVisitor::fixup_struct_refs_in_scope(SymbolTable *scope,
        SymbolTable *kernel_scope,
        char *kernel_fn_name) {
    // Every derived type the scope declares a value of, or names a
    // component of, is given its copy in the kernel first, so the
    // references below have a copy to point to however late the kernel
    // comes to import the type itself -- a BLOCK of the loop that
    // declares a value of it is only looked at after the routines are
    // copied, and the value it passes to one of them has to have the type
    // the routine's dummy has.
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *type_sym = nullptr;
        if (ASR::is_a<ASR::Variable_t>(*item.second)) {
            ASR::Variable_t *var =
                ASR::down_cast<ASR::Variable_t>(item.second);
            if (var->m_type_declaration) {
                type_sym = ASRUtils::symbol_get_past_external(
                    var->m_type_declaration);
            }
        } else if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
            ASR::symbol_t *target = ASRUtils::symbol_get_past_external(
                item.second);
            SymbolTable *tp = target
                ? ASRUtils::symbol_parent_symtab(target) : nullptr;
            if (tp && tp->asr_owner &&
                    tp->asr_owner->type == ASR::asrType::symbol) {
                type_sym = ASR::down_cast<ASR::symbol_t>(tp->asr_owner);
            }
        }
        if (type_sym && ASR::is_a<ASR::Struct_t>(*type_sym) &&
                !kernel_scope->get_symbol(ASRUtils::symbol_name(type_sym))) {
            import_struct_def(ASR::down_cast<ASR::Struct_t>(type_sym),
                nullptr, kernel_scope, item.second->base.loc);
        }
    }
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
                        if (nt) {
                            es->m_external = nt;
                            // The member now lives in the kernel's
                            // struct copy; update m_module_name to
                            // the struct name (the verifier checks
                            // that it matches the containing scope).
                            es->m_module_name = ASR::down_cast<
                                ASR::Struct_t>(ks)->m_name;
                        }
                    }
                }
            }
            // If the ExternalSymbol references a function that has
            // been duplicated into the kernel scope, redirect to the
            // kernel-scope copy so that Call_t_body checks use the
            // fixed-up formal parameter types.
            if (ASR::is_a<ASR::Function_t>(*target)) {
                std::string fn =
                    ASRUtils::symbol_name(target);
                ASR::symbol_t *ks =
                    kernel_scope->get_symbol(fn);
                if (ks && ASR::is_a<ASR::Function_t>(*ks)) {
                    es->m_external = ks;
                    if (kernel_fn_name) {
                        es->m_module_name = kernel_fn_name;
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
            fixup_struct_refs_in_scope(nested, kernel_scope,
                kernel_fn_name);
        }
    }
}

// Find a Struct in kernel_scope by name, with PDT fallback.
// If the exact name is not found (e.g., "network_t"), look for a
// PDT instantiation (e.g., "network_t_4") that has a member named
// member_name. Returns the Struct symbol or nullptr.
ASR::symbol_t* GpuOffloadVisitor::find_kernel_struct(SymbolTable *kernel_scope,
        const std::string &struct_name,
        const std::string &member_name) {
    ASR::symbol_t *sym = kernel_scope->get_symbol(struct_name);
    if (sym && is_a<ASR::Struct_t>(*sym)) return sym;
    for (auto &item : kernel_scope->get_scope()) {
        if (!is_a<ASR::Struct_t>(*item.second)) continue;
        if (gpu_struct_lookup_member(item.second, member_name)) {
            return item.second;
        }
    }
    return nullptr;
}

// Import a Struct definition into kernel scope, recursively handling
// nested struct-typed members. Also creates ExternalSymbol entries
// in kernel_scope for members referenced from orig_scope.
ASR::symbol_t* GpuOffloadVisitor::import_struct_def(ASR::Struct_t *orig_struct,
        SymbolTable *orig_scope, SymbolTable *kernel_scope,
        const Location &loc) {
    std::string struct_name = orig_struct->m_name;

    // If already imported, return existing
    ASR::symbol_t *existing = kernel_scope->get_symbol(struct_name);
    if (existing) return existing;

    // Import the parent type first, so the extending type keeps its
    // inheritance chain in the kernel scope. Members inherited from
    // the parent are declared in the parent's symtab, so without this
    // they would be lost entirely. The parent chain is acyclic, so
    // this terminates; and since the parent is added to kernel_scope
    // before this struct, the early-return guard above stays correct.
    ASR::symbol_t *new_parent = nullptr;
    if (orig_struct->m_parent) {
        ASR::symbol_t *parent_sym = ASRUtils::symbol_get_past_external(
            orig_struct->m_parent);
        if (is_a<ASR::Struct_t>(*parent_sym)) {
            new_parent = import_struct_def(
                down_cast<ASR::Struct_t>(parent_sym),
                orig_scope, kernel_scope, loc);
        }
    }

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
        orig_struct->m_is_sequence,
        nullptr, 0, nullptr, new_parent, nullptr, 0);
    ASR::symbol_t *kernel_struct = down_cast<ASR::symbol_t>(new_struct);
    kernel_scope->add_symbol(struct_name, kernel_struct);

    // Create ExternalSymbol entries in kernel scope for each member,
    // so that StructInstanceMember can reference them. A type imported
    // for a routine's copy alone has no `orig_scope` to take them from.
    // Search orig_scope and walk up through AssociateBlock/Block
    // parent scopes, because when the loop is inside an
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
                        gpu_struct_lookup_member(kernel_struct,
                            es->m_original_name)) {
                    is_member = true;
                }
            }
            if (is_member) {
                std::string es_name = item.first;
                if (kernel_scope->get_symbol(es_name)) continue;
                ASR::symbol_t *new_member_in_struct =
                    gpu_struct_lookup_member(kernel_struct,
                        es->m_original_name);
                if (!new_member_in_struct) continue;
                std::string owner_name = struct_member_owner_name(
                    new_member_in_struct, struct_name);
                ASR::asr_t *new_es = ASR::make_ExternalSymbol_t(al, loc,
                    kernel_scope, s2c(al, es_name),
                    new_member_in_struct, s2c(al, owner_name),
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
ASR::symbol_t* GpuOffloadVisitor::import_struct_type(ASR::symbol_t *orig_sym,
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
    // Use orig_scope (the loop's enclosing scope) rather
    // than var->m_parent_symtab. When the loop is inside
    // an AssociateBlock, ExternalSymbol entries for struct members
    // (e.g., type-bound procedure references) are migrated from
    // inner associate scopes into orig_scope during associate
    // resolution. The variable's declaring scope may be a parent
    // of orig_scope and would not contain these migrated symbols.
    return import_struct_def(down_cast<ASR::Struct_t>(struct_sym),
        orig_scope, kernel_scope, loc);
}

namespace {

// Points every structure constant of an expression, nested ones included,
// at the kernel's copy of the derived type it names.
class GpuStructConstantRetargeter
        : public ASR::BaseWalkVisitor<GpuStructConstantRetargeter> {
public:
    std::function<ASR::symbol_t*(ASR::Struct_t*)> kernel_struct;

    void visit_StructConstant(const ASR::StructConstant_t &x) {
        ASR::symbol_t *type_sym = ASRUtils::symbol_get_past_external(
            x.m_dt_sym);
        if (type_sym && is_a<ASR::Struct_t>(*type_sym)) {
            const_cast<ASR::StructConstant_t&>(x).m_dt_sym = kernel_struct(
                down_cast<ASR::Struct_t>(type_sym));
        }
        ASR::BaseWalkVisitor<GpuStructConstantRetargeter>
            ::visit_StructConstant(x);
    }
};

} // namespace

// The value of a named constant cloned into the kernel: a copy of `value`
// whose structure constants name the kernel's copies of their derived
// types, as the clone's type declaration does. The original value is
// shared with the host's constant; left naming the host's definition, it
// would not have the clone's type where it replaces a reference to the
// clone, as it does under --fast.
ASR::expr_t* GpuOffloadVisitor::import_constant_value(ASR::expr_t *value,
        SymbolTable *orig_scope, SymbolTable *kernel_scope,
        const Location &loc) {
    if (!value) return nullptr;
    ASRUtils::ExprStmtDuplicator dup(al);
    ASR::expr_t *copy = dup.duplicate_expr(value);
    GpuStructConstantRetargeter retarget;
    retarget.kernel_struct = [&](ASR::Struct_t *orig_struct) {
        return import_struct_def(orig_struct, orig_scope, kernel_scope, loc);
    };
    retarget.visit_expr(*copy);
    return copy;
}

} // namespace LCompilers
