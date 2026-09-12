#include <filesystem>
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

// Load any module dependencies of a loaded submodule TU into
// the main TU's symbol table so that fix_external_symbols can
// resolve them.  This handles transitive dependencies: if the
// submodule uses module A which in turn uses module B, both A
// and B are loaded.  After loading, fix_external_symbols is
// called on the main TU to resolve any null m_external pointers
// in the newly loaded modules.
void GpuOffloadVisitor::load_submodule_deps(ASR::TranslationUnit_t &sub_tu) {
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

// A submodule read back from its `.smod` file carries the array
// constructors it was written with: the implied-do lowering ran over
// this translation unit before this pass, so it never saw this body.
// Splicing it into a kernel as it stands would carry an implied-do
// into everything downstream -- the temporary extraction that runs
// after this pass hoists a loop-variant element out of one, which
// evaluates it once instead of once per iteration, and the Metal code
// generator has no rendering for what is left. Lower them here, so a
// body loaded from disk is in the same shape as one compiled
// alongside its caller. Run only once the external symbols of the
// loaded unit are resolved, since the lowering reads their types.
void GpuOffloadVisitor::lower_loaded_implied_do_loops(
        ASR::TranslationUnit_t &sub_tu) {
    pass_replace_implied_do_loops(al, sub_tu, pass_options);
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
    // so that StructInstanceMember can reference them.
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

} // namespace LCompilers
