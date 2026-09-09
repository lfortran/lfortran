#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/pickle.h>
#include <libasr/asr_verify.h>
#include <libasr/diagnostics.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/pass/replace_gpu_offload.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/parallel_dispatch.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_undo.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/string_utils.h>
#include <libasr/codegen/gpu_utils.h>

#include <deque>
#include <filesystem>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

static int gpu_kernel_counter = 0;

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

// Walk a mask expression to find the first ArraySection and extract
// its loop bounds (start, end).
void GpuOffloadVisitor::find_array_section_bounds(ASR::expr_t *e,
        ASR::expr_t *&loop_start, ASR::expr_t *&loop_end) {
    if (loop_start) return;
    if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
        if (as->n_args > 0 && as->m_args[0].m_left && as->m_args[0].m_right) {
            loop_start = as->m_args[0].m_left;
            loop_end = as->m_args[0].m_right;
        }
    } else if (ASR::is_a<ASR::Var_t>(*e)) {
        ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(e));
        if (ASR::is_a<ASR::Array_t>(*type)) {
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, e->base.loc, 4));
            ASR::expr_t *dim1 = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, e->base.loc, 1,
                    int_type, ASR::integerbozType::Decimal));
            loop_start = ASRUtils::EXPR(
                ASR::make_ArrayBound_t(al, e->base.loc,
                    e, dim1, int_type,
                    ASR::arrayboundType::LBound, nullptr));
            loop_end = ASRUtils::EXPR(
                ASR::make_ArrayBound_t(al, e->base.loc,
                    e, dim1, int_type,
                    ASR::arrayboundType::UBound, nullptr));
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

// Collect per-dimension bounds from array sections in an expression.
// Returns bounds for all dimensions of the first ArraySection found.
void GpuOffloadVisitor::find_array_section_all_bounds(ASR::expr_t *e,
        std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> &dim_bounds) {
    if (!dim_bounds.empty()) return;
    if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
        for (size_t i = 0; i < as->n_args; i++) {
            if (as->m_args[i].m_left && as->m_args[i].m_right) {
                dim_bounds.push_back({as->m_args[i].m_left,
                    as->m_args[i].m_right});
            }
        }
    } else if (ASR::is_a<ASR::Var_t>(*e)) {
        ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(e));
        if (ASR::is_a<ASR::Array_t>(*type)) {
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, e->base.loc, 4));
            ASR::dimension_t *dims = nullptr;
            int rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
            for (int d = 0; d < rank; d++) {
                ASR::expr_t *dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, e->base.loc, d + 1,
                        int_type, ASR::integerbozType::Decimal));
                ASR::expr_t *lb = ASRUtils::EXPR(
                    ASR::make_ArrayBound_t(al, e->base.loc,
                        e, dim_expr, int_type,
                        ASR::arrayboundType::LBound, nullptr));
                ASR::expr_t *ub = ASRUtils::EXPR(
                    ASR::make_ArrayBound_t(al, e->base.loc,
                        e, dim_expr, int_type,
                        ASR::arrayboundType::UBound, nullptr));
                dim_bounds.push_back({lb, ub});
            }
        }
    } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
        ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
        find_array_section_all_bounds(rc->m_left, dim_bounds);
        find_array_section_all_bounds(rc->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
        ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
        find_array_section_all_bounds(ic->m_left, dim_bounds);
        find_array_section_all_bounds(ic->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
        ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
        find_array_section_all_bounds(lb->m_left, dim_bounds);
        find_array_section_all_bounds(lb->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
        ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
        find_array_section_all_bounds(rb->m_left, dim_bounds);
        find_array_section_all_bounds(rb->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        find_array_section_all_bounds(ib->m_left, dim_bounds);
        find_array_section_all_bounds(ib->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
        ASR::IntrinsicElementalFunction_t *ief =
            ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
        for (size_t i = 0; i < ief->n_args; i++) {
            if (ief->m_args[i])
                find_array_section_all_bounds(ief->m_args[i], dim_bounds);
        }
    }
}

// Build an element-wise expression by replacing ArraySection and
// whole-array Var nodes with ArrayItem nodes indexed by loop_var.
ASR::expr_t* GpuOffloadVisitor::elementize_mask(
        ASR::expr_t *e, ASR::expr_t *loop_var,
        ASR::ttype_t *logical_type, const Location &loc) {
    std::vector<ASR::expr_t*> vars = {loop_var};
    return elementize_mask_multi(e, vars, logical_type, loc);
}

// Build an element-wise expression by replacing ArraySection and
// whole-array Var nodes with ArrayItem nodes indexed by per-dimension
// loop variables.
ASR::expr_t* GpuOffloadVisitor::elementize_mask_multi(ASR::expr_t *e,
        std::vector<ASR::expr_t*> &loop_vars,
        ASR::ttype_t *logical_type, const Location &loc) {
    if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
        Vec<ASR::array_index_t> new_args;
        new_args.reserve(al, as->n_args);
        size_t lv_idx = 0;
        for (size_t i = 0; i < as->n_args; i++) {
            ASR::array_index_t idx;
            idx.loc = as->m_args[i].loc;
            if (as->m_args[i].m_left && as->m_args[i].m_right) {
                idx.m_left = nullptr;
                idx.m_right = (lv_idx < loop_vars.size())
                    ? loop_vars[lv_idx++] : loop_vars[0];
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
    } else if (ASR::is_a<ASR::Var_t>(*e)) {
        ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(e));
        if (ASR::is_a<ASR::Array_t>(*type)) {
            ASR::dimension_t *dims = nullptr;
            int rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
            Vec<ASR::array_index_t> new_args;
            new_args.reserve(al, rank);
            for (int d = 0; d < rank; d++) {
                ASR::array_index_t idx;
                idx.loc = loc;
                idx.m_left = nullptr;
                idx.m_right = (d < (int)loop_vars.size())
                    ? loop_vars[d] : loop_vars[0];
                idx.m_step = nullptr;
                new_args.push_back(al, idx);
            }
            ASR::ttype_t *elem_type = ASRUtils::extract_type(type);
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                e, new_args.p, new_args.n,
                elem_type, ASR::arraystorageType::ColMajor, nullptr));
        }
        return e;
    } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
        ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
        return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
            elementize_mask_multi(rc->m_left, loop_vars, logical_type, loc),
            rc->m_op,
            elementize_mask_multi(rc->m_right, loop_vars, logical_type, loc),
            logical_type, nullptr));
    } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
        ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
        return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
            elementize_mask_multi(ic->m_left, loop_vars, logical_type, loc),
            ic->m_op,
            elementize_mask_multi(ic->m_right, loop_vars, logical_type, loc),
            logical_type, nullptr));
    } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
        ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
        return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
            elementize_mask_multi(lb->m_left, loop_vars, logical_type, loc),
            lb->m_op,
            elementize_mask_multi(lb->m_right, loop_vars, logical_type, loc),
            logical_type, nullptr));
    } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
        ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
        ASR::ttype_t *real_type = ASRUtils::extract_type(
            ASRUtils::expr_type(e));
        return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
            elementize_mask_multi(rb->m_left, loop_vars, logical_type, loc),
            rb->m_op,
            elementize_mask_multi(rb->m_right, loop_vars, logical_type, loc),
            real_type, nullptr));
    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        ASR::ttype_t *int_elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(e));
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            elementize_mask_multi(ib->m_left, loop_vars, logical_type, loc),
            ib->m_op,
            elementize_mask_multi(ib->m_right, loop_vars, logical_type, loc),
            int_elem_type, nullptr));
    } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
        ASR::IntrinsicElementalFunction_t *ief =
            ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
        Vec<ASR::expr_t*> new_args;
        new_args.reserve(al, ief->n_args);
        for (size_t i = 0; i < ief->n_args; i++) {
            new_args.push_back(al, ief->m_args[i]
                ? elementize_mask_multi(ief->m_args[i], loop_vars,
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
ASR::expr_t* GpuOffloadVisitor::inline_single_all(
        ASR::IntrinsicArrayFunction_t *iaf,
        const Location &loc, Vec<ASR::stmt_t*> &preamble) {
    if (iaf->n_args < 1 || !iaf->m_args[0]) return nullptr;
    ASR::expr_t *mask = iaf->m_args[0];

    std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> dim_bounds;
    find_array_section_all_bounds(mask, dim_bounds);
    if (dim_bounds.empty()) return nullptr;

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

    // Create loop variables for each dimension
    std::vector<ASR::expr_t*> loop_vars;
    for (size_t d = 0; d < dim_bounds.size(); d++) {
        std::string loop_var_name = var_scope->get_unique_name(
            "__gpu_all_i" + std::to_string(d));
        ASR::symbol_t *loop_var_sym = gpu_new_variable(al, loc, var_scope,
            loop_var_name, ASRUtils::duplicate_type(al, int_type));
        loop_vars.push_back(ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, loop_var_sym)));
    }

    // Create result variable
    std::string res_var_name = var_scope->get_unique_name("__gpu_all_res");
    ASR::symbol_t *res_var_sym = gpu_new_variable(al, loc, var_scope,
        res_var_name, ASRUtils::duplicate_type(al, logical_type));
    ASR::expr_t *res_var = ASRUtils::EXPR(
        ASR::make_Var_t(al, loc, res_var_sym));

    // __gpu_all_res = .true.
    preamble.push_back(al, ASRUtils::STMT(
        ASR::make_Assignment_t(al, loc, res_var,
            ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                true, logical_type)),
            nullptr, false, false)));

    ASR::expr_t *elem_mask = elementize_mask_multi(mask, loop_vars,
        logical_type, loc);

    // Build innermost body: if (.not. elem_mask) __gpu_all_res = .false.
    Vec<ASR::stmt_t*> if_body;
    if_body.reserve(al, 1);
    if_body.push_back(al, ASRUtils::STMT(
        ASR::make_Assignment_t(al, loc, res_var,
            ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                false, logical_type)),
            nullptr, false, false)));
    Vec<ASR::stmt_t*> if_else;
    if_else.reserve(al, 0);
    ASR::expr_t *not_mask = ASRUtils::EXPR(
        ASR::make_LogicalNot_t(al, loc, elem_mask, logical_type, nullptr));
    ASR::stmt_t *inner_stmt = ASRUtils::STMT(
        ASR::make_If_t(al, loc, nullptr, not_mask,
            if_body.p, if_body.n, if_else.p, if_else.n));

    // Build nested loops from innermost dimension outward
    ASR::stmt_t *loop_nest = inner_stmt;
    for (int d = (int)dim_bounds.size() - 1; d >= 0; d--) {
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = loop_vars[d];
        head.m_start = dim_bounds[d].first;
        head.m_end = dim_bounds[d].second;
        head.m_increment = nullptr;
        Vec<ASR::stmt_t*> loop_body;
        loop_body.reserve(al, 1);
        loop_body.push_back(al, loop_nest);
        loop_nest = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
            head, loop_body.p, loop_body.n, nullptr, 0));
    }
    preamble.push_back(al, loop_nest);

    return res_var;
}

// Check if an expression tree contains any IntrinsicArrayFunction All.
bool GpuOffloadVisitor::contains_intrinsic_all(ASR::expr_t *e) {
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
ASR::expr_t* GpuOffloadVisitor::replace_all_in_expr(
        ASR::expr_t *e, const Location &loc,
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

// Inline IntrinsicArrayFunction All inside a parallel loop body.
// Replaces:
//   eq(l) = all(a(:,l) == b(:,l))
// or:
//   eq(l) = all(a(1:l) > 0) .and. all(b(1:l) > 0)
// With inlined loops that compute the All result into temporaries.
// This avoids complex lowered code (Associate, Allocate, FunctionCall)
// that the Metal backend cannot handle inside GPU kernels.
void GpuOffloadVisitor::inline_intrinsic_all(ParallelLoopNest &nest) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, nest.n_body * 3);
    bool changed = false;

    for (size_t si = 0; si < nest.n_body; si++) {
        ASR::stmt_t *stmt = nest.body[si];
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
            // Track non-array scalar targets as reduction liveouts
            if (ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                ASR::ttype_t *tgt_type =
                    ASRUtils::expr_type(asgn->m_target);
                if (!ASRUtils::is_array(tgt_type)) {
                    all_reduction_targets.insert(
                        ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                asgn->m_target)->m_v));
                }
            }
        } else {
            new_body.push_back(al, stmt);
        }
    }

    if (changed) {
        // Through set_body, so the loop the nest reads from carries
        // the rewrite too and not just the view of it.
        nest.set_body(new_body.p, new_body.n);
    }
}

// Inline IntrinsicArrayFunction MatMul inside a parallel loop body.
// Replaces:
//   c = matmul(a, b)
// With nested DoLoops that compute the matrix multiplication directly.
// This avoids generating a call to _lcompilers_matmul which is not
// available inside Metal GPU kernels.
// The MatMul shapes `inline_matmul_stmts` lowers on an Assignment:
// the matmul is either the whole right-hand side, or a direct operand
// of a RealBinOp on the right-hand side (`z = matmul(w, a) + b`). On a
// match of the second shape the other operand, the operator and the
// side of the matmul are reported back to the caller.
ASR::IntrinsicArrayFunction_t* GpuOffloadVisitor::match_statement_matmul(
        ASR::expr_t *value, ASR::expr_t *&binop_other,
        ASR::binopType &binop_op, bool &matmul_is_left) {
    auto is_matmul = [](ASR::expr_t *e) -> ASR::IntrinsicArrayFunction_t* {
        if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) return nullptr;
        auto *f = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
        if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                f->m_arr_intrinsic_id)
                    != ASRUtils::IntrinsicArrayFunctions::MatMul) {
            return nullptr;
        }
        return f;
    };
    if (!value) return nullptr;
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*value)) {
        return is_matmul(value);
    }
    if (!ASR::is_a<ASR::RealBinOp_t>(*value)) return nullptr;
    ASR::RealBinOp_t *rbop = ASR::down_cast<ASR::RealBinOp_t>(value);
    ASR::expr_t *left = rbop->m_left;
    ASR::expr_t *right = rbop->m_right;
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*left))
        left = ASR::down_cast<ASR::ArrayPhysicalCast_t>(left)->m_arg;
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*right))
        right = ASR::down_cast<ASR::ArrayPhysicalCast_t>(right)->m_arg;
    if (auto *f = is_matmul(left)) {
        binop_other = rbop->m_right;
        binop_op = rbop->m_op;
        matmul_is_left = true;
        return f;
    }
    if (auto *f = is_matmul(right)) {
        binop_other = rbop->m_left;
        binop_op = rbop->m_op;
        matmul_is_left = false;
        return f;
    }
    return nullptr;
}

void GpuOffloadVisitor::inline_matmul_stmts(
        ASR::stmt_t** &body, size_t &n_body) {
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
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_matmul_stmts(ab->m_body, ab->n_body);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

        ASR::expr_t *binop_other = nullptr;
        ASR::binopType binop_op = ASR::binopType::Add;
        bool matmul_is_left = true;
        ASR::IntrinsicArrayFunction_t *iaf = match_statement_matmul(
            asgn->m_value, binop_other, binop_op, matmul_is_left);
        if (!iaf) {
            new_body.push_back(al, stmt);
            continue;
        }

        // A matmul is not elementwise: it reads the whole of an
        // operand for every element it writes. Lowering `v = matmul(w,
        // v)` into loops over `v` would read elements it has already
        // overwritten, so the result goes to a temporary first and is
        // copied over the target afterwards.
        if (matmul_operand_aliases_target(iaf, asgn->m_target)) {
            ASR::expr_t *tmp = make_matmul_result_temp(iaf, stmt->base.loc,
                current_scope, false, asgn->m_target);
            if (tmp != nullptr) {
                Vec<ASR::stmt_t*> two;
                two.reserve(al, 2);
                two.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
                    al, stmt->base.loc, tmp, asgn->m_value, nullptr,
                    false, false)));
                two.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
                    al, stmt->base.loc, asgn->m_target,
                    matmul_temp_section(tmp, asgn->m_target), nullptr,
                    false, false)));
                ASR::stmt_t **two_body = two.p;
                size_t two_n = two.n;
                inline_matmul_stmts(two_body, two_n);
                for (size_t k = 0; k < two_n; k++) {
                    new_body.push_back(al, two_body[k]);
                }
                changed = true;
                continue;
            }
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
        ASR::dimension_t *dims_c = nullptr;
        ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(asgn->m_target), dims_c);

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
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, int_type));
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
            return get_dim_bounds(al, arg->base.loc, dims,
                (size_t)dim_idx, arg);
        };

        // matmul pairs its operands by position: the k-th column of
        // `a` multiplies the k-th element of `b` whatever lower bound
        // either operand declares and wherever an operand that is an
        // array section starts inside its parent array.  The loop
        // variables run over the index space of one chosen operand,
        // so a variable used to index a different operand is first
        // rebased onto that operand's own first index.  When both
        // spaces are known to start at the same index the variable is
        // used as it is, which leaves the usual lower-bound-of-one
        // case exactly as it was.
        auto rebase_index = [&](ASR::expr_t *operand,
                ASR::dimension_t *operand_dims, int dim_idx,
                ASR::expr_t *var,
                ASR::expr_t *ref_start) -> ASR::expr_t* {
            ASR::expr_t *start = get_loop_bounds(operand, operand_dims,
                dim_idx).first;
            if (start == nullptr || ref_start == nullptr) return var;
            if (start == ref_start) return var;
            if (is_int_literal(start, 1) && is_int_literal(ref_start, 1))
                return var;
            ASR::expr_t *offset = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc, var,
                    ASR::binopType::Sub, to_int32(loc, ref_start),
                    int_type, nullptr));
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                to_int32(loc, start), ASR::binopType::Add, offset,
                int_type, nullptr));
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

        // The other operand of `z = matmul(w, a) <op> b` is combined
        // with the matmul result element by element. A scalar operand
        // is the same for every element, so it is used as it is; only
        // an array operand is indexed by the loop variables. A scalar
        // reaches here wrapped in an ArrayBroadcast, whose type is an
        // array, so the wrapper is stripped before the rank is
        // checked.
        auto make_binop_other_item = [&](ASR::expr_t *other,
                std::vector<ASR::expr_t*> loop_vars,
                std::vector<ASR::expr_t*> ref_starts) -> ASR::expr_t* {
            while (true) {
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*other)) {
                    other = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                        other)->m_arg;
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*other)) {
                    other = ASR::down_cast<ASR::ArrayBroadcast_t>(
                        other)->m_array;
                } else {
                    break;
                }
            }
            if (!ASRUtils::is_array(ASRUtils::expr_type(other)))
                return other;
            ASR::dimension_t *other_dims = nullptr;
            ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(other), other_dims);
            std::vector<ASR::expr_t*> idx;
            for (size_t d = 0; d < loop_vars.size(); d++) {
                idx.push_back(rebase_index(other, other_dims, (int)d,
                    loop_vars[d], ref_starts[d]));
            }
            return make_section_item(other, idx);
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

            int i_dim = transpose_a ? 1 : 0;
            int k_dim = transpose_a ? 0 : 1;
            auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, k_dim);
            auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, i_dim);

            ASR::expr_t *c_i = make_section_item(asgn->m_target,
                {rebase_index(asgn->m_target, dims_c, 0, var_i,
                    i_start)});
            ASR::expr_t *a_ik = transpose_a
                ? make_section_item(arg_a, {var_k, var_i})
                : make_section_item(arg_a, {var_i, var_k});
            ASR::expr_t *b_k = make_section_item(arg_b,
                {rebase_index(arg_b, dims_b, 0, var_k, k_start)});

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
                ASR::expr_t *other_i = make_binop_other_item(
                    binop_other, {var_i}, {i_start});
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

            int k_dim = transpose_b ? 1 : 0;
            int j_dim = transpose_b ? 0 : 1;
            auto [k_start, k_end] = get_loop_bounds(arg_b, dims_b, k_dim);
            auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, j_dim);

            ASR::expr_t *c_j = make_section_item(asgn->m_target,
                {rebase_index(asgn->m_target, dims_c, 0, var_j,
                    j_start)});
            ASR::expr_t *a_k = make_section_item(arg_a,
                {rebase_index(arg_a, dims_a, 0, var_k, k_start)});
            ASR::expr_t *b_kj = transpose_b
                ? make_section_item(arg_b, {var_j, var_k})
                : make_section_item(arg_b, {var_k, var_j});

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
                ASR::expr_t *other_j = make_binop_other_item(
                    binop_other, {var_j}, {j_start});
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

            int a_k_dim = transpose_a ? 0 : 1;
            int a_i_dim = transpose_a ? 1 : 0;
            int b_j_dim = transpose_b ? 0 : 1;
            int b_k_dim = transpose_b ? 1 : 0;
            auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, a_k_dim);
            auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, b_j_dim);
            auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, a_i_dim);

            // `k` runs over `a`'s contraction dimension, so only
            // `b`'s copy of it is rebased; `j` already runs over
            // `b`'s own dimension.
            ASR::expr_t *var_k_b = rebase_index(arg_b, dims_b, b_k_dim,
                var_k, k_start);
            ASR::expr_t *c_ij = make_section_item(asgn->m_target,
                {rebase_index(asgn->m_target, dims_c, 0, var_i, i_start),
                 rebase_index(asgn->m_target, dims_c, 1, var_j, j_start)});
            ASR::expr_t *a_ik = transpose_a
                ? make_section_item(arg_a, {var_k, var_i})
                : make_section_item(arg_a, {var_i, var_k});
            ASR::expr_t *b_kj = transpose_b
                ? make_section_item(arg_b, {var_j, var_k_b})
                : make_section_item(arg_b, {var_k_b, var_j});

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
                ASR::expr_t *other_ij = make_binop_other_item(
                    binop_other, {var_i, var_j}, {i_start, j_start});
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

// A matmul that `inline_matmul_stmts` does not match -- one nested
// inside a unary minus, inside another intrinsic, inside a call
// argument, inside an array constructor (`r = [0.0, matmul(a, b)]`)
// or as an argument of another matmul -- survives into the shader as
// a call to the host runtime helper `_lcompilers_matmul*`, which does
// not exist on the device. Hoist every such matmul into its own
// temporary first, so the existing whole-right-hand-side lowering
// applies to it and the enclosing expression is left with a plain
// array variable (a shape the Metal backend already handles).
void GpuOffloadVisitor::hoist_nested_matmuls(ParallelLoopNest &nest) {
    SymbolTable *var_scope = current_scope;
    while (var_scope && var_scope->asr_owner &&
           var_scope->asr_owner->type == ASR::asrType::symbol &&
           ASR::is_a<ASR::AssociateBlock_t>(
               *ASR::down_cast<ASR::symbol_t>(var_scope->asr_owner))) {
        var_scope = var_scope->parent;
    }
    NestBodyWriteBack back(nest);
    hoist_nested_matmuls_in_body(back.body, back.n_body, var_scope,
        false, true);
}

// `scope_has_workspaces` says whether a run-time sized temporary put
// into `var_scope` will be given a per-thread VLA workspace buffer.
// Only a BLOCK that is a direct statement of the loop body is scanned
// for those; a temporary at kernel scope would be a single buffer
// shared by every thread.  `at_loop_top` tracks whether this
// statement list is that loop body itself.
void GpuOffloadVisitor::hoist_nested_matmuls_in_body(
        ASR::stmt_t** &body, size_t &n_body,
        SymbolTable *var_scope, bool scope_has_workspaces,
        bool at_loop_top) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 2);
    bool changed = false;

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            hoist_nested_matmuls_in_body(dl->m_body, dl->n_body,
                var_scope, scope_has_workspaces, false);
            new_body.push_back(al, stmt);
            continue;
        }
        // A spliced-in device function body lives in its own BLOCK;
        // hoist inside it too, into that block's scope.
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                hoist_nested_matmuls_in_body(blk->m_body, blk->n_body,
                    blk->m_symtab, at_loop_top, false);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            hoist_nested_matmuls_in_body(ab->m_body, ab->n_body,
                var_scope, scope_has_workspaces, false);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        size_t before = new_body.size();
        hoist_matmuls_from_assignment(
            ASR::down_cast<ASR::Assignment_t>(stmt), new_body, var_scope,
            scope_has_workspaces);
        new_body.push_back(al, stmt);
        if (new_body.size() != before + 1) changed = true;
    }

    if (changed) {
        body = new_body.p;
        n_body = new_body.n;
    }
}

// Hoist every matmul in the value of `asgn` that the statement-level
// lowering cannot see into a temporary, appending the temporaries'
// assignments to `out`. The matmul the lowering does match is left in
// place; its arguments are still searched, so a nested matmul such as
// `matmul(a, matmul(a, b))` has its inner operand hoisted.
void GpuOffloadVisitor::hoist_matmuls_from_assignment(ASR::Assignment_t *asgn,
        Vec<ASR::stmt_t*> &out, SymbolTable *var_scope,
        bool scope_has_workspaces) {
    ASR::expr_t *binop_other = nullptr;
    ASR::binopType binop_op = ASR::binopType::Add;
    bool matmul_is_left = true;
    ASR::IntrinsicArrayFunction_t *handled = match_statement_matmul(
        asgn->m_value, binop_other, binop_op, matmul_is_left);
    Location loc = asgn->base.base.loc;
    while (true) {
        ASR::IntrinsicArrayFunction_t *mm = find_array_intrinsic_in_expr(
            asgn->m_value, ASRUtils::IntrinsicArrayFunctions::MatMul,
            handled);
        if (!mm) break;
        ASR::expr_t *tmp_var = make_matmul_result_temp(mm, loc,
            var_scope, scope_has_workspaces);
        if (!tmp_var) break;
        ASR::stmt_t *tmp_asgn = ASRUtils::STMT(ASR::make_Assignment_t(
            al, loc, tmp_var, (ASR::expr_t*)mm, nullptr, false, false));
        if (!replace_array_intrinsic_in_expr(asgn->m_value, mm,
                tmp_var)) {
            break;
        }
        hoist_matmuls_from_assignment(
            ASR::down_cast<ASR::Assignment_t>(tmp_asgn), out, var_scope,
            scope_has_workspaces);
        out.push_back(al, tmp_asgn);
    }
}

// Whether an operand of `mm` reads the storage the assignment writes.
bool GpuOffloadVisitor::matmul_operand_aliases_target(
        ASR::IntrinsicArrayFunction_t *mm,
        ASR::expr_t *target) {
    GpuDesignatorBase base = gpu_designator_base(target);
    if (!base.is_known()) return false;
    GpuWrittenRootCollector unused;
    (void)unused;
    for (size_t i = 0; i < mm->n_args; i++) {
        if (!mm->m_args[i]) continue;
        GpuDesignatorBaseFinder finder;
        finder.wanted = base;
        finder.visit_expr(*mm->m_args[i]);
        if (finder.found) return true;
    }
    return false;
}

// The whole of `tmp`, shaped like `target` so the copy back covers
// exactly what the assignment was going to write.
ASR::expr_t* GpuOffloadVisitor::matmul_temp_section(
        ASR::expr_t *tmp, ASR::expr_t *target) {
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(target);
    if (!ASR::is_a<ASR::ArraySection_t>(*v)) return tmp;
    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(v);
    Vec<ASR::array_index_t> args;
    args.reserve(al, as->n_args);
    const Location &loc = tmp->base.loc;
    for (size_t i = 0; i < as->n_args; i++) {
        ASR::array_index_t idx;
        idx.loc = loc;
        if (as->m_args[i].m_left && as->m_args[i].m_right) {
            idx.m_left = int32_const(loc, 1);
            idx.m_right = section_extent(loc, as->m_args[i]);
            idx.m_step = int32_const(loc, 1);
        } else {
            idx.m_left = nullptr;
            idx.m_right = as->m_args[i].m_right;
            idx.m_step = nullptr;
        }
        args.push_back(al, idx);
    }
    return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc, tmp,
        args.p, args.n, ASRUtils::expr_type(tmp), nullptr));
}

// The compile-time extents an expression is declared with, if it has
// them. A section of such an array is bounded by them too.
bool GpuOffloadVisitor::declared_constant_dims(
        ASR::expr_t *e, Vec<ASR::dimension_t> &out) {
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    if (ASR::is_a<ASR::ArraySection_t>(*v)) {
        v = ASR::down_cast<ASR::ArraySection_t>(v)->m_v;
    }
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(v));
    if (!t || !ASR::is_a<ASR::Array_t>(*t)) return false;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
    out.reserve(al, arr->n_dims);
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (!arr->m_dims[d].m_length ||
                !ASRUtils::expr_value(arr->m_dims[d].m_length)) {
            return false;
        }
        out.push_back(al, arr->m_dims[d]);
    }
    return out.n > 0;
}

// A local temporary holding the result of `mm`, or nullptr if the
// result shape cannot be determined from the operands.
ASR::expr_t* GpuOffloadVisitor::make_matmul_result_temp(
        ASR::IntrinsicArrayFunction_t *mm,
        const Location &loc, SymbolTable *var_scope,
        bool scope_has_workspaces, ASR::expr_t *target) {
    ASR::expr_t *e = (ASR::expr_t*)mm;
    if (!ASRUtils::is_array(ASRUtils::expr_type(e))) return nullptr;
    Vec<ASR::dimension_t> dims;
    if (!intrinsic_array_result_dims(e, dims)) return nullptr;
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(e));
    bool all_const = true;
    for (size_t d = 0; d < dims.n; d++) {
        if (!dims[d].m_length ||
                !ASRUtils::expr_value(dims[d].m_length)) {
            all_const = false;
        }
    }
    // The result never outgrows what it is assigned to, so a target
    // declared with compile-time extents bounds the temporary even
    // when the result's own extents are only known at run time. That
    // buys a thread-local temporary where a run-time sized one would
    // have to be a workspace, or nothing at all.
    if (!all_const && target != nullptr) {
        Vec<ASR::dimension_t> target_dims;
        if (declared_constant_dims(target, target_dims)
                && target_dims.n == dims.n) {
            dims = target_dims;
            all_const = true;
        }
    }
    // A run-time sized temporary is only correct where each thread
    // gets its own workspace slice; anywhere else it would be one
    // buffer written by every thread at once.
    if (!all_const && !scope_has_workspaces) return nullptr;
    ASR::ttype_t *tmp_type = ASRUtils::TYPE(
        ASR::make_Array_t(al, loc, elem_type, dims.p, dims.n,
            all_const
                ? ASR::array_physical_typeType::FixedSizeArray
                : ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
    std::string name = var_scope->get_unique_name("__gpu_matmul_tmp");
    ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
        tmp_type);
    return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
}

// Shape of an array-valued intrinsic's result, taken from its
// operands' declared dimensions.
bool GpuOffloadVisitor::intrinsic_array_result_dims(ASR::expr_t *e,
        Vec<ASR::dimension_t> &dims) {
    ASR::IntrinsicArrayFunction_t *iaf =
        ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
    if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
            iaf->m_arr_intrinsic_id)
                != ASRUtils::IntrinsicArrayFunctions::MatMul) {
        return false;
    }
    if (iaf->n_args < 2) return false;
    ASR::expr_t *a = iaf->m_args[0];
    ASR::expr_t *b = iaf->m_args[1];
    while (a && ASR::is_a<ASR::ArrayPhysicalCast_t>(*a))
        a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a)->m_arg;
    while (b && ASR::is_a<ASR::ArrayPhysicalCast_t>(*b))
        b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(b)->m_arg;
    if (!a || !b) return false;
    ASR::dimension_t *da = nullptr, *db = nullptr;
    int ra = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(a)), da);
    int rb = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(b)), db);
    dims.reserve(al, 2);
    if (ra == 2 && rb == 1) {
        dims.push_back(al, dim_or_runtime_extent(a, da, 0));
    } else if (ra == 1 && rb == 2) {
        dims.push_back(al, dim_or_runtime_extent(b, db, 1));
    } else if (ra == 2 && rb == 2) {
        dims.push_back(al, dim_or_runtime_extent(a, da, 0));
        dims.push_back(al, dim_or_runtime_extent(b, db, 1));
    } else {
        return false;
    }
    return true;
}

// Dimension `d` of `operand`, described so that the extent is
// available wherever the shape is needed.  A deferred-shape operand
// -- an allocatable, a pointer or an assumed-shape dummy -- carries no
// declared length, so its extent is the operand's own run-time
// `size(operand, d + 1)` instead.  That expression is what the VLA
// workspace machinery resolves back to the extents the host already
// passes to the kernel.
ASR::dimension_t GpuOffloadVisitor::dim_or_runtime_extent(ASR::expr_t *operand,
        ASR::dimension_t *dims, size_t d) {
    if (dims && dims[d].m_length) return dims[d];
    const Location &loc = operand->base.loc;
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::dimension_t res;
    res.loc = loc;
    res.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        1, int_type, ASR::integerbozType::Decimal));
    ASR::expr_t *dim_expr = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, (int64_t)d + 1, int_type,
            ASR::integerbozType::Decimal));
    res.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
        operand, dim_expr, int_type, nullptr));
    return res;
}

void GpuOffloadVisitor::inline_intrinsic_matmul(ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    inline_matmul_stmts(back.body, back.n_body);
}

// Distribute ArrayItem indexing through an array expression tree
// to produce a scalar expression. For example:
//   sum(a + b) with index k  -->  a(k) + b(k)
// instead of the incorrect (a + b)[k] which would be pointer arithmetic.
ASR::expr_t* GpuOffloadVisitor::index_array_expr(ASR::expr_t *expr,
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

// Inline IntrinsicArrayFunction Sum inside a parallel loop body.
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
// Search an expression tree for an IntrinsicArrayFunction node of the
// given kind. `skip` names a node the caller already handles: it is
// not reported, but its arguments are still searched.
ASR::IntrinsicArrayFunction_t* GpuOffloadVisitor::find_array_intrinsic_in_expr(
        ASR::expr_t *expr, ASRUtils::IntrinsicArrayFunctions which,
        ASR::IntrinsicArrayFunction_t *skip) {
    if (!expr) return nullptr;
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
        auto *iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expr);
        if (iaf != skip &&
                static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id) == which) {
            return iaf;
        }
        for (size_t i = 0; i < iaf->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(iaf->m_args[i],
                which, skip);
            if (found) return found;
        }
        return nullptr;
    }
    if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
        auto *found = find_array_intrinsic_in_expr(op->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(op->m_right, which, skip);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        auto *found = find_array_intrinsic_in_expr(op->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(op->m_right, which, skip);
    }
    if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_arg, which,
            skip);
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg, which,
            skip);
    }
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg, which, skip);
    }
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg, which,
            skip);
    }
    if (ASR::is_a<ASR::RealCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::RealCompare_t>(expr);
        auto *found = find_array_intrinsic_in_expr(cmp->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(cmp->m_right, which, skip);
    }
    if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::IntegerCompare_t>(expr);
        auto *found = find_array_intrinsic_in_expr(cmp->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(cmp->m_right, which, skip);
    }
    if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
        auto *ief = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
        for (size_t i = 0; i < ief->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(ief->m_args[i],
                which, skip);
            if (found) return found;
        }
    }
    if (ASR::is_a<ASR::FunctionCall_t>(*expr)) {
        auto *fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
        for (size_t i = 0; i < fc->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(
                fc->m_args[i].m_value, which, skip);
            if (found) return found;
        }
    }
    if (ASR::is_a<ASR::ArrayConstructor_t>(*expr)) {
        auto *ac = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
        for (size_t i = 0; i < ac->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(ac->m_args[i],
                which, skip);
            if (found) return found;
        }
    }
    return nullptr;
}

// Replace a specific IntrinsicArrayFunction node in an expression tree
// with a replacement expression.
bool GpuOffloadVisitor::replace_array_intrinsic_in_expr(ASR::expr_t* &expr,
        ASR::IntrinsicArrayFunction_t *target,
        ASR::expr_t *replacement) {
    if (!expr) return false;
    if (expr == (ASR::expr_t*)target) {
        expr = replacement;
        return true;
    }
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
        auto *c = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
        if (c->m_arg == (ASR::expr_t*)target) {
            c->m_arg = replacement;
            c->m_old = ASRUtils::extract_physical_type(
                ASRUtils::expr_type(replacement));
            return true;
        }
        return replace_array_intrinsic_in_expr(c->m_arg, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
        auto *iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expr);
        for (size_t i = 0; i < iaf->n_args; i++) {
            if (replace_array_intrinsic_in_expr(iaf->m_args[i], target,
                    replacement))
                return true;
        }
        return false;
    }
    if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
        if (replace_array_intrinsic_in_expr(op->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(op->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        if (replace_array_intrinsic_in_expr(op->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(op->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
        return replace_array_intrinsic_in_expr(
            ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_arg, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
        return replace_array_intrinsic_in_expr(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg, target,
            replacement);
    }
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return replace_array_intrinsic_in_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg, target, replacement);
    }
    if (ASR::is_a<ASR::RealCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::RealCompare_t>(expr);
        if (replace_array_intrinsic_in_expr(cmp->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(cmp->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::IntegerCompare_t>(expr);
        if (replace_array_intrinsic_in_expr(cmp->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(cmp->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
        auto *ief = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
        for (size_t i = 0; i < ief->n_args; i++) {
            if (replace_array_intrinsic_in_expr(ief->m_args[i], target,
                    replacement))
                return true;
        }
    }
    if (ASR::is_a<ASR::FunctionCall_t>(*expr)) {
        auto *fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
        for (size_t i = 0; i < fc->n_args; i++) {
            if (replace_array_intrinsic_in_expr(fc->m_args[i].m_value,
                    target, replacement))
                return true;
        }
    }
    if (ASR::is_a<ASR::ArrayConstructor_t>(*expr)) {
        auto *ac = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
        for (size_t i = 0; i < ac->n_args; i++) {
            if (replace_array_intrinsic_in_expr(ac->m_args[i], target,
                    replacement))
                return true;
        }
    }
    return false;
}

// Extract nested Sum calls from assignment values into separate
// temporary assignments so the main Sum inlining logic can handle them.
// E.g., "cost = cost + sum(a)" becomes:
//   "__gpu_sum_tmp = sum(a)"
//   "cost = cost + __gpu_sum_tmp"
void GpuOffloadVisitor::extract_nested_sums(
        ASR::stmt_t** &stmts, size_t &n_stmts,
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
            find_array_intrinsic_in_expr(asgn->m_value,
                ASRUtils::IntrinsicArrayFunctions::Sum);
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
        ASR::symbol_t *tmp_sym = gpu_new_variable(al, loc, var_scope,
            tmp_name, ASRUtils::duplicate_type(al, sum_type));
        ASR::expr_t *tmp_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, tmp_sym));

        // Create: __gpu_sum_tmp = sum(a)
        ASR::expr_t *sum_expr = (ASR::expr_t*)sum_node;
        expanded.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, tmp_var, sum_expr,
                nullptr, false, false)));

        // Replace sum node in original expression with tmp_var
        replace_array_intrinsic_in_expr(asgn->m_value, sum_node,
            tmp_var);

        // Add modified original assignment
        expanded.push_back(al, stmt);
        changed = true;
    }

    if (changed) {
        stmts = expanded.p;
        n_stmts = expanded.n;
    }
}

void GpuOffloadVisitor::inline_sum_in_stmts(
        ASR::stmt_t** &stmts, size_t &n_stmts,
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

        // Recurse into AssociateBlock bodies
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_sum_in_stmts(ab->m_body, ab->n_body,
                ab->m_symtab);
            new_body.push_back(al, stmt);
            continue;
        }

        // Recurse into If bodies
        if (ASR::is_a<ASR::If_t>(*stmt)) {
            ASR::If_t *if_stmt = ASR::down_cast<ASR::If_t>(stmt);
            inline_sum_in_stmts(if_stmt->m_body, if_stmt->n_body,
                scope);
            inline_sum_in_stmts(if_stmt->m_orelse, if_stmt->n_orelse,
                scope);
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
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, type));
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

                // If arr_arg is a FunctionCall returning an
                // allocatable copy of a struct member (e.g.,
                // sum(vals(x)) where vals returns x%v), resolve
                // to the actual struct member access (x%v) so the
                // sum loop iterates directly over the member data
                // without allocating a temporary array.
                if (ASR::is_a<ASR::FunctionCall_t>(*arr_arg)) {
                    ASR::FunctionCall_t *fc2 =
                        ASR::down_cast<ASR::FunctionCall_t>(
                            arr_arg);
                    ASR::symbol_t *fn_sym2 =
                        ASRUtils::symbol_get_past_external(
                            fc2->m_name);
                    if (ASR::is_a<ASR::Function_t>(*fn_sym2)) {
                        ASR::Function_t *fn2 =
                            ASR::down_cast<ASR::Function_t>(
                                fn_sym2);
                        if (fn2->m_return_var &&
                                ASR::is_a<ASR::Var_t>(
                                    *fn2->m_return_var)) {
                            std::string ret_name2 =
                                ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::Var_t>(
                                        fn2->m_return_var)->m_v);
                            for (size_t bi = 0;
                                    bi < fn2->n_body; bi++) {
                                if (!ASR::is_a<ASR::Assignment_t>(
                                        *fn2->m_body[bi]))
                                    continue;
                                ASR::Assignment_t *ba =
                                    ASR::down_cast<
                                        ASR::Assignment_t>(
                                            fn2->m_body[bi]);
                                if (!ASR::is_a<ASR::Var_t>(
                                        *ba->m_target))
                                    continue;
                                std::string tname =
                                    ASRUtils::symbol_name(
                                        ASR::down_cast<
                                            ASR::Var_t>(
                                                ba->m_target)
                                            ->m_v);
                                if (tname != ret_name2) continue;
                                if (!ASR::is_a<
                                        ASR::StructInstanceMember_t>(
                                            *ba->m_value))
                                    continue;
                                ASR::StructInstanceMember_t *sim =
                                    ASR::down_cast<
                                        ASR::StructInstanceMember_t>(
                                            ba->m_value);
                                if (!ASR::is_a<ASR::Var_t>(
                                        *sim->m_v))
                                    continue;
                                ASR::symbol_t *param_sym2 =
                                    ASR::down_cast<ASR::Var_t>(
                                        sim->m_v)->m_v;
                                int pidx = -1;
                                for (size_t pi = 0;
                                        pi < fn2->n_args; pi++) {
                                    if (ASR::is_a<ASR::Var_t>(
                                            *fn2->m_args[pi]) &&
                                        ASR::down_cast<ASR::Var_t>(
                                            fn2->m_args[pi])
                                            ->m_v == param_sym2) {
                                        pidx = (int)pi;
                                        break;
                                    }
                                }
                                if (pidx < 0 ||
                                    (size_t)pidx >= fc2->n_args ||
                                    !fc2->m_args[pidx].m_value)
                                    break;
                                ASR::expr_t *actual =
                                    fc2->m_args[pidx].m_value;
                                // Create ExternalSymbol for the
                                // struct member in the caller scope
                                ASR::symbol_t *orig_mem =
                                    ASRUtils::
                                        symbol_get_past_external(
                                            sim->m_m);
                                std::string mem_name =
                                    ASRUtils::symbol_name(orig_mem);
                                SymbolTable *mem_st =
                                    ASRUtils::
                                        symbol_parent_symtab(
                                            orig_mem);
                                ASR::symbol_t *struct_sym2 =
                                    ASR::down_cast<ASR::symbol_t>(
                                        mem_st->asr_owner);
                                std::string sname =
                                    ASRUtils::symbol_name(
                                        struct_sym2);
                                std::string ext_name =
                                    var_scope->get_unique_name(
                                        "1_" + sname + "_"
                                        + mem_name);
                                ASR::symbol_t *ext_sym =
                                    ASR::down_cast<ASR::symbol_t>(
                                        ASR::make_ExternalSymbol_t(
                                            al, loc, var_scope,
                                            s2c(al, ext_name),
                                            orig_mem,
                                            s2c(al, sname),
                                            nullptr, 0,
                                            s2c(al, mem_name),
                                            ASR::accessType::
                                                Public));
                                var_scope->add_symbol(
                                    ext_name, ext_sym);
                                arr_arg = ASRUtils::EXPR(
                                    ASR::make_StructInstanceMember_t(
                                        al, loc, actual, ext_sym,
                                        sim->m_type, nullptr));
                                arr_type =
                                    ASRUtils::
                                        type_get_past_allocatable_pointer(
                                            ASRUtils::expr_type(
                                                arr_arg));
                                dims = nullptr;
                                rank =
                                    ASRUtils::
                                        extract_dimensions_from_ttype(
                                            arr_type, dims);
                                break;
                            }
                        }
                    }
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

void GpuOffloadVisitor::inline_intrinsic_sum(ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    inline_sum_in_stmts(back.body, back.n_body, current_scope);
}

// Build the `k`-th element (k is 1-based within the dot_product) of a
// rank-1 dot_product argument. Returns nullptr when the argument's
// shape cannot be indexed directly.
ASR::expr_t* GpuOffloadVisitor::dot_product_operand_element(ASR::expr_t *arg,
        ASR::expr_t *k, ASR::ttype_t *elem_type, const Location &loc) {
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
        arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    auto mk_int = [&](int64_t v) -> ASR::expr_t* {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, v,
            int_type, ASR::integerbozType::Decimal));
    };
    auto binop = [&](ASR::expr_t *l, ASR::binopType op,
            ASR::expr_t *r) -> ASR::expr_t* {
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l, op, r,
            int_type, nullptr));
    };
    // k - 1
    ASR::expr_t *km1 = binop(k, ASR::binopType::Sub, mk_int(1));
    if (ASR::is_a<ASR::ArraySection_t>(*arg)) {
        ASR::ArraySection_t *sec =
            ASR::down_cast<ASR::ArraySection_t>(arg);
        int range_dim = -1;
        for (size_t d = 0; d < sec->n_args; d++) {
            if (sec->m_args[d].m_left != nullptr) {
                if (range_dim >= 0) return nullptr;
                range_dim = (int)d;
            }
        }
        if (range_dim < 0) return nullptr;
        Vec<ASR::array_index_t> idx;
        idx.reserve(al, sec->n_args);
        for (size_t d = 0; d < sec->n_args; d++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_step = nullptr;
            if ((int)d == range_dim) {
                ASR::expr_t *delta = km1;
                if (sec->m_args[d].m_step != nullptr) {
                    delta = binop(km1, ASR::binopType::Mul,
                        sec->m_args[d].m_step);
                }
                ai.m_right = binop(sec->m_args[d].m_left,
                    ASR::binopType::Add, delta);
            } else {
                ai.m_right = sec->m_args[d].m_right;
            }
            idx.push_back(al, ai);
        }
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, sec->m_v,
            idx.p, idx.n, elem_type, ASR::arraystorageType::ColMajor,
            nullptr));
    }
    ASR::ttype_t *arr_type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(arg));
    ASR::dimension_t *dims = nullptr;
    int rank = ASRUtils::extract_dimensions_from_ttype(arr_type, dims);
    if (rank != 1) return nullptr;
    ASR::expr_t *lbound = dims[0].m_start;
    ASR::expr_t *index = nullptr;
    if (lbound == nullptr) {
        lbound = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arg,
            mk_int(1), int_type, ASR::arrayboundType::LBound, nullptr));
        index = binop(lbound, ASR::binopType::Add, km1);
    } else if (ASR::is_a<ASR::IntegerConstant_t>(*lbound) &&
            ASR::down_cast<ASR::IntegerConstant_t>(lbound)->m_n == 1) {
        index = k;
    } else {
        index = binop(lbound, ASR::binopType::Add, km1);
    }
    Vec<ASR::array_index_t> idx;
    idx.reserve(al, 1);
    ASR::array_index_t ai;
    ai.loc = loc;
    ai.m_left = nullptr;
    ai.m_step = nullptr;
    ai.m_right = index;
    idx.push_back(al, ai);
    return index_array_expr(arg, idx.p, idx.n, elem_type, loc);
}

// Number of elements of a rank-1 dot_product argument. When
// `allow_bound` is false, only shapes whose extent is available from
// the type (or from an explicit section range) are accepted, so that
// an ArrayBound on an allocatable is used only as a last resort.
ASR::expr_t* GpuOffloadVisitor::dot_product_extent(
        ASR::expr_t *arg, const Location &loc,
        bool allow_bound) {
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
        arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    auto mk_int = [&](int64_t v) -> ASR::expr_t* {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, v,
            int_type, ASR::integerbozType::Decimal));
    };
    auto binop = [&](ASR::expr_t *l, ASR::binopType op,
            ASR::expr_t *r) -> ASR::expr_t* {
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l, op, r,
            int_type, nullptr));
    };
    if (ASR::is_a<ASR::ArraySection_t>(*arg)) {
        ASR::ArraySection_t *sec =
            ASR::down_cast<ASR::ArraySection_t>(arg);
        int range_dim = -1;
        for (size_t d = 0; d < sec->n_args; d++) {
            if (sec->m_args[d].m_left != nullptr) {
                if (range_dim >= 0) return nullptr;
                range_dim = (int)d;
            }
        }
        if (range_dim < 0 || sec->m_args[range_dim].m_right == nullptr) {
            return nullptr;
        }
        ASR::expr_t *span = binop(sec->m_args[range_dim].m_right,
            ASR::binopType::Sub, sec->m_args[range_dim].m_left);
        if (sec->m_args[range_dim].m_step != nullptr) {
            span = binop(span, ASR::binopType::Div,
                sec->m_args[range_dim].m_step);
        }
        return binop(span, ASR::binopType::Add, mk_int(1));
    }
    ASR::ttype_t *arr_type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(arg));
    ASR::dimension_t *dims = nullptr;
    int rank = ASRUtils::extract_dimensions_from_ttype(arr_type, dims);
    if (rank != 1) return nullptr;
    if (dims[0].m_length != nullptr) {
        return dims[0].m_length;
    }
    if (!allow_bound) return nullptr;
    ASR::expr_t *ub = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arg,
        mk_int(1), int_type, ASR::arrayboundType::UBound, nullptr));
    ASR::expr_t *lb = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arg,
        mk_int(1), int_type, ASR::arrayboundType::LBound, nullptr));
    return binop(binop(ub, ASR::binopType::Sub, lb),
        ASR::binopType::Add, mk_int(1));
}

// Inline IntrinsicArrayFunction DotProduct inside a parallel loop
// body. Replaces:
//   r(i) = dot_product(a, b)
// With:
//   __gpu_dot_res = 0
//   do __gpu_dot_k = 1, n
//     __gpu_dot_res = __gpu_dot_res + a(...) * b(...)
//   end do
//   r(i) = __gpu_dot_res
// Unlike matmul, dot_product survives array lowering as a call to the
// generated helper `_lcompilers_dot_product_*`, whose definition is
// never emitted into the Metal shader. Expanding it here keeps the
// kernel self-contained.
void GpuOffloadVisitor::inline_dot_product_in_stmts(
        ASR::stmt_t** &stmts, size_t &n_stmts,
                                 SymbolTable *scope) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_stmts * 4);
    bool changed = false;

    for (size_t si = 0; si < n_stmts; si++) {
        ASR::stmt_t *stmt = stmts[si];

        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            inline_dot_product_in_stmts(dl->m_body, dl->n_body, scope);
            new_body.push_back(al, stmt);
            continue;
        }
        // A parallel loop nested in the loop being offloaded runs
        // serially inside the kernel, so its body is device code too
        // and its dot products have to be expanded as well.
        if (ASR::is_a<ASR::OMPRegion_t>(*stmt)) {
            ASR::OMPRegion_t *inner =
                ASR::down_cast<ASR::OMPRegion_t>(stmt);
            inline_dot_product_in_stmts(inner->m_body, inner->n_body,
                scope);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(stmt);
            if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
                inline_dot_product_in_stmts(block->m_body, block->n_body,
                    block->m_symtab);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_dot_product_in_stmts(ab->m_body, ab->n_body,
                ab->m_symtab);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::If_t>(*stmt)) {
            ASR::If_t *if_stmt = ASR::down_cast<ASR::If_t>(stmt);
            inline_dot_product_in_stmts(if_stmt->m_body,
                if_stmt->n_body, scope);
            inline_dot_product_in_stmts(if_stmt->m_orelse,
                if_stmt->n_orelse, scope);
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
                    != ASRUtils::IntrinsicArrayFunctions::DotProduct) {
            new_body.push_back(al, stmt);
            continue;
        }
        if (iaf->n_args < 2) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        ASR::ttype_t *elem_type = iaf->m_type;
        // complex dot_product conjugates its first argument and the
        // logical form is a masked any(); neither is handled here.
        if (!ASR::is_a<ASR::Real_t>(*elem_type) &&
                !ASR::is_a<ASR::Integer_t>(*elem_type)) {
            new_body.push_back(al, stmt);
            continue;
        }

        ASR::expr_t *n_elems = dot_product_extent(iaf->m_args[0], loc,
            false);
        if (n_elems == nullptr) {
            n_elems = dot_product_extent(iaf->m_args[1], loc, false);
        }
        if (n_elems == nullptr) {
            n_elems = dot_product_extent(iaf->m_args[0], loc, true);
        }
        if (n_elems == nullptr) {
            new_body.push_back(al, stmt);
            continue;
        }

        SymbolTable *var_scope = scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        auto make_var = [&](const std::string &prefix,
                ASR::ttype_t *type) -> ASR::expr_t* {
            std::string name = var_scope->get_unique_name(prefix);
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, type));
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        };

        ASR::expr_t *k_var = make_var("__gpu_dot_k", int_type);
        ASR::expr_t *lhs_elem = dot_product_operand_element(
            iaf->m_args[0], k_var, elem_type, loc);
        ASR::expr_t *rhs_elem = dot_product_operand_element(
            iaf->m_args[1], k_var, elem_type, loc);
        if (lhs_elem == nullptr || rhs_elem == nullptr) {
            new_body.push_back(al, stmt);
            continue;
        }

        ASR::expr_t *res_var = make_var("__gpu_dot_res", elem_type);
        ASR::expr_t *zero;
        if (ASR::is_a<ASR::Real_t>(*elem_type)) {
            zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                0.0, elem_type));
        } else {
            zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                0, elem_type, ASR::integerbozType::Decimal));
        }
        new_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var, zero,
                nullptr, false, false)));

        ASR::expr_t *prod, *acc;
        if (ASR::is_a<ASR::Real_t>(*elem_type)) {
            prod = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                lhs_elem, ASR::binopType::Mul, rhs_elem, elem_type,
                nullptr));
            acc = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                res_var, ASR::binopType::Add, prod, elem_type, nullptr));
        } else {
            prod = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                lhs_elem, ASR::binopType::Mul, rhs_elem, elem_type,
                nullptr));
            acc = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                res_var, ASR::binopType::Add, prod, elem_type, nullptr));
        }
        Vec<ASR::stmt_t*> loop_body;
        loop_body.reserve(al, 1);
        loop_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var, acc,
                nullptr, false, false)));

        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = k_var;
        head.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            1, int_type, ASR::integerbozType::Decimal));
        head.m_end = n_elems;
        head.m_increment = nullptr;
        new_body.push_back(al, ASRUtils::STMT(ASR::make_DoLoop_t(al, loc,
            nullptr, head, loop_body.p, loop_body.n, nullptr, 0)));

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

void GpuOffloadVisitor::inline_intrinsic_dot_product(ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    inline_dot_product_in_stmts(back.body, back.n_body, current_scope);
}

// Inline IntrinsicArrayFunction Transpose inside a parallel loop body.
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
void GpuOffloadVisitor::inline_intrinsic_transpose(ParallelLoopNest &nest) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, nest.n_body * 4);
    bool changed = false;

    for (size_t si = 0; si < nest.n_body; si++) {
        ASR::stmt_t *stmt = nest.body[si];
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
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, int_type));
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
        ASR::do_loop_head_t inner_head;
        inner_head.loc = loc;
        inner_head.m_v = var_i;
        set_loop_head_bounds(al, loc, inner_head, dims, 1, arr_arg);
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
        set_loop_head_bounds(al, loc, outer_head, dims, 0, arr_arg);
        outer_head.m_increment = nullptr;
        ASR::stmt_t *outer_loop = ASRUtils::STMT(
            ASR::make_DoLoop_t(al, loc, nullptr, outer_head,
                outer_body.p, outer_body.n, nullptr, 0));

        new_body.push_back(al, outer_loop);
        changed = true;
    }

    if (changed) {
        // Through set_body, so the loop the nest reads from carries
        // the rewrite too and not just the view of it.
        nest.set_body(new_body.p, new_body.n);
    }
}

// Is `e` the integer literal `value`?
bool GpuOffloadVisitor::is_int_literal(ASR::expr_t *e, int64_t value) {
    if (!e) return false;
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (!v) v = e;
    if (!ASR::is_a<ASR::IntegerConstant_t>(*v)) return false;
    return ASR::down_cast<ASR::IntegerConstant_t>(v)->m_n == value;
}

ASR::expr_t *GpuOffloadVisitor::int32_const(const Location &loc, int64_t n) {
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, n,
        int_type, ASR::integerbozType::Decimal));
}

// The loop counters generated below are integer(4); a section bound
// of another integer kind has to be converted before it can be
// combined with them.
ASR::expr_t *GpuOffloadVisitor::to_int32(const Location &loc, ASR::expr_t *e) {
    ASR::ttype_t *t = ASRUtils::extract_type(ASRUtils::expr_type(e));
    if (ASR::is_a<ASR::Integer_t>(*t)
            && ASR::down_cast<ASR::Integer_t>(t)->m_kind == 4) {
        return e;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    return ASRUtils::EXPR(ASR::make_Cast_t(al, loc, e,
        ASR::cast_kindType::IntegerToInteger, int_type, nullptr,
        nullptr));
}

// Number of elements of a section dimension `lo:hi:step`, which is
// (hi - lo)/step + 1. Truncating integer division gives the right
// answer for a negative step too, since numerator and denominator
// then have the same sign.
ASR::expr_t *GpuOffloadVisitor::section_extent(const Location &loc,
        const ASR::array_index_t &d) {
    if (is_int_literal(d.m_left, 1) && is_int_literal(d.m_step, 1)) {
        return d.m_right;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::expr_t *span = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
        loc, to_int32(loc, d.m_right), ASR::binopType::Sub,
        to_int32(loc, d.m_left), int_type, nullptr));
    if (!is_int_literal(d.m_step, 1)) {
        span = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, span,
            ASR::binopType::Div, to_int32(loc, d.m_step), int_type,
            nullptr));
    }
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, span,
        ASR::binopType::Add, int32_const(loc, 1), int_type, nullptr));
}

// Array index of the `counter`-th element (counter = 1..extent) of a
// section dimension `lo:hi:step`, which is lo + (counter - 1)*step.
ASR::expr_t *GpuOffloadVisitor::section_index(const Location &loc,
        const ASR::array_index_t &d, ASR::expr_t *counter) {
    if (is_int_literal(d.m_left, 1) && is_int_literal(d.m_step, 1)) {
        return counter;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::expr_t *offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
        loc, counter, ASR::binopType::Sub, int32_const(loc, 1),
        int_type, nullptr));
    if (!is_int_literal(d.m_step, 1)) {
        offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            offset, ASR::binopType::Mul, to_int32(loc, d.m_step),
            int_type, nullptr));
    }
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
        to_int32(loc, d.m_left), ASR::binopType::Add, offset,
        int_type, nullptr));
}

// Inline ArraySection assignments inside a parallel loop body.
// Replaces:
//   b(1:n(l), l) = 1.0   (ArraySection = ArrayBroadcast)
// With:
//   do __gpu_sec_i = 1, n(l)
//     b(__gpu_sec_i, l) = 1.0
//   end do
// This avoids complex lowered code (descriptor temps, ArrayBound)
// that the Metal backend cannot handle inside GPU kernels.
// Evaluate an integer expression made up entirely of literals.
// `section_extent` builds its result unfolded (`(6 - 3) + 1`), and an
// unfolded extent would make the temporary below a descriptor array
// even though its size is known, so fold it here.
bool GpuOffloadVisitor::eval_int_literal(ASR::expr_t *e, int64_t &out) {
    if (!e) return false;
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (v) e = v;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
        out = ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
        return true;
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
        int64_t a;
        if (!eval_int_literal(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, a)) {
            return false;
        }
        out = -a;
        return true;
    }
    if (ASR::is_a<ASR::Cast_t>(*e)) {
        return eval_int_literal(
            ASR::down_cast<ASR::Cast_t>(e)->m_arg, out);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        int64_t l, r;
        if (!eval_int_literal(b->m_left, l)
                || !eval_int_literal(b->m_right, r)) {
            return false;
        }
        switch (b->m_op) {
            case ASR::binopType::Add: out = l + r; return true;
            case ASR::binopType::Sub: out = l - r; return true;
            case ASR::binopType::Mul: out = l * r; return true;
            case ASR::binopType::Div: {
                if (r == 0) return false;
                out = l / r;
                return true;
            }
            default: return false;
        }
    }
    return false;
}

// Declare a temporary array with `n_extents` dimensions of the given
// extents in `var_scope` and return a reference to it. A dimension
// whose extent is not a compile-time constant makes the temporary a
// descriptor array, exactly as the array-constructor hoisting above
// does, so the same run-time sizing machinery applies.
ASR::expr_t *GpuOffloadVisitor::declare_temp_array(const Location &loc,
        SymbolTable *var_scope, ASR::ttype_t *elem_type,
        ASR::expr_t **extents, size_t n_extents,
        const std::string &prefix) {
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, n_extents);
    bool all_const = true;
    for (size_t i = 0; i < n_extents; i++) {
        ASR::dimension_t d;
        d.loc = loc;
        d.m_start = int32_const(loc, 1);
        d.m_length = extents[i];
        int64_t n;
        if (extents[i] && eval_int_literal(extents[i], n)) {
            d.m_length = int32_const(loc, (int)n);
        } else {
            all_const = false;
        }
        dims.push_back(al, d);
    }
    ASR::ttype_t *tmp_type = ASRUtils::TYPE(
        ASR::make_Array_t(al, loc, elem_type, dims.p, dims.n,
            all_const
                ? ASR::array_physical_typeType::FixedSizeArray
                : ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
    std::string name = var_scope->get_unique_name(prefix);
    ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
        tmp_type);
    return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
}

// The target of `asgn` when its value reads the target's storage
// through a designator that is not element-for-element identical to
// it, and nullptr otherwise. See the comment above
// gpu_designator_base for why such an assignment needs a temporary.
ASR::expr_t *GpuOffloadVisitor::self_aliasing_target(ASR::Assignment_t *asgn) {
    ASR::expr_t *target = asgn->m_target;
    while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*target)) {
        target = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
            target)->m_arg;
    }
    if (!ASRUtils::is_array(ASRUtils::expr_type(target))) {
        return nullptr;
    }
    GpuDesignatorBase base = gpu_designator_base(target);
    if (!base.is_known()) return nullptr;
    GpuSelfAliasChecker checker;
    checker.base = base;
    checker.target = target;
    checker.visit_expr(*asgn->m_value);
    return checker.aliased ? target : nullptr;
}

// Whether the temporary such an assignment needs can be given
// compile-time constant extents. Metal has no variable-length
// arrays, and a run-time sized kernel temporary would have to become
// a device buffer shared by every thread of the kernel, so a loop
// that would need one is not offloaded at all and runs on the host.
bool GpuOffloadVisitor::alias_temp_is_fixed_size(ASR::expr_t *target) {
    const Location &loc = target->base.loc;
    int64_t n;
    if (ASR::is_a<ASR::ArraySection_t>(*target)) {
        ASR::ArraySection_t *as =
            ASR::down_cast<ASR::ArraySection_t>(target);
        size_t n_ranges = 0;
        for (size_t i = 0; i < as->n_args; i++) {
            if (as->m_args[i].m_left && as->m_args[i].m_right
                    && as->m_args[i].m_step) {
                n_ranges++;
                if (!eval_int_literal(
                        section_extent(loc, as->m_args[i]), n)) {
                    return false;
                }
            }
        }
        return n_ranges > 0;
    }
    ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(ASRUtils::expr_type(target)));
    if (!ASR::is_a<ASR::Array_t>(*tt)) return false;
    ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
    for (size_t i = 0; i < at->n_dims; i++) {
        if (!eval_int_literal(at->m_dims[i].m_length, n)) return false;
    }
    return at->n_dims > 0;
}

// The extents of the temporary an aliased assignment to `target`
// needs, one per dimension, in order. False when the shape is not
// written anywhere the temporary could be sized from, so nothing
// could give the temporary the right extent.
bool GpuOffloadVisitor::alias_temp_extents(ASR::expr_t *target,
        Vec<ASR::expr_t*> &extents) {
    const Location &loc = target->base.loc;
    if (ASR::is_a<ASR::ArraySection_t>(*target)) {
        ASR::ArraySection_t *as =
            ASR::down_cast<ASR::ArraySection_t>(target);
        extents.reserve(al, as->n_args);
        for (size_t i = 0; i < as->n_args; i++) {
            if (as->m_args[i].m_left && as->m_args[i].m_right
                    && as->m_args[i].m_step) {
                extents.push_back(al,
                    section_extent(loc, as->m_args[i]));
            }
        }
        return extents.n > 0;
    }
    ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(ASRUtils::expr_type(target)));
    if (!ASR::is_a<ASR::Array_t>(*tt)) return false;
    ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
    if (at->n_dims == 0) return false;
    extents.reserve(al, at->n_dims);
    for (size_t i = 0; i < at->n_dims; i++) {
        if (!at->m_dims[i].m_length) return false;
        extents.push_back(al, at->m_dims[i].m_length);
    }
    return true;
}

// Reports a self-aliasing array assignment whose temporary this pass
// cannot give a per-thread home. Called before any of the destructive
// inline_* helpers, so the loop can still be left on the host.
//
// A fixed-size temporary is a kernel-scope stack array, private to
// the thread by construction. A run-time sized one has to be a
// BLOCK local instead, so that the workspace machinery binds it to a
// per-thread slice of a device buffer -- and only a BLOCK at the top
// level of the loop body is scanned for those, so only a top-level
// assignment can have one. Whether the host can then evaluate the
// extents is settled by the workspace pre-flight further down.
// Whether the backend could size the temporary an aliased assignment
// needs. It is a run-time sized BLOCK local, so it becomes a workspace,
// and the backend describes one with declared_shape_to_vla_workspace.
// An extent that function cannot resolve is a code-generation error
// later, so it is a decline here.
bool GpuOffloadVisitor::alias_temp_extents_resolvable(ASR::expr_t *target,
        ASR::expr_t **extents, size_t n_extents,
        const std::vector<std::string> &arg_names) {
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, n_extents);
    for (size_t i = 0; i < n_extents; i++) {
        ASR::dimension_t d;
        d.loc = extents[i]->base.loc;
        d.m_start = int32_const(extents[i]->base.loc, 1);
        d.m_length = extents[i];
        dims.push_back(al, d);
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(target));
    ASR::ttype_t *arr_type = ASRUtils::TYPE(ASR::make_Array_t(al,
        target->base.loc, elem_type, dims.p, dims.n,
        ASR::array_physical_typeType::FixedSizeArray,
        ASR::memory_spaceType::Global));
    GpuVlaWorkspace ws;
    return declared_shape_to_vla_workspace(
        ASR::down_cast<ASR::Array_t>(arr_type), "__gpu_alias",
        GpuExtentScope{nullptr, arg_names, nullptr, nullptr, 0}, ws);
}

bool GpuOffloadVisitor::body_needs_unsupported_alias_temp(ASR::stmt_t **body,
        size_t n_body, bool top_level,
        const std::vector<std::string> &arg_names) {
    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            if (body_needs_unsupported_alias_temp(dl->m_body,
                    dl->n_body, false, arg_names)) return true;
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                if (body_needs_unsupported_alias_temp(blk->m_body,
                        blk->n_body, false, arg_names)) return true;
            }
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        stmt)->m_m);
            if (body_needs_unsupported_alias_temp(ab->m_body,
                    ab->n_body, false, arg_names)) return true;
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) continue;
        ASR::expr_t *target = self_aliasing_target(
            ASR::down_cast<ASR::Assignment_t>(stmt));
        if (!target) continue;
        if (alias_temp_is_fixed_size(target)) continue;
        Vec<ASR::expr_t*> extents;
        if (top_level && alias_temp_extents(target, extents)
                && alias_temp_extents_resolvable(target, extents.p,
                    extents.n, arg_names)) {
            continue;
        }
        return true;
    }
    return false;
}

// Give a materialised ASSOCIATE array temporary the shape of its
// selector, in its own type.
//
// `associate(r => sqrt((x-x0)**2 + (y(j)-y0)**2))` over an
// assumed-shape `x` becomes an allocatable local of the ASSOCIATE's
// symbol table with deferred extents and no ALLOCATE; the shape lives
// only in the expression assigned to it. In a kernel that local
// becomes a per-thread workspace, which has to be sized -- and the
// rewrites further down lower the whole-array assignment into an
// element loop bounded by `ubound(r)`, after which the shape is gone.
// So write it into the type here, while the assignment it can be read
// from is still whole-array. Every replaced dimension list is
// recorded in `undo` so the loop can still be left untouched if a
// later check declines the offload.
void GpuOffloadVisitor::size_scope_array_temporaries(
        ASR::stmt_t **body, size_t n_body,
        std::vector<ScopeArrayDims> &undo) {
    for (size_t si = 0; si < n_body; si++) {
        SymbolTable *symtab = nullptr;
        ASR::stmt_t **inner_body = nullptr;
        size_t inner_n_body = 0;
        if (ASR::is_a<ASR::BlockCall_t>(*body[si])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(body[si])->m_m);
            if (!b || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            symtab = blk->m_symtab;
            inner_body = blk->m_body;
            inner_n_body = blk->n_body;
        } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*body[si])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(
                    body[si])->m_m);
            if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) continue;
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            symtab = ab->m_symtab;
            inner_body = ab->m_body;
            inner_n_body = ab->n_body;
        } else if (ASR::is_a<ASR::DoLoop_t>(*body[si])) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(body[si]);
            size_scope_array_temporaries(dl->m_body, dl->n_body, undo);
            continue;
        } else {
            continue;
        }
        for (auto &item : symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var =
                ASR::down_cast<ASR::Variable_t>(item.second);
            ASR::expr_t *shape_src = gpu_scope_array_shape_source(
                var, inner_body, inner_n_body);
            if (!shape_src) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_allocatable(var->m_type));
            const Location &vloc = var->base.base.loc;
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, arr->n_dims);
            for (size_t d = 0; d < arr->n_dims; d++) {
                ASR::dimension_t dim;
                dim.loc = vloc;
                dim.m_start = int32_const(vloc, 1);
                dim.m_length = ASRUtils::get_size(shape_src,
                    (int)d + 1, al);
                dims.push_back(al, dim);
            }
            // An allocatable must keep deferred extents, so the
            // temporary becomes an automatic array of the same
            // shape -- which is what the workspace machinery binds.
            undo.push_back({var, var->m_type});
            var->m_type = ASRUtils::TYPE(ASR::make_Array_t(al, vloc,
                arr->m_type, dims.p, dims.n,
                ASR::array_physical_typeType::DescriptorArray,
                ASR::memory_spaceType::Global));
        }
        size_scope_array_temporaries(inner_body, inner_n_body, undo);
    }
}

// Give the temporary of a run-time sized aliased assignment a BLOCK
// of its own, at the top level of the loop body.
//
//   a(:,c) = a(n:1:-1,c)
// becomes
//   block
//     real :: __gpu_alias(n)
//     __gpu_alias(1:n) = a(n:1:-1,c)
//     a(:,c)           = __gpu_alias(1:n)
//   end block
//
// The BLOCK is what makes the temporary safe: a run-time sized
// kernel-scope local becomes one device buffer shared by every
// thread, and every thread would write it -- a race. A BLOCK local is
// bound to a per-thread slice of a workspace buffer instead
// (analyze_gpu_vla_workspaces), which is private to the iteration.
// Only a top-level BLOCK of the kernel body is scanned for
// workspaces, which is why only a top-level assignment is rewritten
// here; body_needs_unsupported_alias_temp has already declined the
// rest.
void GpuOffloadVisitor::materialize_runtime_alias_blocks(
        ParallelLoopNest &nest) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, nest.n_body);
    bool changed = false;
    for (size_t si = 0; si < nest.n_body; si++) {
        ASR::stmt_t *stmt = nest.body[si];
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn =
            ASR::down_cast<ASR::Assignment_t>(stmt);
        ASR::expr_t *target = self_aliasing_target(asgn);
        if (!target || alias_temp_is_fixed_size(target)) {
            new_body.push_back(al, stmt);
            continue;
        }
        Vec<ASR::expr_t*> extents;
        if (!alias_temp_extents(target, extents)) {
            new_body.push_back(al, stmt);
            continue;
        }
        Location loc = stmt->base.loc;
        SymbolTable *block_scope =
            al.make_new<SymbolTable>(current_scope);
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(target));
        ASR::expr_t *tmp = declare_temp_array(loc, block_scope,
            elem_type, extents.p, extents.n, "__gpu_alias");
        bool sectioned = ASR::is_a<ASR::ArraySection_t>(*target);
        // A full 1:extent:1 section over the temporary, built once
        // per use so the two statements do not share nodes.
        auto tmp_ref = [&]() -> ASR::expr_t* {
            if (!sectioned) return tmp;
            Vec<ASR::array_index_t> args;
            args.reserve(al, extents.n);
            for (size_t i = 0; i < extents.n; i++) {
                ASR::array_index_t idx;
                idx.loc = loc;
                idx.m_left = int32_const(loc, 1);
                idx.m_right = extents[i];
                idx.m_step = int32_const(loc, 1);
                args.push_back(al, idx);
            }
            return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
                tmp, args.p, args.n, ASRUtils::expr_type(tmp),
                nullptr));
        };
        Vec<ASR::stmt_t*> block_body;
        block_body.reserve(al, 2);
        block_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, tmp_ref(),
                asgn->m_value, nullptr, false, false)));
        asgn->m_value = tmp_ref();
        block_body.push_back(al, stmt);
        std::string block_name = current_scope->get_unique_name(
            "__gpu_alias_scope");
        ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
            s2c(al, block_name), block_body.p, block_body.n);
        block_scope->asr_owner = block;
        ASR::symbol_t *block_sym =
            ASR::down_cast<ASR::symbol_t>(block);
        current_scope->add_symbol(block_name, block_sym);
        kernel_blocks.push_back(block_sym);
        new_body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(
            al, loc, -1, block_sym)));
        changed = true;
    }
    if (changed) {
        // Through set_body, so the loop the nest reads from carries
        // the rewrite too and not just the view of it.
        nest.set_body(new_body.p, new_body.n);
    }
}

// Materialise a temporary for an array assignment whose target and
// value designate overlapping storage of the same array. See the
// comment above gpu_designator_base: without it the element loops
// built below read elements the same statement has already written.
//   a(:) = a(n:1:-1)
// becomes
//   __gpu_alias(1:n) = a(n:1:-1)
//   a(:)            = __gpu_alias(1:n)
// Both halves are alias-free, and the existing ArraySection and
// whole-array lowerings turn each of them into an element loop.
void GpuOffloadVisitor::materialize_aliased_assignments(
        ParallelLoopNest &nest) {
    bool changed = false;
    NestBodyWriteBack back(nest);
    materialize_aliased_in_body(back.body, back.n_body, changed);
}

void GpuOffloadVisitor::materialize_aliased_in_body(
        ASR::stmt_t** &body, size_t &n_body,
        bool &changed) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 2);
    bool local_changed = false;

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            materialize_aliased_in_body(dl->m_body, dl->n_body, changed);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                materialize_aliased_in_body(blk->m_body, blk->n_body,
                    changed);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        stmt)->m_m);
            materialize_aliased_in_body(ab->m_body, ab->n_body,
                changed);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
        ASR::expr_t *target = self_aliasing_target(asgn);
        // A loop holding an assignment that needs a temporary this
        // pass cannot size was already declined for offload, so the
        // second test only guards the statements spliced in since.
        if (!target || !alias_temp_is_fixed_size(target)) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(target));

        if (ASR::is_a<ASR::ArraySection_t>(*target)) {
            ASR::ArraySection_t *as =
                ASR::down_cast<ASR::ArraySection_t>(target);
            Vec<ASR::expr_t*> extents;
            extents.reserve(al, as->n_args);
            for (size_t i = 0; i < as->n_args; i++) {
                if (as->m_args[i].m_left && as->m_args[i].m_right
                        && as->m_args[i].m_step) {
                    extents.push_back(al,
                        section_extent(loc, as->m_args[i]));
                }
            }
            if (extents.n == 0) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::expr_t *tmp = declare_temp_array(loc, var_scope,
                elem_type, extents.p, extents.n, "__gpu_alias");
            // A full 1:extent:1 section over the temporary, built
            // twice so the two statements do not share nodes.
            auto tmp_section = [&]() -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, extents.n);
                for (size_t i = 0; i < extents.n; i++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = int32_const(loc, 1);
                    idx.m_right = extents[i];
                    idx.m_step = int32_const(loc, 1);
                    args.push_back(al, idx);
                }
                return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
                    tmp, args.p, args.n,
                    ASRUtils::expr_type(tmp), nullptr));
            };
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, tmp_section(),
                    asgn->m_value, nullptr, false, false)));
            asgn->m_value = tmp_section();
        } else {
            Vec<ASR::expr_t*> extents;
            ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
                ASRUtils::type_get_past_pointer(
                    ASRUtils::expr_type(target)));
            ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
            extents.reserve(al, at->n_dims);
            for (size_t i = 0; i < at->n_dims; i++) {
                extents.push_back(al, at->m_dims[i].m_length);
            }
            ASR::expr_t *tmp = declare_temp_array(loc, var_scope,
                elem_type, extents.p, extents.n, "__gpu_alias");
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, tmp, asgn->m_value,
                    nullptr, false, false)));
            asgn->m_value = tmp;
        }
        new_body.push_back(al, stmt);
        local_changed = true;
    }

    if (local_changed) {
        body = new_body.p;
        n_body = new_body.n;
        changed = true;
    }
}

void GpuOffloadVisitor::inline_array_section_assignment(
        ParallelLoopNest &nest) {
    bool changed = false;
    NestBodyWriteBack back(nest);
    inline_array_section_in_body(back.body, back.n_body, changed);
}

void GpuOffloadVisitor::inline_array_section_in_body(
        ASR::stmt_t** &body, size_t &n_body,
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
        // Recurse into AssociateBlockCall bodies
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_array_section_in_body(ab->m_body,
                ab->n_body, changed);
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
            ASR::symbol_t *loop_var_sym = gpu_new_variable(al, loc,
                var_scope, loop_var_name, ASRUtils::duplicate_type(al,
                    int_type));
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
                    idx.m_right = section_index(loc, as->m_args[i],
                        loop_vars[ri]);
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
                            idx.m_right = section_index(loc,
                                rhs_as->m_args[i], loop_vars[rv_idx]);
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
            } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                ASR::RealUnaryMinus_t *ru =
                    ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al,
                    loc, elementize_rhs(ru->m_arg), et, nullptr));
            } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                ASR::IntegerUnaryMinus_t *iu =
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al,
                    loc, elementize_rhs(iu->m_arg), et, nullptr));
            } else if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
                ASR::LogicalNot_t *ln =
                    ASR::down_cast<ASR::LogicalNot_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_LogicalNot_t(al,
                    loc, elementize_rhs(ln->m_arg), et, nullptr));
            } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                ASR::RealCompare_t *rc =
                    ASR::down_cast<ASR::RealCompare_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealCompare_t(al,
                    loc, elementize_rhs(rc->m_left), rc->m_op,
                    elementize_rhs(rc->m_right), et, nullptr));
            } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                ASR::IntegerCompare_t *ic =
                    ASR::down_cast<ASR::IntegerCompare_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al,
                    loc, elementize_rhs(ic->m_left), ic->m_op,
                    elementize_rhs(ic->m_right), et, nullptr));
            } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                ASR::LogicalCompare_t *lc =
                    ASR::down_cast<ASR::LogicalCompare_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al,
                    loc, elementize_rhs(lc->m_left), lc->m_op,
                    elementize_rhs(lc->m_right), et, nullptr));
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
            ASR::ttype_t *e_type_inner =
                ASRUtils::type_get_past_allocatable(e_type);
            if (ASR::is_a<ASR::Array_t>(*e_type_inner)) {
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
            head.m_start = int32_const(loc, 1);
            head.m_end = section_extent(loc, as->m_args[dim]);
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

bool GpuOffloadVisitor::linear_form(
        ASR::expr_t *e, LinearForm &f, int64_t scale) {
    if (!e) return false;
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (v) e = v;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
        f.constant += scale
            * ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
        return true;
    }
    if (ASR::is_a<ASR::Cast_t>(*e)) {
        return linear_form(ASR::down_cast<ASR::Cast_t>(e)->m_arg, f,
            scale);
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
        return linear_form(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, f,
            -scale);
    }
    if (ASR::is_a<ASR::Var_t>(*e)) {
        f.terms[ASR::down_cast<ASR::Var_t>(e)->m_v] += scale;
        return true;
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        int64_t c;
        switch (b->m_op) {
            case ASR::binopType::Add:
                return linear_form(b->m_left, f, scale)
                    && linear_form(b->m_right, f, scale);
            case ASR::binopType::Sub:
                return linear_form(b->m_left, f, scale)
                    && linear_form(b->m_right, f, -scale);
            case ASR::binopType::Mul:
                if (eval_int_literal(b->m_left, c)) {
                    return linear_form(b->m_right, f, scale * c);
                }
                if (eval_int_literal(b->m_right, c)) {
                    return linear_form(b->m_left, f, scale * c);
                }
                return false;
            default: return false;
        }
    }
    return false;
}

// Number of elements of the section dimension `lo:hi:step` when it is
// a compile-time constant, which it is whenever `hi - lo` and `step`
// are -- the bounds themselves need not be.
bool GpuOffloadVisitor::const_section_extent(
        const ASR::array_index_t &d, int64_t &n) {
    int64_t step;
    if (!d.m_left || !d.m_right || !d.m_step) return false;
    if (!eval_int_literal(d.m_step, step) || step == 0) return false;
    LinearForm f;
    if (!linear_form(d.m_right, f, 1)) return false;
    if (!linear_form(d.m_left, f, -1)) return false;
    for (auto &t : f.terms) {
        if (t.second != 0) return false;
    }
    n = f.constant / step + 1;
    if (n < 0) n = 0;
    return true;
}

bool GpuOffloadVisitor::section_is_strided(const ASR::ArraySection_t *as) {
    for (size_t i = 0; i < as->n_args; i++) {
        if (!as->m_args[i].m_left || !as->m_args[i].m_right
                || !as->m_args[i].m_step) {
            continue;
        }
        if (!is_int_literal(as->m_args[i].m_step, 1)) return true;
    }
    return false;
}

// A dummy the callee may write has to be copied back. An unknown
// intent is treated as writable: a wrong answer is worse than a copy.
bool GpuOffloadVisitor::dummy_is_written(
        ASR::Function_t *fn, size_t arg_index) {
    if (!fn || arg_index >= fn->n_args) return true;
    if (!ASR::is_a<ASR::Var_t>(*fn->m_args[arg_index])) return true;
    ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(
        fn->m_args[arg_index])->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*s)) return true;
    return ASR::down_cast<ASR::Variable_t>(s)->m_intent
        != ASR::intentType::In;
}

// Build `do c = 1, extent ... end do` nests copying between the
// section `as` of its base array and the contiguous temporary `tmp`.
// With `to_temp` the section is read into the temporary (gather);
// otherwise the temporary is written back into the section (scatter).
ASR::stmt_t* GpuOffloadVisitor::build_section_copy_loops(const Location &loc,
        SymbolTable *var_scope, ASR::ArraySection_t *as,
        const std::vector<int> &range_dims, ASR::expr_t *tmp,
        bool to_temp) {
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    std::vector<ASR::expr_t*> counters(range_dims.size());
    for (size_t ri = 0; ri < range_dims.size(); ri++) {
        std::string name = var_scope->get_unique_name("__gpu_gather_i");
        ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
            ASRUtils::duplicate_type(al, int_type));
        counters[ri] = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(as->m_v));

    // The section element for this iteration.
    Vec<ASR::array_index_t> src_args;
    src_args.reserve(al, as->n_args);
    size_t ri = 0;
    for (size_t d = 0; d < as->n_args; d++) {
        ASR::array_index_t idx;
        idx.loc = as->m_args[d].loc;
        bool is_range = ri < range_dims.size()
            && (int)d == range_dims[ri];
        if (is_range) {
            idx.m_left = nullptr;
            idx.m_right = section_index(loc, as->m_args[d],
                counters[ri]);
            idx.m_step = nullptr;
            ri++;
        } else {
            idx.m_left = as->m_args[d].m_left;
            idx.m_right = as->m_args[d].m_right;
            idx.m_step = as->m_args[d].m_step;
        }
        src_args.push_back(al, idx);
    }
    ASR::expr_t *src_elem = ASRUtils::EXPR(ASR::make_ArrayItem_t(al,
        loc, as->m_v, src_args.p, src_args.n, elem_type,
        ASR::arraystorageType::ColMajor, nullptr));

    // The temporary's element for the same iteration: the temporary is
    // 1-based and contiguous, so the counters index it directly.
    Vec<ASR::array_index_t> tmp_args;
    tmp_args.reserve(al, range_dims.size());
    for (size_t k = 0; k < range_dims.size(); k++) {
        ASR::array_index_t idx;
        idx.loc = loc;
        idx.m_left = nullptr;
        idx.m_right = counters[k];
        idx.m_step = nullptr;
        tmp_args.push_back(al, idx);
    }
    ASR::expr_t *tmp_elem = ASRUtils::EXPR(ASR::make_ArrayItem_t(al,
        loc, tmp, tmp_args.p, tmp_args.n, elem_type,
        ASR::arraystorageType::ColMajor, nullptr));

    ASR::stmt_t *inner = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
        to_temp ? tmp_elem : src_elem,
        to_temp ? src_elem : tmp_elem, nullptr, false, false));
    for (int k = (int)range_dims.size() - 1; k >= 0; k--) {
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = counters[k];
        head.m_start = int32_const(loc, 1);
        head.m_end = section_extent(loc, as->m_args[range_dims[k]]);
        head.m_increment = nullptr;
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, inner);
        inner = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
            head, body.p, body.n, nullptr, 0));
    }
    return inner;
}

// The strided section under any physical casts of an actual argument,
// or nullptr when the argument is not one.
ASR::ArraySection_t* GpuOffloadVisitor::strided_section_actual(
        ASR::expr_t *e) {
    while (e && ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
    }
    if (!e || !ASR::is_a<ASR::ArraySection_t>(*e)) return nullptr;
    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
    return section_is_strided(as) ? as : nullptr;
}

// Can this strided section be gathered into a contiguous temporary?
// The base has to be a designator the copy loops can index, and every
// extent has to fold to a constant, because the temporary is a
// kernel-local array.
bool GpuOffloadVisitor::strided_section_is_gatherable(
        ASR::ArraySection_t *as) {
    if (!ASR::is_a<ASR::Var_t>(*as->m_v)
            && !ASR::is_a<ASR::StructInstanceMember_t>(*as->m_v)) {
        return false;
    }
    bool any_range = false;
    for (size_t d = 0; d < as->n_args; d++) {
        if (!as->m_args[d].m_left || !as->m_args[d].m_right
                || !as->m_args[d].m_step) {
            continue;
        }
        any_range = true;
        int64_t n;
        if (!const_section_extent(as->m_args[d], n)) return false;
    }
    return any_range;
}

// Replace a strided section actual argument in `slot` with a gathered
// temporary, appending the gather to `before` and, when the dummy may
// be written, the scatter to `after`. Returns true when it did.
bool GpuOffloadVisitor::gather_strided_section_arg(const Location &loc,
        SymbolTable *block_scope, ASR::expr_t **slot, bool writable,
        std::vector<ASR::stmt_t*> &before,
        std::vector<ASR::stmt_t*> &after) {
    ASR::ArrayPhysicalCast_t *cast = nullptr;
    ASR::expr_t *inner = *slot;
    while (inner && ASR::is_a<ASR::ArrayPhysicalCast_t>(*inner)) {
        cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(inner);
        inner = cast->m_arg;
    }
    if (!inner || !ASR::is_a<ASR::ArraySection_t>(*inner)) return false;
    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(inner);
    if (!section_is_strided(as)) return false;
    if (!strided_section_is_gatherable(as)) return false;
    std::vector<int> range_dims;
    for (size_t d = 0; d < as->n_args; d++) {
        if (as->m_args[d].m_left && as->m_args[d].m_right
                && as->m_args[d].m_step) {
            range_dims.push_back((int)d);
        }
    }
    if (range_dims.empty()) return false;

    // Every extent has to fold to a constant: the gathered buffer is a
    // kernel-local array, and a device function cannot declare one
    // whose size is only known per thread.
    Vec<ASR::expr_t*> extents;
    extents.reserve(al, range_dims.size());
    for (int d : range_dims) {
        int64_t n = 0;
        const_section_extent(as->m_args[d], n);
        extents.push_back(al, int32_const(loc, (int)n));
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(as->m_v));
    ASR::expr_t *tmp = declare_temp_array(loc, block_scope, elem_type,
        extents.p, extents.n, "__gpu_gather");

    before.push_back(build_section_copy_loops(loc, block_scope, as,
        range_dims, tmp, true));
    if (writable) {
        after.push_back(build_section_copy_loops(loc, block_scope, as,
            range_dims, tmp, false));
    }
    // The temporary is contiguous, so no physical-type cast is left to
    // make: the dummy takes the array as it stands.
    *slot = tmp;
    (void)cast;
    return true;
}

// Rewrite every strided section actual argument of every call in
// `stmt`, collecting the gather and scatter statements that have to
// bracket it.
bool GpuOffloadVisitor::gather_strided_sections_in_stmt(ASR::stmt_t *stmt,
        SymbolTable *block_scope, std::vector<ASR::stmt_t*> &before,
        std::vector<ASR::stmt_t*> &after) {
    bool changed = false;
    const Location &loc = stmt->base.loc;
    auto do_call = [&](ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        ASR::symbol_t *resolved =
            ASRUtils::symbol_get_past_external(name);
        ASR::Function_t *fn =
            (resolved && ASR::is_a<ASR::Function_t>(*resolved))
                ? ASR::down_cast<ASR::Function_t>(resolved) : nullptr;
        for (size_t i = 0; i < n_args; i++) {
            if (!args[i].m_value) continue;
            if (gather_strided_section_arg(loc, block_scope,
                    &args[i].m_value, dummy_is_written(fn, i),
                    before, after)) {
                changed = true;
            }
        }
    };
    if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
        ASR::SubroutineCall_t *sc =
            ASR::down_cast<ASR::SubroutineCall_t>(stmt);
        do_call(sc->m_name, sc->m_args, sc->n_args);
    }
    GpuCallSiteCollector csc;
    csc.visit_stmt(*stmt);
    for (const ASR::FunctionCall_t *c : csc.calls) {
        ASR::FunctionCall_t *fc = const_cast<ASR::FunctionCall_t*>(c);
        do_call(fc->m_name, fc->m_args, fc->n_args);
    }
    return changed;
}

// True when some call in `body` takes a strided section this pass
// cannot gather. Passing it on would drop the stride silently, so the
// loop is declined for offload instead, while the body is untouched.
bool GpuOffloadVisitor::body_has_ungatherable_strided_section(
        ASR::stmt_t **body,
        size_t n_body) {
    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            if (body_has_ungatherable_strided_section(dl->m_body,
                    dl->n_body)) return true;
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                if (body_has_ungatherable_strided_section(blk->m_body,
                        blk->n_body)) return true;
            }
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                if (body_has_ungatherable_strided_section(ab->m_body,
                        ab->n_body)) return true;
            }
            continue;
        }
        std::vector<ASR::call_arg_t*> arg_lists;
        std::vector<size_t> arg_counts;
        if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
            ASR::SubroutineCall_t *sc =
                ASR::down_cast<ASR::SubroutineCall_t>(stmt);
            arg_lists.push_back(sc->m_args);
            arg_counts.push_back(sc->n_args);
        }
        GpuCallSiteCollector csc;
        csc.visit_stmt(*stmt);
        for (const ASR::FunctionCall_t *c : csc.calls) {
            arg_lists.push_back(c->m_args);
            arg_counts.push_back(c->n_args);
        }
        for (size_t li = 0; li < arg_lists.size(); li++) {
            for (size_t i = 0; i < arg_counts[li]; i++) {
                if (!arg_lists[li][i].m_value) continue;
                ASR::ArraySection_t *as = strided_section_actual(
                    arg_lists[li][i].m_value);
                if (as && !strided_section_is_gatherable(as)) {
                    return true;
                }
            }
        }
    }
    return false;
}

void GpuOffloadVisitor::gather_strided_section_arguments(
        ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    gather_strided_sections_in_body(back.body, back.n_body,
        current_scope);
}

// `scope` owns the statements in `body`: the new BLOCK has to be
// registered there, not in the procedure, or it will not resolve from
// the BlockCall that replaces the statement.
void GpuOffloadVisitor::gather_strided_sections_in_body(ASR::stmt_t** &body,
        size_t &n_body, SymbolTable *scope) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body);
    bool changed = false;
    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            gather_strided_sections_in_body(dl->m_body, dl->n_body,
                scope);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                gather_strided_sections_in_body(blk->m_body,
                    blk->n_body, blk->m_symtab);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                gather_strided_sections_in_body(ab->m_body,
                    ab->n_body, ab->m_symtab);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        // The gathered buffers and their loop counters live in a
        // BLOCK scope nested inside the loop. A variable owned by such
        // a scope travels into the kernel with the block instead of
        // becoming a kernel parameter, which is what makes the buffer
        // per-thread: one shared buffer written by every thread would
        // be a race.
        SymbolTable *block_scope = al.make_new<SymbolTable>(scope);
        std::vector<ASR::stmt_t*> before, after;
        if (!gather_strided_sections_in_stmt(stmt, block_scope, before,
                after)) {
            new_body.push_back(al, stmt);
            continue;
        }
        Vec<ASR::stmt_t*> block_body;
        block_body.reserve(al, before.size() + after.size() + 1);
        for (ASR::stmt_t *s : before) block_body.push_back(al, s);
        block_body.push_back(al, stmt);
        for (ASR::stmt_t *s : after) block_body.push_back(al, s);
        std::string block_name = scope->get_unique_name(
            "__gpu_gather_scope");
        ASR::asr_t *block = ASR::make_Block_t(al, stmt->base.loc,
            block_scope, s2c(al, block_name), block_body.p,
            block_body.n);
        block_scope->asr_owner = block;
        ASR::symbol_t *block_sym =
            ASR::down_cast<ASR::symbol_t>(block);
        scope->add_symbol(block_name, block_sym);
        if (scope == current_scope) {
            kernel_blocks.push_back(block_sym);
        }
        new_body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(
            al, stmt->base.loc, -1, block_sym)));
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
void GpuOffloadVisitor::inline_elemental_array_var_assignment(
        ParallelLoopNest &nest) {
    bool changed = false;
    NestBodyWriteBack back(nest);
    inline_elemental_array_var_in_body(back.body, back.n_body, changed);
}

void GpuOffloadVisitor::inline_elemental_array_var_in_body(ASR::stmt_t** &body,
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
        // Recurse into AssociateBlockCall bodies
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_elemental_array_var_in_body(ab->m_body,
                ab->n_body, changed);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

        // An array constructor is not an elementwise expression: its
        // i-th element does not come from the i-th element of each
        // operand. Rewriting `r = [a, b]` as `r(i) = [a, b]` would
        // make the backend emit a whole constructor per element. The
        // Metal backend already expands a whole-array constructor
        // assignment into element writes, so leave it alone.
        {
            ASR::expr_t *rhs = asgn->m_value;
            while (rhs && ASR::is_a<ASR::ArrayPhysicalCast_t>(*rhs)) {
                rhs = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    rhs)->m_arg;
            }
            if (rhs && ASR::is_a<ASR::ArrayConstructor_t>(*rhs)) {
                new_body.push_back(al, stmt);
                continue;
            }
        }

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
            } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
                find_array_section(rc->m_left);
                find_array_section(rc->m_right);
            } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
                find_array_section(ic->m_left);
                find_array_section(ic->m_right);
            } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                ASR::LogicalCompare_t *lc = ASR::down_cast<ASR::LogicalCompare_t>(e);
                find_array_section(lc->m_left);
                find_array_section(lc->m_right);
            } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
                ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
                find_array_section(lb->m_left);
                find_array_section(lb->m_right);
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
                    ASR::symbol_t *sym = gpu_new_variable(al, loc,
                        var_scope, name, ASRUtils::duplicate_type(al,
                            int_type));
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
                    set_loop_head_bounds(al, loc, head, dims, (size_t)d,
                        asgn->m_target);
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
                ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope,
                    name, ASRUtils::duplicate_type(al, int_type));
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
                set_loop_head_bounds(al, loc, head, dims, (size_t)d,
                    asgn->m_target);
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

        ASR::expr_t *loop_start = int32_const(loc, 1);
        ASR::expr_t *loop_end = section_extent(loc,
            first_as->m_args[range_dim]);

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
        ASR::symbol_t *loop_var_sym = gpu_new_variable(al, loc, var_scope,
            loop_var_name, ASRUtils::duplicate_type(al, int_type));
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
                        idx.m_right = section_index(loc,
                            as->m_args[i], loop_var);
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
            } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                ASR::RealCompare_t *rc =
                    ASR::down_cast<ASR::RealCompare_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
                    elementize(rc->m_left), rc->m_op,
                    elementize(rc->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                ASR::IntegerCompare_t *ic =
                    ASR::down_cast<ASR::IntegerCompare_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                    elementize(ic->m_left), ic->m_op,
                    elementize(ic->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                ASR::LogicalCompare_t *lc =
                    ASR::down_cast<ASR::LogicalCompare_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al, loc,
                    elementize(lc->m_left), lc->m_op,
                    elementize(lc->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
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

// A GPU backend was asked for, so a loop left on the host is a failure to
// deliver what was asked for: report it as an error and let the user opt
// into host execution with --gpu-allow-cpu-fallback. Every declining loop
// in the unit is reported before the compilation is stopped, so one run
// lists all of the gaps rather than only the first.
void GpuOffloadVisitor::report_not_offloaded(const Location &where,
        const GpuDecline &decline) {
    if (pass_options.diagnostics == nullptr) return;
    if (region_being_decided != nullptr &&
            !reported_regions.insert(region_being_decided).second) {
        return;
    }
    std::string why = gpu_decline_message(decline);
    // The class is what tells a gap in this compiler apart from a limit
    // of the device. The policy below does not act on it yet -- today a
    // decline of either class is an error unless the fallback is asked
    // for -- but `--gpu-decline-stats` makes the two countable, so that
    // the gaps can be worked through and the waiver list stays honest.
    if (pass_options.gpu_decline_stats) {
        std::cerr << "gpu-decline: " << gpu_decline_class_name(
            gpu_decline_class(decline, device_caps))
            << ": " << why << std::endl;
    }
    if (pass_options.gpu_allow_cpu_fallback) {
        pass_options.diagnostics->message_label(
            "parallel loop not offloaded to the GPU, "
            "it runs on the CPU instead",
            {where}, why,
            diag::Level::Warning, diag::Stage::ASRPass);
    } else {
        pass_options.diagnostics->message_label(
            "parallel loop cannot be offloaded to the GPU: " + why
                + "; pass `--gpu-allow-cpu-fallback` to run it on the "
                  "CPU instead",
            {where}, why,
            diag::Level::Error, diag::Stage::ASRPass);
    }
}

// A clause a kernel launch has no way to honour. The loop still runs on
// the device, which chooses its own layout, so say what was dropped
// rather than drop it in silence.
void GpuOffloadVisitor::report_clause_ignored(
        const Location &where, const std::string &name) {
    if (pass_options.diagnostics == nullptr) return;
    pass_options.diagnostics->message_label(
        "the '" + name + "' clause is ignored on the gpu",
        {where}, "the device decides how to run the iterations",
        diag::Level::Warning, diag::Stage::ASRPass);
}

// What a clause a launch cannot honour is called, or an empty name for
// one it can.
std::string GpuOffloadVisitor::unhonoured_clause(
        const ASR::omp_clause_t *clause) {
    switch (clause->type) {
        case ASR::omp_clauseType::OMPNumTeams: return "num_teams";
        case ASR::omp_clauseType::OMPThreadLimit: return "thread_limit";
        case ASR::omp_clauseType::OMPIf: return "if";
        case ASR::omp_clauseType::OMPSchedule: return "schedule";
        case ASR::omp_clauseType::OMPNumThreads: return "num_threads";
        default: return "";
    }
}

// Collects every symbol the kernel would have to reference for `x`:
// the symbols appearing in the loop head and body, plus the symbols
// that only appear inside the array-dimension expressions of those
// symbols' types (e.g. `tmp(size(b))` pulls in `b`).
// The names the kernel arguments will carry, in the spelling the
// workspace extent resolver expects: every symbol the loop involves,
// plus the synthetic per-dimension extent scalar the kernel
// extraction adds for each dimension of an array argument.
void GpuOffloadVisitor::collect_kernel_arg_names(const ParallelLoopNest &nest,
        const std::set<SymbolTable*> &enclosing_block_scopes,
        std::vector<std::string> &arg_names) {
    std::map<std::string,
        std::pair<ASR::ttype_t*, ASR::expr_t*>> syms;
    collect_involved_syms(nest, enclosing_block_scopes, syms);
    // A loop index is not one of them: the kernel works it out from the
    // thread id. Counting it would tell the workspace pre-flight that
    // the host can read `n(l)`, which it cannot -- `l` only exists once
    // the kernel is running.
    std::set<std::string> indices;
    for (size_t d = 0; d < nest.n_heads(); d++) {
        if (!nest.head(d).m_v) continue;
        if (!ASR::is_a<ASR::Var_t>(*nest.head(d).m_v)) continue;
        indices.insert(ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(nest.head(d).m_v)->m_v));
    }
    for (auto &sym : syms) {
        if (indices.count(sym.first)) continue;
        arg_names.push_back(sym.first);
        if (sym.second.first == nullptr) continue;
        ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
            sym.second.first);
        if (!ASR::is_a<ASR::Array_t>(*type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            arg_names.push_back(GpuNames::dim_arg(sym.first, d));
        }
    }
}

void GpuOffloadVisitor::collect_involved_syms(const ParallelLoopNest &nest,
        const std::set<SymbolTable*> &enclosing_block_scopes,
        std::map<std::string,
            std::pair<ASR::ttype_t*, ASR::expr_t*>> &involved_syms) {
    GpuSymbolCollector collector(al, involved_syms,
        enclosing_block_scopes);
    // The whole nest: the loop heads name symbols too, and an index of
    // an outer level is read by the levels inside it.
    for (ASR::DoLoop_t *level : nest.loops) {
        collector.visit_stmt(*ASRUtils::STMT((ASR::asr_t*)level));
    }
    bool added = true;
    while (added) {
        added = false;
        std::map<std::string,
            std::pair<ASR::ttype_t*, ASR::expr_t*>> extra_syms;
        GpuSymbolCollector type_collector(al, extra_syms,
            enclosing_block_scopes);
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

// An AssociateBlock nested inside a do concurrent body is normally
// inlined away by substituting each associate name with its selector
// expression. An array-valued selector, however, is materialised into
// a temporary variable owned by the AssociateBlock's own symbol table,
// and that temporary is still referenced by the inlined statements.
// It has to stay private to each loop iteration, so the scope cannot
// simply be dropped. Rebuild it as an equivalent Block over the same
// symbol table, which the kernel extraction already knows how to carry
// into the generated kernel, and return the BlockCall replacing the
// AssociateBlockCall. Returns nullptr when no symbol of the
// AssociateBlock survives, in which case the caller inlines the
// statements directly as before.
ASR::stmt_t* GpuOffloadVisitor::wrap_assoc_scope_in_block(
        ASR::AssociateBlock_t *ab,
        Vec<ASR::stmt_t*> &resolved_stmts, SymbolTable *parent_scope) {
    std::set<ASR::symbol_t*> referenced_syms;
    VarSymbolCollector collector(referenced_syms);
    for (size_t i = 0; i < resolved_stmts.n; i++) {
        collector.visit_stmt(*resolved_stmts.p[i]);
    }
    bool scope_needed = false;
    for (auto &item : ab->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*item.second) &&
                referenced_syms.find(item.second)
                    != referenced_syms.end()) {
            scope_needed = true;
            break;
        }
    }
    if (!scope_needed) return nullptr;
    std::string block_name = parent_scope->get_unique_name(
        std::string(ab->m_name) + "_scope");
    ab->m_symtab->parent = parent_scope;
    ASR::asr_t *block = ASR::make_Block_t(al, ab->base.base.loc,
        ab->m_symtab, s2c(al, block_name), resolved_stmts.p,
        resolved_stmts.n);
    ab->m_symtab->asr_owner = block;
    ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
    parent_scope->add_symbol(block_name, block_sym);
    if (parent_scope == current_scope) {
        kernel_blocks.push_back(block_sym);
    }
    return ASRUtils::STMT(ASR::make_BlockCall_t(al, ab->base.base.loc,
        -1, block_sym));
}

// Move the symbols of an inlined AssociateBlock that are still
// reachable from the resolved statements into the scope that now owns
// those statements. ExternalSymbol entries (e.g. type-bound procedure
// references) remain referenced by call nodes, and a Block scope may
// have been created for a nested AssociateBlock.
void GpuOffloadVisitor::migrate_inlined_assoc_symbols(
        ASR::AssociateBlock_t *ab,
        SymbolTable *parent_scope) {
    std::vector<std::pair<std::string, ASR::symbol_t*>> to_move;
    for (auto &item : ab->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second) ||
                ASR::is_a<ASR::Block_t>(*item.second)) {
            to_move.push_back({item.first, item.second});
        }
    }
    for (auto &item : to_move) {
        std::string name = item.first;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
            if (parent_scope->get_symbol(name)) continue;
            ASR::down_cast<ASR::ExternalSymbol_t>(item.second)
                ->m_parent_symtab = parent_scope;
        } else {
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                item.second);
            if (parent_scope->get_symbol(name)) {
                name = parent_scope->get_unique_name(name);
                block->m_name = s2c(al, name);
            }
            block->m_symtab->parent = parent_scope;
        }
        parent_scope->add_symbol(name, item.second);
        if (ASR::is_a<ASR::Block_t>(*item.second)
                && parent_scope == current_scope) {
            kernel_blocks.push_back(item.second);
        }
    }
}

// Copy every loop-invariant, read-only element of an array of
// derived type that the loop reaches through a component --
// `x%c_(k)` -- into a temporary of the enclosing scope, and let the
// loop body name the temporary instead.  The copy runs once, on the
// host, before the launch; the temporary is then an ordinary
// derived-type kernel argument whose own components have extents
// the device can be told about, which is what the chain itself does
// not.
//
// An element the loop writes into is handled the same way, with a
// copy back over the original after the launch: because the gather
// brought the whole element in first, the bytes the kernel left
// alone still hold what was read, so the copy back is exact even
// when only part of the element was written.  It is only allowed
// when every write to the object lands inside that one element --
// a write anywhere else, `allocate` of the array the element comes
// from included, would be undone by the copy back.
//
// `undo` records every slot that was overwritten and `temp_names`
// every symbol that was added, so `GpuGatherGuard` can put the loop
// back exactly as it was if the offload is declined further down.
bool GpuOffloadVisitor::hoist_struct_element_gathers(
        const ParallelLoopNest &nest,
        Vec<ASR::stmt_t*> &gather_stmts,
        Vec<ASR::stmt_t*> &scatter_stmts,
        std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo,
        std::vector<std::string> &temp_names) {
    GpuStructElementGatherCollector collector;
    for (size_t i = 0; i < nest.n_body; i++) {
        collector.visit_stmt(*nest.body[i]);
    }
    if (collector.found.empty()) return true;

    GpuWrittenRootCollector written;
    for (size_t i = 0; i < nest.n_body; i++) {
        written.visit_stmt(*nest.body[i]);
    }
    std::set<ASR::symbol_t*> loop_indices;
    for (size_t d = 0; d < nest.n_heads(); d++) {
        if (!nest.head(d).m_v) continue;
        if (!ASR::is_a<ASR::Var_t>(*nest.head(d).m_v)) continue;
        loop_indices.insert(ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(nest.head(d).m_v)->m_v));
    }

    std::vector<GpuStructElementGather> gathers;
    for (ASR::ArrayItem_t *item : collector.found) {
        ASR::expr_t *chain = ASRUtils::EXPR((ASR::asr_t*)item);
        GpuDesignatorBase base = gpu_designator_base(chain);
        if (!base.is_known()) return false;
        // A write to the object the chain hangs off makes a
        // read-only copy stale. The copy can still stand in for the
        // element when every such write lands inside that element,
        // because it is then put back over the original after the
        // launch. A write anywhere else in the object would be lost
        // or, worse, undone by that copy back.
        bool scatter = false;
        if (written.roots.count(base.root)) {
            // A callee that writes the object through one of its
            // dummies is spliced into the kernel, and the pass does
            // not build a kernel for that shape yet; the copy back
            // would be over storage the launch never reached.
            if (written.call_roots.count(base.root)) return false;
            for (ASR::expr_t *target : written.targets) {
                if (gpu_designator_base(target).root != base.root) {
                    continue;
                }
                if (!gpu_designator_within(target, chain)) return false;
            }
            scatter = true;
        }
        if (!host_nameable(base.root)) return false;
        // The subscripts have to mean the same thing for every
        // iteration, and mean it in the scope the copy is made in.
        bool invariant = true;
        for (size_t k = 0; k < item->n_args && invariant; k++) {
            GpuExprSymbolCollector sc;
            sc.visit_expr(*item->m_args[k].m_right);
            for (ASR::symbol_t *sym : sc.syms) {
                if (loop_indices.count(sym) || written.roots.count(sym)
                        || !host_nameable(sym)) {
                    invariant = false;
                    break;
                }
            }
        }
        if (!invariant) return false;
        bool seen = false;
        for (const GpuStructElementGather &g : gathers) {
            if (gpu_same_designator(g.chain, chain)) {
                seen = true;
                break;
            }
        }
        if (seen) continue;
        ASR::symbol_t *struct_sym =
            ASRUtils::get_struct_sym_from_struct_expr(chain);
        if (struct_sym == nullptr) return false;
        GpuStructElementGather g;
        g.chain = chain;
        g.temp = nullptr;
        g.scatter = scatter;
        gathers.push_back(g);
    }
    if (gathers.empty()) return true;

    // Two elements of the same object can designate the same
    // storage at run time -- `x%c_(i)` and `x%c_(k)` with `i == k`
    // -- and then one copy back would silently undo the other. Take
    // only a single element per written object.
    for (const GpuStructElementGather &g : gathers) {
        if (!g.scatter) continue;
        ASR::symbol_t *root = gpu_designator_base(g.chain).root;
        for (const GpuStructElementGather &other : gathers) {
            if (&other == &g) continue;
            if (gpu_designator_base(other.chain).root == root) {
                return false;
            }
        }
    }

    gather_stmts.reserve(al, gathers.size());
    scatter_stmts.reserve(al, gathers.size());
    for (GpuStructElementGather &g : gathers) {
        const Location &gloc = g.chain->base.loc;
        GpuDesignatorBase base = gpu_designator_base(g.chain);
        std::string stem = base.members.empty()
            ? std::string("elem")
            : std::string(ASRUtils::symbol_name(base.members.front()));
        std::string name = current_scope->get_unique_name(
            "__gpu_gather_" + stem);
        ASR::symbol_t *temp = gpu_new_variable(al, gloc, current_scope,
            name, ASRUtils::duplicate_type(al,
                ASRUtils::expr_type(g.chain)), ASR::intentType::Local,
            ASRUtils::get_struct_sym_from_struct_expr(g.chain));
        temp_names.push_back(name);
        g.temp = temp;
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.success = true;
        ASR::expr_t *rhs = dup.duplicate_expr(g.chain);
        if (!rhs || !dup.success) rhs = g.chain;
        gather_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, gloc,
                ASRUtils::EXPR(ASR::make_Var_t(al, gloc, temp)),
                rhs, nullptr, false, false)));
        if (!g.scatter) continue;
        ASRUtils::ExprStmtDuplicator back(al);
        back.success = true;
        ASR::expr_t *lhs = back.duplicate_expr(g.chain);
        if (!lhs || !back.success) lhs = g.chain;
        scatter_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, gloc, lhs,
                ASRUtils::EXPR(ASR::make_Var_t(al, gloc, temp)),
                nullptr, false, false)));
    }

    GpuStructElementGatherVisitor sub(al, gathers, undo);
    for (size_t i = 0; i < nest.n_body; i++) {
        sub.visit_stmt(*nest.body[i]);
    }
    return true;
}

// True when `sym` is the very symbol the enclosing scope resolves its
// name to, so an expression written in terms of it can be repeated
// outside the loop.
bool GpuOffloadVisitor::host_nameable(ASR::symbol_t *sym) {
    if (sym == nullptr) return false;
    ASR::symbol_t *found = current_scope->resolve_symbol(
        ASRUtils::symbol_name(sym));
    return found != nullptr
        && ASRUtils::symbol_get_past_external(found) == sym;
}

// Copies a loop nest so that the pass can rewrite it without touching
// the loop the host would run if the offload is declined.
//
// A BLOCK or ASSOCIATE is copied along with it, including one nested
// in `if` or `while`. The kernel takes the copy and the host keeps
// its own, so no rewrite on the way to a kernel can reach the host,
// and a decline has nothing to put back.
ASR::stmt_t* GpuOffloadVisitor::copy_loop_stmt(ASR::stmt_t *stmt,
        ASRUtils::ExprStmtDuplicator &dup) {
    if (stmt == nullptr) return nullptr;
    if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
        ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(stmt);
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(bc->m_m);
        if (sym == nullptr || !ASR::is_a<ASR::Block_t>(*sym)) return stmt;
        ASR::Block_t *orig = ASR::down_cast<ASR::Block_t>(sym);
        ASRUtils::SymbolDuplicator sym_dup(al);
        ASR::symbol_t *copy = sym_dup.duplicate_Block(orig,
            current_scope);
        if (copy == nullptr) return nullptr;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(copy);
        retarget_block_calls_in(blk);
        remap_block_to_own_scope(blk);
        std::string name = current_scope->get_unique_name(
            std::string(orig->m_name) + "_gpu");
        blk->m_name = s2c(al, name);
        current_scope->add_symbol(name, copy);
        kernel_blocks.push_back(copy);
        return ASRUtils::STMT(ASR::make_BlockCall_t(al,
            stmt->base.loc, bc->m_label, copy));
    }
    if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
        ASR::AssociateBlockCall_t *abc =
            ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(abc->m_m);
        if (sym == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*sym)) {
            return stmt;
        }
        ASR::AssociateBlock_t *orig =
            ASR::down_cast<ASR::AssociateBlock_t>(sym);
        ASRUtils::SymbolDuplicator sym_dup(al);
        ASR::symbol_t *copy = sym_dup.duplicate_AssociateBlock(orig,
            current_scope);
        if (copy == nullptr) return nullptr;
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(copy);
        remap_associate_to_own_scope(ab);
        std::string name = current_scope->get_unique_name(
            std::string(orig->m_name) + "_gpu");
        ab->m_name = s2c(al, name);
        current_scope->add_symbol(name, copy);
        kernel_blocks.push_back(copy);
        return ASRUtils::STMT(ASR::make_AssociateBlockCall_t(al,
            stmt->base.loc, copy));
    }
    if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
        // Only the nest itself is rebuilt, so that each level keeps a
        // body of its own to rewrite; the statements inside are copied
        // by the same rules.
        ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, dl->n_body);
        for (size_t i = 0; i < dl->n_body; i++) {
            ASR::stmt_t *c = copy_loop_stmt(dl->m_body[i], dup);
            if (c == nullptr) return nullptr;
            body.push_back(al, c);
        }
        return ASRUtils::STMT(ASR::make_DoLoop_t(al, dl->base.base.loc,
            dl->m_name, dl->m_head, body.p, body.n, dl->m_orelse,
            dl->n_orelse));
    }
    if (ASR::is_a<ASR::If_t>(*stmt)) {
        ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmt);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, ifs->n_body);
        for (size_t i = 0; i < ifs->n_body; i++) {
            ASR::stmt_t *c = copy_loop_stmt(ifs->m_body[i], dup);
            if (c == nullptr) return nullptr;
            body.push_back(al, c);
        }
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, ifs->n_orelse);
        for (size_t i = 0; i < ifs->n_orelse; i++) {
            ASR::stmt_t *c = copy_loop_stmt(ifs->m_orelse[i], dup);
            if (c == nullptr) return nullptr;
            orelse.push_back(al, c);
        }
        return ASRUtils::STMT(ASR::make_If_t(al, ifs->base.base.loc,
            ifs->m_name, ifs->m_test, body.p, body.n, orelse.p,
            orelse.n));
    }
    if (ASR::is_a<ASR::WhileLoop_t>(*stmt)) {
        ASR::WhileLoop_t *wl = ASR::down_cast<ASR::WhileLoop_t>(stmt);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, wl->n_body);
        for (size_t i = 0; i < wl->n_body; i++) {
            ASR::stmt_t *c = copy_loop_stmt(wl->m_body[i], dup);
            if (c == nullptr) return nullptr;
            body.push_back(al, c);
        }
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, wl->n_orelse);
        for (size_t i = 0; i < wl->n_orelse; i++) {
            ASR::stmt_t *c = copy_loop_stmt(wl->m_orelse[i], dup);
            if (c == nullptr) return nullptr;
            orelse.push_back(al, c);
        }
        return ASRUtils::STMT(ASR::make_WhileLoop_t(al,
            wl->base.base.loc, wl->m_name, wl->m_test, body.p, body.n,
            orelse.p, orelse.n));
    }
    dup.success = true;
    ASR::stmt_t *c = dup.duplicate_stmt(stmt);
    if (c == nullptr || !dup.success) return nullptr;
    return c;
}

// duplicate_Block copies the symbol table but leaves Var nodes in the
// body pointing at the original Variables. Point them at the copies so
// the copied block is self-contained. Nested blocks resolve through the
// copy's parent chain: an inner use of an outer-block local must find
// the outer copy, not the original the host keeps.
// Point the extents of a scope's own array locals at that scope's
// symbols, and do the same for every scope nested in it. Duplicating a
// symbol table copies its symbols one by one, so an extent that names
// a symbol copied later is left pointing at the original.
void GpuOffloadVisitor::retarget_local_extents(SymbolTable *scope) {
    GpuReplaceSymbols type_replacer(*scope);
    for (auto &item : scope->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*item.second)) {
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
                ASR::down_cast<ASR::Variable_t>(item.second)->m_type);
            if (!type || !ASR::is_a<ASR::Array_t>(*type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_start) {
                    type_replacer.current_expr =
                        &(arr->m_dims[d].m_start);
                    type_replacer.replace_expr(arr->m_dims[d].m_start);
                }
                if (arr->m_dims[d].m_length) {
                    type_replacer.current_expr =
                        &(arr->m_dims[d].m_length);
                    type_replacer.replace_expr(arr->m_dims[d].m_length);
                }
            }
        } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
            retarget_local_extents(
                ASR::down_cast<ASR::Block_t>(item.second)->m_symtab);
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
            retarget_local_extents(ASR::down_cast<ASR::AssociateBlock_t>(
                item.second)->m_symtab);
        }
    }
}

// duplicate_AssociateBlock copies the symbol table but leaves Var
// nodes in the body pointing at the original Variables. Point them at
// the copies so the inliner rewrites the draft, not the host
// construct the original nest still names.
void GpuOffloadVisitor::remap_associate_to_own_scope(
        ASR::AssociateBlock_t *ab) {
    GpuReplaceSymbolsVisitor body_v(*ab->m_symtab);
    body_v.replacer.resolve_through_parents = true;
    for (size_t j = 0; j < ab->n_body; j++) {
        body_v.visit_stmt(*ab->m_body[j]);
    }
    retarget_local_extents(ab->m_symtab);
    for (auto &item : ab->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Block_t>(*item.second)) {
            remap_block_to_own_scope(
                ASR::down_cast<ASR::Block_t>(item.second));
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)
                && item.second != (ASR::symbol_t*)ab) {
            remap_associate_to_own_scope(
                ASR::down_cast<ASR::AssociateBlock_t>(item.second));
        }
    }
    retarget_nested_calls_in((ASR::symbol_t*)ab, ab->m_symtab,
        ab->m_body, ab->n_body);
}

void GpuOffloadVisitor::remap_block_to_own_scope(ASR::Block_t *block) {
    GpuReplaceSymbolsVisitor body_v(*block->m_symtab);
    body_v.replacer.resolve_through_parents = true;
    for (size_t j = 0; j < block->n_body; j++) {
        body_v.visit_stmt(*block->m_body[j]);
    }
    GpuReplaceSymbols type_replacer(*block->m_symtab);
    type_replacer.resolve_through_parents = true;
    for (auto &item : block->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Block_t>(*item.second)) {
            remap_block_to_own_scope(
                ASR::down_cast<ASR::Block_t>(item.second));
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
            // Through the ASSOCIATE's own table: nested calls and
            // the names it binds live there, and resolving them
            // against the block's would leave them pointing at the
            // original's.
            remap_associate_to_own_scope(
                ASR::down_cast<ASR::AssociateBlock_t>(item.second));
            continue;
        }
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var =
            ASR::down_cast<ASR::Variable_t>(item.second);
        if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            if (arr->m_dims[d].m_start) {
                type_replacer.current_expr = &(arr->m_dims[d].m_start);
                type_replacer.replace_expr(arr->m_dims[d].m_start);
            }
            if (arr->m_dims[d].m_length) {
                type_replacer.current_expr = &(arr->m_dims[d].m_length);
                type_replacer.replace_expr(arr->m_dims[d].m_length);
            }
        }
    }
}

// A call to a nested BLOCK or ASSOCIATE names the original; point it
// at the copy that `scope` holds.
void GpuOffloadVisitor::retarget_nested_calls_in(
        ASR::symbol_t *owner, SymbolTable *scope,
        ASR::stmt_t **body, size_t n_body) {
    std::function<void(ASR::stmt_t**, size_t)> walk =
        [&](ASR::stmt_t **stmts, size_t n_stmts) {
        for (size_t i = 0; i < n_stmts; i++) {
            if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                ASR::Block_t *inner =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                ASR::symbol_t *local =
                    scope->get_symbol(inner->m_name);
                if (local && ASR::is_a<ASR::Block_t>(*local)
                        && local != owner) {
                    bc->m_m = local;
                    retarget_block_calls_in(
                        ASR::down_cast<ASR::Block_t>(local));
                }
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmts[i])) {
                // An ASSOCIATE inside the construct is copied with it,
                // and the call has to name the copy for the same
                // reason a nested block's does.
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmts[i]);
                ASR::symbol_t *inner = ASRUtils::symbol_get_past_external(
                    abc->m_m);
                if (inner == nullptr
                        || !ASR::is_a<ASR::AssociateBlock_t>(*inner)) {
                    continue;
                }
                ASR::symbol_t *local = scope->get_symbol(
                    ASRUtils::symbol_name(inner));
                if (local && ASR::is_a<ASR::AssociateBlock_t>(*local)) {
                    abc->m_m = local;
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(local);
                    retarget_nested_calls_in(local, ab->m_symtab,
                        ab->m_body, ab->n_body);
                }
                continue;
            }
            if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
                ASR::DoLoop_t *dl =
                    ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                walk(dl->m_body, dl->n_body);
            } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmts[i]);
                walk(ifs->m_body, ifs->n_body);
                walk(ifs->m_orelse, ifs->n_orelse);
            } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
                ASR::WhileLoop_t *wl =
                    ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                walk(wl->m_body, wl->n_body);
            }
        }
    };
    walk(body, n_body);
}

void GpuOffloadVisitor::retarget_block_calls_in(ASR::Block_t *block) {
    retarget_nested_calls_in((ASR::symbol_t*)block, block->m_symtab,
        block->m_body, block->n_body);
}

// A region this pass does not take is left exactly as it was, and is
// looked inside for the regions it can take.
void GpuOffloadVisitor::decline(const ASR::OMPRegion_t &x) {
    ASR::ASRPassBaseWalkVisitor<GpuOffloadVisitor>::visit_OMPRegion(x);
}

void GpuOffloadVisitor::visit_OMPRegion(const ASR::OMPRegion_t &region) {
    DecisionScope decision(*this, &region);
    if (!device_caps.device_selected()) {
        decline(region);
        return;
    }

    // Only the regions the dispatch pass gave to the device. Every other
    // exit of this function leaves the region alone, and the regions
    // still marked for the device once the pass is done are the ones it
    // declined; they are handed back to the host below.
    if (region.m_exec_target != ASR::exec_targetType::ExecDevice) {
        decline(region);
        return;
    }

    // Only a canonical parallel loop is offloaded: one region, one
    // perfectly nested loop nest, and the whole data environment in one
    // clause list. The kernel is built out of the nest.
    ParallelLoopNest nest;
    if (!parallel_loop_nest(region, nest)) {
        decline(region);
        return;
    }

    Location loc = region.base.base.loc;
    size_t n_dims = nest.n_heads();

    // A reduction combines what the threads computed, which the launch
    // does not do yet, so the loop stays where that already works.
    for (size_t i = 0; i < region.n_clauses; i++) {
        if (region.m_clauses[i]->type ==
                ASR::omp_clauseType::OMPReduction) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::ReductionClause));
            return;
        }
    }

    // Any rank is offloaded: the nest is dispatched as a flat 1-D grid of
    // product(extents) threads and every index is recovered from the flat
    // thread id by successive divmod over the per-dimension extents. The
    // 3-D shape of the underlying dispatch grid therefore does not limit
    // the number of loop indices.
    if (n_dims == 0) {
        report_not_offloaded(loc,
            GpuDecline(GpuDeclineReason::LoopWithoutIndex));
        return;
    }

    for (size_t d = 0; d < n_dims; d++) {
        if (!nest.head(d).m_v || !nest.head(d).m_start ||
                !nest.head(d).m_end) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::IncompleteLoopHead));
            return;
        }
    }

    // The kernel maps a flat thread id onto `start + (flat % extent)`, which
    // is only the loop's iteration set when the stride is one. A strided
    // head would silently address the wrong elements, so it stays on the
    // host until the index arithmetic carries the stride.
    for (size_t d = 0; d < n_dims; d++) {
        ASR::expr_t *step = nest.head(d).m_increment;
        if (!step) continue;
        ASR::expr_t *step_value = ASRUtils::expr_value(step);
        int64_t step_constant = 0;
        if (!step_value ||
                !ASRUtils::extract_value(step_value, step_constant) ||
                step_constant != 1) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::StridedLoop));
            return;
        }
    }

    // Everything below rewrites the loop as it goes -- inlining an
    // intrinsic, splicing a callee, gathering an argument -- and
    // several of those rewrites change a statement in place. A rewrite
    // must not reach the loop the host would run, because the offload
    // can still be declined further down. So the region's loop nest is
    // copied and it is the copy that is rewritten, the original
    // standing until the launch replaces it.
    //
    // A BLOCK or ASSOCIATE in the nest is copied with it. The kernel
    // takes that copy and the host keeps its own, so a rewrite on the
    // way to a kernel cannot reach the host, and a decline has
    // nothing to put back.
    ParallelLoopNest work;
    kernel_blocks.clear();
    // From here on the pass is drafting a kernel: it copies the blocks of
    // the nest into this scope and takes a kernel number. Every exit
    // below that leaves the loop on the host drops both, whichever exit
    // it is; the draft is handed to the kernel by committing the guard
    // once the launch is known to be supported.
    GpuKernelDraftGuard draft_guard(current_scope, kernel_blocks,
        gpu_kernel_counter);
    {
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        dup.allow_reshape = true;
        ASR::stmt_t *loop_copy = copy_loop_stmt(region.m_body[0], dup);
        if (loop_copy == nullptr ||
                !parallel_loop_nest_of(loop_copy,
                    parallel_collapse_count(region), work)) {
            // Nothing here can be rewritten safely.
            decline(region);
            return;
        }
    }

    // Resolve associate variables to their original targets if this
    // loop is inside one or more nested AssociateBlocks.
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
                    // Only scalar selectors are inlined. An
                    // array-valued selector is materialized by the
                    // compiler into a temporary that lives in this
                    // AssociateBlock's symtab, and its defining
                    // expression may itself reference further
                    // AssociateBlock-local symbols (the array
                    // constant buffer, the resolved specific
                    // procedure of a generic constructor, ...).
                    // Inlining it would drag those unreachable
                    // symbols into the kernel. The associate name is
                    // backed by real storage here, so it is passed
                    // into the kernel as an ordinary buffer instead.
                    ASR::Assignment_t *asgn = down_cast<ASR::Assignment_t>(
                        ab->m_body[i]);
                    if (is_a<ASR::Var_t>(*asgn->m_target)) {
                        ASR::symbol_t *sym =
                            down_cast<ASR::Var_t>(asgn->m_target)->m_v;
                        if (is_a<ASR::Variable_t>(*sym) &&
                            down_cast<ASR::Variable_t>(sym)->m_parent_symtab
                                == ab->m_symtab &&
                            !ASRUtils::is_array(
                                down_cast<ASR::Variable_t>(sym)->m_type) &&
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
            for (size_t d = 0; d < n_dims; d++) {
                ASR::do_loop_head_t &head = work.head(d);
                if (head.m_start) {
                    resolver.current_expr = &(head.m_start);
                    resolver.replace_expr(head.m_start);
                }
                if (head.m_end) {
                    resolver.current_expr = &(head.m_end);
                    resolver.replace_expr(head.m_end);
                }
                if (head.m_increment) {
                    resolver.current_expr = &(head.m_increment);
                    resolver.replace_expr(head.m_increment);
                }
            }
            AssociateVarResolverVisitor resolver_visitor(al, assoc_map);
            for (size_t i = 0; i < work.n_body; i++) {
                resolver_visitor.visit_stmt(*work.body[i]);
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
            resolve_assoc_in_blocks(work.body, work.n_body);
            // Resolve associate aliases in enclosing Block scopes'
            // variable type expressions. When a parallel loop is
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
            if (is_a<ASR::Block_t>(*owner_sym) ||
                is_a<ASR::AssociateBlock_t>(*owner_sym)) {
                enclosing_block_scopes.insert(scope);
                scope = scope->parent;
            } else {
                break;
            }
        }
    }

    // An element of an array of derived type reached through a
    // component -- `x%c_(k)` -- is copied to a temporary of this
    // scope before the launch, and the loop body reads the temporary.
    // This runs ahead of the checks below on purpose: they must judge
    // the shape the kernel would really be built from. The guard puts
    // the loop back untouched if any of them declines.
    Vec<ASR::stmt_t*> gather_stmts;
    gather_stmts.reserve(al, 1);
    Vec<ASR::stmt_t*> scatter_stmts;
    scatter_stmts.reserve(al, 1);
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> gather_undo;
    std::vector<std::string> gather_temp_names;
    GpuGatherGuard gather_guard(current_scope, gather_undo,
        gather_temp_names);
    // The gather is a copy the host makes before it launches, so this
    // holds for every dialect.
    if (!hoist_struct_element_gathers(work, gather_stmts,
            scatter_stmts, gather_undo, gather_temp_names)) {
        // The element could not be hoisted -- a subscript that moves
        // with the loop, or a write to the object that the copy back
        // after the launch could not reproduce exactly. Passing
        // the chain on unchanged reaches the device as a component of
        // the wrong element, which is a wrong number and no
        // diagnostic, so decline the loop instead.
        report_not_offloaded(loc,
            GpuDecline(GpuDeclineReason::StructElementGather));
        return;
    }

    // Decide whether this loop can be offloaded at all *before* any of
    // the inline_* helpers below rewrite the loop body. Those helpers
    // are destructive: they lower array-section and intrinsic-array
    // assignments into explicit element loops, a half-lowered shape
    // that only the kernel extractor understands. If we declined the
    // offload after rewriting, the loop would stay on the host in a
    // form the later array_op pass no longer normalizes, and codegen
    // would fail. So: no mutation until the decision is made.
    // What the pass's own lowering can and cannot do, which is the same
    // whichever device the launch targets: a local with no extent, an
    // aliased assignment that would need a run-time sized temporary, and
    // a strided actual that has to be gathered into a contiguous one.
    // These used to run for Metal only, so the CUDA path went on to build
    // a kernel that read the wrong elements and said nothing.
    {
        GpuLocalArrayChecker local_array_checker;
        for (size_t i = 0; i < work.n_body; i++) {
            local_array_checker.visit_stmt(*work.body[i]);
        }
        if (local_array_checker.has_unsized_local_array) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::UnsizedLocalArray,
                    local_array_checker.unsized_name));
            return;
        }
        // An array assignment whose two sides overlap the same array
        // needs a temporary (see materialize_aliased_assignments).
        // If that temporary cannot be fixed-size, decline here,
        // while the body is still untouched.
        std::vector<std::string> alias_arg_names;
        collect_kernel_arg_names(work, enclosing_block_scopes,
            alias_arg_names);
        if (body_needs_unsupported_alias_temp(work.body, work.n_body,
                true, alias_arg_names)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::AliasTemporaryRuntimeSized));
            return;
        }
        // A strided section actual argument is gathered into a
        // contiguous kernel-local temporary below. When that temporary
        // cannot be sized at compile time the gather is impossible,
        // and passing the section on would silently drop its stride.
        if (body_has_ungatherable_strided_section(work.body, work.n_body)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::UngatherableStridedSection));
            return;
        }
        GpuLocalWidthChecker width_checker;
        width_checker.caps = device_caps;
        for (size_t i = 0; i < work.n_body; i++) {
            width_checker.visit_stmt(*work.body[i]);
        }
        if (width_checker.unsupported) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::LocalTypeWidth,
                    width_checker.bad_name, width_checker.bad_type));
            return;
        }
    }

    // A device whose scalar type set is narrower than the shared width
    // table has to be asked about every symbol that reaches the kernel:
    // where the two sets are the same, the kernel-argument and
    // kernel-local checks that run on every device already ask it.
    if (device_caps.narrows_scalar_types()) {
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>>
            candidate_syms;
        collect_involved_syms(work, enclosing_block_scopes, candidate_syms);
        // Every symbol reaching the kernel — buffer parameters,
        // by-value members of the __ScalarArgs struct and kernel-local
        // temporaries alike — is collected here, so a single sweep
        // covers all of them.
        for (auto &sym : candidate_syms) {
            if (!gpu_device_can_represent_type(device_caps,
                    sym.second.first, sym.second.second)) {
                report_not_offloaded(loc, GpuDecline(
                    GpuDeclineReason::SymbolTypeNotRepresentable,
                    sym.first, scalar_type_of(sym.second.first)));
                return;
            }
        }
    }

    // A device function may need a run-time sized local -- an
    // array-constructor temporary sized from an assumed-shape dummy,
    // say -- which a device that has no variable-length arrays cannot
    // declare. Work out here which callees have to be spliced into the
    // kernel body to move those locals to kernel scope, where the VLA
    // workspace machinery applies. This is analysis only; the splice
    // itself happens below, after the offload decision.
    functions_to_inline.clear();
    if (device_caps.splices_device_functions()) {
        std::map<ASR::Function_t*, bool> needs_inline_memo;
        std::set<ASR::Function_t*> on_stack;
        if (!plan_device_function_inlining(work.body, work.n_body,
                needs_inline_memo, on_stack)) {
            // Some callee that must be inlined cannot be (recursive,
            // early `return`, nested scopes, or called from a position
            // with nowhere to put the result). Emitting a kernel that
            // cannot compile would be worse than not offloading at all.
            functions_to_inline.clear();
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::DeviceFunctionInlining));
            return;
        }
    }

    // Splice the planned device functions into the loop body. This
    // must come first among the rewrites below: the intrinsic and
    // array-section inliners then see the spliced-in statements too.
    // The splice is recorded so that it can be undone: the workspace
    // pre-flight right below needs the spliced shape, but must still
    // be able to leave the loop untouched when it declines.
    GpuLoopBodySnapshot splice_snapshot;
    std::vector<ScopeArrayDims> scope_dims_undo;
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>>
        member_extent_undo;
    GpuSpliceRestoreGuard splice_guard(splice_snapshot, scope_dims_undo);
    {
        splice_snapshot.record(work, current_scope);
        if (!inline_device_function_calls(work.body, work.n_body)) {
            functions_to_inline.clear();
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::DeviceFunctionInlining));
            return;
        }
        functions_to_inline.clear();
        // Run-time sized alias temporaries become BLOCK locals here,
        // ahead of the workspace pre-flight below, so that the
        // pre-flight sizes them too and can still decline the loop.
        materialize_runtime_alias_blocks(work);
        size_scope_array_temporaries(work.body, work.n_body,
            scope_dims_undo);
    }

    // Each run-time sized local of a kernel BLOCK becomes a per-thread
    // workspace buffer, which the host has to size before it launches
    // the kernel. An extent the host cannot work out from the kernel
    // arguments is a code generation error -- raised long after the
    // pass has committed to offloading, and so a hard build failure.
    // Run the backend's own resolution here instead, while the loop
    // can still be left on the host. This is the last point at which
    // it can be: the workspaces only exist once the callees are
    // spliced in, and the rewrites below are not reversible.
    //
    // The host sizes the workspace the same way whichever device it
    // launches on, so this holds for every dialect: an extent written
    // in terms of a spliced callee's own dummy names a symbol that no
    // longer exists once the callee is gone.
    //
    // What this cannot see, and what nothing here can: the workspaces
    // the passes after this one create. `subroutine_from_function`
    // turns a call whose result is an array into a temporary at the
    // call site, `array_struct_temporary` and `array_op` lower array
    // expressions into temporaries of their own, and every one of
    // those is a local of the kernel that does not exist yet. Over the
    // GPU corpus a third of the launches that carry a workspace at all
    // reach `device_launch_expand` with more of them than were counted
    // here. So this pre-flight is a filter, not a verdict: it keeps on
    // the host the loops it can already tell apart, and the launch
    // layout asks the same question again of the kernel that exists.
    {
        std::vector<std::string> kernel_arg_names;
        collect_kernel_arg_names(work, enclosing_block_scopes,
            kernel_arg_names);
        std::string unresolved_name;
        if (!gpu_block_workspace_extents_resolvable(work.body,
                work.n_body, kernel_arg_names, unresolved_name)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::WorkspaceNotSizeableOnHost,
                    unresolved_name));
            return;
        }
    }
    // Splicing a callee is what can leave a section of a section in the
    // body, so the shape is only possible where the pass splices.
    if (device_caps.splices_device_functions()) {
        GpuNestedSectionFinder nested_section;
        for (size_t i = 0; i < work.n_body; i++) {
            nested_section.visit_stmt(*work.body[i]);
        }
        if (nested_section.found) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::NestedArraySection));
            return;
        }
    }

    // Inline IntrinsicArrayFunction All before kernel extraction
    all_reduction_targets.clear();
    inline_intrinsic_all(work);

    // Hoist matmuls out of the expression positions the matmul
    // lowering below cannot see them in.
    hoist_nested_matmuls(work);

    // Inline IntrinsicArrayFunction MatMul before kernel extraction
    inline_intrinsic_matmul(work);

    // Inline IntrinsicArrayFunction DotProduct before kernel
    // extraction
    inline_intrinsic_dot_product(work);

    // Inline IntrinsicArrayFunction Sum before kernel extraction
    inline_intrinsic_sum(work);

    // Also inline Sum in helper functions called from the
    // loop body. This ensures that sum(f(x)) patterns
    // inside helper functions are expanded into loops before
    // kernel extraction, avoiding allocatable temporaries that
    // cannot be represented as VLAs in Metal shaders.
    {
        GpuFunctionCollector sum_fc;
        for (size_t i = 0; i < work.n_body; i++) {
            sum_fc.visit_stmt(*work.body[i]);
        }
        bool sum_added = true;
        while (sum_added) {
            sum_added = false;
            GpuFunctionCollector sum_tc;
            for (auto &[fn_name, fn_sym] : sum_fc.functions) {
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(fn_sym);
                if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    ASR::Function_t *fn =
                        ASR::down_cast<ASR::Function_t>(resolved);
                    for (size_t i = 0; i < fn->n_body; i++) {
                        sum_tc.visit_stmt(*fn->m_body[i]);
                    }
                }
            }
            for (auto &[name, sym] : sum_tc.functions) {
                if (sum_fc.functions.find(name) ==
                        sum_fc.functions.end()) {
                    sum_fc.functions[name] = sym;
                    sum_added = true;
                }
            }
        }
        for (auto &[fn_name, fn_sym] : sum_fc.functions) {
            ASR::symbol_t *resolved =
                ASRUtils::symbol_get_past_external(fn_sym);
            if (ASR::is_a<ASR::Function_t>(*resolved)) {
                ASR::Function_t *fn =
                    ASR::down_cast<ASR::Function_t>(resolved);
                inline_dot_product_in_stmts(fn->m_body, fn->n_body,
                    fn->m_symtab);
                inline_sum_in_stmts(fn->m_body, fn->n_body,
                    fn->m_symtab);
            }
        }
    }

    // Inline IntrinsicArrayFunction Transpose before kernel extraction
    inline_intrinsic_transpose(work);

    // Materialise temporaries for assignments whose target and value
    // overlap the same array, before the element loops below are
    // built from them.
    materialize_aliased_assignments(
        work);

    // Inline ArraySection assignments before kernel extraction
    inline_array_section_assignment(
        work);

    // Inline whole-array elemental assignments (e.g., b = abs(a(:,l)))
    inline_elemental_array_var_assignment(
        work);

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
                    Vec<ASR::stmt_t*> inner_stmts;
                    inner_stmts.reserve(al, inner_ab->n_body);
                    inline_assoc_body(inner_ab, assoc_map,
                        inner_stmts);
                    // Resolve the inner statements now: once they are
                    // wrapped in a Block below the caller's resolver
                    // no longer reaches them.
                    if (!assoc_map.empty()) {
                        AssociateVarResolverVisitor inner_resolver(
                            al, assoc_map);
                        for (size_t ii = 0; ii < inner_stmts.n; ii++) {
                            inner_resolver.visit_stmt(
                                *inner_stmts.p[ii]);
                        }
                    }
                    ASR::stmt_t *inner_call =
                        wrap_assoc_scope_in_block(inner_ab,
                            inner_stmts, ab->m_symtab);
                    if (inner_call) {
                        resolved_stmts.push_back(al, inner_call);
                    } else {
                        for (size_t ii = 0; ii < inner_stmts.n; ii++) {
                            resolved_stmts.push_back(al,
                                inner_stmts.p[ii]);
                        }
                        migrate_inlined_assoc_symbols(inner_ab,
                            ab->m_symtab);
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
                        assoc_map.find(sym) == assoc_map.end() &&
                        is_single_assignment_binding(sym,
                            ab->m_body, ab->n_body)) {
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

    // Resolve AssociateBlocks inside the loop body (e.g.,
    // block { associate(nh => n) ... } within the loop). GPU kernels
    // cannot use Pointer-based associate aliases, so we inline the
    // associate targets and replace the AssociateBlockCall with the
    // resolved statements.
    for (size_t bi = 0; bi < work.n_body; bi++) {
        if (!ASR::is_a<ASR::BlockCall_t>(*work.body[bi])) continue;
        ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
            work.body[bi]);
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
            // If the AssociateBlock still owns variables referenced by
            // the resolved statements (an array-valued selector
            // temporary), keep the scope alive as a Block instead of
            // dropping it.
            ASR::stmt_t *block_call = wrap_assoc_scope_in_block(
                ab, resolved_stmts, block->m_symtab);
            if (block_call) {
                new_block_body.push_back(al, block_call);
            } else {
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
                migrate_inlined_assoc_symbols(ab, block->m_symtab);
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
    // loop body (not wrapped in a BlockCall). GPU kernels
    // cannot use Pointer-based associate aliases, so we inline the
    // associate targets and replace each AssociateBlockCall with
    // the resolved statements.
    {
        Vec<ASR::stmt_t*> new_dc_body;
        new_dc_body.reserve(al, work.n_body);
        bool dc_changed = false;
        for (size_t bi = 0; bi < work.n_body; bi++) {
            if (!ASR::is_a<ASR::AssociateBlockCall_t>(*work.body[bi])) {
                new_dc_body.push_back(al, work.body[bi]);
                continue;
            }
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(
                    work.body[bi]);
            if (!ASR::is_a<ASR::AssociateBlock_t>(*abc->m_m)) {
                new_dc_body.push_back(al, work.body[bi]);
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
            // Keep the scope alive as a Block when it still owns
            // variables referenced by the resolved statements.
            ASR::stmt_t *block_call = wrap_assoc_scope_in_block(
                ab, resolved_stmts, current_scope);
            if (block_call) {
                new_dc_body.push_back(al, block_call);
            } else {
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    new_dc_body.push_back(al, resolved_stmts.p[ri]);
                }
                // Migrate ExternalSymbol entries from the
                // AssociateBlock's symtab to the enclosing scope
                // before erasing it (same as above for BlockCall).
                migrate_inlined_assoc_symbols(ab, current_scope);
            }
            std::string ab_name = ab->m_name;
            current_scope->erase_symbol(ab_name);
            dc_changed = true;
        }
        if (dc_changed) {
            work.body = new_dc_body.p;
            work.n_body = new_dc_body.n;
        }
    }

    // A strided section actual argument cannot be handed to a device
    // function as a base pointer; gather it into a contiguous
    // temporary first. This runs after the ASSOCIATE scopes above have
    // been inlined, so a section whose bounds come from an associate
    // name is gathered with the selector substituted in.
    gather_strided_section_arguments(
        work);

    // 1. Collect all symbols from body AND head expressions
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> involved_syms;
    collect_involved_syms(work, enclosing_block_scopes, involved_syms);

    // The rewrites above can bring in symbols the sweep before them
    // never saw, so ask again of the widths this device narrows. A
    // device that narrows none of them answers no to every symbol.
    for (auto &sym : involved_syms) {
        ASR::ttype_t *base_t =
            ASRUtils::type_get_past_array(sym.second.first);
        if (device_caps.narrows_scalar_type(base_t)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::WideTypeNotOnDevice,
                    sym.first, base_t));
            return;
        }
    }

    // A statement no device can run keeps the loop on the CPU whichever
    // backend is selected.
    {
        GpuUnsupportedStatementFinder finder;
        for (size_t i = 0; i < work.n_body; i++) {
            finder.visit_stmt(*work.body[i]);
        }
        std::string in_routine;
        if (finder.reason == GpuDeclineReason::None) {
            for (ASR::Function_t *fn : reachable_routines(work.body,
                    work.n_body)) {
                GpuUnsupportedStatementFinder callee_finder;
                for (size_t i = 0; i < fn->n_body; i++) {
                    callee_finder.visit_stmt(*fn->m_body[i]);
                }
                if (callee_finder.reason != GpuDeclineReason::None) {
                    finder = callee_finder;
                    in_routine = fn->m_name;
                    break;
                }
            }
        }
        if (finder.reason != GpuDeclineReason::None) {
            report_not_offloaded(finder.loc,
                GpuDecline(finder.reason, in_routine));
            return;
        }
    }

    // Collect loop variable names
    std::vector<std::string> loop_var_names;
    for (size_t d = 0; d < n_dims; d++) {
        ASR::Var_t *lv = down_cast<ASR::Var_t>(work.head(d).m_v);
        loop_var_names.push_back(ASRUtils::symbol_name(lv->m_v));
    }

    // Find local scalar temporaries (assigned but not arrays, not loop vars)
    std::set<std::string> local_vars, assigned_vars;
    GpuLocalVarCollector lv_collector(local_vars, assigned_vars, enclosing_block_scopes);
    for (size_t i = 0; i < work.n_body; i++) {
        lv_collector.visit_stmt(*work.body[i]);
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
    // vs kernel parameters (arrays or read-only scalars).
    // Assigned scalars are kernel-local unless they are reduction
    // targets from inlined all(), or are referenced after the do
    // concurrent loop (liveout) — those need to be communicated back
    // to the host via 1-element array device buffers.

    // Collect variables referenced in statements after this parallel loop
    // in the parent body, to identify liveout scalars.
    std::set<std::string> post_loop_vars;
    {
        ASR::stmt_t **parent_body = nullptr;
        size_t parent_n_body = 0;
        SymbolTable *scope = current_scope;
        while (scope && scope->asr_owner) {
            if (scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(
                    scope->asr_owner);
                if (ASR::is_a<ASR::Program_t>(*owner_sym)) {
                    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(
                        owner_sym);
                    parent_body = prog->m_body;
                    parent_n_body = prog->n_body;
                    break;
                } else if (ASR::is_a<ASR::Function_t>(*owner_sym)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                        owner_sym);
                    parent_body = fn->m_body;
                    parent_n_body = fn->n_body;
                    break;
                } else if (ASR::is_a<ASR::Block_t>(*owner_sym)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(
                        owner_sym);
                    parent_body = blk->m_body;
                    parent_n_body = blk->n_body;
                    break;
                }
            }
            scope = scope->parent;
        }
        if (parent_body) {
            bool found_dc = false;
            for (size_t si = 0; si < parent_n_body; si++) {
                if (!found_dc) {
                    if (parent_body[si]->base.loc.first == loc.first &&
                            parent_body[si]->base.loc.last == loc.last) {
                        found_dc = true;
                    }
                    continue;
                }
                PostLoopVarCollector plvc(post_loop_vars);
                plvc.visit_stmt(*parent_body[si]);
            }
        }
    }

    std::set<std::string> local_scalar_names;
    for (auto &name : assigned_vars) {
        if (loop_var_set.count(name)) continue;
        if (all_reduction_targets.count(name)) continue;
        if (post_loop_vars.count(name)) continue;
        auto it = involved_syms.find(name);
        if (it != involved_syms.end()) {
            ASR::ttype_t *type = it->second.first;
            if (!ASRUtils::is_array(type)) {
                local_scalar_names.insert(name);
            }
        }
    }

    // Remove local scalars from involved_syms (they become kernel locals)
    for (auto &name : local_scalar_names) {
        involved_syms.erase(name);
    }

    // Collect optional variables from involved_syms. When an optional
    // argument is used inside a loop body guarded by present(),
    // the kernel launch and all buffer setup must be skipped when the
    // argument is not present, otherwise the host will segfault trying
    // to read a null descriptor.
    std::vector<ASR::symbol_t*> optional_syms;
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::symbol_t *sym = this->current_scope->resolve_symbol(sym_name);
        if (!sym) continue;
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Variable_t>(*resolved)) continue;
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(resolved);
        if (var->m_presence == ASR::presenceType::Optional) {
            optional_syms.push_back(sym);
        }
    }

    // Wrap liveout scalars (assigned user variables still in
    // involved_syms) in 1-element FixedSizeArrays so they can be
    // passed as writable device buffers and read back after the kernel.
    struct LiveoutScalarInfo {
        std::string orig_name;
        std::string buf_name;
        ASR::symbol_t *host_buf_sym;
        ASR::symbol_t *orig_scalar_sym;
        ASR::ttype_t *scalar_type;
    };
    std::vector<LiveoutScalarInfo> liveout_scalars;
    {
        ASR::ttype_t *int4_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        std::vector<std::string> liveout_names;
        for (auto &name : assigned_vars) {
            if (loop_var_set.count(name)) continue;
            if (local_scalar_names.count(name)) continue;
            auto it = involved_syms.find(name);
            if (it != involved_syms.end()) {
                ASR::ttype_t *type = it->second.first;
                if (!ASRUtils::is_array(type)) {
                    liveout_names.push_back(name);
                }
            }
        }
        for (auto &name : liveout_names) {
            auto it = involved_syms.find(name);
            ASR::ttype_t *scalar_type = it->second.first;
            ASR::symbol_t *orig_sym = current_scope->resolve_symbol(name);

            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 1, int4_type,
                    ASR::integerbozType::Decimal));
            dim.m_length = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 1, int4_type,
                    ASR::integerbozType::Decimal));
            Vec<ASR::dimension_t> dims_vec;
            dims_vec.reserve(al, 1);
            dims_vec.push_back(al, dim);
            ASR::ttype_t *arr_type = ASRUtils::TYPE(
                ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, scalar_type),
                    dims_vec.p, 1,
                    ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));

            std::string buf_name = current_scope->get_unique_name(
                "__gpu_buf_" + name);
            ASR::symbol_t *buf_sym = gpu_new_variable(al, loc,
                current_scope, buf_name, ASRUtils::duplicate_type(al,
                    arr_type));
            it->second.first = arr_type;

            liveout_scalars.push_back(
                {name, buf_name, buf_sym, orig_sym, scalar_type});
        }
    }

    // Decompose struct variables with allocatable array members.
    // Metal cannot represent allocatable descriptors inside structs,
    // so we extract each allocatable array member into a separate
    // kernel buffer parameter and replace StructInstanceMember
    // references in the body with the new flat-array Var.
    GpuAllocStructMemberCollector alloc_collector;
    for (size_t i = 0; i < work.n_body; i++) {
        alloc_collector.visit_stmt(*work.body[i]);
    }
    // Also scan array dimension expressions of involved symbols for
    // StructInstanceMember accesses. VLA arrays sized by struct
    // members (e.g., `integer :: n(x%m)`) constitute a non-allocatable
    // access that must prevent struct removal from involved_syms.
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::symbol_t *sym = current_scope->resolve_symbol(sym_name);
        if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
        if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            if (arr->m_dims[d].m_start)
                alloc_collector.visit_expr(*arr->m_dims[d].m_start);
            if (arr->m_dims[d].m_length)
                alloc_collector.visit_expr(*arr->m_dims[d].m_length);
        }
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
        if (orig_sym == nullptr && sym_info.second != nullptr
                && ASR::is_a<ASR::Var_t>(*sym_info.second)) {
            // A name the loop body reads that the enclosing scope
            // cannot look up: a module `parameter` that a spliced-in
            // callee uses and the caller's `use ... , only:` list
            // leaves out. The reference the body carries still names
            // the symbol, so take it from there -- without it the
            // launch is handed an argument built over a null symbol.
            orig_sym = ASR::down_cast<ASR::Var_t>(sym_info.second)->m_v;
        }
        if (orig_sym) {
            type_decl = import_struct_type(orig_sym,
                orig_scope, kernel_scope, loc);
        }

        // Parameter variables are compile-time constants without
        // runtime storage. Clone them into the kernel scope with
        // their values preserved instead of passing as GPU buffers.
        if (orig_sym) {
            ASR::symbol_t *resolved =
                ASRUtils::symbol_get_past_external(orig_sym);
            if (ASR::is_a<ASR::Variable_t>(*resolved)) {
                ASR::Variable_t *orig_var =
                    ASR::down_cast<ASR::Variable_t>(resolved);
                if (orig_var->m_storage ==
                        ASR::storage_typeType::Parameter) {
                    ASR::symbol_t *cloned =
                        ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                kernel_scope, s2c(al, sym_name),
                                nullptr, 0,
                                ASR::intentType::Local,
                                orig_var->m_value,
                                orig_var->m_value,
                                ASR::storage_typeType::Parameter,
                                ASRUtils::duplicate_type(al,
                                    orig_var->m_type),
                                nullptr, orig_var->m_abi,
                                orig_var->m_access,
                                ASR::presenceType::Required, false));
                    kernel_scope->add_symbol(sym_name, cloned);
                    continue;
                }
            }
        }

        // Strip Allocatable/Pointer wrapper: GPU kernel parameters
        // receive raw array data, not array descriptors
        ASR::ttype_t *dup_type = ASRUtils::duplicate_type(al,
            ASRUtils::type_get_past_allocatable_pointer(type));

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
        // For liveout scalars, use the host-side 1-element array
        // buffer as the call arg so it's passed as a device buffer
        bool is_liveout = false;
        for (auto &ls : liveout_scalars) {
            if (ls.orig_name == sym_name) {
                carg.m_value = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, ls.host_buf_sym));
                is_liveout = true;
                break;
            }
        }
        if (!is_liveout) {
            carg.m_value = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, orig_sym));
        }
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
                std::string dim_name =
                    GpuNames::dim_arg(di.param_name, d);
                ASR::symbol_t *dim_sym = gpu_new_variable(al, loc,
                    kernel_scope, dim_name, ASRUtils::duplicate_type(al,
                        int_type_dim), ASR::intentType::InOut);
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
                    // Pass lower bound as kernel parameter
                    std::string lb_name = GpuNames::lower_bound(
                        di.param_name, d);
                    ASR::symbol_t *lb_sym = gpu_new_variable(al, loc,
                        kernel_scope, lb_name, ASRUtils::duplicate_type(al,
                            int_type_dim), ASR::intentType::InOut);
                    kernel_args.push_back(al,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, lb_sym)));
                    // Host-side value: lbound(struct%member, dim=d+1)
                    ASR::expr_t *lb_dim_expr = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                            int_type_dim, ASR::integerbozType::Decimal));
                    ASR::expr_t *host_lb = ASRUtils::EXPR(
                        ASR::make_ArrayBound_t(al, loc,
                            host_member_expr, lb_dim_expr,
                            int_type_dim,
                            ASR::arrayboundType::LBound, nullptr));
                    ASR::call_arg_t lb_carg;
                    lb_carg.loc = loc;
                    lb_carg.m_value = host_lb;
                    call_args.push_back(al, lb_carg);
                    k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, lb_sym));
                }
            }
        }
    }
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::ttype_t *orig_type = sym_info.first;
        // Any array whose shape is not known until run time: a
        // deferred-shape allocatable or pointer, and an automatic
        // array like `z(size(x), 3)` just the same. The kernel
        // computes strides from those extents, so it has to be given
        // them; without them it linearizes with a zero stride and
        // writes every column over the first.
        ASR::ttype_t *inner =
            ASRUtils::type_get_past_allocatable_pointer(orig_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Array_t *orig_arr = ASR::down_cast<ASR::Array_t>(inner);
        // An explicit-shape array says what it can at compile time, so
        // only the dimensions it cannot are passed; a deferred-shape
        // one says nothing, and passes them all.
        bool shape_from_type =
            !ASRUtils::is_allocatable_or_pointer(orig_type);
        if (shape_from_type) {
            bool runtime_shape = false;
            for (size_t d = 0; d < orig_arr->n_dims; d++) {
                ASR::expr_t *len = orig_arr->m_dims[d].m_length;
                if (len == nullptr ||
                        ASRUtils::expr_value(len) == nullptr) {
                    runtime_shape = true;
                    break;
                }
            }
            if (!runtime_shape) continue;
        }

        // Locate the kernel-scope Variable whose type we must update
        ASR::symbol_t *k_sym = kernel_scope->get_symbol(sym_name);
        LCOMPILERS_ASSERT(k_sym);
        ASR::Variable_t *k_var = ASR::down_cast<ASR::Variable_t>(k_sym);
        ASR::ttype_t *k_type = k_var->m_type;
        if (!ASR::is_a<ASR::Array_t>(
                *ASRUtils::type_get_past_allocatable_pointer(k_type))) {
            continue;
        }
        ASR::Array_t *k_arr = ASR::down_cast<ASR::Array_t>(
            ASRUtils::type_get_past_allocatable_pointer(k_type));

        ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
        ASR::ttype_t *int_type_dim = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        for (size_t d = 0; d < k_arr->n_dims; d++) {
            if (shape_from_type && d < orig_arr->n_dims
                    && orig_arr->m_dims[d].m_length != nullptr
                    && ASRUtils::expr_value(
                        orig_arr->m_dims[d].m_length) != nullptr) {
                continue;
            }
            std::string dim_name = GpuNames::dim_arg(sym_name, d);
            ASR::symbol_t *dim_sym = gpu_new_variable(al, loc,
                kernel_scope, dim_name, ASRUtils::duplicate_type(al,
                    int_type_dim), ASR::intentType::InOut);
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
                // Pass lower bound as kernel parameter
                std::string lb_name = GpuNames::lower_bound(
                    sym_name, d);
                ASR::symbol_t *lb_sym = gpu_new_variable(al, loc,
                    kernel_scope, lb_name, ASRUtils::duplicate_type(al,
                        int_type_dim), ASR::intentType::InOut);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, lb_sym)));
                // Host-side value: lbound(arr, dim=d+1)
                ASR::expr_t *lb_dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                        int_type_dim, ASR::integerbozType::Decimal));
                ASR::expr_t *host_lb = ASRUtils::EXPR(
                    ASR::make_ArrayBound_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, orig_sym)),
                        lb_dim_expr, int_type_dim,
                        ASR::arrayboundType::LBound, nullptr));
                ASR::call_arg_t lb_carg;
                lb_carg.loc = loc;
                lb_carg.m_value = host_lb;
                call_args.push_back(al, lb_carg);
                k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, lb_sym));
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
        for (auto &mem_entry :
                ASRUtils::collect_allocatable_array_members(st)) {
            const std::string &mem_name = mem_entry.first;
            ASR::Variable_t *mv = mem_entry.second;
            ASR::symbol_t *mem_sym = (ASR::symbol_t*)mv;
            std::string size_name = GpuNames::member_size(
                sym_name, mem_name);
            ASR::symbol_t *size_sym = gpu_new_variable(al, loc,
                kernel_scope, size_name, ASRUtils::duplicate_type(al,
                    int_type_sz), ASR::intentType::InOut);
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

            // For a component of rank > 1 the total size is not the
            // extent of any single dimension, so also pass each
            // per-dimension extent; size(struct%member, dim) in the
            // kernel reads these.
            ASR::ttype_t *mem_inner =
                ASRUtils::type_get_past_allocatable(mv->m_type);
            size_t rank = ASR::down_cast<ASR::Array_t>(
                mem_inner)->n_dims;
            if (rank <= 1) continue;
            for (size_t d = 0; d < rank; d++) {
                std::string dim_size_name = size_name + "_dim"
                    + std::to_string(d + 1);
                ASR::symbol_t *dim_size_sym = gpu_new_variable(al, loc,
                    kernel_scope, dim_size_name,
                    ASRUtils::duplicate_type(al, int_type_sz),
                    ASR::intentType::InOut);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        dim_size_sym)));
                ASR::expr_t *dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc,
                        (int64_t)(d + 1), int_type_sz,
                        ASR::integerbozType::Decimal));
                ASR::expr_t *dim_member = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_sym)),
                        orig_mem_ref, mv->m_type, nullptr));
                ASR::call_arg_t dim_carg;
                dim_carg.loc = loc;
                dim_carg.m_value = ASRUtils::EXPR(
                    ASR::make_ArraySize_t(al, loc, dim_member,
                        dim_expr, int_type_sz, nullptr));
                call_args.push_back(al, dim_carg);
            }
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
        for (auto &mem_entry :
                ASRUtils::collect_allocatable_array_members(st)) {
            const std::string &mem_name = mem_entry.first;
            ASR::Variable_t *mv = mem_entry.second;
            ASR::symbol_t *mem_sym = (ASR::symbol_t*)mv;
            ASR::ttype_t *mem_inner =
                ASRUtils::type_get_past_allocatable(mv->m_type);
            std::string data_name = GpuNames::member_data(
                sym_name, mem_name);
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
        ASR::Var_t *lv = down_cast<ASR::Var_t>(work.head(d).m_v);
        ASR::ttype_t *loop_var_type = ASRUtils::symbol_type(lv->m_v);
        std::string lvn = loop_var_names[d];
        gpu_new_variable(al, loc, kernel_scope, lvn,
            ASRUtils::duplicate_type(al, loop_var_type));
    }

    // Create local scalar temporaries in kernel scope
    for (auto &name : local_scalar_names) {
        auto it_orig = orig_scope->resolve_symbol(name);
        if (!it_orig) continue;
        ASR::ttype_t *type = ASRUtils::symbol_type(it_orig);
        ASR::symbol_t *type_decl = import_struct_type(it_orig,
            orig_scope, kernel_scope, loc);
        gpu_new_variable(al, loc, kernel_scope, name,
            ASRUtils::duplicate_type(al, type),
            ASR::intentType::Local, type_decl);
    }

    // Import functions/subroutines called in the loop body
    // into the kernel scope so FunctionCall/SubroutineCall nodes
    // can reference them after symbol remapping.
    // Collect transitively: if f() calls g(), both must be imported.
    {
        GpuFunctionCollector func_collector;
        for (size_t i = 0; i < work.n_body; i++) {
            func_collector.visit_stmt(*work.body[i]);
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
                        ASR::Function_t *fn_impl =
                            resolve_function_implementation(fn);
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
                                    lower_loaded_implied_do_loops(
                                        *res.result);
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
                            kernel_scope,
                            s2c(al, kernel_name));
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
                            ASR::symbol_t *kernel_method =
                                gpu_struct_lookup_member(kernel_struct,
                                    orig_name);
                            if (kernel_method) {
                                struct_name = struct_member_owner_name(
                                    kernel_method, struct_name);
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
                                            lower_loaded_implied_do_loops(
                                                *res.result);
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
                kernel_scope, s2c(al, kernel_name));
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
            // A local whose extent is written in terms of another
            // symbol of the same function -- `real :: r(self%m_ + 1)`
            // -- is copied before that symbol is, and the copy is then
            // left naming the original. Point every such extent at the
            // copy's own symbols.
            retarget_local_extents(func->m_symtab);
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
        dim_info.push_back({work.head(d).m_start, work.head(d).m_end});
    }

    // Deep-copy the body statements so that in-place symbol remapping
    // does not corrupt types shared with the original function scope
    // (e.g., ArrayBroadcast type sharing the same Array dimension Var
    // nodes as the original variable's type).
    ASRUtils::ExprStmtDuplicator body_dup(al);
    body_dup.success = true;
    Vec<ASR::stmt_t*> body_copy;
    body_copy.reserve(al, work.n_body);
    for (size_t i = 0; i < work.n_body; i++) {
        ASR::stmt_t *copy = body_dup.duplicate_stmt(work.body[i]);
        LCOMPILERS_ASSERT(copy);
        body_copy.push_back(al, copy);
    }

    // Turn every `size(a(i)%m, d)` in the copied body into a scalar
    // kernel argument the host computes at launch time.  The extent
    // of an allocatable component reached through a subscript into an
    // array of derived types is otherwise available to neither side:
    // the kernel is handed only that component's flattened data and
    // its per-element total size.  A workspace sized by such an
    // extent would be declined -- or, worse, sized by a guess -- so
    // it is resolved here into a plain integer parameter.  This has
    // to happen before the decomposition below rewrites the component
    // access into a flat-array Var, and while the body still names
    // the host symbols the launch site passes as actuals.

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
    kernel_body.reserve(al, work.n_body + 2 * n_dims + 1);

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
    // For an iteration space (i=1:m, j=1:n, k=1:p):
    //   flat = flat_idx
    //   i = flat % m + 1;  flat = flat / m
    //   j = flat % n + 1;  flat = flat / n
    //   k = flat + 1  (last dim)
    //   guard: flat_idx >= m*n*k → return

    // Create kernel-scope versions of start/end for each dimension.
    // Instead of duplicating host expressions (which may contain
    // ArrayBound/ArraySize on allocatable arrays that cannot be
    // correctly evaluated in the kernel scope), pass the loop
    // bounds as pre-computed scalar parameters from the host.
    std::vector<ASR::expr_t*> kernel_starts, kernel_ends;
    for (size_t d = 0; d < n_dims; d++) {
        ASR::expr_t *host_start = dim_info[d].host_start;
        ASR::expr_t *host_end = dim_info[d].host_end;
        bool start_is_const = ASR::is_a<ASR::IntegerConstant_t>(*host_start);
        bool end_is_const = ASR::is_a<ASR::IntegerConstant_t>(*host_end);

        if (start_is_const) {
            kernel_starts.push_back(dup_expr_to_scope(host_start, kernel_scope));
        } else {
            std::string name = "__loop_start_" + std::to_string(d);
            ASR::symbol_t *param = gpu_new_variable(al, loc, kernel_scope,
                name, ASRUtils::duplicate_type(al, int_type),
                ASR::intentType::InOut);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = host_start;
            call_args.push_back(al, carg);
            kernel_starts.push_back(
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
        }

        if (end_is_const) {
            kernel_ends.push_back(dup_expr_to_scope(host_end, kernel_scope));
        } else {
            std::string name = "__loop_end_" + std::to_string(d);
            ASR::symbol_t *param = gpu_new_variable(al, loc, kernel_scope,
                name, ASRUtils::duplicate_type(al, int_type),
                ASR::intentType::InOut);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = host_end;
            call_args.push_back(al, carg);
            kernel_ends.push_back(
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
        }
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
        gpu_new_variable(al, loc, kernel_scope, remain_name,
            ASRUtils::duplicate_type(al, int_type));
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
        // A BLOCK-local derived type is not a kernel argument, so
        // involved_syms never imported it. Bring the Struct into the
        // kernel and retarget the local's type_declaration: the
        // kernel's parent is the translation unit, which cannot see
        // the host procedure's types.
        {
            std::function<void(SymbolTable*)> import_scope_structs =
                [&](SymbolTable *st) {
                for (auto &item : st->get_scope()) {
                    if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                        import_struct_type(item.second, orig_scope,
                            kernel_scope, loc);
                    } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
                        import_scope_structs(
                            ASR::down_cast<ASR::Block_t>(
                                item.second)->m_symtab);
                    } else if (ASR::is_a<ASR::AssociateBlock_t>(
                            *item.second)) {
                        import_scope_structs(
                            ASR::down_cast<ASR::AssociateBlock_t>(
                                item.second)->m_symtab);
                    }
                }
            };
            import_scope_structs(block->m_symtab);
            fixup_struct_refs_in_scope(block->m_symtab, kernel_scope,
                s2c(al, kernel_name));
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
            std::set<std::string> loop_index_names;
            for (size_t d = 0; d < work.n_heads(); d++) {
                if (!work.head(d).m_v) continue;
                if (!ASR::is_a<ASR::Var_t>(*work.head(d).m_v)) continue;
                loop_index_names.insert(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(work.head(d).m_v)->m_v));
            }
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
                        // The host evaluates this expression before
                        // it launches, where the loop index has no
                        // value -- so a `size(f(i, ...))` must be
                        // taken from the shape in its own type
                        // rather than by calling `f` on the host.
                        ASR::expr_t *host_expr =
                            gpu_simplify_array_sizes(al,
                                dim_dup.duplicate_expr(
                                    *dim_ptrs[e]));
                        gpu_check_host_expr_index_free(host_expr,
                            loop_index_names,
                            "the extent of '" +
                                std::string(bvar->m_name) + "'");
                        std::string pname =
                            kernel_scope->get_unique_name(
                                "__lfortran_gpu_dim_", false);
                        ASR::ttype_t *ptype =
                            ASRUtils::duplicate_type(al,
                                ASRUtils::expr_type(
                                    *dim_ptrs[e]));
                        ASR::symbol_t *psym = gpu_new_variable(al, loc,
                            kernel_scope, pname, ptype,
                            ASR::intentType::InOut);
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
            // Also decompose StructInstanceMember references in
            // block-local variable type expressions (e.g., VLA
            // dimensions like size(self%x) after associate
            // resolution). Without this, a fully-decomposed struct
            // removed from involved_syms leaves dangling Var refs.
            GpuDecomposeStructReplacer block_type_decomp(al,
                kernel_scope, decomp_map);
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
                    if (arr->m_dims[d].m_start) {
                        block_type_decomp.current_expr =
                            &(arr->m_dims[d].m_start);
                        block_type_decomp.replace_expr(
                            arr->m_dims[d].m_start);
                    }
                    if (arr->m_dims[d].m_length) {
                        block_type_decomp.current_expr =
                            &(arr->m_dims[d].m_length);
                        block_type_decomp.replace_expr(
                            arr->m_dims[d].m_length);
                    }
                }
            }
        }
        // Recursively process nested BlockCall statements
        for (size_t j = 0; j < block->n_body; j++) {
            if (ASR::is_a<ASR::BlockCall_t>(*block->m_body[j])) {
                ASR::BlockCall_t *inner_bc =
                    ASR::down_cast<ASR::BlockCall_t>(block->m_body[j]);
                if (ASR::is_a<ASR::Block_t>(*inner_bc->m_m)) {
                    ASR::Block_t *inner =
                        ASR::down_cast<ASR::Block_t>(inner_bc->m_m);
                    std::string inner_name = inner->m_name;
                    bool host_owned = orig_scope->get_symbol(inner_name)
                        == inner_bc->m_m;
                    process_block_for_kernel(inner, host_owned);
                    if (host_owned) {
                        orig_scope->erase_symbol(inner_name);
                        if (!kernel_scope->get_symbol(inner_name)) {
                            kernel_scope->add_symbol(inner_name,
                                inner_bc->m_m);
                        }
                    }
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
    // loop body) into the kernel scope.
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
                    // Only a copy this pass made is safe to take. A
                    // BlockCall that still names the host's own block
                    // is a copy_loop_stmt miss; mutating it here
                    // would steal the host nest if the launch is then
                    // declined.
                    bool is_copy = false;
                    for (ASR::symbol_t *b : kernel_blocks) {
                        if (b == bc->m_m) {
                            is_copy = true;
                            break;
                        }
                    }
                    if (!is_copy) continue;
                    process_block_for_kernel(block, true);
                    if (orig_scope->get_symbol(block_name) == bc->m_m) {
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

    {
        std::vector<std::string> kernel_arg_names;
        for (size_t i = 0; i < kernel_args.n; i++) {
            kernel_arg_names.push_back(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(kernel_args.p[i])->m_v));
        }
        std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>>
            member_extent_args;
        GpuStructArrayMemberExtentVisitor mev(al, orig_scope,
            kernel_scope, kernel_arg_names, member_extent_args);
        for (size_t i = 0; i < body_copy.n; i++) {
            mev.visit_stmt(*body_copy.p[i]);
        }
        member_extent_undo = mev.replacer.undo;
        for (auto &pair : member_extent_args) {
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, pair.first)));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = pair.second;
            call_args.push_back(al, carg);
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
                arr->m_physical_type, arr->m_memory_space));
        }
        arg_types.push_back(al, t);
    }
    ASR::ttype_t *fn_sig = ASRUtils::TYPE(
        ASR::make_FunctionType_t(al, loc,
            arg_types.p, arg_types.n, nullptr,
            ASR::abiType::Source, ASR::deftypeType::Implementation,
            nullptr, false, false, false, false, false, nullptr, 0, false,
            ASR::exec_spaceType::Kernel));

    // 6. Create the kernel as a Kernel function. A kernel is
    // subroutine-shaped, so it has no return variable, and `fn_sig` is
    // built above rather than by `make_Function_t_util` because the
    // argument types must not carry scope-bound dimension expressions.
    ASR::asr_t *kernel_func = ASR::make_Function_t(al, loc,
        kernel_scope, s2c(al, kernel_name), fn_sig,
        nullptr, 0,
        kernel_args.p, kernel_args.n,
        kernel_body.p, kernel_body.n,
        nullptr, ASR::accessType::Public, false, false,
        nullptr, nullptr, nullptr);

    // `device_launch_expand` builds the host side of the launch, laying
    // every argument out exactly as the device code generator does. An
    // argument shape it cannot lay out keeps the loop on the host, where
    // ordinary Fortran semantics always apply. The kernel is checked
    // before it enters the symbol table, so nothing is left behind.
    {
        GpuDecline decline;
        if (!gpu_launch_is_supported(al,
                ASR::down_cast<ASR::symbol_t>(kernel_func),
                call_args.p, call_args.n, decline)) {
            report_not_offloaded(loc, decline);
            for (auto it = member_extent_undo.rbegin();
                    it != member_extent_undo.rend(); ++it) {
                *it->first = it->second;
            }
            member_extent_undo.clear();
            // The host's own blocks were never touched: the kernel was
            // given copies, which the draft guard drops.
            return;
        }
    }
    // The launch stands, so the blocks, the spliced shape, and the
    // kernel number are the kernel's from here on.
    draft_guard.commit();
    splice_guard.commit();

    // The loop is offloaded from here on, so this is where a clause the
    // launch cannot honour is reported: before this every exit still
    // leaves the loop on the host, where the clause is honoured.
    for (size_t i = 0; i < region.n_clauses; i++) {
        std::string clause_name = unhonoured_clause(region.m_clauses[i]);
        if (!clause_name.empty()) {
            report_clause_ignored(region.m_clauses[i]->base.loc,
                clause_name);
        }
    }

    tu_symtab->add_symbol(kernel_name,
        ASR::down_cast<ASR::symbol_t>(kernel_func));

    // Pre-allocate host-side allocatable arrays that are assigned
    // from a FunctionCall inside the loop body. The GPU
    // kernel receives the buffer pointer at launch time, so the
    // array must already be allocated on the host before dispatch.
    Vec<ASR::stmt_t*> pre_launch_stmts;
    pre_launch_stmts.reserve(al, 4);
    for (size_t si = 0; si < work.n_body; si++) {
        ASR::stmt_t *stmt = work.body[si];
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
                    host_arg.m_codims = nullptr;
                    host_arg.n_codims = 0;

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

    // 7. Replace the region with GpuKernelLaunch + GpuSync
    // Collect all launch-related statements into a temporary Vec.
    // If any involved variable is optional, wrap them in a
    // present() guard so the host never reads a null descriptor.
    gather_guard.commit();
    Vec<ASR::stmt_t*> launch_stmts;
    launch_stmts.reserve(al, gather_stmts.n + pre_launch_stmts.n
        + scatter_stmts.n + liveout_scalars.size() + 2
        + liveout_scalars.size());
    for (size_t gi = 0; gi < gather_stmts.n; gi++) {
        launch_stmts.push_back(al, gather_stmts.p[gi]);
    }
    for (size_t pi = 0; pi < pre_launch_stmts.n; pi++) {
        launch_stmts.push_back(al, pre_launch_stmts.p[pi]);
    }

    // Copy liveout scalars into their 1-element array buffers
    // before the kernel launch so the buffer has the initial value
    for (auto &ls : liveout_scalars) {
        ASR::expr_t *buf_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.host_buf_sym));
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *idx_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));
        Vec<ASR::array_index_t> ai_args;
        ai_args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = idx_one;
        ai.m_step = nullptr;
        ai_args.push_back(al, ai);
        ASR::expr_t *buf_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, buf_var,
                ai_args.p, 1, ls.scalar_type,
                ASR::arraystorageType::ColMajor, nullptr));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, buf_item, scalar_var,
                nullptr, false, false)));
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

    launch_stmts.push_back(al, ASRUtils::STMT(
        ASR::make_GpuKernelLaunch_t(al, loc,
            ASR::down_cast<ASR::symbol_t>(kernel_func),
            grid_size, block_size_const,
            call_args.p, call_args.n)));

    launch_stmts.push_back(al, ASRUtils::STMT(
        ASR::make_GpuSync_t(al, loc)));

    // Put every gathered element the kernel wrote into back over the
    // original, before anything on the host can read it again.
    for (size_t si = 0; si < scatter_stmts.n; si++) {
        launch_stmts.push_back(al, scatter_stmts.p[si]);
    }

    // Copy liveout scalar results back from the 1-element array
    // buffers after the kernel has completed
    for (auto &ls : liveout_scalars) {
        ASR::expr_t *buf_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.host_buf_sym));
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *idx_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));
        Vec<ASR::array_index_t> ai_args;
        ai_args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = idx_one;
        ai.m_step = nullptr;
        ai_args.push_back(al, ai);
        ASR::expr_t *buf_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, buf_var,
                ai_args.p, 1, ls.scalar_type,
                ASR::arraystorageType::ColMajor, nullptr));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, scalar_var, buf_item,
                nullptr, false, false)));
    }

    // If any involved variable is optional, wrap the whole kernel
    // launch block in if(present(v1) .and. present(v2) ...) so
    // the host never tries to read a null descriptor or compute
    // ArraySize on an absent argument.
    if (!optional_syms.empty()) {
        ASR::ttype_t *log_type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, loc, 4));
        ASR::expr_t *guard = nullptr;
        for (ASR::symbol_t *opt_sym : optional_syms) {
            Vec<ASR::expr_t*> present_args;
            present_args.reserve(al, 1);
            present_args.push_back(al, ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, opt_sym)));
            ASR::expr_t *present_call = ASRUtils::EXPR(
                ASR::make_IntrinsicElementalFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicElementalFunctions::Present),
                    present_args.p, present_args.n, 0,
                    log_type, nullptr));
            if (guard == nullptr) {
                guard = present_call;
            } else {
                guard = ASRUtils::EXPR(
                    ASR::make_LogicalBinOp_t(al, loc, guard,
                        ASR::logicalbinopType::And, present_call,
                        log_type, nullptr));
            }
        }
        Vec<ASR::stmt_t*> empty_else;
        empty_else.reserve(al, 0);
        pass_result.reserve(al, 1);
        pass_result.push_back(al, ASRUtils::STMT(
            ASR::make_If_t(al, loc, nullptr, guard,
                launch_stmts.p, launch_stmts.n,
                empty_else.p, empty_else.n)));
    } else {
        pass_result.reserve(al, launch_stmts.n);
        for (size_t i = 0; i < launch_stmts.n; i++) {
            pass_result.push_back(al, launch_stmts.p[i]);
        }
    }
}

// A loop the offload pass turned down is still a parallel loop, so it goes
// back to whoever else can run it rather than to a single thread by default.
class DeclinedLoopVisitor : public ASR::BaseWalkVisitor<DeclinedLoopVisitor>
{
public:
    const PassOptions &pass_options;

    DeclinedLoopVisitor(const PassOptions &pass_options_) :
        pass_options(pass_options_) {
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        ASR::OMPRegion_t &xx = const_cast<ASR::OMPRegion_t&>(x);
        if (xx.m_exec_target == ASR::exec_targetType::ExecDevice) {
            xx.m_exec_target = host_exec_target(pass_options);
        }
        ASR::BaseWalkVisitor<DeclinedLoopVisitor>::visit_OMPRegion(x);
    }
};

void pass_replace_gpu_offload(Allocator &al, ASR::TranslationUnit_t &unit,
                              const LCompilers::PassOptions& pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) return;
    GpuOffloadVisitor v(al, pass_options, unit);
    v.asr_changed = true;
    while (v.asr_changed) {
        v.asr_changed = false;
        v.mark_regions_device_code_runs();
        v.visit_TranslationUnit(unit);
    }
    DeclinedLoopVisitor d(pass_options);
    d.visit_TranslationUnit(unit);
    // Kernel extraction moves Block symbols out of their enclosing
    // function, which can leave stale entries in that function's
    // dependency list. Recompute all dependencies to fix this.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
