#include <cstring>
#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/intent_out_deallocate.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>


namespace LCompilers {
// Deallocate allocatable `intent(out)` dummy arguments at function entry.
//
// Notes / limitations:
//
// - This pass only inserts an explicit `deallocate()` statement guarded by
//   `allocated()` (and additionally by `present()` for optional dummies).
//   Correct finalization / deep deallocation semantics are handled downstream by
//   the runtime/codegen and are currently incomplete in some cases (#9097).
//
// - It only considers dummy arguments that appear as `Var` entries in
//   `Function_t::m_args` and are `allocatable` with `intent(out)`. It does not
//   handle pointers, components, or more complex argument expressions.
//
// - We intentionally skip compiler-generated intrinsic implementations
//   (`deftype == Implementation`) to avoid changing their internal ownership
//   conventions.
static inline bool variable_is_converted_function_result(
        const ASR::Variable_t* v) {
    static const char* kMarker = "__lcompilers_marker_was_function_result";
    for (size_t i = 0; i < v->n_dependencies; i++) {
        if (v->m_dependencies[i] && std::strcmp(v->m_dependencies[i],
                kMarker) == 0) {
            return true;
        }
    }
    return false;
}

class IntentOutDeallocateVisitor : public ASR::BaseWalkVisitor<IntentOutDeallocateVisitor>
{
    Allocator &al;

    void emit_struct_cleanup_stmts(
            ASR::expr_t* struct_expr,
            ASR::Struct_t* struct_type,
            SymbolTable* current_scope,
            const Location& loc,
            ASR::ttype_t* logical_type,
            Vec<ASR::stmt_t*>& out_stmts) {
        ASR::Struct_t* st = struct_type;
        while (st != nullptr) {
            for (auto& m : st->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*m.second)) continue;
                ASR::Variable_t* m_var = ASR::down_cast<ASR::Variable_t>(
                    m.second);
                ASR::ttype_t* m_type = m_var->m_type;

                ASR::expr_t* member_expr = ASRUtils::EXPR(
                    ASRUtils::getStructInstanceMember_t(al, loc,
                        (ASR::asr_t*)struct_expr, m.second,
                        m.second, current_scope));

                if (ASRUtils::is_allocatable(m_type)) {
                    Vec<ASR::expr_t*> alloc_args;
                    alloc_args.reserve(al, 1);
                    alloc_args.push_back(al, member_expr);
                    ASR::expr_t* is_alloc = ASRUtils::EXPR(
                        ASR::make_IntrinsicImpureFunction_t(al, loc,
                            static_cast<int64_t>(
                                ASRUtils::IntrinsicImpureFunctions::Allocated),
                            alloc_args.p, alloc_args.n, 0, logical_type,
                            nullptr));

                    Vec<ASR::expr_t*> dealloc_args;
                    dealloc_args.reserve(al, 1);
                    dealloc_args.push_back(al, member_expr);
                    ASR::stmt_t* dealloc_stmt = ASRUtils::STMT(
                        ASR::make_ExplicitDeallocate_t(al, loc,
                            dealloc_args.p, dealloc_args.n));

                    Vec<ASR::stmt_t*> if_body;
                    if_body.reserve(al, 1);
                    if_body.push_back(al, dealloc_stmt);
                    ASR::stmt_t* if_stmt = ASRUtils::STMT(ASR::make_If_t(
                        al, loc, nullptr, is_alloc, if_body.p, if_body.n,
                        nullptr, 0));
                    out_stmts.push_back(al, if_stmt);
                    continue;
                }

                ASR::ttype_t* element_t = ASRUtils::type_get_past_array(
                    m_type);
                if (!ASR::is_a<ASR::StructType_t>(*element_t)) continue;
                if (!m_var->m_type_declaration) continue;
                ASR::symbol_t* m_decl_sym = ASRUtils::symbol_get_past_external(
                    m_var->m_type_declaration);
                if (!ASR::is_a<ASR::Struct_t>(*m_decl_sym)) continue;
                ASR::Struct_t* m_struct = ASR::down_cast<ASR::Struct_t>(
                    m_decl_sym);

                if (ASRUtils::is_array(m_type)) {
                    int n_dims = ASRUtils::extract_n_dims_from_ttype(m_type);
                    emit_array_of_struct_cleanup_stmts(member_expr, m_struct,
                        n_dims, current_scope, loc, logical_type, out_stmts);
                } else {
                    emit_struct_cleanup_stmts(member_expr, m_struct,
                        current_scope, loc, logical_type, out_stmts);
                }
            }
            if (st->m_parent != nullptr) {
                st = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(st->m_parent));
            } else {
                st = nullptr;
            }
        }
    }
    
    void emit_array_of_struct_cleanup_stmts(
            ASR::expr_t* arr_expr,
            ASR::Struct_t* struct_type,
            int n_dims,
            SymbolTable* current_scope,
            const Location& loc,
            ASR::ttype_t* logical_type,
            Vec<ASR::stmt_t*>& out_stmts) {
        Vec<ASR::expr_t*> idx_vars;
        PassUtils::create_idx_vars(idx_vars, n_dims, loc, al, current_scope,
            "_intent_out_dealloc_idx_");

        ASR::expr_t* arr_ref = PassUtils::create_array_ref(arr_expr,
            idx_vars, al, current_scope);

        Vec<ASR::stmt_t*> innermost_body;
        innermost_body.reserve(al, 1);
        emit_struct_cleanup_stmts(arr_ref, struct_type, current_scope,
            loc, logical_type, innermost_body);

        if (innermost_body.size() == 0) return;

        Vec<ASR::stmt_t*> current_body = innermost_body;
        for (int d = 0; d < n_dims; d++) {
            ASR::do_loop_head_t head;
            head.m_v = idx_vars[d];
            head.m_start = PassUtils::get_bound(arr_expr, d + 1, "lbound",
                al, 4);
            head.m_end = PassUtils::get_bound(arr_expr, d + 1, "ubound",
                al, 4);
            head.m_increment = nullptr;
            head.loc = loc;

            ASR::stmt_t* doloop = ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr, head, current_body.p,
                    current_body.size(), nullptr, 0));
            Vec<ASR::stmt_t*> init_and_while = PassUtils::replace_doloop(al,
                *ASR::down_cast<ASR::DoLoop_t>(doloop), -1, false,
                current_scope);

            current_body.reserve(al, init_and_while.size());
            current_body.n = 0;
            for (size_t k = 0; k < init_and_while.size(); k++) {
                current_body.push_back(al, init_and_while[k]);
            }
        }

        for (size_t i = 0; i < current_body.size(); i++) {
            out_stmts.push_back(al, current_body[i]);
        }
    }

    // Helper: Wrap statement in optional presence check if needed
    ASR::stmt_t* wrap_optional_check(Location loc, ASR::expr_t* var_expr,
                                      ASR::presenceType presence, ASR::stmt_t* stmt_to_wrap) {
        if (presence != ASR::presenceType::Optional) {
            return stmt_to_wrap;
        }

        // Create present(var_expr) check
        ASR::ttype_t* logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));  // 4 = default logical kind
        Vec<ASR::expr_t*> present_args;
        present_args.reserve(al, 1);
        present_args.push_back(al, var_expr);

        ASR::expr_t* is_present = ASRUtils::EXPR(ASR::make_IntrinsicElementalFunction_t(
            al, loc,
            static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Present),
            present_args.p, present_args.n, 0, logical_type, nullptr));

        // Wrap stmt_to_wrap in: if (present(var_expr)) then stmt_to_wrap end if
        Vec<ASR::stmt_t*> present_body;
        present_body.reserve(al, 1);
        present_body.push_back(al, stmt_to_wrap);

        return ASRUtils::STMT(ASR::make_If_t(
            al, loc, nullptr, is_present, present_body.p, present_body.n, nullptr, 0));
    }

public:
    IntentOutDeallocateVisitor(Allocator& al_) : al(al_) {}

    void visit_Function(const ASR::Function_t &x) {
        ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(&x);
        if (func_type->m_abi == ASR::abiType::ExternalUndefined) {
            return;
        }
        // Skip compiler-generated intrinsic implementations
        // These functions handle their own intent(out) allocatable deallocation internally
        // We identify them by:
        // 1. Function name starts with "_lcompilers_" or "__libasr_created__", OR
        // 2. deftype == Implementation AND parent module is lfortran_intrinsic_*
        std::string func_name = x.m_name;
        bool is_compiler_generated =
            func_name.rfind("_lcompilers_", 0) == 0 ||
            func_name.rfind("__libasr_created__", 0) == 0;
        if (!is_compiler_generated && func_type->m_deftype == ASR::deftypeType::Implementation) {
            ASR::asr_t* parent = x.m_symtab->parent->asr_owner;
            if (parent && ASR::is_a<ASR::symbol_t>(*parent) &&
                    ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(parent))) {
                std::string mod_name = ASR::down_cast<ASR::Module_t>(
                    ASR::down_cast<ASR::symbol_t>(parent))->m_name;
                if (mod_name.rfind("lfortran_intrinsic_", 0) == 0) {
                    is_compiler_generated = true;
                }
            }
        }
        if (is_compiler_generated) {
            for (auto &a : x.m_symtab->get_scope()) {
                visit_symbol(*a.second);
            }
            return;
        }
        ASR::Function_t &xx = const_cast<ASR::Function_t&>(x);

        // Collect intent(out) allocatable arguments
        Vec<ASR::stmt_t*> dealloc_stmts;
        dealloc_stmts.reserve(al, 1);

        for (size_t i = 0; i < xx.n_args; i++) {
            ASR::expr_t* arg_expr = xx.m_args[i];
            if (!ASR::is_a<ASR::Var_t>(*arg_expr)) continue;

            ASR::symbol_t* arg_sym = ASR::down_cast<ASR::Var_t>(arg_expr)->m_v;
            ASR::symbol_t* arg_sym_deref = ASRUtils::symbol_get_past_external(arg_sym);
            if (!ASR::is_a<ASR::Variable_t>(*arg_sym_deref)) continue;
            ASR::Variable_t* arg_var = ASR::down_cast<ASR::Variable_t>(arg_sym_deref);

            // Check if intent(out) and (allocatable or struct with allocatable components)
            if (arg_var->m_intent != ASR::intentType::Out) continue;
            bool is_array_of_struct =
                !ASRUtils::is_allocatable(arg_var->m_type) &&
                ASRUtils::is_array(arg_var->m_type) &&
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::type_get_past_array(arg_var->m_type));
            if (!ASRUtils::is_allocatable(arg_var->m_type) &&
                !ASR::is_a<ASR::StructType_t>(*arg_var->m_type) &&
                !is_array_of_struct) continue;

            // Skip if this is the function's return variable (used in intrinsic implementations)
            if (xx.m_return_var && ASR::is_a<ASR::Var_t>(*xx.m_return_var)) {
                ASR::symbol_t* return_sym = ASR::down_cast<ASR::Var_t>(xx.m_return_var)->m_v;
                if (arg_sym == return_sym) continue;
            }

            Location loc = arg_var->base.base.loc;
            ASR::ttype_t* logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));  // 4 = default logical kind

            // Handle allocatable arguments (scalars, arrays, or structs)
            // CRITICAL: If the struct itself is allocatable, deallocate it as a whole.
            // Deep deallocation of nested components is handled by runtime/codegen.
            // DO NOT manually deallocate components here - that would be use-after-free!
            if (ASRUtils::is_allocatable(arg_var->m_type)) {
                // Create: if (allocated(arg)) deallocate(arg)
                ASR::expr_t* var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg_sym));

                // Create Allocated check
                Vec<ASR::expr_t*> allocated_args;
                allocated_args.reserve(al, 1);
                allocated_args.push_back(al, var_expr);

                ASR::expr_t* is_allocated = ASRUtils::EXPR(ASR::make_IntrinsicImpureFunction_t(
                    al, loc,
                    static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated),
                    allocated_args.p, allocated_args.n, 0, logical_type, nullptr));

                // Create Deallocate statement
                Vec<ASR::expr_t*> dealloc_args;
                dealloc_args.reserve(al, 1);
                dealloc_args.push_back(al, var_expr);
                ASR::stmt_t* dealloc_stmt = ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
                    al, loc, dealloc_args.p, dealloc_args.n));

                // Create If statement: if (allocated(arg)) deallocate(arg)
                Vec<ASR::stmt_t*> if_body;
                if_body.reserve(al, 1);
                if_body.push_back(al, dealloc_stmt);
                ASR::stmt_t* if_stmt = ASRUtils::STMT(ASR::make_If_t(
                    al, loc, nullptr, is_allocated, if_body.p, if_body.n, nullptr, 0));

                // Wrap in optional presence check if needed
                ASR::stmt_t* wrapped_stmt = wrap_optional_check(loc, var_expr, arg_var->m_presence, if_stmt);
                dealloc_stmts.push_back(al, wrapped_stmt);
            } else if (ASR::is_a<ASR::StructType_t>(*arg_var->m_type)) {
                // Handle non-allocatable StructType arguments with allocatable components
                // (If the struct itself is allocatable, we already handled it above)
                ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(arg_var->m_type_declaration));

                if (variable_is_converted_function_result(arg_var)) {
                    continue;
                }

                // Call user-defined FINAL procedures for non-allocatable
                // intent(out) struct args (Fortran 2018 §7.5.6.3 ¶7):
                //   "When a procedure is invoked with a nonpointer,
                //    nonallocatable, INTENT(OUT) dummy argument of a type
                //    for which a final subroutine is defined, the
                //    finalization occurs before the procedure body executes."
                if (struct_type->n_member_functions > 0) {
                    ASR::expr_t* var_expr = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, arg_sym));
                    for (size_t fi = 0; fi < struct_type->n_member_functions; fi++) {
                        std::string final_proc_name =
                            struct_type->m_member_functions[fi];
                        ASR::symbol_t* final_sym =
                            struct_type->m_symtab->parent->get_symbol(
                                final_proc_name);
                        if (!final_sym) continue;

                        // Ensure the FINAL procedure is accessible from the
                        // function's own symbol table; create an ExternalSymbol
                        // if one does not already exist.
                        ASR::symbol_t* local_final_sym =
                            xx.m_symtab->resolve_symbol(final_proc_name);
                        if (!local_final_sym) {
                            std::string module_name = "";
                            ASR::asr_t* owner =
                                struct_type->m_symtab->parent->asr_owner;
                            if (owner &&
                                ASR::is_a<ASR::symbol_t>(*owner)) {
                                module_name = ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::symbol_t>(owner));
                            }
                            ASR::asr_t* ext = ASR::make_ExternalSymbol_t(
                                al, loc, xx.m_symtab,
                                s2c(al, final_proc_name), final_sym,
                                s2c(al, module_name), nullptr, 0,
                                s2c(al, final_proc_name),
                                ASR::accessType::Private);
                            xx.m_symtab->add_symbol(final_proc_name,
                                ASR::down_cast<ASR::symbol_t>(ext));
                            local_final_sym =
                                ASR::down_cast<ASR::symbol_t>(ext);
                        }

                        Vec<ASR::call_arg_t> call_args;
                        call_args.reserve(al, 1);
                        ASR::call_arg_t call_arg;
                        call_arg.loc = loc;
                        call_arg.m_value = var_expr;
                        call_args.push_back(al, call_arg);

                        ASR::stmt_t* call_stmt = ASRUtils::STMT(
                            ASR::make_SubroutineCall_t(
                                al, loc, local_final_sym, local_final_sym,
                                call_args.p, call_args.n, nullptr, false));

                        ASR::stmt_t* wrapped_stmt = wrap_optional_check(
                            loc, var_expr, arg_var->m_presence, call_stmt);
                        dealloc_stmts.push_back(al, wrapped_stmt);
                    }
                }

                // collect deallocation statements for all allocatable
                // components nested at any depth within the struct hierarchy are handled recursively.
                // @param parent_expr is the base access expression (e.g., arg, arg%member, etc.)
                // @param parent_sym is the symbol (structSymbol) corresponding to parent_expr
                // @param self pass self lambda to recurise call.
                // TODO : Clean into regular function
                auto collect_deallocs_of_struct = [&](auto self,
                    ASR::Struct_t* st, ASR::expr_t* parent_expr, ASR::symbol_t* parent_sym) -> void {
                    ASR::Struct_t* current_st = st;
                    SymbolTable* sym_table = current_st->m_symtab;
                    while (sym_table != nullptr) {
                        for (auto& struct_member : sym_table->get_scope()) {
                            if (!ASR::is_a<ASR::Variable_t>(*struct_member.second)) continue;

                int n_dims = ASRUtils::extract_n_dims_from_ttype(arg_var->m_type);

                ASR::expr_t* var_expr_full = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, arg_sym));

                Vec<ASR::stmt_t*> cleanup;
                cleanup.reserve(al, 1);
                emit_array_of_struct_cleanup_stmts(var_expr_full, struct_type,
                    n_dims, xx.m_symtab, loc, logical_type, cleanup);

                if (cleanup.size() > 0) {
                    ASR::stmt_t* wrapper_block = nullptr;
                    if (cleanup.size() == 1) {
                        wrapper_block = cleanup[0];
                    } else {
                        ASR::expr_t* true_cond = ASRUtils::EXPR(
                            ASR::make_LogicalConstant_t(al, loc, true,
                                logical_type));
                        wrapper_block = ASRUtils::STMT(ASR::make_If_t(al, loc,
                            nullptr, true_cond, cleanup.p, cleanup.size(),
                            nullptr, 0));
                    }

                    ASR::stmt_t* wrapped_stmt = wrap_optional_check(
                        loc, var_expr_full, arg_var->m_presence, wrapper_block);
                    dealloc_stmts.push_back(al, wrapped_stmt);
                }
            }
        }

        // Prepend deallocation statements to function body
        if (dealloc_stmts.size() > 0) {
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, dealloc_stmts.size() + xx.n_body);
            for (size_t i = 0; i < dealloc_stmts.size(); i++) {
                new_body.push_back(al, dealloc_stmts[i]);
            }
            for (size_t i = 0; i < xx.n_body; i++) {
                new_body.push_back(al, xx.m_body[i]);
            }
            xx.m_body = new_body.p;
            xx.n_body = new_body.size();
        }

        // Continue visiting nested functions
        for (auto &a : x.m_symtab->get_scope()) {
            visit_symbol(*a.second);
        }
    }
};


void pass_intent_out_deallocate(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &/*pass_options*/) {
    // Deallocate intent(out) allocatable arguments at function entry
    IntentOutDeallocateVisitor iod(al);
    iod.visit_TranslationUnit(unit);

    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
