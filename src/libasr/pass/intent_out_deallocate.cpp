#include <set>

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
class IntentOutDeallocateVisitor : public ASR::BaseWalkVisitor<IntentOutDeallocateVisitor>
{
    Allocator &al;
    static bool struct_hierarchy_has_finalizer(ASR::Struct_t* st) {
        while (st != nullptr) {
            if (st->n_member_functions > 0) return true;
            if (st->m_parent == nullptr) return false;
            st = ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(st->m_parent));
        }
        return false;
    }
    // The derived type a scalar component is declared with, or nullptr when
    // the component is not one.  `StructType_t` on its own is not enough:
    // allocatable, pointer and array components wrap the type.
    static ASR::Struct_t* component_struct_type(ASR::Variable_t* m_var) {
        if (ASRUtils::is_array(m_var->m_type)) return nullptr;
        if (!ASR::is_a<ASR::StructType_t>(*m_var->m_type)) return nullptr;
        if (m_var->m_type_declaration == nullptr) return nullptr;
        ASR::symbol_t* decl_sym = ASRUtils::symbol_get_past_external(
            m_var->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*decl_sym)) return nullptr;
        return ASR::down_cast<ASR::Struct_t>(decl_sym);
    }

    // The type's components in the order a struct constant's arguments follow
    // them, which is the inherited ones first.  A name that does not resolve
    // is still recorded, so that the position of every later component stays
    // right; placing a value on it is refused later.  `visited` guards
    // against a malformed cyclic parent chain.
    static void flatten_members(ASR::Struct_t* dt,
            std::vector<ASR::symbol_t*>& members,
            std::set<ASR::Struct_t*>& visited) {
        if (visited.find(dt) != visited.end()) {
            return;
        }
        visited.insert(dt);
        if (dt->m_parent != nullptr) {
            ASR::symbol_t* parent = ASRUtils::symbol_get_past_external(
                dt->m_parent);
            if (ASR::is_a<ASR::Struct_t>(*parent)) {
                flatten_members(ASR::down_cast<ASR::Struct_t>(parent), members,
                    visited);
            }
        }
        for (size_t i = 0; i < dt->n_members; i++) {
            members.push_back(dt->m_symtab->get_symbol(dt->m_members[i]));
        }
    }

    // Assign `value` to `target`, returning false when it cannot be done here.
    // A struct constant is assigned member by member: this pass runs after the
    // passes that would have lowered such a node, so a whole-struct assignment
    // reaches a backend that never sees one otherwise.  The assignment is by
    // position, so it is only emitted when the arguments really do line up
    // with the components, which they do not for every spelling an extended
    // type can be written with (see #13302).  Nothing at all is emitted then,
    // which leaves the component as the caller left it rather than emitting a
    // node no backend can lower.
    bool emit_value_assignment(
            ASR::expr_t* target,
            ASR::expr_t* value,
            SymbolTable* current_scope,
            const Location& loc,
            Vec<ASR::stmt_t*>& out_stmts) {
        if (!ASR::is_a<ASR::StructConstant_t>(*value)) {
            out_stmts.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
                al, loc, target, value, nullptr, false, false)));
            return true;
        }
        ASR::StructConstant_t* sc = ASR::down_cast<ASR::StructConstant_t>(
            value);
        ASR::symbol_t* dt_sym = ASRUtils::symbol_get_past_external(
            sc->m_dt_sym);
        if (!ASR::is_a<ASR::Struct_t>(*dt_sym)) {
            return false;
        }
        std::vector<ASR::symbol_t*> members;
        std::set<ASR::Struct_t*> visited;
        flatten_members(ASR::down_cast<ASR::Struct_t>(dt_sym), members,
            visited);
        // asr_verify requires the same equality of a struct constant, so this
        // only rejects ASR that would not verify anyway.
        if (members.size() != sc->n_args) {
            return false;
        }
        // Built separately so that a partial expansion is discarded rather
        // than left behind when a later argument cannot be placed.
        // A hint only: an argument that is itself a struct constant expands
        // to more than one statement, and `Vec` grows on its own.
        Vec<ASR::stmt_t*> expanded;
        expanded.reserve(al, sc->n_args);
        for (size_t i = 0; i < sc->n_args; i++) {
            ASR::expr_t* arg = sc->m_args[i].m_value;
            // Refuse the whole constant rather than placing the rest of it:
            // a partial expansion is what this is built to avoid.
            if (arg == nullptr) {
                return false;
            }
            if (members[i] == nullptr ||
                    !ASR::is_a<ASR::Variable_t>(*members[i])) {
                return false;
            }
            ASR::ttype_t* member_type = ASRUtils::symbol_type(members[i]);
            // `check_equal_type` compares the element types, so the ranks are
            // compared here: an array component given a scalar would otherwise
            // be assigned element-wise after the array passes have run.
            if (ASRUtils::extract_n_dims_from_ttype(member_type) !=
                    ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::expr_type(arg))) {
                return false;
            }
            if (!ASRUtils::check_equal_type(member_type,
                    ASRUtils::expr_type(arg), nullptr, arg)) {
                return false;
            }
            ASR::expr_t* member_expr = ASRUtils::EXPR(
                ASRUtils::getStructInstanceMember_t(al, loc,
                    (ASR::asr_t*)target, members[i], members[i],
                    current_scope));
            if (!emit_value_assignment(member_expr, arg, current_scope, loc,
                    expanded)) {
                return false;
            }
        }
        for (size_t i = 0; i < expanded.size(); i++) {
            out_stmts.push_back(al, expanded[i]);
        }
        return true;
    }

    void emit_struct_default_init_stmts(
            ASR::expr_t* struct_expr,
            ASR::Struct_t* struct_type,
            SymbolTable* current_scope,
            const Location& loc,
            Vec<ASR::stmt_t*>& out_stmts) {
        ASR::Struct_t* st = struct_type;
        while (st != nullptr) {
            for (auto& m : st->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*m.second)) continue;
                ASR::Variable_t* m_var = ASR::down_cast<ASR::Variable_t>(
                    m.second);
                if (m_var->m_symbolic_value == nullptr) {
                    // A component of a derived type carries its own type's
                    // default initialization even when the component itself
                    // has no default.  The dynamic type of a polymorphic
                    // component decides its initialization, which is not
                    // known here.
                    ASR::Struct_t* m_struct = component_struct_type(m_var);
                    if (m_struct != nullptr &&
                            !ASRUtils::is_class_type(m_var->m_type)) {
                        ASR::expr_t* nested_expr = ASRUtils::EXPR(
                            ASRUtils::getStructInstanceMember_t(al, loc,
                                (ASR::asr_t*)struct_expr, m.second,
                                m.second, current_scope));
                        emit_struct_default_init_stmts(nested_expr, m_struct,
                            current_scope, loc, out_stmts);
                    }
                    continue;
                }
                if (ASRUtils::is_allocatable(m_var->m_type)) continue;

                ASR::expr_t* member_expr = ASRUtils::EXPR(
                    ASRUtils::getStructInstanceMember_t(al, loc,
                        (ASR::asr_t*)struct_expr, m.second,
                        m.second, current_scope));

                ASR::stmt_t* init_stmt = nullptr;
                if (ASRUtils::is_pointer(m_var->m_type)) {
                    // Only handle pointer-null defaults. We rebuild the
                    // PointerNullConstant using `member_expr` as its
                    // `var_expr` so codegen can resolve the underlying
                    // procedure interface (when the pointer targets a
                    // procedure) without relying on a symbol from the
                    // declaring module's scope.
                    if (!ASR::is_a<ASR::PointerNullConstant_t>(
                            *m_var->m_symbolic_value)) continue;
                    ASR::expr_t* null_value = ASRUtils::EXPR(
                        ASR::make_PointerNullConstant_t(al, loc,
                            ASRUtils::expr_type(m_var->m_symbolic_value),
                            member_expr));
                    init_stmt = ASRUtils::STMT(
                        ASRUtils::make_Associate_t_util(al, loc,
                            member_expr, null_value));
                } else {
                    // Use the folded value, not `m_symbolic_value`: the
                    // symbolic form may name a constant declared in the type's
                    // own module, which is not in scope where these statements
                    // are emitted. A default that does not fold to a value —
                    // an array of a derived type, say — is left for #13306;
                    // emitting the unfolded form here produces a node the
                    // backends cannot lower.
                    if (m_var->m_value == nullptr) continue;
                    // Nothing is pushed when this returns false, so there is
                    // nothing for the caller to undo.
                    (void)emit_value_assignment(member_expr, m_var->m_value,
                        current_scope, loc, out_stmts);
                    continue;
                }
                out_stmts.push_back(al, init_stmt);
            }
            if (st->m_parent != nullptr) {
                st = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(st->m_parent));
            } else {
                st = nullptr;
            }
        }
    }

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

                ASR::Struct_t* m_struct = component_struct_type(m_var);
                if (m_struct == nullptr) continue;

                emit_struct_cleanup_stmts(member_expr, m_struct,
                    current_scope, loc, logical_type, out_stmts);
            }
            if (st->m_parent != nullptr) {
                st = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(st->m_parent));
            } else {
                st = nullptr;
            }
        }
    }
    
    void emit_array_of_struct_entry_stmts(
            ASR::expr_t* arr_expr,
            ASR::Struct_t* struct_type,
            int n_dims,
            SymbolTable* current_scope,
            const Location& loc,
            ASR::ttype_t* logical_type,
            bool emit_default_init,
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
        if (emit_default_init) {
            emit_struct_default_init_stmts(arr_ref, struct_type,
                current_scope, loc, innermost_body);
        }

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
        // 1. Function name carries a compiler-generated prefix, OR
        // 2. deftype == Implementation AND parent module is lfortran_intrinsic_*
        std::string func_name = x.m_name;
        bool is_compiler_generated = ASRUtils::is_compiler_generated_name(func_name);
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

                // Fortran 2018 §7.5.6.3 ¶7:
                //   "When a procedure is invoked with a nonpointer,
                //    nonallocatable, INTENT(OUT) dummy argument of a type
                //    for which a final subroutine is defined, the
                bool has_finalizer =
                    struct_hierarchy_has_finalizer(struct_type);
                if (has_finalizer) {
                    ASR::expr_t* var_expr = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, arg_sym));
                    for (size_t fi = 0;
                            fi < struct_type->n_member_functions; fi++) {
                        std::string final_proc_name =
                            struct_type->m_member_functions[fi];
                        ASR::symbol_t* final_sym =
                            struct_type->m_symtab->parent->get_symbol(
                                final_proc_name);
                        LCOMPILERS_ASSERT(final_sym != nullptr);

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
                                al, loc, local_final_sym,
                                local_final_sym, call_args.p,
                                call_args.n, nullptr, false));

                        ASR::stmt_t* wrapped_stmt = wrap_optional_check(
                            loc, var_expr, arg_var->m_presence,
                            call_stmt);
                        dealloc_stmts.push_back(al, wrapped_stmt);
                    }
                    Vec<ASR::stmt_t*> init_stmts;
                    init_stmts.reserve(al, 1);
                    emit_struct_default_init_stmts(var_expr, struct_type,
                        xx.m_symtab, loc, init_stmts);
                    for (size_t k = 0; k < init_stmts.size(); k++) {
                        ASR::stmt_t* wrapped_stmt = wrap_optional_check(
                            loc, var_expr, arg_var->m_presence,
                            init_stmts[k]);
                        dealloc_stmts.push_back(al, wrapped_stmt);
                    }
                }

                ASR::expr_t* var_expr = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, arg_sym));
                Vec<ASR::stmt_t*> cleanup;
                cleanup.reserve(al, 1);
                emit_struct_cleanup_stmts(var_expr, struct_type, xx.m_symtab,
                    loc, logical_type, cleanup);
                for (size_t k = 0; k < cleanup.size(); k++) {
                    ASR::stmt_t* wrapped_stmt = wrap_optional_check(loc,
                        var_expr, arg_var->m_presence, cleanup[k]);
                    dealloc_stmts.push_back(al, wrapped_stmt);
                }
            } else if (is_array_of_struct) {
                ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(arg_var->m_type_declaration));

                int n_dims = ASRUtils::extract_n_dims_from_ttype(arg_var->m_type);

                ASR::expr_t* var_expr_full = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, arg_sym));

                // Fortran 2018 8.5.10: an `intent(out)` dummy of a type with
                // default initialization is default-initialized on entry.
                // The dynamic type of a polymorphic dummy decides its
                // initialization, which this pass cannot see, so that form is
                // left alone.
                bool emit_default_init = !ASRUtils::is_class_type(
                    ASRUtils::type_get_past_array(arg_var->m_type));

                Vec<ASR::stmt_t*> cleanup;
                cleanup.reserve(al, 1);
                emit_array_of_struct_entry_stmts(var_expr_full, struct_type,
                    n_dims, xx.m_symtab, loc, logical_type, emit_default_init,
                    cleanup);

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
