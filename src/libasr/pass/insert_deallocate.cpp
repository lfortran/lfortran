#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/insert_deallocate.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>


namespace LCompilers {

// Collect temporary variables which are inside Assignment targets
class CollectTempVarsVisitor : public ASR::BaseWalkVisitor<CollectTempVarsVisitor>
{
    Allocator &al;
    Vec<ASR::expr_t*>& res;
    public:
        CollectTempVarsVisitor(Allocator& al_, Vec<ASR::expr_t*>& res_) : al(al_), res{res_} {}

        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::Variable_t* target_variable = ASRUtils::expr_to_variable_or_null(x.m_target);
            if (target_variable &&
                ASRUtils::is_allocatable_or_pointer(target_variable->m_type) &&
                ASRUtils::symbol_StorageType((ASR::symbol_t *)target_variable) == ASR::storage_typeType::Default) {
                if (std::string(target_variable->m_name).rfind("__libasr_created") != std::string::npos) {
                    res.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.m_target->base.loc, (ASR::symbol_t *)target_variable)));
                }
            }
        }

        // Don't nest into other loops
        void visit_WhileLoop(const ASR::WhileLoop_t &/*x*/){
        }

        void visit_DoLoop(const ASR::DoLoop_t &/*x*/){
        }
};

// Insert implicit deallocate before Cycle, Return, or Exit in a loop
class LoopDeallocateInserter : public ASR::CallReplacerOnExpressionsVisitor<LoopDeallocateInserter>
{
    Allocator &al;
    ASR::stmt_t* node;
    public:
        LoopDeallocateInserter(Allocator& al_, ASR::stmt_t* node_) : al(al_), node(node_) {}

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, 1);
            for (size_t i = 0; i < n_body; i++){
                if (ASR::is_a<ASR::Cycle_t>(*m_body[i]) ||
                    ASR::is_a<ASR::Return_t>(*m_body[i]) ||
                    ASR::is_a<ASR::Exit_t>(*m_body[i])) {
                    new_body.push_back(al, node);
                }
                new_body.push_back(al, m_body[i]);
            }
            m_body = new_body.p;
            n_body = new_body.size();
        }

        // Don't nest into other loops
        void visit_WhileLoop(const ASR::WhileLoop_t &/*x*/){
        }

        void visit_DoLoop(const ASR::DoLoop_t &/*x*/){
        }
};

// Collects temporary variables in assignment targets inside loops and deallocates them when going out of the loop body
class LoopTempVarDeallocateVisitor : public ASR::BaseWalkVisitor<LoopTempVarDeallocateVisitor>
{
    Allocator &al;
    public:
        LoopTempVarDeallocateVisitor(Allocator& al_) : al(al_) {}

        template<typename T>
        void collect_temp_vars_and_insert_deallocate_in_loop(T& xx) {
            Vec<ASR::expr_t*> v;
            v.reserve(al, 1);
            CollectTempVarsVisitor c(al, v);
            for (size_t i = 0; i < xx.n_body; i++) {
                c.visit_stmt(*xx.m_body[i]);
            }
            if (v.size() > 0) {
                ASR::stmt_t* implicit_deallocation_node = ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
                    al, xx.base.base.loc, v.p, v.size()));

                // Insert ImplicitDeallocate after Cycle, Return, or Exit
                LoopDeallocateInserter I(al, implicit_deallocation_node);
                for (size_t i = 0; i < xx.n_body; i++) {
                    I.visit_stmt(*xx.m_body[i]);
                }

                // Insert ImplicitDeallocate at the end of the loop body
                Vec<ASR::stmt_t*> new_body;
                new_body.reserve(al, 1);
                for (size_t i = 0; i < xx.n_body; i++) {
                    new_body.push_back(al, xx.m_body[i]);
                }
                new_body.push_back(al, implicit_deallocation_node);

                xx.m_body = new_body.p;
                xx.n_body = new_body.n;
            }
        }

        void visit_WhileLoop(const ASR::WhileLoop_t &x){
            ASR::WhileLoop_t &xx = const_cast<ASR::WhileLoop_t&>(x);
            collect_temp_vars_and_insert_deallocate_in_loop(xx);

            ASR::BaseWalkVisitor<LoopTempVarDeallocateVisitor>::visit_WhileLoop(x);
        }

        void visit_DoLoop(const ASR::DoLoop_t &x){
            ASR::DoLoop_t &xx = const_cast<ASR::DoLoop_t&>(x);
            collect_temp_vars_and_insert_deallocate_in_loop(xx);

            ASR::BaseWalkVisitor<LoopTempVarDeallocateVisitor>::visit_DoLoop(x);
        }
};

// Insert finalization calls before intrinsic assignment to non-pointer
// variables of finalizable type (Fortran 2018 §7.5.6.3 ¶1):
//   "When an intrinsic assignment statement is executed, if the variable is
//    not an unallocated allocatable variable and is of a finalizable type or
//    has a finalizable component, it is finalized before the definition."
//
// For allocatable targets the finalization is guarded by an `allocated` check.
class LHSFinalizationVisitor : public PassUtils::PassVisitor<LHSFinalizationVisitor>
{
public:
    LHSFinalizationVisitor(Allocator& al_, SymbolTable* scope)
        : PassVisitor(al_, scope) {}

    void visit_Assignment(const ASR::Assignment_t &x) {
        // Only handle intrinsic assignment (no overloaded operator)
        if (x.m_overloaded) return;

        ASR::Variable_t* target_var = ASRUtils::expr_to_variable_or_null(x.m_target);
        if (!target_var) return;

        // Skip pointer targets
        if (ASRUtils::is_pointer(target_var->m_type)) return;

        bool is_alloc = ASRUtils::is_allocatable(target_var->m_type);

        ASR::ttype_t* t = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable(target_var->m_type));
        if (!ASR::is_a<ASR::StructType_t>(*t)) return;

        if (!target_var->m_type_declaration) return;
        ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(
            target_var->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*struct_sym)) return;

        ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(struct_sym);
        if (struct_type->n_member_functions == 0) return;

        Location loc = x.base.base.loc;
        ASR::expr_t* var_expr = ASRUtils::EXPR(ASR::make_Var_t(
            al, loc, (ASR::symbol_t*)target_var));

        // Build the list of finalization SubroutineCall statements
        Vec<ASR::stmt_t*> final_calls;
        final_calls.reserve(al, struct_type->n_member_functions);
        for (size_t fi = 0; fi < struct_type->n_member_functions; fi++) {
            std::string final_proc_name = struct_type->m_member_functions[fi];
            ASR::symbol_t* final_sym =
                struct_type->m_symtab->parent->get_symbol(final_proc_name);
            if (!final_sym) continue;

            // Ensure the FINAL procedure is accessible from current scope
            ASR::symbol_t* local_final_sym =
                current_scope->resolve_symbol(final_proc_name);
            if (!local_final_sym) {
                std::string module_name = "";
                ASR::asr_t* owner = struct_type->m_symtab->parent->asr_owner;
                if (owner && ASR::is_a<ASR::symbol_t>(*owner)) {
                    module_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::symbol_t>(owner));
                }
                ASR::asr_t* ext = ASR::make_ExternalSymbol_t(
                    al, loc, current_scope,
                    s2c(al, final_proc_name), final_sym,
                    s2c(al, module_name), nullptr, 0,
                    s2c(al, final_proc_name),
                    ASR::accessType::Private);
                current_scope->add_symbol(final_proc_name,
                    ASR::down_cast<ASR::symbol_t>(ext));
                local_final_sym = ASR::down_cast<ASR::symbol_t>(ext);
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

            final_calls.push_back(al, call_stmt);
        }

        pass_result.reserve(al, final_calls.n);
        if (is_alloc) {
            // Wrap finalization calls in: if (allocated(lhs)) then ...
            ASR::ttype_t* logical_type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, loc, 4));

            Vec<ASR::expr_t*> allocated_args;
            allocated_args.reserve(al, 1);
            allocated_args.push_back(al, var_expr);

            ASR::expr_t* is_allocated = ASRUtils::EXPR(
                ASR::make_IntrinsicImpureFunction_t(
                    al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicImpureFunctions::Allocated),
                    allocated_args.p, allocated_args.n,
                    0, logical_type, nullptr));

            ASR::stmt_t* if_stmt = ASRUtils::STMT(ASR::make_If_t(
                al, loc, nullptr, is_allocated,
                final_calls.p, final_calls.n, nullptr, 0));

            pass_result.push_back(al, if_stmt);
        } else {
            for (size_t i = 0; i < final_calls.n; i++) {
                pass_result.push_back(al, final_calls[i]);
            }
        }

        // Keep the original assignment after the finalization call(s)
        retain_original_stmt = true;
    }
};

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
            if (!ASRUtils::is_allocatable(arg_var->m_type) &&
                !ASR::is_a<ASR::StructType_t>(*arg_var->m_type)) continue;

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

                SymbolTable* sym_table_of_struct = struct_type->m_symtab;
                while (sym_table_of_struct != nullptr) {
                    for (auto& struct_member : sym_table_of_struct->get_scope()) {
                        if (ASR::is_a<ASR::Variable_t>(*struct_member.second) &&
                            ASRUtils::is_allocatable(ASRUtils::symbol_type(struct_member.second))) {

                            // Create struct member access: arg%member
                            ASR::expr_t* var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg_sym));
                            ASR::expr_t* member_expr = ASRUtils::EXPR(
                                ASRUtils::getStructInstanceMember_t(al, loc,
                                (ASR::asr_t*)var_expr, const_cast<ASR::symbol_t*>(arg_sym),
                                struct_member.second, x.m_symtab));

                            // Create: if (allocated(arg%member)) deallocate(arg%member)
                            Vec<ASR::expr_t*> allocated_args;
                            allocated_args.reserve(al, 1);
                            allocated_args.push_back(al, member_expr);

                            ASR::expr_t* is_allocated = ASRUtils::EXPR(ASR::make_IntrinsicImpureFunction_t(
                                al, loc,
                                static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated),
                                allocated_args.p, allocated_args.n, 0, logical_type, nullptr));

                            // Create Deallocate statement for member
                            Vec<ASR::expr_t*> dealloc_args;
                            dealloc_args.reserve(al, 1);
                            dealloc_args.push_back(al, member_expr);
                            ASR::stmt_t* dealloc_stmt = ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
                                al, loc, dealloc_args.p, dealloc_args.n));

                            // Create If statement: if (allocated(arg%member)) deallocate(arg%member)
                            Vec<ASR::stmt_t*> if_body;
                            if_body.reserve(al, 1);
                            if_body.push_back(al, dealloc_stmt);
                            ASR::stmt_t* if_stmt = ASRUtils::STMT(ASR::make_If_t(
                                al, loc, nullptr, is_allocated, if_body.p, if_body.n, nullptr, 0));

                            // Wrap in optional presence check if needed (reuse var_expr from above)
                            ASR::stmt_t* wrapped_stmt = wrap_optional_check(loc, var_expr, arg_var->m_presence, if_stmt);
                            dealloc_stmts.push_back(al, wrapped_stmt);
                        }
                    }
                    if (struct_type->m_parent != nullptr) {
                        struct_type = ASR::down_cast<ASR::Struct_t>(
                            ASRUtils::symbol_get_past_external(struct_type->m_parent));
                        sym_table_of_struct = struct_type->m_symtab;
                    } else {
                        sym_table_of_struct = nullptr;
                    }
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


void pass_insert_deallocate(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &/*pass_options*/) {
    // Deallocate intent(out) allocatable arguments at function entry
    IntentOutDeallocateVisitor iod(al);
    iod.visit_TranslationUnit(unit);

    // Finalize LHS of intrinsic assignment (Fortran 2018 §7.5.6.3 ¶1)
    LHSFinalizationVisitor lhsf(al, unit.m_symtab);
    lhsf.visit_TranslationUnit(unit);

    LoopTempVarDeallocateVisitor m(al);
    m.visit_TranslationUnit(unit);

    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
