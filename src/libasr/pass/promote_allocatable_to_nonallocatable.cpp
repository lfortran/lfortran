#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/containers.h>
#include <map>

#include <libasr/pass/intrinsic_function_registry.h>

namespace LCompilers {

class IsAllocatedCalled: public ASR::CallReplacerOnExpressionsVisitor<IsAllocatedCalled> {
    public:

        std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var;
        std::map<ASR::symbol_t*, int> alloc_count;

        IsAllocatedCalled(std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var_):
            scope2var(scope2var_) {}

        // Push symbol to all scopes from current_scope up to and including the
        // scope where the symbol is defined. This handles cases where operations
        // like reallocate happen inside nested scopes (e.g., associate blocks)
        // but the variable is defined in an outer scope.
        void push_to_scopes_until_symbol_scope(ASR::symbol_t* sym) {
            SymbolTable* sym_scope = ASRUtils::symbol_parent_symtab(sym);
            SymbolTable* scope = current_scope;
            while (scope != nullptr) {
                scope2var[scope].push_back(sym);
                if (scope->get_counter() == sym_scope->get_counter()) {
                    break;
                }
                scope = scope->parent;
            }
        }

        void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t& x) {
            if( x.m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated) ) {
                LCOMPILERS_ASSERT(x.n_args == 1);
                if( ASR::is_a<ASR::Var_t>(*x.m_args[0]) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_args[0])->m_v;
                    push_to_scopes_until_symbol_scope(sym);
                }
            }
        }

        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(x.m_name);
            for( size_t i = 0; i < x.n_args; i++ ) {
                if( ASR::is_a<ASR::Allocatable_t>(*func_type->m_arg_types[i]) ||
                    ASR::is_a<ASR::Pointer_t>(*func_type->m_arg_types[i]) ) {
                    if( ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value) ) {
                        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_args[i].m_value)->m_v;
                        push_to_scopes_until_symbol_scope(sym);
                    }
                }
            }
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
            ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(x.m_name);
            for( size_t i = 0; i < x.n_args; i++ ) {
                if( ASR::is_a<ASR::Allocatable_t>(*func_type->m_arg_types[i]) ||
                    ASR::is_a<ASR::Pointer_t>(*func_type->m_arg_types[i]) ) {
                    if( ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value) ) {
                        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_args[i].m_value)->m_v;
                        push_to_scopes_until_symbol_scope(sym);
                    }
                }
            }
        }

        void visit_ReAlloc(const ASR::ReAlloc_t& x) {
            for( size_t i = 0; i < x.n_args; i++ ) {
                if( ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(x.m_args[i].m_a)) ||
                    ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x.m_args[i].m_a)) ) {
                    if( ASR::is_a<ASR::Var_t>(*x.m_args[i].m_a) ) {
                        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_args[i].m_a)->m_v;
                        push_to_scopes_until_symbol_scope(sym);
                    }
                }
            }
        }

        bool is_array_size_called_on_pointer(ASR::dimension_t* m_dims, size_t n_dims) {
            for( size_t i = 0; i < n_dims; i++ ) {
                #define check_pointer_in_array_size(expr) if( expr && ASR::is_a<ASR::ArraySize_t>(*expr) ) { \
                    ASR::ArraySize_t* array_size_t = ASR::down_cast<ASR::ArraySize_t>(expr); \
                    if( ASRUtils::is_pointer(ASRUtils::expr_type(array_size_t->m_v)) ) { \
                        return true; \
                    } \
                } \

                check_pointer_in_array_size(m_dims[i].m_start)
                check_pointer_in_array_size(m_dims[i].m_length)

            }

            return false;
        }

        void visit_Allocate(const ASR::Allocate_t& x) {
            for( size_t i = 0; i < x.n_args; i++ ) {
                ASR::alloc_arg_t alloc_arg = x.m_args[i];
                if( ASR::is_a<ASR::Var_t>(*alloc_arg.m_a) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v;
                    alloc_count[sym] += 1;
                    if( alloc_count[sym] > 1 ) {
                        push_to_scopes_until_symbol_scope(sym);
                    }
                }
                if( !ASRUtils::is_dimension_dependent_only_on_arguments(
                        alloc_arg.m_dims, alloc_arg.n_dims, true) ||
                    is_array_size_called_on_pointer(alloc_arg.m_dims, alloc_arg.n_dims) ) {
                    if( ASR::is_a<ASR::Var_t>(*alloc_arg.m_a) ) {
                        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v;
                        push_to_scopes_until_symbol_scope(sym);
                    }
                }
            }
        }

        void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t& x) {
            for( size_t i = 0; i < x.n_vars; i++ ) {
                if( ASR::is_a<ASR::Var_t>(*x.m_vars[i]) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_vars[i])->m_v;
                    push_to_scopes_until_symbol_scope(sym);
                }
            }
        }

        void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t& x) {
            for( size_t i = 0; i < x.n_vars; i++ ) {
                if( ASR::is_a<ASR::Var_t>(*x.m_vars[i]) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_vars[i])->m_v;
                    push_to_scopes_until_symbol_scope(sym);
                }
            }
        }

        void visit_Assignment(const ASR::Assignment_t& x) {
            ASR::CallReplacerOnExpressionsVisitor<IsAllocatedCalled>::visit_Assignment(x);
            if (x.m_move_allocation) {
                if( ASR::is_a<ASR::Var_t>(*x.m_target) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
                    push_to_scopes_until_symbol_scope(sym);
                }
                if( ASR::is_a<ASR::Var_t>(*x.m_value) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_value)->m_v;
                    push_to_scopes_until_symbol_scope(sym);
                }
            }
        }

};

class PromoteAllocatableToNonAllocatable:
    public ASR::CallReplacerOnExpressionsVisitor<PromoteAllocatableToNonAllocatable>
{
    private:

        Allocator& al;
        bool remove_original_statement;

    public:

        std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var;

        PromoteAllocatableToNonAllocatable(Allocator& al_,
            std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var_):
            al(al_), remove_original_statement(false), scope2var(scope2var_) {}

        void visit_Allocate(const ASR::Allocate_t& x) {
            ASR::Allocate_t& xx = const_cast<ASR::Allocate_t&>(x);
            Vec<ASR::alloc_arg_t> x_args;
            x_args.reserve(al, x.n_args);
            for( size_t i = 0; i < x.n_args; i++ ) {
                ASR::alloc_arg_t alloc_arg = x.m_args[i];
                bool is_allocatable_array = ASR::is_a<ASR::Allocatable_t>(
                    *ASRUtils::expr_type(alloc_arg.m_a)) &&
                    ASRUtils::is_array(ASRUtils::expr_type(alloc_arg.m_a));
                bool is_deferred_len_character_array = false;
                bool is_class_array = false;
                if (is_allocatable_array) {
                    ASR::ttype_t* element_type = ASRUtils::type_get_past_array(
                        ASRUtils::type_get_past_allocatable(
                            ASRUtils::expr_type(alloc_arg.m_a)));
                    if (ASRUtils::is_character(*element_type)) {
                        ASR::String_t* str = ASR::down_cast<ASR::String_t>(element_type);
                        is_deferred_len_character_array =
                            str->m_len_kind == ASR::string_length_kindType::DeferredLength;
                    }
                    is_class_array = ASRUtils::is_class_type(element_type);
                }
                if( ASR::is_a<ASR::Var_t>(*alloc_arg.m_a) &&
                    is_allocatable_array &&
                    !is_deferred_len_character_array &&
                    !is_class_array &&
                    ASR::is_a<ASR::Variable_t>(
                        *ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v) &&
                    !ASR::is_a<ASR::Module_t>(
                        *ASRUtils::get_asr_owner(ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v)) &&
                    ASRUtils::expr_intent(alloc_arg.m_a) == ASRUtils::intent_local &&
                    ASRUtils::is_dimension_dependent_only_on_arguments(
                        alloc_arg.m_dims, alloc_arg.n_dims) &&
                    std::find(scope2var[current_scope].begin(),
                        scope2var[current_scope].end(),
                        ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v) ==
                        scope2var[current_scope].end() ) {
                    ASR::Variable_t* alloc_variable = ASR::down_cast<ASR::Variable_t>(
                        ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v);
                    ASR::ttype_t* array_type /*Array's type*/  = ASRUtils::duplicate_type(al,
                        ASRUtils::type_get_past_array(
                            ASRUtils::type_get_past_allocatable(alloc_variable->m_type)));
                        // Set length of String type -> e.g. `character(:), allocatable :: arr(:)`
                        if(ASRUtils::is_character(*array_type) && 
                            ASR::down_cast<ASR::String_t>(array_type)->m_len_kind ==
                            ASR::string_length_kindType::DeferredLength){
                            ASR::String_t* str = ASR::down_cast<ASR::String_t>(array_type);
                            str->m_len = alloc_arg.m_len_expr;
                            str->m_len_kind = ASR::string_length_kindType::ExpressionLength;
                        }
                    alloc_variable->m_type = ASRUtils::make_Array_t_util(al, x.base.base.loc,
                    array_type, alloc_arg.m_dims, alloc_arg.n_dims);
                } else if( ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(alloc_arg.m_a)) ||
                           ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(alloc_arg.m_a)) ) {
                    x_args.push_back(al, alloc_arg);
                }
            }
            if( x_args.size() > 0 ) {
                xx.m_args = x_args.p;
                xx.n_args = x_args.size();
            } else {
                remove_original_statement = true;
            }
        }

        template <typename T>
        void visit_Deallocate(const T& x) {
            T& xx = const_cast<T&>(x);
            Vec<ASR::expr_t*> x_args;
            x_args.reserve(al, x.n_vars);
            for( size_t i = 0; i < x.n_vars; i++ ) {
                if( ASR::is_a<ASR::Allocatable_t>(
                        *ASRUtils::expr_type(x.m_vars[i])) ||
                    ASR::is_a<ASR::Pointer_t>(
                        *ASRUtils::expr_type(x.m_vars[i])) ) {
                    x_args.push_back(al, x.m_vars[i]);
                }
            }
            if( x_args.size() > 0 ) {
                xx.m_vars = x_args.p;
                xx.n_vars = x_args.size();
            } else {
                remove_original_statement = true;
            }
        }

        void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t& x) {
            visit_Deallocate(x);
        }

        void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t& x) {
            visit_Deallocate(x);
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            bool remove_original_statement_copy = remove_original_statement;
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            for (size_t i = 0; i < n_body; i++) {
                remove_original_statement = false;
                visit_stmt(*m_body[i]);
                if( !remove_original_statement ) {
                    body.push_back(al, m_body[i]);
                }
            }
            m_body = body.p;
            n_body = body.size();
            remove_original_statement = remove_original_statement_copy;
        }

};

class FixArrayPhysicalCast: public ASR::BaseExprReplacer<FixArrayPhysicalCast> {
    private:
        Allocator& al;

    public:

        FixArrayPhysicalCast(Allocator& al_): al(al_) {}

        void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
            ASR::BaseExprReplacer<FixArrayPhysicalCast>::replace_ArrayPhysicalCast(x);
            if( x->m_old != ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg)) ) {
                x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
            }
            if( (x->m_old == x->m_new &&
                x->m_old != ASR::array_physical_typeType::DescriptorArray) ||
                (x->m_old == x->m_new && x->m_old == ASR::array_physical_typeType::DescriptorArray &&
                (ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(x->m_arg)) ||
                ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x->m_arg))) ) ) {
                *current_expr = x->m_arg;
            }
        }

        void replace_FunctionCall(ASR::FunctionCall_t* x) {
            ASR::BaseExprReplacer<FixArrayPhysicalCast>::replace_FunctionCall(x);
            ASR::expr_t* call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(
                al, x->base.base.loc, x->m_name, x->m_original_name, x->m_args,
                x->n_args, x->m_type, x->m_value, x->m_dt));
            ASR::FunctionCall_t* function_call = ASR::down_cast<ASR::FunctionCall_t>(call);
            x->m_args = function_call->m_args;
            x->n_args = function_call->n_args;
        }

        void replace_ArrayReshape(ASR::ArrayReshape_t* x) {
            ASR::BaseExprReplacer<FixArrayPhysicalCast>::replace_ArrayReshape(x);
            if( ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_array)) ==
                ASR::array_physical_typeType::FixedSizeArray &&
                ASRUtils::extract_physical_type(x->m_type) !=
                ASR::array_physical_typeType::FixedSizeArray ) {
                size_t n_dims = ASRUtils::extract_n_dims_from_ttype(x->m_type);
                Vec<ASR::dimension_t> empty_dims; empty_dims.reserve(al, n_dims);
                for( size_t i = 0; i < n_dims; i++ ) {
                    ASR::dimension_t empty_dim;
                    empty_dim.loc = x->base.base.loc;
                    empty_dim.m_start = nullptr;
                    empty_dim.m_length = nullptr;
                    empty_dims.push_back(al, empty_dim);
                }
                x->m_type = ASRUtils::TYPE(ASR::make_Array_t(al, x->base.base.loc,
                    ASRUtils::extract_type(x->m_type), empty_dims.p, empty_dims.size(),
                    ASR::array_physical_typeType::FixedSizeArray));
            }
        }
};

class FixArrayPhysicalCastVisitor: public ASR::CallReplacerOnExpressionsVisitor<FixArrayPhysicalCastVisitor> {
    public:

        Allocator& al;
        FixArrayPhysicalCast replacer;
        bool remove_original_stmt;

        FixArrayPhysicalCastVisitor(Allocator& al_):
            al(al_), replacer(al_), remove_original_stmt(false) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
            ASR::CallReplacerOnExpressionsVisitor<FixArrayPhysicalCastVisitor>::visit_SubroutineCall(x);
            ASR::stmt_t* call = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                al, x.base.base.loc, x.m_name, x.m_original_name, x.m_args,
                x.n_args, x.m_dt, nullptr, false));
            ASR::SubroutineCall_t* subrout_call = ASR::down_cast<ASR::SubroutineCall_t>(call);
            ASR::SubroutineCall_t& xx = const_cast<ASR::SubroutineCall_t&>(x);
            xx.m_args = subrout_call->m_args;
            xx.n_args = subrout_call->n_args;
        }

        void visit_Associate(const ASR::Associate_t& x) {
            if( ASRUtils::is_fixed_size_array(
                    ASRUtils::expr_type(x.m_value)) &&
                !ASR::is_a<ASR::ArraySection_t>(*x.m_value) ) {
                ASR::Associate_t& xx = const_cast<ASR::Associate_t&>(x);
                xx.m_value = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                    al, x.m_value->base.loc, xx.m_value,
                    ASRUtils::extract_physical_type(ASRUtils::expr_type(xx.m_value)),
                    ASR::array_physical_typeType::DescriptorArray,
                    ASRUtils::duplicate_type(al, ASRUtils::expr_type(x.m_value),
                    nullptr, ASR::array_physical_typeType::DescriptorArray, true), nullptr));
            } else if( ASRUtils::is_fixed_size_array(
                        ASRUtils::expr_type(x.m_target)) ) {
                remove_original_stmt = true;
            }
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            bool remove_original_stmt_copy = remove_original_stmt;
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            for (size_t i = 0; i < n_body; i++) {
                remove_original_stmt = false;
                visit_stmt(*m_body[i]);
                if( !remove_original_stmt ) {
                    body.push_back(al, m_body[i]);
                    remove_original_stmt = false;
                }
            }
            m_body = body.p;
            n_body = body.size();
            remove_original_stmt = remove_original_stmt_copy;
        }
};

class FixMoveAssignment: public ASR::CallReplacerOnExpressionsVisitor<FixMoveAssignment> {
    public:

        Allocator& al;

        FixMoveAssignment(Allocator& al_):
            al(al_) {}

        void visit_Assignment(const ASR::Assignment_t& x) {
            ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);

            ASR::ttype_t* target_type = ASRUtils::expr_type(x.m_target);
            ASR::ttype_t* value_type = ASRUtils::expr_type(x.m_value);
            bool is_target_allocatable_array = ASRUtils::is_array(target_type) &&
                                            ASRUtils::is_allocatable(target_type) &&
                                            ASRUtils::extract_physical_type(target_type) == ASR::array_physical_typeType::DescriptorArray;
            bool is_value_allocatable_array = ASRUtils::is_array(value_type) &&
                                            ASRUtils::is_allocatable(value_type) &&
                                            ASRUtils::extract_physical_type(value_type) == ASR::array_physical_typeType::DescriptorArray;

            if (x.m_move_allocation && (!is_target_allocatable_array || !is_value_allocatable_array)) {
                xx.m_move_allocation = false;
            }
        }
};

void pass_promote_allocatable_to_nonallocatable(
    Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &/*pass_options*/) {
    std::map<SymbolTable*, std::vector<ASR::symbol_t*>> scope2var;
    IsAllocatedCalled is_allocated_called(scope2var);
    is_allocated_called.visit_TranslationUnit(unit);
    PromoteAllocatableToNonAllocatable promoter(al, scope2var);
    promoter.visit_TranslationUnit(unit);
    promoter.visit_TranslationUnit(unit);
    FixArrayPhysicalCastVisitor fix_array_physical_cast(al);
    fix_array_physical_cast.visit_TranslationUnit(unit);
    FixMoveAssignment fix_move_assignment(al);
    fix_move_assignment.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
