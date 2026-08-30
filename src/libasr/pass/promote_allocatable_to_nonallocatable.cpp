#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/containers.h>
#include <map>
#include <set>

#include <libasr/pass/intrinsic_function_registry.h>

namespace LCompilers {

class IsAllocatedCalled: public ASR::CallReplacerOnExpressionsVisitor<IsAllocatedCalled> {
    public:

        std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var;
        std::map<ASR::symbol_t*, int> alloc_count;
        bool device_only;
        bool in_device;

        IsAllocatedCalled(std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var_,
            bool device_only_=false):
            scope2var(scope2var_), device_only(device_only_), in_device(false) {}

        void visit_Function(const ASR::Function_t& x) {
            bool in_device_copy = in_device;
            if( device_only && ASRUtils::is_device_function(x) ) {
                in_device = true;
            }
            ASR::CallReplacerOnExpressionsVisitor<
                IsAllocatedCalled>::visit_Function(x);
            in_device = in_device_copy;
        }

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
            size_t n = std::min(x.n_args, func_type->n_arg_types);
            for( size_t i = 0; i < n; i++ ) {
                if( x.m_args[i].m_value &&
                    (ASR::is_a<ASR::Allocatable_t>(*func_type->m_arg_types[i]) ||
                    ASR::is_a<ASR::Pointer_t>(*func_type->m_arg_types[i])) ) {
                    if( ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value) ) {
                        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_args[i].m_value)->m_v;
                        push_to_scopes_until_symbol_scope(sym);
                    }
                }
            }
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
            ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(x.m_name);
            size_t n = std::min(x.n_args, func_type->n_arg_types);
            for( size_t i = 0; i < n; i++ ) {
                if( x.m_args[i].m_value &&
                    (ASR::is_a<ASR::Allocatable_t>(*func_type->m_arg_types[i]) ||
                    ASR::is_a<ASR::Pointer_t>(*func_type->m_arg_types[i])) ) {
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
                        alloc_arg.m_dims, alloc_arg.n_dims, true, in_device) ||
                    is_array_size_called_on_pointer(alloc_arg.m_dims, alloc_arg.n_dims) ) {
                    if( ASR::is_a<ASR::Var_t>(*alloc_arg.m_a) ) {
                        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v;
                        push_to_scopes_until_symbol_scope(sym);
                    }
                }
            }
        }

        // A device has no heap, so a deallocation there frees nothing and
        // does not stop the variable from becoming an array of a fixed shape.
        template <typename T>
        void collect_deallocated(const T& x) {
            if( in_device ) {
                return;
            }
            for( size_t i = 0; i < x.n_vars; i++ ) {
                if( ASR::is_a<ASR::Var_t>(*x.m_vars[i]) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_vars[i])->m_v;
                    push_to_scopes_until_symbol_scope(sym);
                }
            }
        }

        void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t& x) {
            collect_deallocated(x);
        }

        void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t& x) {
            collect_deallocated(x);
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
        // When true, only allocatables of GPU device code are promoted. A
        // device has no heap, so an allocatable there has to become an array
        // whose shape is known when the routine is entered.
        bool device_only;
        bool in_device;

    public:

        std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var;
        std::set<ASR::symbol_t*> promoted_symbols;
        // Functions whose result variable was promoted. Their signature and
        // every call to them have to be rebuilt from the new result type.
        std::set<ASR::symbol_t*> promoted_result_functions;

        PromoteAllocatableToNonAllocatable(Allocator& al_,
            std::map<SymbolTable*, std::vector<ASR::symbol_t*>>& scope2var_,
            bool device_only_=false):
            al(al_), remove_original_statement(false),
            device_only(device_only_), in_device(false),
            scope2var(scope2var_) {}

        void visit_Function(const ASR::Function_t& x) {
            bool in_device_copy = in_device;
            if( device_only && ASRUtils::is_device_function(x) ) {
                in_device = true;
            }
            ASR::CallReplacerOnExpressionsVisitor<
                PromoteAllocatableToNonAllocatable>::visit_Function(x);
            if( in_device && x.m_return_var &&
                ASR::is_a<ASR::Var_t>(*x.m_return_var) &&
                promoted_symbols.count(
                    ASR::down_cast<ASR::Var_t>(x.m_return_var)->m_v) > 0 ) {
                promoted_result_functions.insert((ASR::symbol_t*) &x.base);
            }
            in_device = in_device_copy;
        }

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
                    (!device_only || in_device) &&
                    (ASRUtils::expr_intent(alloc_arg.m_a) == ASRUtils::intent_local ||
                     (in_device && ASRUtils::expr_intent(alloc_arg.m_a) ==
                        ASRUtils::intent_return_var)) &&
                    ASRUtils::is_dimension_dependent_only_on_arguments(
                        alloc_arg.m_dims, alloc_arg.n_dims, false, in_device) &&
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
                    promoted_symbols.insert(ASR::down_cast<ASR::Var_t>(alloc_arg.m_a)->m_v);
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
                    ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));
            }
        }
};

class FixArrayPhysicalCastVisitor: public ASR::CallReplacerOnExpressionsVisitor<FixArrayPhysicalCastVisitor> {
    public:

        Allocator& al;
        FixArrayPhysicalCast replacer;
        bool remove_original_stmt;
        const std::set<ASR::symbol_t*>& promoted_symbols;

        FixArrayPhysicalCastVisitor(Allocator& al_,
            const std::set<ASR::symbol_t*>& promoted_symbols_):
            al(al_), replacer(al_), remove_original_stmt(false),
            promoted_symbols(promoted_symbols_) {}

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
            } else if (ASR::is_a<ASR::ArraySection_t>(*x.m_value)) {
                ASR::ArraySection_t* as = ASR::down_cast<ASR::ArraySection_t>(
                    const_cast<ASR::expr_t*>(x.m_value));
                ASR::ttype_t* base_type = ASRUtils::expr_type(as->m_v);
                ASR::ttype_t* section_type = ASRUtils::expr_type(x.m_value);
                bool base_was_promoted = ASR::is_a<ASR::Var_t>(*as->m_v) &&
                    promoted_symbols.count(ASR::down_cast<ASR::Var_t>(as->m_v)->m_v) > 0;
                if (base_was_promoted &&
                    ASRUtils::is_fixed_size_array(base_type) &&
                    ASRUtils::extract_physical_type(section_type) ==
                        ASR::array_physical_typeType::DescriptorArray) {
                    as->m_v = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                        al, as->m_v->base.loc, as->m_v,
                        ASRUtils::extract_physical_type(base_type),
                        ASR::array_physical_typeType::DescriptorArray,
                        ASRUtils::duplicate_type(al, base_type,
                            nullptr, ASR::array_physical_typeType::DescriptorArray, true),
                        nullptr));
                }
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

// Collects the variables that an explicit Allocate statement gives a shape to.
class AllocatedSymbolCollector:
    public ASR::BaseWalkVisitor<AllocatedSymbolCollector> {
    public:

        std::set<ASR::symbol_t*>& syms;

        AllocatedSymbolCollector(std::set<ASR::symbol_t*>& syms_): syms(syms_) {}

        void visit_Allocate(const ASR::Allocate_t& x) {
            for( size_t i = 0; i < x.n_args; i++ ) {
                if( ASR::is_a<ASR::Var_t>(*x.m_args[i].m_a) ) {
                    syms.insert(ASR::down_cast<ASR::Var_t>(x.m_args[i].m_a)->m_v);
                }
            }
        }

        void visit_Function(const ASR::Function_t& /*x*/) {
            // Nested routines allocate their own variables.
        }
};

// Fortran reallocates the left hand side of an array assignment when it is an
// allocatable of a different shape. A device has no heap, so the shape has to
// be known when the routine is entered: make it explicit by giving the
// variable the Allocate statement the assignment implies, which
// PromoteAllocatableToNonAllocatable then turns into the array's declared
// shape.
class DeviceImplicitAllocInserter:
    public PassUtils::PassVisitor<DeviceImplicitAllocInserter> {
    private:

        bool in_device;
        std::set<ASR::symbol_t*> explicitly_allocated;

    public:

        DeviceImplicitAllocInserter(Allocator& al_):
            PassVisitor(al_, nullptr), in_device(false) {
            pass_result.n = 0;
            pass_result.reserve(al_, 0);
        }

        void visit_Function(const ASR::Function_t& x) {
            bool in_device_copy = in_device;
            std::set<ASR::symbol_t*> allocated_copy = explicitly_allocated;
            if( ASRUtils::is_device_function(x) ) {
                in_device = true;
            }
            if( in_device ) {
                explicitly_allocated.clear();
                AllocatedSymbolCollector collector(explicitly_allocated);
                for( size_t i = 0; i < x.n_body; i++ ) {
                    collector.visit_stmt(*x.m_body[i]);
                }
            }
            PassUtils::PassVisitor<DeviceImplicitAllocInserter>::visit_Function(x);
            in_device = in_device_copy;
            explicitly_allocated = allocated_copy;
        }

        // The shape an assignment gives its target has to be readable where
        // the target is declared, so only a value whose extents come from a
        // variable can be used.
        ASR::expr_t* shape_source(ASR::expr_t* value) {
            ASR::expr_t* v = ASRUtils::get_past_array_physical_cast(value);
            if( ASR::is_a<ASR::Var_t>(*v) ||
                ASR::is_a<ASR::StructInstanceMember_t>(*v) ) {
                return v;
            }
            return nullptr;
        }

        // The variable an argument passes, past a physical cast, when it is a
        // local of device code that no Allocate gives a shape to.
        ASR::symbol_t* unshaped_local(ASR::expr_t* arg) {
            if( arg == nullptr ) {
                return nullptr;
            }
            ASR::expr_t* v = ASRUtils::get_past_array_physical_cast(arg);
            if( !ASR::is_a<ASR::Var_t>(*v) ) {
                return nullptr;
            }
            ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(v)->m_v;
            if( !ASR::is_a<ASR::Variable_t>(*sym) ||
                explicitly_allocated.count(sym) > 0 ) {
                return nullptr;
            }
            ASR::ttype_t* type = ASRUtils::symbol_type(sym);
            if( !ASRUtils::is_allocatable(type) || !ASRUtils::is_array(type) ||
                ASRUtils::is_character(*ASRUtils::extract_type(type)) ) {
                return nullptr;
            }
            return sym;
        }

        // A routine that returns an array of an explicit shape gives that
        // shape to the variable it writes its result into.
        void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
            if( !in_device ) {
                return;
            }
            ASR::symbol_t* fn = ASRUtils::symbol_get_past_StructMethodDeclaration(
                ASRUtils::symbol_get_past_external(x.m_name));
            if( !ASR::is_a<ASR::Function_t>(*fn) ) {
                return;
            }
            ASR::FunctionType_t* ftype = ASRUtils::get_FunctionType(fn);
            size_t n = std::min(x.n_args, ftype->n_arg_types);
            for( size_t i = 0; i < n; i++ ) {
                ASR::symbol_t* sym = unshaped_local(x.m_args[i].m_value);
                if( sym == nullptr ) {
                    continue;
                }
                // The shape the call expects, taken from the cast the
                // argument already carries, or from the routine's own
                // parameter type with the arguments substituted in.
                ASR::ttype_t* shape =
                    ASR::is_a<ASR::ArrayPhysicalCast_t>(*x.m_args[i].m_value)
                    ? ASRUtils::expr_type(x.m_args[i].m_value)
                    : ftype->m_arg_types[i];
                if( ASRUtils::is_allocatable(shape) ||
                    !ASRUtils::is_array(shape) ) {
                    continue;
                }
                ASR::dimension_t* shape_dims = nullptr;
                size_t n_shape_dims = ASRUtils::extract_dimensions_from_ttype(
                    shape, shape_dims);
                if( n_shape_dims == 0 ||
                    n_shape_dims != (size_t) ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::symbol_type(sym)) ) {
                    continue;
                }
                ASRUtils::ReplaceFunctionParamWithArg replacer(
                    al, x.m_args, x.n_args);
                ASRUtils::ExprStmtDuplicator duplicator(al);
                duplicator.allow_procedure_calls = true;
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, n_shape_dims);
                bool complete = true;
                for( size_t d = 0; d < n_shape_dims; d++ ) {
                    if( shape_dims[d].m_length == nullptr ) {
                        complete = false;
                        break;
                    }
                    ASR::dimension_t dim;
                    dim.loc = shape_dims[d].loc;
                    duplicator.success = true;
                    dim.m_length = duplicator.duplicate_expr(
                        replacer.replace_FunctionParam_with_arg(
                            shape_dims[d].m_length));
                    dim.m_start = shape_dims[d].m_start
                        ? duplicator.duplicate_expr(
                            replacer.replace_FunctionParam_with_arg(
                                shape_dims[d].m_start))
                        : nullptr;
                    if( !duplicator.success ) {
                        complete = false;
                        break;
                    }
                    dims.push_back(al, dim);
                }
                if( !complete ) {
                    continue;
                }
                insert_allocate(x.base.base.loc, ASRUtils::EXPR(
                    ASR::make_Var_t(al, x.base.base.loc, sym)),
                    dims.p, dims.size());
                explicitly_allocated.insert(sym);
            }
        }

        void insert_allocate(const Location& loc, ASR::expr_t* target,
            ASR::dimension_t* dims, size_t n_dims) {
            Vec<ASR::alloc_arg_t> alloc_args;
            alloc_args.reserve(al, 1);
            ASR::alloc_arg_t alloc_arg;
            alloc_arg.loc = loc;
            alloc_arg.m_a = target;
            alloc_arg.m_dims = dims;
            alloc_arg.n_dims = n_dims;
            alloc_arg.m_codims = nullptr;
            alloc_arg.n_codims = 0;
            alloc_arg.m_len_expr = nullptr;
            alloc_arg.m_type = nullptr;
            alloc_arg.m_sym_subclass = nullptr;
            alloc_args.push_back(al, alloc_arg);
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr,
                nullptr)));
            retain_original_stmt = true;
        }

        void visit_Assignment(const ASR::Assignment_t& x) {
            if( !in_device || x.m_move_allocation ) {
                return;
            }
            if( !ASR::is_a<ASR::Var_t>(*x.m_target) ) {
                return;
            }
            ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
            if( !ASR::is_a<ASR::Variable_t>(*sym) ||
                explicitly_allocated.count(sym) > 0 ) {
                return;
            }
            ASR::ttype_t* target_type = ASRUtils::expr_type(x.m_target);
            if( !ASRUtils::is_allocatable(target_type) ||
                !ASRUtils::is_array(target_type) ||
                ASRUtils::is_character(*ASRUtils::extract_type(target_type)) ) {
                return;
            }
            ASR::expr_t* source = shape_source(x.m_value);
            if( source == nullptr ) {
                return;
            }
            size_t n_dims = ASRUtils::extract_n_dims_from_ttype(target_type);
            if( n_dims == 0 ||
                (size_t) ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(source)) != n_dims ) {
                return;
            }
            const Location& loc = x.base.base.loc;
            ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASRUtils::ExprStmtDuplicator duplicator(al);
            duplicator.allow_procedure_calls = true;
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, n_dims);
            for( size_t i = 0; i < n_dims; i++ ) {
                duplicator.success = true;
                ASR::expr_t* source_copy = duplicator.duplicate_expr(source);
                if( !duplicator.success ) {
                    return;
                }
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, loc, 1, int_type, ASR::integerbozType::Decimal));
                dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
                    source_copy, ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, loc, i + 1, int_type, ASR::integerbozType::Decimal)),
                    int_type, nullptr));
                dims.push_back(al, dim);
            }
            if( !ASRUtils::is_dimension_dependent_only_on_arguments(
                    dims.p, dims.size(), false, true) ) {
                return;
            }
            insert_allocate(loc, x.m_target, dims.p, dims.size());
            explicitly_allocated.insert(sym);
        }
};

// A promoted result variable changes the function's signature, so the type
// every call to it reports has to be recomputed from the new signature.
class FixPromotedResultCalls:
    public ASR::CallReplacerOnExpressionsVisitor<FixPromotedResultCalls> {
    public:

        Allocator& al;
        const std::set<ASR::symbol_t*>& promoted_result_functions;

        FixPromotedResultCalls(Allocator& al_,
            const std::set<ASR::symbol_t*>& promoted_result_functions_):
            al(al_), promoted_result_functions(promoted_result_functions_) {}

        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            ASR::CallReplacerOnExpressionsVisitor<
                FixPromotedResultCalls>::visit_FunctionCall(x);
            ASR::symbol_t* fn = ASRUtils::symbol_get_past_external(x.m_name);
            if( promoted_result_functions.count(fn) == 0 ) {
                return;
            }
            ASR::FunctionType_t* ftype = ASRUtils::get_FunctionType(fn);
            if( ftype->m_return_var_type == nullptr ) {
                return;
            }
            ASR::ttype_t* return_type = ASRUtils::duplicate_type(
                al, ftype->m_return_var_type);
            ASR::dimension_t* dims = nullptr;
            size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
                return_type, dims);
            // The extents of the result come from the actual arguments. Take
            // them from the argument itself rather than from a physical cast
            // of it, so that the result type carries no cast of its own.
            Vec<ASR::call_arg_t> shape_args;
            shape_args.reserve(al, x.n_args);
            for( size_t i = 0; i < x.n_args; i++ ) {
                ASR::call_arg_t shape_arg = x.m_args[i];
                if( shape_arg.m_value ) {
                    shape_arg.m_value = ASRUtils::get_past_array_physical_cast(
                        shape_arg.m_value);
                }
                shape_args.push_back(al, shape_arg);
            }
            ASRUtils::ReplaceFunctionParamWithArg replacer(
                al, shape_args.p, shape_args.size());
            for( size_t i = 0; i < n_dims; i++ ) {
                if( dims[i].m_length ) {
                    dims[i].m_length = replacer.replace_FunctionParam_with_arg(
                        dims[i].m_length);
                }
                if( dims[i].m_start ) {
                    dims[i].m_start = replacer.replace_FunctionParam_with_arg(
                        dims[i].m_start);
                }
            }
            ASR::FunctionCall_t& xx = const_cast<ASR::FunctionCall_t&>(x);
            xx.m_type = return_type;
        }
};

// Rebuilds the signature of every function whose result variable was promoted.
inline void rebuild_promoted_signatures(Allocator& al,
    const std::set<ASR::symbol_t*>& promoted_result_functions) {
    for( ASR::symbol_t* sym: promoted_result_functions ) {
        ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(sym);
        fn->m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, fn->base.base.loc,
                fn->m_args, fn->n_args, fn->m_return_var,
                ASRUtils::get_FunctionType(fn), fn->m_symtab));
    }
}

// Promotes the allocatables of GPU device code to arrays of an explicit
// shape. This is what lets the shared array_op and intrinsic_function passes
// lower device code: an array operation there expands into a loop over the
// array's extents, and a device has no runtime descriptor to read them from.
void pass_promote_device_allocatable(
    Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &/*pass_options*/) {
    DeviceImplicitAllocInserter implicit_alloc(al);
    implicit_alloc.visit_TranslationUnit(unit);
    std::map<SymbolTable*, std::vector<ASR::symbol_t*>> scope2var;
    IsAllocatedCalled is_allocated_called(scope2var, true);
    is_allocated_called.visit_TranslationUnit(unit);
    PromoteAllocatableToNonAllocatable promoter(al, scope2var, true);
    promoter.visit_TranslationUnit(unit);
    if( promoter.promoted_symbols.empty() ) {
        return;
    }
    rebuild_promoted_signatures(al, promoter.promoted_result_functions);
    FixPromotedResultCalls fix_calls(al, promoter.promoted_result_functions);
    fix_calls.visit_TranslationUnit(unit);
    FixArrayPhysicalCastVisitor fix_array_physical_cast(
        al, promoter.promoted_symbols);
    fix_array_physical_cast.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

void pass_promote_allocatable_to_nonallocatable(
    Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &/*pass_options*/) {
    std::map<SymbolTable*, std::vector<ASR::symbol_t*>> scope2var;
    IsAllocatedCalled is_allocated_called(scope2var);
    is_allocated_called.visit_TranslationUnit(unit);
    PromoteAllocatableToNonAllocatable promoter(al, scope2var);
    promoter.visit_TranslationUnit(unit);
    promoter.visit_TranslationUnit(unit);
    FixArrayPhysicalCastVisitor fix_array_physical_cast(al, promoter.promoted_symbols);
    fix_array_physical_cast.visit_TranslationUnit(unit);
    FixMoveAssignment fix_move_assignment(al);
    fix_move_assignment.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
