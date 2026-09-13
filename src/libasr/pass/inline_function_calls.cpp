#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/inline_function_calls.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/scoped_inlining.h>
#include <libasr/pass/pass_utils.h>

#include <vector>
#include <map>
#include <unordered_map>
#include <utility>


namespace LCompilers {

class VarCollector: public ASR::BaseWalkVisitor<VarCollector> {
    private:

    Allocator& al;
    Vec<ASR::symbol_t*>& vars;

    public:

    VarCollector(Allocator& al_, Vec<ASR::symbol_t*>& vars_): al(al_), vars(vars_) {}

    void visit_Var(const ASR::Var_t& x) {
        vars.push_back(al, const_cast<ASR::symbol_t*>(x.m_v));
    }
};

void transform_stmts_impl(Allocator& al, ASR::stmt_t**& m_body, size_t& n_body,
    Vec<ASR::stmt_t*>*& current_body, std::function<void(const ASR::stmt_t&)> visit_stmt) {
    Vec<ASR::stmt_t*>* current_body_copy = current_body;
    Vec<ASR::stmt_t*> current_body_vec; current_body_vec.reserve(al, 1);
    current_body_vec.reserve(al, n_body);
    current_body = &current_body_vec;
    for (size_t i = 0; i < n_body; i++) {
        visit_stmt(*m_body[i]);
        current_body->push_back(al, m_body[i]);
    }
    m_body = current_body_vec.p; n_body = current_body_vec.size();
    current_body = current_body_copy;
}

class CollectReturnStmt: public ASR::BaseWalkVisitor<CollectReturnStmt> {

    public:

    Allocator& al;
    Vec<ASR::stmt_t*>& return_stmts;
    size_t current_stmt_index;

    CollectReturnStmt(Allocator& al_, Vec<ASR::stmt_t*>& return_stmts_):
        al(al_), return_stmts(return_stmts_), current_stmt_index(0) {
    }

    void visit_Return(const ASR::Return_t& x) {
        return_stmts.push_back(al, const_cast<ASR::stmt_t*>(&(x.base)));
    }

};

class CollectCalls: public ASR::BaseWalkVisitor<CollectCalls> {

    public:

    Allocator& al;
    Vec<ASR::symbol_t*>& called_syms;

    CollectCalls(Allocator& al_, Vec<ASR::symbol_t*>& called_syms_):
        al(al_), called_syms(called_syms_) {
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        called_syms.push_back(al, ASRUtils::symbol_get_past_external(x.m_name));
        ASR::BaseWalkVisitor<CollectCalls>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        called_syms.push_back(al, ASRUtils::symbol_get_past_external(x.m_name));
        ASR::BaseWalkVisitor<CollectCalls>::visit_FunctionCall(x);
    }

};

class InlineFunctionCalls: public ASR::BaseExprReplacer<InlineFunctionCalls> {

    public:

    Allocator& al;
    Vec<ASR::stmt_t*>* current_body = nullptr;
    SymbolTable* current_scope = nullptr;
    SymbolTable* global_scope = nullptr;

    InlineFunctionCalls(Allocator& al_): al(al_) {}

    bool check_non_global_function_calls(ASR::Function_t* function) {
        Vec<ASR::symbol_t*> called_syms; called_syms.reserve(al, 1);
        for( size_t i = 0; i < function->n_body; i++ ) {
            called_syms.n = 0;
            CollectCalls call_collector(al, called_syms);
            call_collector.visit_stmt(*function->m_body[i]);
            for( size_t j = 0; j < called_syms.size(); j++ ) {
                if( ASRUtils::symbol_parent_symtab(called_syms[j])->parent != nullptr ) {
                    return true;
                }
            }
        }

        return false;
    }

    bool check_inline_possibility(ASR::symbol_t* func, ASR::FunctionCall_t* func_call) {
        if( current_body == nullptr ) {
            // Inlining not possible because the function call is not
            // inside the body of a Function, Program
            return false;
        }
        // Symbol should be a ASR::Function_t
        func = ASRUtils::symbol_get_past_external(func);
        if( !ASR::is_a<ASR::Function_t>(*func) ) {
            return false;
        }

        ASR::Function_t* function = ASR::down_cast<ASR::Function_t>(func);

        if( ASRUtils::symbol_abi(func) != ASR::abiType::Source ||
            ASR::down_cast<ASR::FunctionType_t>(function->m_function_signature)->m_deftype
                != ASR::deftypeType::Implementation ) {
            return false;
        }

        // Number of arguments in function call
        // must match the number of arguments
        // in function definition.
        if( function->n_args != func_call->n_args ) {
            return false;
        }

        // ⁠Function should have only Variable symbols in its symtab.
        // The type of those Variable symbols shouldn't be FunctionType.
        for( auto sym: function->m_symtab->get_scope() ) {
            if( !ASR::is_a<ASR::Variable_t>(*sym.second) ||
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(ASR::down_cast<ASR::Variable_t>(sym.second)->m_type)) ||
                ASRUtils::is_class_type(
                    ASRUtils::extract_type(ASR::down_cast<ASR::Variable_t>(sym.second)->m_type)) ||
                ASR::is_a<ASR::String_t>(
                    *ASRUtils::extract_type(ASR::down_cast<ASR::Variable_t>(sym.second)->m_type)) ||  // TODO: Remove this check as well
                ASRUtils::is_pointer(ASR::down_cast<ASR::Variable_t>(sym.second)->m_type) ||
                ASRUtils::is_allocatable(ASR::down_cast<ASR::Variable_t>(sym.second)->m_type) ) {
                return false;
            }

            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(sym.second);
            // Inlining would give each call its own copy of persistent local state.
            if( variable->m_storage == ASR::storage_typeType::Save ) {
                return false;
            }

            if( ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASR::down_cast<ASR::Variable_t>(sym.second)->m_type))) ) {
                return false;
            }

            // Don't inline functions with assumed-size array parameters
            ASR::ttype_t* var_type = variable->m_type;
            if( ASRUtils::is_array(var_type) ) {
                ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(
                    ASRUtils::type_get_past_allocatable_pointer(var_type));
                if( arr->m_physical_type == ASR::array_physical_typeType::UnboundedPointerArray ) {
                    return false;
                }
            }

            // Inlined locals are allocated at the caller's entry, before
            // copies of the actuals are assigned. A local whose extent is
            // an argument would be allocated empty.
            if( (variable->m_intent == ASRUtils::intent_local ||
                 variable->m_intent == ASRUtils::intent_unspecified) &&
                ASRUtils::is_array(var_type) &&
                !ASRUtils::is_fixed_size_array(var_type) ) {
                return false;
            }

        }

        for( size_t i = 0; i < func_call->n_args; i++ ) {
            if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*func_call->m_args[i].m_value) ||
                ASRUtils::is_pointer(ASRUtils::expr_type(func_call->m_args[i].m_value)) ||
                ASRUtils::is_allocatable(ASRUtils::expr_type(func_call->m_args[i].m_value)) ) {
                return false;
            }
        }

        // Function should only call Functions from global scope
        for( size_t i = 0; i < function->n_dependencies; i++ ) {
            if( global_scope->resolve_symbol(
                    function->m_dependencies[i]) == nullptr ) {
                return false;
            }
        }

        // Function should only have local variables and no external dependencies (nested variables, etc).
        Vec<ASR::symbol_t*> vars; vars.reserve(al, 1);
        VarCollector var_collector(al, vars);
        for( size_t i = 0; i < function->n_body; i++ ) {
            var_collector.visit_stmt(*function->m_body[i]);
        }

        for( size_t i = 0; i < vars.size(); i++ ) {
            if( ASRUtils::symbol_parent_symtab(vars[i])->get_counter() !=
                function->m_symtab->get_counter() ) {
                return false;
            }
        }

        // Functions/Subroutines which are being called
        // from non-global scope (like the ones defined in external module)
        // and are also not present in the dependencies of a function,
        // are detected via the following `check_non_global_function_calls`.
        // A function (say X, defined in module A) doesn't contain functions
        // from other modules (say Y, defined in module B) in
        // its dependencies because module A contains module B in its
        // dependencies there by ensuring the dependency of X on Y.
        // Changing this dependency design is a separate issue,
        // for now the following call performs the check according
        // to current dependency design.
        if( check_non_global_function_calls(function) ) {
            return false;
        }

        // Check if there is only one return statement
        // and that too in the end of the function
        Vec<ASR::stmt_t*> return_stmts; return_stmts.reserve(al, 1);
        CollectReturnStmt collect_return_stmts(al, return_stmts);
        collect_return_stmts.visit_Function(*function);
        // More than one return statements
        if( return_stmts.size() > 1 ) {
            return false;
        }

        // Only one return statement but not in the end.
        if( return_stmts.size() == 1 &&
            function->m_body[function->n_body - 1] != return_stmts[0] ) {
            return false;
        }


        return true;
    }

    // Only the arm of a conditional expression that is chosen is evaluated
    // (Fortran 2023, 10.1.4 NOTE 3), while inlining appends the body of the
    // called function to the enclosing statement list, which would run it
    // whichever arm is taken. Clearing current_body makes
    // check_inline_possibility decline every call inside the node. The
    // condition is evaluated unconditionally and could still be inlined, but
    // declining it too keeps this to one rule.
    void replace_IfExp(ASR::IfExp_t* x) {
        Vec<ASR::stmt_t*>* current_body_copy = current_body;
        current_body = nullptr;
        ASR::BaseExprReplacer<InlineFunctionCalls>::replace_IfExp(x);
        current_body = current_body_copy;
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        if( !check_inline_possibility(x->m_name, x) ) {
            return ;
        }

        ASR::Function_t* function = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x->m_name));

        const Location &loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        SymbolTable *scope = al.make_new<SymbolTable>(current_scope);
        std::map<ASR::symbol_t*, ASR::expr_t*> substitutions;
        std::vector<ASR::stmt_t*> bindings;
        for (size_t i = 0; i < function->n_args; i++) {
            auto *dummy = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(function->m_args[i])->m_v);
            ASR::ttype_t *type = ASRUtils::duplicate_type(al, dummy->m_type);
            bool reference = dummy->m_intent == ASRUtils::intent_out ||
                dummy->m_intent == ASRUtils::intent_inout ||
                ASRUtils::is_array(type);
            if (reference) {
                if (ASRUtils::is_array(type)) {
                    type = ASRUtils::duplicate_type_with_empty_dims(al, type,
                        ASR::array_physical_typeType::DescriptorArray, true);
                }
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    ASRUtils::type_get_past_allocatable_pointer(type)));
            }
            ASR::expr_t *local = b.Variable(scope,
                PassUtils::inline_local_name(scope, dummy->m_name), type,
                ASR::intentType::Local);
            substitutions.emplace(&dummy->base, local);
            bindings.push_back(reference
                ? ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, loc,
                    local, x->m_args[i].m_value))
                : b.Assignment(local, x->m_args[i].m_value));
        }
        ASR::expr_t *result = b.Variable(current_scope,
            current_scope->get_unique_name(
                std::string(function->m_name) + "_result"),
            ASRUtils::duplicate_type(al, ASRUtils::expr_type(function->m_return_var)),
            ASR::intentType::Local);
        ASR::stmt_t *inlined = PassUtils::inline_in_block(al, loc, *function,
            scope, std::move(substitutions), result, bindings, {});
        if (!inlined) return;
        current_body->push_back(al, inlined);
        *current_expr = result;
    }

    void replace_OverloadedCompare(ASR::OverloadedCompare_t* /*x*/) {
    }

};

class InlineFunctionCallsVisitor: public ASR::CallReplacerOnExpressionsVisitor<InlineFunctionCallsVisitor> {

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>* current_body = nullptr;
    InlineFunctionCalls replacer;

    public:

    InlineFunctionCallsVisitor(Allocator& al_): al(al_), replacer(al) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_body = current_body;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        transform_stmts_impl(al, m_body, n_body, current_body,
            [this](const ASR::stmt_t& stmt) { visit_stmt(stmt); });
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t& x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        replacer.global_scope = x.m_symtab;
        Vec<ASR::symbol_t*> functions, modules, programme;
        functions.reserve(al, 1); modules.reserve(al, 1); programme.reserve(al, 1);
        for( auto sym: x.m_symtab->get_scope() ) {
            switch( sym.second->type ) {
                case ASR::symbolType::Function: {
                    functions.push_back(al, sym.second);
                    break;
                }
                case ASR::symbolType::Module: {
                    modules.push_back(al, sym.second);
                    break;
                }
                case ASR::symbolType::Program: {
                    programme.push_back(al, sym.second);
                    break;
                }
                default: {
                }
            }
        }

        for( size_t i = 0; i < functions.size(); i++ ) {
            visit_Function(*ASR::down_cast<ASR::Function_t>(functions.p[i]));
        }

        for( size_t i = 0; i < modules.size(); i++ ) {
            visit_Module(*ASR::down_cast<ASR::Module_t>(modules.p[i]));
        }

        for( size_t i = 0; i < programme.size(); i++ ) {
            visit_Program(*ASR::down_cast<ASR::Program_t>(programme.p[i]));
        }
        current_scope = current_scope_copy;

    }

    void visit_Module(const ASR::Module_t& x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for( auto sym: x.m_symtab->get_scope() ) {
            if( ASR::is_a<ASR::Function_t>(*sym.second) ) {
                visit_Function(*ASR::down_cast<ASR::Function_t>(sym.second));
            }
        }
        current_scope = current_scope_copy;
    }

    void visit_Program(const ASR::Program_t& x) {
        ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for( auto sym: x.m_symtab->get_scope() ) {
            if( ASR::is_a<ASR::Function_t>(*sym.second) ) {
                visit_Function(*ASR::down_cast<ASR::Function_t>(sym.second));
            }
        }
        transform_stmts(xx.m_body, xx.n_body);
        current_scope = current_scope_copy;
    }

    // See InlineFunctionCalls::replace_IfExp: an arm of a conditional
    // expression has no statement list that a call in it may be inlined into.
    void visit_IfExp(const ASR::IfExp_t& x) {
        Vec<ASR::stmt_t*>* current_body_copy = current_body;
        current_body = nullptr;
        ASR::CallReplacerOnExpressionsVisitor<InlineFunctionCallsVisitor>::visit_IfExp(x);
        current_body = current_body_copy;
    }

    void visit_OverloadedCompare(const ASR::OverloadedCompare_t& /*x*/) {
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        ASR::WhileLoop_t& xx = const_cast<ASR::WhileLoop_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        ASR::DoLoop_t& xx = const_cast<ASR::DoLoop_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        ASR::DoConcurrentLoop_t& xx = const_cast<ASR::DoConcurrentLoop_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
    }

};

void pass_inline_function_calls(Allocator &al, ASR::TranslationUnit_t &unit,
                                const LCompilers::PassOptions& /*pass_options*/) {
    InlineFunctionCallsVisitor inline_functions(al);
    inline_functions.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor update_dependencies(al);
    update_dependencies.visit_TranslationUnit(unit);
}


} // namespace LCompilers
