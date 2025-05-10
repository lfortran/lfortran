#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/inline_function_calls.h>
#include <libasr/pass/pass_utils.h>

#include <vector>
#include <map>
#include <unordered_map>
#include <utility>


namespace LCompilers {

typedef std::unordered_map<ASR::symbol_t*, ASR::symbol_t*> SymbolToSymbol;

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

class FixSymbols: public ASR::BaseWalkVisitor<FixSymbols> {
    private:

    SymbolToSymbol& function2currentscope;

    public:

    FixSymbols(SymbolToSymbol& function2currentscope_):
        function2currentscope(function2currentscope_) {}

    void visit_Var(const ASR::Var_t& x) {
        ASR::Var_t& xx = const_cast<ASR::Var_t&>(x);
        xx.m_v = function2currentscope[xx.m_v];
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
        // The type of those Variable symbols shouldn’t be FunctionType.
        for( auto sym: function->m_symtab->get_scope() ) {
            if( !ASR::is_a<ASR::Variable_t>(*sym.second) ||
                (ASR::down_cast<ASR::Variable_t>(sym.second)->m_intent != ASRUtils::intent_in && // TODO: Remove these intent checks
                ASR::down_cast<ASR::Variable_t>(sym.second)->m_intent != ASRUtils::intent_local && // IntentOut can be accomodated
                ASR::down_cast<ASR::Variable_t>(sym.second)->m_intent != ASRUtils::intent_unspecified && // via pointer variables
                ASR::down_cast<ASR::Variable_t>(sym.second)->m_intent != ASRUtils::intent_return_var) ||
                ASRUtils::is_array(ASR::down_cast<ASR::Variable_t>(sym.second)->m_type) ||
                ASR::is_a<ASR::String_t>(
                    *ASRUtils::extract_type(ASR::down_cast<ASR::Variable_t>(sym.second)->m_type)) ) { // TODO: Remove this check as well, use pointers for arrays
                return false;
            }

            if( ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASR::down_cast<ASR::Variable_t>(sym.second)->m_type))) ) {
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

        return true;
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        if( !check_inline_possibility(x->m_name, x) ) {
            return ;
        }

        ASR::Function_t* function = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x->m_name));

        // Step 1
        // Duplicate entire symbol table of function
        // into the current scope
        SymbolToSymbol function2currentscope, currentscope2function;
        std::unordered_map<uint64_t, ASR::symbol_t*> argidx2function;
        std::unordered_map<ASR::symbol_t*, ASR::expr_t*> function_locals2init_expr;
        ASR::symbol_t* return_variable = nullptr;

        const Location& loc = x->base.base.loc;

        ASRUtils::ExprStmtDuplicator type_duplicator(al);
        for( auto& sym: function->m_symtab->get_scope() ) {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*sym.second));
            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(sym.second);
            std::string local_sym_unique_name = current_scope->get_unique_name(variable->m_name);
            ASR::ttype_t* local_ttype_copy = type_duplicator.duplicate_ttype(variable->m_type);
            ASR::symbol_t* local_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, current_scope, s2c(al, local_sym_unique_name),
                nullptr, 0, ASRUtils::intent_local, nullptr, nullptr, variable->m_storage,
                local_ttype_copy, variable->m_type_declaration, variable->m_abi, variable->m_access,
                variable->m_presence, variable->m_value_attr, variable->m_target_attr, variable->m_contiguous_attr));
            current_scope->add_symbol(local_sym_unique_name, local_sym);
            if( variable->m_intent == ASRUtils::intent_local ||
                variable->m_intent == ASRUtils::intent_unspecified ) {
                if( variable->m_symbolic_value ) {
                    function_locals2init_expr[local_sym] = variable->m_symbolic_value;
                }
            } else if( variable->m_intent == ASRUtils::intent_return_var ) {
                return_variable = local_sym;
            }

            function2currentscope[sym.second] = local_sym;
            currentscope2function[local_sym] = sym.second;
        }

        FixSymbols fix_symbols(function2currentscope);
        for( auto sym: currentscope2function ) {
            fix_symbols.visit_symbol(*sym.first);
        }

        for( size_t i = 0; i < function->n_args; i++ ) {
            argidx2function[i] = ASR::down_cast<ASR::Var_t>(function->m_args[i])->m_v;
        }

        // Step 2
        // Initialise local copies of argument variables.
        LCOMPILERS_ASSERT(x->n_args == function->n_args);
        for( size_t i = 0; i < x->n_args; i++ ) {
            ASR::symbol_t* original_symbol = argidx2function.at(i);
            ASR::symbol_t* local_symbol = function2currentscope[original_symbol];
            ASR::stmt_t* init_stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                al, loc, ASRUtils::EXPR(ASR::make_Var_t(al, loc, local_symbol)),
                x->m_args[i].m_value, nullptr, false
            ));
            current_body->push_back(al, init_stmt);
        }

        // Initialise local copies of variables declared after
        // arguments in the function
        for( auto sym: currentscope2function ) {
            if( function_locals2init_expr.find(sym.first)
                != function_locals2init_expr.end() ) {
                ASR::stmt_t* init_stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, loc, ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym.first)),
                    function_locals2init_expr[sym.first], nullptr, false
                ));
                current_body->push_back(al, init_stmt);
            }
        }

        // Duplicate entire body of function
        // into the current body
        // Step 3 - Replace symbols in the duplicated body
        // with their local copies
        ASRUtils::ExprStmtDuplicator stmt_duplicator(al);
        for( size_t i = 0; i < function->n_body; i++ ) {
            ASR::stmt_t* stmt_copy = stmt_duplicator.duplicate_stmt(function->m_body[i]);
            fix_symbols.visit_stmt(*stmt_copy);
            current_body->push_back(al, stmt_copy);
        }

        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_variable));
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
