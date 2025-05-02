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

class InlineFunctionCalls: public ASR::BaseExprReplacer<InlineFunctionCalls> {

    public:

    Allocator& al;
    Vec<ASR::stmt_t*>* current_body = nullptr;
    SymbolTable* current_scope = nullptr;
    SymbolTable* global_scope = nullptr;

    InlineFunctionCalls(Allocator& al_): al(al_) {}

    bool check_inline_possibility(ASR::symbol_t* func) {
        // Symbol should be a ASR::Function_t
        func = ASRUtils::symbol_get_past_external(func);
        if( !ASR::is_a<ASR::Function_t>(*func) ) {
            return false;
        }

        ASR::Function_t* function = ASR::down_cast<ASR::Function_t>(func);

        // ⁠Function should have only Variable symbols in its symtab.
        // The type of those Variable symbols shouldn’t be FunctionType.
        for( auto sym: function->m_symtab->get_scope() ) {
            if( !ASR::is_a<ASR::Variable_t>(*sym.second) ) {
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
        Vec<ASR::symbol_t*> vars;
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

        return true;
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        if( !check_inline_possibility(x->m_name) ) {
            return ;
        }

        ASR::Function_t* function = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x->m_name));

        // Step 1
        // Duplicate entire symbol table of function
        // into the current scope
        typedef std::unordered_map<ASR::symbol_t*, ASR::symbol_t*> SymbolToSymbol;
        SymbolToSymbol function2currentscope, currentscope2function;
        std::unordered_map<ASR::symbol_t*, uint64_t> function2argidx;
        std::unordered_map<ASR::symbol_t*, ASR::expr_t*> function_locals2init_expr;

        const Location& loc = x->base.base.loc;

        ASRUtils::ExprStmtDuplicator type_duplicator(al);
        for( auto& sym: function->m_symtab->get_scope() ) {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*sym.second));
            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(sym.second);
            std::string local_sym_unique_name = current_scope->get_unique_name(variable->m_name);
            ASR::ttype_t* local_ttype_copy = type_duplicator.duplicate_ttype(variable->m_type);
            ASR::symbol_t* local_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, current_scope, s2c(al, local_sym_unique_name),
                nullptr, 0, variable->m_intent, nullptr, nullptr, variable->m_storage,
                local_ttype_copy, variable->m_type_declaration, variable->m_abi, variable->m_access,
                variable->m_presence, variable->m_value_attr, variable->m_target_attr, variable->m_contiguous_attr));
            ASR::Variable_t* local_variable = ASR::down_cast<ASR::Variable_t>(local_sym);
            if( variable->m_intent == ASRUtils::intent_local ||
                variable->m_intent == ASRUtils::intent_unspecified ) {
                function_locals2init_expr[local_sym] = variable->m_symbolic_value;
            }

            function2currentscope[sym.second] = local_sym;
            currentscope2function[local_sym] = sym.second;
        }

        for( size_t i = 0; i < function->n_args; i++ ) {
            function2argidx[ASR::down_cast<ASR::Var_t>(function->m_args[i])->m_v] = i;
        }

        // Step 2
        // Initialise local copies of argument variables.

        // Initialise local copies of variables declared after
        // arguments in the function
        // Duplicate entire body of function
        // into the current body

        // Step 3 - Replace symbols in the duplicated body
        // with their local copies

    }

};

class InlineFunctionCallsVisitor: public ASR::CallReplacerOnExpressionsVisitor<InlineFunctionCallsVisitor> {

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>* current_body;
    InlineFunctionCalls replacer;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_body = current_body;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
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

};

void pass_inline_function_calls(Allocator &al, ASR::TranslationUnit_t &unit,
                                const LCompilers::PassOptions& pass_options) {

}


} // namespace LCompilers
