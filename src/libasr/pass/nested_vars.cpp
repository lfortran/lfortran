#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/nested_vars.h>
#include <libasr/pass/pass_utils.h>
#include <llvm/IR/Verifier.h>
#include <unordered_map>

namespace LCompilers {

using ASR::down_cast;

/*

TODO: add docs

*/

class NestedVarVisitor : public ASR::BaseWalkVisitor<NestedVarVisitor>
{
public:
    Allocator &al;
    size_t nesting_depth = 0;
    SymbolTable* current_scope;
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> nesting_map;

    NestedVarVisitor(Allocator& al_): al(al_) {
        current_scope = nullptr;
    };

    ASR::symbol_t *cur_func_sym = nullptr;
    ASR::symbol_t *par_func_sym = nullptr;

    template<typename T>
    void visit_procedure(const T &x) {
        nesting_depth++;
        bool is_func_visited = false;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                par_func_sym = cur_func_sym;
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(
                    item.second);
                if (!is_func_visited) {
                    is_func_visited = true;
                    for (size_t i = 0; i < x.n_body; i++) {
                        visit_stmt(*x.m_body[i]);
                    }
                }
                visit_Function(*s);
            }
        }
        if (!is_func_visited) {
            is_func_visited = true;
            for (size_t i = 0; i < x.n_body; i++) {
                visit_stmt(*x.m_body[i]);
            }
        }
        nesting_depth--;
    }


    void visit_Program(const ASR::Program_t &x) {
        ASR::symbol_t *cur_func_sym_copy = cur_func_sym;
        cur_func_sym = (ASR::symbol_t*)(&x);
        current_scope = x.m_symtab;
        visit_procedure(x);
        cur_func_sym = cur_func_sym_copy;
    }

    void visit_Function(const ASR::Function_t &x) {
        ASR::symbol_t *cur_func_sym_copy = cur_func_sym;
        cur_func_sym = (ASR::symbol_t*)(&x);
        current_scope = x.m_symtab;
        visit_procedure(x);
        cur_func_sym = cur_func_sym_copy;
    }

    void visit_Var(const ASR::Var_t &x) {
        // Only attempt if we are actually in a nested function
        if (nesting_depth > 1) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(x.m_v));
            // If the variable is not defined in the current scope, it is a
            // "needed global" since we need to be able to access it from the
            // nested procedure.
            if (current_scope && current_scope->get_symbol(v->m_name) == nullptr) {
                nesting_map[par_func_sym].insert(x.m_v);
            }
        }
    }
};


class ReplacerNestedVars: public ASR::BaseExprReplacer<ReplacerNestedVars> {

public:
    std::map<ASR::symbol_t*, ASR::symbol_t*> nested_var_to_ext_var;
    ReplacerNestedVars(){}

    void replace_Var(ASR::Var_t* x) {
        if (nested_var_to_ext_var.find(x->m_v) != nested_var_to_ext_var.end()) {
            x->m_v = nested_var_to_ext_var[x->m_v];
        }
    }
};

class ReplaceNestedVisitor: public ASR::CallReplacerOnExpressionsVisitor<ReplaceNestedVisitor> {
    private:

    Allocator& al;
    ReplacerNestedVars replacer;

    public:

    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &nesting_map;
    std::map<ASR::symbol_t*, ASR::symbol_t*> nested_var_to_ext_var;

    ReplaceNestedVisitor(Allocator& al_,
        std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &n_map) : al(al_),
        replacer(ReplacerNestedVars()), nesting_map(n_map) {}


    // void call_replacer() {
    //     replacer.current_expr = current_expr;
    //     replacer.current_scope = current_scope;
    //     replacer.replace_expr(*current_expr);
    // }


    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        SymbolTable* current_scope_copy = current_scope;

        // Add the nested vars by creating a new module

        for (auto &it: nesting_map) {
            // Iterate on each function with nested vars and create a context in
            // a new module.
            current_scope = al.make_new<SymbolTable>(current_scope_copy);
            std::string module_name = "__lcompilers_created__nested_context__" + std::string(
                                    ASRUtils::symbol_name(it.first));
            std::map<ASR::symbol_t*, std::string> sym_to_name;
            module_name = current_scope->get_unique_name(module_name);
            for (auto &it2: it.second) {
                std::string new_ext_var = module_name + std::string(ASRUtils::symbol_name(it2));
                ASR::ttype_t* type = ASR::down_cast<ASR::Variable_t>(it2)->m_type;
                new_ext_var = current_scope->get_unique_name(new_ext_var);
                PassUtils::create_auxiliary_variable(
                        it2->base.loc, new_ext_var,
                        al, current_scope, type, ASR::intentType::In);
                sym_to_name[it2] = new_ext_var;
            }
            ASR::asr_t *tmp = ASR::make_Module_t(al, x.base.base.loc,
                                            /* a_symtab */ current_scope,
                                            /* a_name */ s2c(al, module_name),
                                            nullptr,
                                            0,
                                            false, false);
            ASR::symbol_t* mod_sym = ASR::down_cast<ASR::symbol_t>(tmp);
            current_scope->add_symbol(module_name, mod_sym);
        }


        current_scope = x.m_global_scope;
        for (auto &a : x.m_global_scope->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_scope = current_scope_copy;
    }


};

void pass_nested_vars(Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    NestedVarVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor x(al);
    x.visit_TranslationUnit(unit);
    // {
    //
    //     for (auto &it: v.nesting_map) {
    //         std::cout<<"func name " << ASRUtils::symbol_name(it.first) << '\n';
    //         for (auto &it2: it.second) {
    //             std::cout<<ASRUtils::symbol_name(it2)<<' ';
    //         }
    //     }
    // }
}


} // namespace LCompilers
