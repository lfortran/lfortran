#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/nested_vars.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <set>

namespace LCompilers {

using ASR::down_cast;

static SymbolTable *get_host_scope(SymbolTable *scope) {
    SymbolTable *start = scope;
    while (scope) {
        if (scope->asr_owner && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
            ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
            if (ASR::is_a<ASR::Function_t>(*owner_sym) || ASR::is_a<ASR::Program_t>(*owner_sym)) {
                return scope;
            }
        }
        scope = scope->parent;
    }
    return start;
}

static bool is_nested_call_symbol(SymbolTable *current_scope, ASR::symbol_t *sym) {
    SymbolTable *host_scope = get_host_scope(current_scope);
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
    if (ASR::is_a<ASR::Function_t>(*s)) {
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
        return fn->m_symtab->parent == host_scope;
    }
    if (ASR::is_a<ASR::Variable_t>(*s)) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        if (v->m_type_declaration &&
            ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(v->m_type_declaration))) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                ASRUtils::symbol_get_past_external(v->m_type_declaration));
            return fn->m_symtab->parent == host_scope;
        }
    }
    return false;
}

static bool is_sym_in_scope_chain(SymbolTable *current_scope, SymbolTable *sym_parent) {
    for (SymbolTable *scope = current_scope; scope; scope = scope->parent) {
        if (sym_parent == scope) {
            return true;
        }
    }
    return false;
}

static ASR::symbol_t *make_external_symbol(Allocator &al, SymbolTable *scope,
        ASR::symbol_t *target, const std::string &sym_name,
        const std::string &owner_name, const std::string &original_name,
        ASR::accessType access) {
    ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
        al, target->base.loc,
        /* a_symtab */ scope,
        /* a_name */ s2c(al, sym_name),
        target,
        s2c(al, owner_name), nullptr, 0, s2c(al, original_name),
        access
    );
    ASR::symbol_t *ext_sym = ASR::down_cast<ASR::symbol_t>(fn);
    scope->add_symbol(sym_name, ext_sym);
    return ext_sym;
}

static constexpr int64_t NESTED_CONTEXT_STACK_SIZE = 1048576;

/*

This pass captures the global variables that are used by the
nested functions. This is handled in the following way:

Consider,

subroutine A()
    variables declared: x, y, w, k, t

    subroutine B(l)
        variables declared: p, l
        x += y
        w += l
        p += k
    end

    call_subroutine B(k)
end

Then using this pass, we first capture the variables that are used by
nested functions. In our example, they are x, y and w from function A.
Then we create a __lcompilers_created__nested_context__A which contains all those
variables with same types but different names, say, global_context_module_for_A_x,
global_context_module_for_A_y, global_context_module_for_A_w, and
global_context_module_for_A_k.

This pass will then transform the above code to:

module __lcompilers_created__nested_context__A
    variables declared: global_context_module_for_A_x,
                        global_context_module_for_A_y,
                        global_context_module_for_A_w,
                        global_context_module_for_A_k
end

subroutine A()
    use __lcompilers_created__nested_context__A
    variables declared: x, y, w, k, t

    subroutine B(l)
        variables declared: p, l
        global_context_module_for_A_x += global_context_module_for_A_y
        global_context_module_for_A_w += l
        p += global_context_module_for_A_k
    end

    global_context_module_for_A_x = x
    global_context_module_for_A_y = y
    global_context_module_for_A_w = w
    global_context_module_for_A_k = k

    call_subroutine B(global_context_module_for_A_k)

    x = global_context_module_for_A_x
    y = global_context_module_for_A_y
    w = global_context_module_for_A_w
    k = global_context_module_for_A_k
end

This assignment and re-assignment to the variables is done only before and
after a function call. The same applies when a global variable from a program
is used by a nested function.

Note: We do change any variables inside the parent function A ** except **
the captured variables inside the function call arguments. The reason is to
preserve the assignments in case of intent out. This highlighted in the line
where we change to `call_subroutine B(global_context_module_for_A_k)`.


This Pass is designed using three classes:
1. NestedVarVisitor - This captures the variables for each function that
                      are used by nested functions and creates a map of
                      function_syms -> {variables_syms}.

2. ReplaceNestedVisitor - This class replaces all the variables inside the
                          nested functions with external module variables.

3. AssignNestedVars - This class add assignment stmts before and after
                      each function call stmts in the parent function.

*/

class NestedVarVisitor : public ASR::BaseWalkVisitor<NestedVarVisitor>
{
public:
    Allocator &al;
    size_t nesting_depth = 0;
    SymbolTable* current_scope;
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> nesting_map;
    std::set<ASR::symbol_t*> active_functions;

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
            if ( ASR::is_a<ASR::Variable_t>(*item.second) ) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
                if ( ASRUtils::is_array(v->m_type) ) {
                    ASR::dimension_t* m_dims;
                    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(v->m_type, m_dims);
                    for( size_t i = 0; i < n_dims; i++ ) {
                        if (m_dims[i].m_start) {
                            if ( ASR::is_a<ASR::ArraySize_t>(*m_dims[i].m_start)) {
                                visit_expr(*m_dims[i].m_start);
                            }
                        }
                        if (m_dims[i].m_length) {
                            if ( ASR::is_a<ASR::ArraySize_t>(*m_dims[i].m_length)) {
                                visit_expr(*m_dims[i].m_length);
                            } else if ( ASR::is_a<ASR::Var_t>(*m_dims[i].m_length)) {
                                visit_expr(*m_dims[i].m_length);
                            }
                        }
                    }
                }
            }
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::symbol_t* par_func_sym_copy = par_func_sym;
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
                par_func_sym = par_func_sym_copy;
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
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        visit_procedure(x);
        current_scope = current_scope_copy;
        cur_func_sym = cur_func_sym_copy;
    }

    void visit_Function(const ASR::Function_t &x) {
        ASR::symbol_t* x_sym = (ASR::symbol_t*)(&x);
        if (active_functions.find(x_sym) != active_functions.end()) {
            return;
        }
        active_functions.insert(x_sym);
        ASR::symbol_t *cur_func_sym_copy = cur_func_sym;
        cur_func_sym = x_sym;
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        visit_procedure(x);
        bool has_internal_procedures = false;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                has_internal_procedures = true;
                break;
            }
        }
        if (has_internal_procedures) {
            for (size_t i = 0; i < x.n_args; i++) {
                if (!ASR::is_a<ASR::Var_t>(*x.m_args[i])) {
                    continue;
                }
                ASR::symbol_t *arg_sym = ASR::down_cast<ASR::Var_t>(x.m_args[i])->m_v;
                ASR::symbol_t *arg_sym_past = ASRUtils::symbol_get_past_external(arg_sym);
                if (ASR::is_a<ASR::Function_t>(*arg_sym_past)) {
                    ASR::Function_t *arg_fn = ASR::down_cast<ASR::Function_t>(arg_sym_past);
                    if (ASRUtils::get_FunctionType(arg_fn)->m_deftype == ASR::deftypeType::Interface) {
                        nesting_map[x_sym].insert(arg_sym);
                    }
                } else if (ASR::is_a<ASR::Variable_t>(*arg_sym_past)) {
                    ASR::Variable_t *arg_var = ASR::down_cast<ASR::Variable_t>(arg_sym_past);
                    if (arg_var->m_type_declaration &&
                        ASR::is_a<ASR::Function_t>(
                            *ASRUtils::symbol_get_past_external(arg_var->m_type_declaration))) {
                        nesting_map[x_sym].insert(arg_sym);
                    }
                }
            }
        }
        current_scope = current_scope_copy;
        cur_func_sym = cur_func_sym_copy;
        active_functions.erase(x_sym);
    }

    void visit_Module(const ASR::Module_t &x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(item.second);
                visit_Function(*s);
            }
        }
        current_scope = current_scope_copy;
    }
    /// Is a variable declared in a module scope
    bool is_module_variable(ASR::Variable_t* const v){
        ASR::asr_t* const asr_owner = v->m_parent_symtab->asr_owner;
        return  ASR::is_a<ASR::symbol_t>(*asr_owner) &&
                ASR::is_a<ASR::Module_t>(*(ASR::symbol_t*)asr_owner);
    }

    void visit_Var(const ASR::Var_t &x) {
        // Only attempt if we are actually in a nested function
        if (nesting_depth > 1) {
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_v);
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                if (current_scope && par_func_sym &&
                    ASRUtils::symbol_parent_symtab(sym)->get_counter() != current_scope->get_counter()) {
                    nesting_map[par_func_sym].insert(x.m_v);
                }
                return;
            } else {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                if(is_module_variable(v)) return;
                visit_ttype(*v->m_type);
                // If the variable is not defined in the current scope, it is a
                // "needed global" since we need to be able to access it from the
                // nested procedure.
                if ( current_scope && par_func_sym &&
                    v->m_parent_symtab->get_counter() != current_scope->get_counter()) {
                    nesting_map[par_func_sym].insert(x.m_v);
                }
            }
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::symbol_t* fn_sym = x.m_name;
        if ( current_scope && par_func_sym && ASR::is_a<ASR::Variable_t>(*x.m_name) && ASR::down_cast<ASR::Variable_t>(x.m_name)->m_type_declaration &&
             ASRUtils::symbol_parent_symtab(fn_sym)->get_counter() != current_scope->get_counter() &&
             current_scope->parent && current_scope->parent->parent != nullptr &&
             (current_scope->parent)->get_counter() == ASRUtils::symbol_parent_symtab(fn_sym)->get_counter() ) {
            nesting_map[par_func_sym].insert(fn_sym);
        }
        ASR::BaseWalkVisitor<NestedVarVisitor>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (current_scope && par_func_sym && ASR::is_a<ASR::Variable_t>(*x.m_name)) {
            ASR::Variable_t* fn_var = ASR::down_cast<ASR::Variable_t>(x.m_name);
            if (fn_var->m_type_declaration &&
                ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(fn_var->m_type_declaration)) &&
                ASRUtils::symbol_parent_symtab(x.m_name)->get_counter() != current_scope->get_counter() &&
                current_scope->parent && current_scope->parent->parent != nullptr &&
                (current_scope->parent)->get_counter() == ASRUtils::symbol_parent_symtab(x.m_name)->get_counter()) {
                nesting_map[par_func_sym].insert(x.m_name);
            }
        }
        ASR::BaseWalkVisitor<NestedVarVisitor>::visit_FunctionCall(x);
    }

    void capture_namelist_vars(ASR::symbol_t *nml_sym) {
        if (nesting_depth <= 1 || !par_func_sym || !nml_sym) {
            return;
        }
        ASR::symbol_t *nml_sym_past = ASRUtils::symbol_get_past_external(nml_sym);
        if (!ASR::is_a<ASR::Namelist_t>(*nml_sym_past)) {
            return;
        }
        ASR::Namelist_t *nml = ASR::down_cast<ASR::Namelist_t>(nml_sym_past);
        for (size_t i = 0; i < nml->n_var_list; i++) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(nml->m_var_list[i]);
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                continue;
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            if (is_module_variable(v)) {
                continue;
            }
            nesting_map[par_func_sym].insert(nml->m_var_list[i]);
        }
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        if (x.m_nml) {
            capture_namelist_vars(x.m_nml);
        }
        ASR::BaseWalkVisitor<NestedVarVisitor>::visit_FileWrite(x);
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        if (x.m_nml) {
            capture_namelist_vars(x.m_nml);
        }
        ASR::BaseWalkVisitor<NestedVarVisitor>::visit_FileRead(x);
    }

    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t& x) {
        visit_expr(*x.m_array);
    }
};


class ReplacerNestedVars: public ASR::BaseExprReplacer<ReplacerNestedVars> {
private:
    Allocator &al;
public:
    SymbolTable *current_scope;
    std::map<ASR::symbol_t*, std::pair<std::string, ASR::symbol_t*>> nested_var_to_ext_var;
    bool skip_replace=false;
    ReplacerNestedVars(Allocator &_al) : al(_al) {}

    void replace_Var(ASR::Var_t* x) {
        if (nested_var_to_ext_var.find(x->m_v) != nested_var_to_ext_var.end()) {
            if (skip_replace) {
                return;
            }
            std::string m_name = nested_var_to_ext_var[x->m_v].first;
            ASR::symbol_t *t = nested_var_to_ext_var[x->m_v].second;
            std::string sym_name = ASRUtils::symbol_name(t);
            ASR::symbol_t *existing = current_scope->get_symbol(sym_name);
            if (existing != nullptr &&
                    ASR::is_a<ASR::ExternalSymbol_t>(*existing) &&
                    ASRUtils::symbol_get_past_external(existing) == t) {
                x->m_v = existing;
                return;
            }
            std::string unique_name = sym_name;
            if (existing != nullptr) {
                unique_name = current_scope->get_unique_name(sym_name, false);
            }
            ASR::symbol_t *ext_sym = make_external_symbol(al, current_scope, t, unique_name,
                m_name, sym_name, ASR::accessType::Public);
            x->m_v = ext_sym;
        }
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        ASR::BaseExprReplacer<ReplacerNestedVars>::replace_ArrayPhysicalCast(x);
        x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
    }

    void replace_ArrayBroadcast(ASR::ArrayBroadcast_t* x) {
        ASR::expr_t** current_expr_copy_161 = current_expr;
        current_expr = &(x->m_array);
        replace_expr(x->m_array);
        current_expr = current_expr_copy_161;
    }
};

class ReplaceNestedVisitor: public ASR::CallReplacerOnExpressionsVisitor<ReplaceNestedVisitor> {
    private:

    Allocator& al;
    ReplacerNestedVars replacer;

    public:
    size_t nesting_depth = 0;
    bool is_in_call = false;
    std::vector<ASR::symbol_t*> func_stack;
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &nesting_map;
    std::map<ASR::symbol_t*, std::pair<std::string, ASR::symbol_t*>> nested_var_to_ext_var;
    std::map<ASR::symbol_t*, ASR::symbol_t*> func_to_nested_module;
    std::map<ASR::symbol_t*, ASR::symbol_t*> ext_var_to_stack_var;
    std::map<ASR::symbol_t*, ASR::symbol_t*> func_to_stack_ptr;
    std::map<ASR::symbol_t*, ASR::symbol_t*> func_to_stack_next;
    std::map<ASR::symbol_t*, ASR::symbol_t*> nested_proc_ctx_var;
    std::map<std::pair<ASR::symbol_t*, ASR::symbol_t*>, ASR::symbol_t*> nested_namelists;

    ReplaceNestedVisitor(Allocator& al_,
        std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &n_map) : al(al_),
        replacer(al_), nesting_map(n_map) {}

    bool get_class_proc_nopass_val_safe(ASR::symbol_t* sym) {
        ASR::symbol_t* past_sym = ASRUtils::symbol_get_past_external(sym);
        if (ASR::is_a<ASR::Variable_t>(*past_sym) ||
            ASR::is_a<ASR::StructMethodDeclaration_t>(*past_sym)) {
            return ASRUtils::get_class_proc_nopass_val(sym);
        }
        return false;
    }

    ASR::symbol_t *resolve_captured_symbol_key(ASR::symbol_t *sym) {
        auto it = nested_var_to_ext_var.find(sym);
        if (it != nested_var_to_ext_var.end()) {
            return sym;
        }
        ASR::symbol_t *sym_past = ASRUtils::symbol_get_past_external(sym);
        for (auto &kv: nested_var_to_ext_var) {
            if (ASRUtils::symbol_get_past_external(kv.first) == sym_past) {
                return kv.first;
            }
        }
        std::string sym_name = ASRUtils::symbol_name(sym);
        for (auto &kv: nested_var_to_ext_var) {
            if (ASRUtils::symbol_name(kv.first) == sym_name) {
                return kv.first;
            }
        }
        return nullptr;
    }

    void call_replacer() {
        // Skip replacing original variables with context variables
        // in the parent function except for function/subroutine calls
        bool skip_replace = false;
        if (nesting_depth==1 && !is_in_call) skip_replace = true;
        replacer.current_expr = current_expr;
        replacer.current_scope = current_scope;
        replacer.skip_replace = skip_replace;
        replacer.replace_expr(*current_expr);
    }

    bool is_externally_defined(ASR::Variable_t* var) {
        SymbolTable* var_parent_symtab = var->m_parent_symtab;
        ASR::asr_t* asr_owner = var_parent_symtab->asr_owner;
        if (ASR::is_a<ASR::symbol_t>(*asr_owner)) {
            ASR::symbol_t* owner_sym = ASR::down_cast<ASR::symbol_t>(asr_owner);
            if ( ASR::is_a<ASR::Function_t>(*owner_sym) ) {
                ASR::Function_t* owner_func = ASR::down_cast<ASR::Function_t>(owner_sym);
                if (ASRUtils::get_FunctionType(owner_func)->m_abi == ASR::abiType::ExternalUndefined) {
                    return true; // Externally defined
                }
            }
        }
        return false;
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        current_scope = x.m_symtab;
        SymbolTable* current_scope_copy = current_scope;

        // Add the nested vars by creating a new module

        for (auto &it: nesting_map) {
            // Iterate on each function with nested vars and create a context in
            // a new module.
            current_scope = al.make_new<SymbolTable>(current_scope_copy);
            std::string module_name = "__lcompilers_created__nested_context__" + std::string(
                                    ASRUtils::symbol_name(it.first)) + "_";
            bool is_any_variable_externally_defined = false;
            module_name = current_scope->get_unique_name(module_name, false);
            ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4));
            std::string stack_ptr_name = current_scope->get_unique_name(
                "__lfortran_nested_ctx_stack_ptr", false);
            ASR::expr_t *zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                al, x.base.base.loc, 0, int_type));
            ASR::expr_t *stack_ptr_expr = PassUtils::create_auxiliary_variable(
                x.base.base.loc, stack_ptr_name, al, current_scope, int_type,
                ASR::intentType::Unspecified, nullptr, zero);
            ASR::symbol_t *stack_ptr_sym = ASR::down_cast<ASR::Var_t>(stack_ptr_expr)->m_v;
            func_to_stack_ptr[it.first] = stack_ptr_sym;
            std::string stack_next_name = current_scope->get_unique_name(
                "__lfortran_nested_ctx_next_id", false);
            ASR::expr_t *stack_next_expr = PassUtils::create_auxiliary_variable(
                x.base.base.loc, stack_next_name, al, current_scope, int_type,
                ASR::intentType::Unspecified, nullptr, zero);
            ASR::symbol_t *stack_next_sym = ASR::down_cast<ASR::Var_t>(stack_next_expr)->m_v;
            func_to_stack_next[it.first] = stack_next_sym;
            for (auto &it2: it.second) {
                std::string new_ext_var = std::string(ASRUtils::symbol_name(it2));
                new_ext_var = current_scope->get_unique_name(new_ext_var, false);
                ASR::symbol_t* past_it2 = ASRUtils::symbol_get_past_external(it2);
                auto has_unsupported_proc_stack_type = [&](ASR::ttype_t *tt) {
                    ASR::ttype_t *base = ASRUtils::type_get_past_allocatable_pointer(tt);
                    if (!ASR::is_a<ASR::FunctionType_t>(*base)) {
                        return false;
                    }
                    auto is_unsupported = [&](ASR::ttype_t *arg_type) {
                        ASR::ttype_t *arg_base = ASRUtils::type_get_past_allocatable_pointer(arg_type);
                        return ASR::is_a<ASR::StructType_t>(*arg_base) ||
                               ASR::is_a<ASR::UnionType_t>(*arg_base) ||
                               ASRUtils::is_class_type(arg_base);
                    };
                    ASR::FunctionType_t *ft = ASR::down_cast<ASR::FunctionType_t>(base);
                    if (ft->m_return_var_type != nullptr &&
                            is_unsupported(ft->m_return_var_type)) {
                        return true;
                    }
                    for (size_t i = 0; i < ft->n_arg_types; i++) {
                        if (is_unsupported(ft->m_arg_types[i])) {
                            return true;
                        }
                    }
                    return false;
                };
                auto create_stack_var = [&](ASR::symbol_t *captured_var) {
                    if (!ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(captured_var))) {
                        return;
                    }
                    ASR::ttype_t *ext_type = ASRUtils::symbol_type(captured_var);
                    if (has_unsupported_proc_stack_type(ext_type)) {
                        return;
                    }
                    ASR::ttype_t *ext_base_type =
                        ASRUtils::type_get_past_allocatable_pointer(ext_type);
                    if (ASRUtils::is_array(ext_type) ||
                            ASRUtils::is_allocatable(ext_type) ||
                            ASRUtils::is_pointer(ext_type) ||
                            ASRUtils::is_class_type(ext_base_type) ||
                            ASRUtils::is_character(*ext_base_type)) {
                        return;
                    }
                    ASR::dimension_t dim;
                    dim.loc = captured_var->base.loc;
                    dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, dim.loc, 1, int_type));
                    dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, dim.loc, NESTED_CONTEXT_STACK_SIZE, int_type));
                    ASR::dimension_t *dims = al.allocate<ASR::dimension_t>(1);
                    dims[0] = dim;
                    ASR::ttype_t *stack_type = ASRUtils::make_Array_t_util(
                        al, captured_var->base.loc, ASRUtils::duplicate_type(al, ext_type), dims, 1);
                    ASR::symbol_t *stack_type_decl = nullptr;
                    if (ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(captured_var))) {
                        stack_type_decl = ASR::down_cast<ASR::Variable_t>(
                            ASRUtils::symbol_get_past_external(captured_var))->m_type_declaration;
                    }
                    std::string stack_name = current_scope->get_unique_name(
                        std::string(ASRUtils::symbol_name(captured_var)) + "__stack", false);
                    ASR::expr_t *stack_expr = PassUtils::create_auxiliary_variable(
                        captured_var->base.loc, stack_name, al, current_scope, stack_type,
                        ASR::intentType::Unspecified, stack_type_decl, nullptr);
                    ASR::symbol_t *stack_sym = ASR::down_cast<ASR::Var_t>(stack_expr)->m_v;
                    ext_var_to_stack_var[captured_var] = stack_sym;
                };
                auto create_ctx_var = [&](ASR::symbol_t *captured_sym, ASR::symbol_t *captured_var) {
                    ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(
                        al, captured_var->base.loc, 4));
                    std::string ctx_name = current_scope->get_unique_name(
                        std::string(ASRUtils::symbol_name(captured_var)) + "__ctx", false);
                    ASR::expr_t *ctx_init = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, captured_var->base.loc, 0, int_type));
                    ASR::expr_t *ctx_expr = PassUtils::create_auxiliary_variable(
                        captured_var->base.loc, ctx_name, al, current_scope, int_type,
                        ASR::intentType::Unspecified, nullptr, ctx_init);
                    ASR::symbol_t *ctx_sym = ASR::down_cast<ASR::Var_t>(ctx_expr)->m_v;
                    nested_proc_ctx_var[captured_sym] = ctx_sym;
                    create_stack_var(ctx_sym);
                };
                if (ASR::is_a<ASR::Function_t>(*past_it2)) {
                    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(past_it2);
                    ASR::ttype_t* fn_type = ASRUtils::duplicate_type(al, fn->m_function_signature);
                    ASR::expr_t *sym_expr = PassUtils::create_auxiliary_variable(
                        it2->base.loc, new_ext_var, al, current_scope, fn_type,
                        ASR::intentType::Unspecified, (ASR::symbol_t*)fn, nullptr);
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(sym_expr)->m_v;
                    nested_var_to_ext_var[it2] = std::make_pair(module_name, sym);
                    if (ASRUtils::get_FunctionType(fn)->m_deftype != ASR::deftypeType::Implementation) {
                        create_stack_var(sym);
                    }
                    create_ctx_var(it2, sym);
                    continue;
                }
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(past_it2);
                if (!is_any_variable_externally_defined && is_externally_defined(var)) {
                    is_any_variable_externally_defined = true;
                }
                bool is_allocatable = ASRUtils::is_allocatable(var->m_type);
                bool is_pointer = ASRUtils::is_pointer(var->m_type);
                LCOMPILERS_ASSERT(!(is_allocatable && is_pointer));
                ASR::ttype_t* var_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(var->m_type));
                ASR::ttype_t* var_type_ = ASRUtils::type_get_past_array(var_type);
                if ( var->m_type_declaration &&
                     ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(var->m_type_declaration)) ) {
                    ASRUtils::SymbolDuplicator sd(al);
                    ASR::Variable_t* dup_var = ASR::down_cast<ASR::Variable_t>(sd.duplicate_Variable(var, current_scope));
                    dup_var->m_name = s2c(al, new_ext_var);
                    ASR::symbol_t* dup_sym = (ASR::symbol_t*) dup_var;
                    current_scope->add_symbol(new_ext_var, dup_sym);
                    nested_var_to_ext_var[it2] = std::make_pair(module_name, dup_sym);
                    create_stack_var(dup_sym);
                    create_ctx_var(it2, dup_sym);
                    continue;
                }
                ASR::symbol_t* m_derived_type_or_class_type = nullptr;
                if( ASR::is_a<ASR::StructType_t>(*var_type_)) {
                    ASR::symbol_t* derived_type_or_class_type = nullptr;
                    ASR::StructType_t* struct_t = ASR::down_cast<ASR::StructType_t>(var_type_);
                    derived_type_or_class_type = var->m_type_declaration;
                    if( current_scope->get_counter() != ASRUtils::symbol_parent_symtab(derived_type_or_class_type)->get_counter() ) {
                        m_derived_type_or_class_type = current_scope->get_symbol(
                            ASRUtils::symbol_name(derived_type_or_class_type));
                        if( m_derived_type_or_class_type == nullptr ) {
                            if (!ASR::is_a<ASR::Program_t>(
                                    *ASRUtils::get_asr_owner(ASRUtils::symbol_get_past_external(
                                        derived_type_or_class_type)))) {
                                char* fn_name = ASRUtils::symbol_name(derived_type_or_class_type);
                                ASR::symbol_t* original_symbol = ASRUtils::symbol_get_past_external(derived_type_or_class_type);
                                ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                                    al, derived_type_or_class_type->base.loc,
                                    /* a_symtab */ current_scope,
                                    /* a_name */ fn_name,
                                    original_symbol,
                                    ASRUtils::symbol_name(ASRUtils::get_asr_owner(original_symbol)),
                                    nullptr, 0, ASRUtils::symbol_name(original_symbol), ASR::accessType::Public
                                );
                                m_derived_type_or_class_type = ASR::down_cast<ASR::symbol_t>(fn);
                                current_scope->add_symbol(fn_name, m_derived_type_or_class_type);
                            } else {
                                ASRUtils::SymbolDuplicator sd(al);
                                sd.duplicate_symbol(derived_type_or_class_type, current_scope);
                                ASR::down_cast<ASR::Program_t>(
                                    ASRUtils::get_asr_owner(&var->base))->m_symtab->erase_symbol(
                                        ASRUtils::symbol_name(derived_type_or_class_type));
                                m_derived_type_or_class_type = current_scope->get_symbol(
                                    ASRUtils::symbol_name(derived_type_or_class_type));
                            }
                        }
                        if (ASR::is_a<ASR::StructType_t>(*var_type_)) {
                            var_type_ = ASRUtils::make_StructType_t_util(
                                            al,
                                            struct_t->base.base.loc,
                                            m_derived_type_or_class_type,
                                            ASR::down_cast<ASR::StructType_t>(var_type_)->m_is_cstruct);
                        }
                        if( ASR::is_a<ASR::Array_t>(*var_type) ) {
                            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(var_type);
                            var_type = ASRUtils::make_Array_t_util(al, var->base.base.loc,
                                var_type_, array_t->m_dims, array_t->n_dims);
                        } else if (ASRUtils::is_class_type(var->m_type)) {
                            // Preserve class indirection for captured class objects.
                            var_type = ASRUtils::TYPE(ASR::make_Pointer_t(
                                al, var_type->base.loc, var_type_));
                        }
                    }
                }
                if( (ASRUtils::is_array(var_type) && !is_pointer) ) {
                    var_type = ASRUtils::duplicate_type_with_empty_dims(al, var_type);
                    if (is_allocatable) {
                        var_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, var_type->base.loc, var_type));
                    } else {
                        var_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, var_type->base.loc,
                            ASRUtils::type_get_past_allocatable(var_type)));
                    }
                }
                // Create proper string type (a pointer deferred-length string)
                if(ASRUtils::is_string_only(var_type)){ // Any non-allocatable string variable .
                    ASR::String_t* str = ASRUtils::get_string_type(var_type);
                    if(!ASRUtils::is_allocatable(var->m_type)){
                        var_type = 
                            ASRUtils::TYPE(
                                ASR::make_Pointer_t(al, str->base.base.loc,
                                    ASRUtils::TYPE(ASR::make_String_t(al, str->base.base.loc, 1,
                                        nullptr, ASR::DeferredLength, ASR::DescriptorString))));
                    }
                } else if(ASRUtils::is_array_of_strings(var_type)){ // e.g -> `character(len=foo()) :: str(10)`
                    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(var_type));
                    ASR::String_t* string_t = ASRUtils::get_string_type(var_type);
                    if(string_t->m_len_kind == ASR::AssumedLength || (string_t->m_len && !ASRUtils::is_value_constant(string_t->m_len))){
                        // Create a new ASR::String node, To avoid using the original one.
                        array_t->m_type = ASRUtils::TYPE(ASR::make_String_t(al, string_t->base.base.loc, 1,
                                            nullptr, ASR::DeferredLength, ASR::DescriptorString));
                    }
                }
                if(is_allocatable && !ASRUtils::is_allocatable_or_pointer(var_type) ){ // Revert allocatable type back again
                    var_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, var_type->base.loc, var_type));
                }
                ASR::symbol_t* type_decl = nullptr;
                if (m_derived_type_or_class_type) {
                    type_decl = m_derived_type_or_class_type;
                }
                ASR::expr_t *sym_expr = PassUtils::create_auxiliary_variable(
                    it2->base.loc, new_ext_var, al, current_scope, var_type,
                    ASR::intentType::Unspecified, type_decl, nullptr);
                ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(sym_expr)->m_v;
                nested_var_to_ext_var[it2] = std::make_pair(module_name, sym);
                create_stack_var(sym);
            }
            ASR::asr_t *tmp = ASR::make_Module_t(al, x.base.base.loc,
                                            /* a_symtab */ current_scope,
                                            /* a_name */ s2c(al, module_name),
                                            nullptr,
                                            nullptr,
                                            0,
                                            false, false, false);
            if (is_any_variable_externally_defined) {
                // this module is externally defined, so we mark it as external
                current_scope->mark_all_variables_external(al);
            }
            ASR::symbol_t* mod_sym = ASR::down_cast<ASR::symbol_t>(tmp);
            x.m_symtab->add_symbol(module_name, mod_sym);
            func_to_nested_module[it.first] = mod_sym;
        }
        replacer.nested_var_to_ext_var = nested_var_to_ext_var;

        current_scope = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_scope = current_scope_copy;
    }

    void visit_Program(const ASR::Program_t &x) {
        nesting_depth++;
        func_stack.push_back((ASR::symbol_t*)&x);
        ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        transform_stmts(xx.m_body, xx.n_body);
        current_scope = current_scope_copy;
        func_stack.pop_back();
        nesting_depth--;
    }

    void visit_Module(const ASR::Module_t &x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        current_scope = current_scope_copy;
    }

    void visit_Variable(const ASR::Variable_t &x) {
        ASR::Variable_t& xx = const_cast<ASR::Variable_t&>(x);
        if ( ASRUtils::is_array(xx.m_type) ) {
            ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(xx.m_type));
            ASR::dimension_t* m_dims;
            size_t n_dims = ASRUtils::extract_dimensions_from_ttype(xx.m_type, m_dims);
            for( size_t i = 0; i < n_dims; i++ ) {
                if (m_dims[i].m_start) {
                    if ( ASR::is_a<ASR::ArraySize_t>(*m_dims[i].m_start)) {
                        ASR::expr_t** current_expr_copy_1 = current_expr;
                        current_expr = const_cast<ASR::expr_t**>(&(m_dims[i].m_start));
                        call_replacer();
                        current_expr = current_expr_copy_1;
                        visit_expr(*m_dims[i].m_start);
                    }
                }
                if (m_dims[i].m_length) {
                    if ( ASR::is_a<ASR::ArraySize_t>(*m_dims[i].m_length)) {
                        ASR::expr_t** current_expr_copy_2 = current_expr;
                        current_expr = const_cast<ASR::expr_t**>(&(m_dims[i].m_length));
                        call_replacer();
                        current_expr = current_expr_copy_2;
                        visit_expr(*m_dims[i].m_length);
                    } else if ( ASR::is_a<ASR::Var_t>(*m_dims[i].m_length) ) {
                        ASR::expr_t** current_expr_copy_3 = current_expr;
                        ASR::expr_t* m_length = const_cast<ASR::expr_t*>(m_dims[i].m_length);
                        current_expr = const_cast<ASR::expr_t**>(&(m_dims[i].m_length));
                        ASR::symbol_t* prev_sym = ASR::down_cast<ASR::Var_t>(m_length)->m_v;
                        call_replacer();
                        ASR::symbol_t* new_sym = ASR::down_cast<ASR::Var_t>(m_length)->m_v;
                        if ( prev_sym != new_sym ) {
                            // need to convert this to a pointer
                            array->m_physical_type = ASR::array_physical_typeType::PointerArray;
                        }
                        current_expr = current_expr_copy_3;
                        visit_expr(*m_dims[i].m_length);
                    }
                }
            }
        }
        ASR::CallReplacerOnExpressionsVisitor<ReplaceNestedVisitor>::visit_Variable(x);
    }

    void visit_Function(const ASR::Function_t &x) {
        nesting_depth++;
        func_stack.push_back((ASR::symbol_t*)&x);
        ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        visit_ttype(*x.m_function_signature);
        transform_stmts(xx.m_body, xx.n_body);
        if (x.m_return_var) {
            ASR::expr_t** current_expr_copy_1 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_return_var));
            call_replacer();
            current_expr = current_expr_copy_1;
            if( x.m_return_var )
            visit_expr(*x.m_return_var);
        }
        current_scope = current_scope_copy;
        func_stack.pop_back();
        nesting_depth--;
    }

    ASR::symbol_t* get_nested_namelist_symbol(ASR::symbol_t *parent_func_sym,
                                              ASR::symbol_t *nml_sym) {
        if (!parent_func_sym || !nml_sym) {
            return nullptr;
        }
        auto key = std::make_pair(parent_func_sym, nml_sym);
        auto it = nested_namelists.find(key);
        if (it != nested_namelists.end()) {
            return it->second;
        }
        auto mod_it = func_to_nested_module.find(parent_func_sym);
        if (mod_it == func_to_nested_module.end()) {
            return nullptr;
        }
        ASR::Module_t *mod = ASR::down_cast<ASR::Module_t>(mod_it->second);
        SymbolTable *mod_symtab = mod->m_symtab;

        ASR::symbol_t *nml_sym_past = ASRUtils::symbol_get_past_external(nml_sym);
        if (!ASR::is_a<ASR::Namelist_t>(*nml_sym_past)) {
            return nullptr;
        }
        ASR::Namelist_t *nml = ASR::down_cast<ASR::Namelist_t>(nml_sym_past);

        Vec<ASR::symbol_t*> var_list;
        var_list.reserve(al, nml->n_var_list);
        for (size_t i = 0; i < nml->n_var_list; i++) {
            ASR::symbol_t *orig_sym = nml->m_var_list[i];
            auto it_ext = nested_var_to_ext_var.find(orig_sym);
            if (it_ext == nested_var_to_ext_var.end()) {
                continue;
            }
            var_list.push_back(al, it_ext->second.second);
        }

        std::string nml_name = ASRUtils::symbol_name(nml_sym);
        std::string unique_name = mod_symtab->get_unique_name(nml_name, false);
        ASR::asr_t *nml_asr = ASR::make_Namelist_t(
            al, nml->base.base.loc, mod_symtab,
            s2c(al, nml->m_group_name), var_list.p, var_list.n);
        ASR::symbol_t *nml_new_sym = ASR::down_cast<ASR::symbol_t>(nml_asr);
        mod_symtab->add_symbol(unique_name, nml_new_sym);
        nested_namelists[key] = nml_new_sym;
        return nml_new_sym;
    }

    void maybe_replace_namelist(ASR::symbol_t *&nml_sym, const Location &loc) {
        if (!nml_sym || func_stack.size() < 2) {
            return;
        }
        (void)loc;
        ASR::symbol_t *parent_func_sym = func_stack[func_stack.size() - 2];
        ASR::symbol_t *nml_new_sym = get_nested_namelist_symbol(parent_func_sym, nml_sym);
        if (!nml_new_sym) {
            return;
        }
        std::string sym_name = ASRUtils::symbol_name(nml_sym);
        ASR::symbol_t *ext_sym = current_scope->resolve_symbol(sym_name);
        if (!ext_sym || !ASR::is_a<ASR::ExternalSymbol_t>(*ext_sym) ||
                ASRUtils::symbol_get_past_external(ext_sym) != nml_new_sym) {
            std::string owner_name = ASRUtils::symbol_name(ASRUtils::get_asr_owner(nml_new_sym));
            ext_sym = make_external_symbol(al, current_scope, nml_new_sym, sym_name,
                owner_name, sym_name, ASR::accessType::Public);
        }
        nml_sym = ext_sym;
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ReplaceNestedVisitor>::visit_FileWrite(x);
        if (x.m_nml && nesting_depth > 1) {
            ASR::FileWrite_t &xx = const_cast<ASR::FileWrite_t&>(x);
            maybe_replace_namelist(xx.m_nml, x.base.base.loc);
        }
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ReplaceNestedVisitor>::visit_FileRead(x);
        if (x.m_nml && nesting_depth > 1) {
            ASR::FileRead_t &xx = const_cast<ASR::FileRead_t&>(x);
            maybe_replace_namelist(xx.m_nml, x.base.base.loc);
        }
    }

    void visit_Associate(const ASR::Associate_t &x) {
        // Keep direct procedure-symbol associations intact:
        //   procedure(p), pointer :: f
        //   f => p
        //
        // Replacing `p` with nested context globals here changes the pointer depth
        // in LLVM IR (storing a global-address instead of a function pointer value).
        ASR::expr_t **current_expr_copy = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x.m_target));
        call_replacer();
        current_expr = current_expr_copy;
        if (x.m_target) {
            visit_expr(*x.m_target);
        }

        bool keep_value_as_function_symbol = false;
        if (x.m_target && ASR::is_a<ASR::FunctionType_t>(*ASRUtils::expr_type(x.m_target)) &&
                x.m_value && ASR::is_a<ASR::Var_t>(*x.m_value)) {
            ASR::symbol_t *value_sym = ASR::down_cast<ASR::Var_t>(x.m_value)->m_v;
            ASR::symbol_t *value_sym_past = ASRUtils::symbol_get_past_external(value_sym);
            keep_value_as_function_symbol = ASR::is_a<ASR::Function_t>(*value_sym_past);
        }

        if (!keep_value_as_function_symbol) {
            current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
            call_replacer();
            current_expr = current_expr_copy;
        }
        if (x.m_value) {
            visit_expr(*x.m_value);
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        bool is_in_call_copy = is_in_call;
        is_in_call = is_nested_call_symbol(current_scope, x.m_name);
        ASR::FunctionCall_t& xx = const_cast<ASR::FunctionCall_t&>(x);
        ASR::symbol_t *name_past = ASRUtils::symbol_get_past_external(x.m_name);
        ASR::symbol_t *captured_key = resolve_captured_symbol_key(x.m_name);
        if (captured_key == nullptr && ASR::is_a<ASR::Function_t>(*name_past)) {
            ASR::symbol_t *same_name_sym = current_scope->resolve_symbol(
                ASRUtils::symbol_name(x.m_name));
            if (same_name_sym != nullptr &&
                    ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(same_name_sym))) {
                captured_key = resolve_captured_symbol_key(same_name_sym);
            }
        }
        if (captured_key != nullptr) {
            std::string m_name = nested_var_to_ext_var[captured_key].first;
            ASR::symbol_t *t = nested_var_to_ext_var[captured_key].second;
            std::string sym_name = ASRUtils::symbol_name(t);
            ASR::symbol_t *existing = current_scope->get_symbol(sym_name);
            if (existing != nullptr &&
                    ASR::is_a<ASR::ExternalSymbol_t>(*existing) &&
                    ASRUtils::symbol_get_past_external(existing) == t) {
                xx.m_name = existing;
            } else {
                std::string unique_name = sym_name;
                if (existing != nullptr) {
                    unique_name = current_scope->get_unique_name(sym_name, false);
                }
                ASR::symbol_t *ext_sym = make_external_symbol(al, current_scope, t, unique_name,
                    m_name, sym_name, ASR::accessType::Public);
                xx.m_name = ext_sym;
            }
        }
        for (size_t i=0; i<x.n_args; i++) {
            visit_call_arg(x.m_args[i]);
        }
        is_in_call = is_in_call_copy;
        visit_ttype(*x.m_type);
        if (x.m_value) {
            ASR::expr_t** current_expr_copy_118 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
            call_replacer();
            current_expr = current_expr_copy_118;
            if( x.m_value )
            visit_expr(*x.m_value);
        }
        if (x.m_dt) {
            ASR::expr_t** current_expr_copy_119 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_dt));
            call_replacer();
            current_expr = current_expr_copy_119;
            if( x.m_dt )
            visit_expr(*x.m_dt);
        }
        ASRUtils::Call_t_body(al, xx.m_name, xx.m_args, xx.n_args, x.m_dt,
            nullptr, false, get_class_proc_nopass_val_safe(xx.m_name));
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t& xx = const_cast<ASR::SubroutineCall_t&>(x);
        bool is_in_call_copy = is_in_call;
        is_in_call = is_nested_call_symbol(current_scope, x.m_name);
        ASR::symbol_t *name_past = ASRUtils::symbol_get_past_external(x.m_name);
        ASR::symbol_t *captured_key = resolve_captured_symbol_key(x.m_name);
        if (captured_key == nullptr && ASR::is_a<ASR::Function_t>(*name_past)) {
            ASR::symbol_t *same_name_sym = current_scope->resolve_symbol(
                ASRUtils::symbol_name(x.m_name));
            if (same_name_sym != nullptr &&
                    ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(same_name_sym))) {
                captured_key = resolve_captured_symbol_key(same_name_sym);
            }
        }
        if (captured_key != nullptr) {
            std::string m_name = nested_var_to_ext_var[captured_key].first;
            ASR::symbol_t *t = nested_var_to_ext_var[captured_key].second;
            std::string sym_name = ASRUtils::symbol_name(t);
            ASR::symbol_t *existing = current_scope->get_symbol(sym_name);
            if (existing != nullptr &&
                    ASR::is_a<ASR::ExternalSymbol_t>(*existing) &&
                    ASRUtils::symbol_get_past_external(existing) == t) {
                xx.m_name = existing;
            } else {
                std::string unique_name = sym_name;
                if (existing != nullptr) {
                    unique_name = current_scope->get_unique_name(sym_name, false);
                }
                ASR::symbol_t *ext_sym = make_external_symbol(al, current_scope, t, unique_name,
                    m_name, sym_name, ASR::accessType::Public);
                xx.m_name = ext_sym;
            }
        }
        for (size_t i=0; i<x.n_args; i++) {
            visit_call_arg(x.m_args[i]);
        }
        is_in_call = is_in_call_copy;
        if (x.m_dt) {
            ASR::expr_t** current_expr_copy_83 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_dt));
            call_replacer();
            current_expr = current_expr_copy_83;
            if( x.m_dt )
            visit_expr(*x.m_dt);
        }


        ASRUtils::Call_t_body(al, xx.m_name, xx.m_args, xx.n_args, x.m_dt,
            nullptr, false, get_class_proc_nopass_val_safe(xx.m_name));
    }

    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t& x) {
        ASR::expr_t** current_expr_copy_269 = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x.m_array));
        call_replacer();
        current_expr = current_expr_copy_269;
        if( x.m_array ) {
            visit_expr(*x.m_array);
        }
    }

};

class AssignNestedVars: public PassUtils::PassVisitor<AssignNestedVars> {
private : 

    /*
    Creates an if block with call to `allocated` intrinsic
    to check if RHS is allocated or not before doing an assignment.
    That's needed with allocatable RHS as the Fortran standard requires 
    to not reference non-allocated variables.

    #### Example:

    ```fortran
        if(allocated(RHS)) then
            LHS = RHS ! assignment_stmt
        end if
    ```
    */
    ASR::stmt_t* create_if_allocated_block(ASR::expr_t* RHS, ASR::stmt_t* assignment_stmt) {
        /* Some Assertions */
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Assignment_t>(*assignment_stmt))
        LCOMPILERS_ASSERT(ASR::down_cast<ASR::Assignment_t>(assignment_stmt)->m_value == RHS)

        /* Create Call To ImpureIntrinsic `allocated()` */
        ASR::expr_t* allocated_intrinsic_call {};
        {
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, RHS);
            diag::Diagnostics diag_instance;
            allocated_intrinsic_call = ASRUtils::EXPR(ASRUtils::Allocated::create_Allocated(al, RHS->base.loc, args, diag_instance));
            if(diag_instance.has_error()) throw diag_instance;
        }
        /* Create If Body */
        Vec<ASR::stmt_t*> if_body {};
        {
            if_body.reserve(al, 1);
            if_body.push_back(al, assignment_stmt);
        }
        return ASRUtils::STMT(ASR::make_If_t(al, RHS->base.loc, nullptr, allocated_intrinsic_call, if_body.p, if_body.size(), nullptr, 0));
    }

    // Inject sync statements before cycle recursively 
    // Currently used for do-loops
    template<typename T>
    static void inject_before_cycle(Allocator& al, ASR::stmt_t**& stmts, size_t& n, T& sync_stmts) {
        Vec<ASR::stmt_t*> res;
        res.reserve(al, n + sync_stmts.size() * 2);
        for (size_t i = 0; i < n; i++) {
            if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                ASR::If_t* ifs = ASR::down_cast<ASR::If_t>(stmts[i]);
                inject_before_cycle(al, ifs->m_body, ifs->n_body, sync_stmts);
                inject_before_cycle(al, ifs->m_orelse, ifs->n_orelse, sync_stmts);
            }
            if (ASR::is_a<ASR::Cycle_t>(*stmts[i])) {
                for (size_t j = 0; j < sync_stmts.size(); j++) res.push_back(al, sync_stmts[j]);
            }
            res.push_back(al, stmts[i]);
        }
        stmts = res.p;
        n = res.size();
    }

public:
    std::map<ASR::symbol_t*, std::pair<std::string, ASR::symbol_t*>> &nested_var_to_ext_var;
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &nesting_map;
    std::map<ASR::symbol_t*, ASR::symbol_t*> &ext_var_to_stack_var;
    std::map<ASR::symbol_t*, ASR::symbol_t*> &func_to_stack_ptr;
    std::map<ASR::symbol_t*, ASR::symbol_t*> &func_to_stack_next;
    std::map<ASR::symbol_t*, ASR::symbol_t*> &nested_proc_ctx_var;
    std::map<ASR::symbol_t*, ASR::symbol_t*> module_var_to_external;

    ASR::symbol_t *cur_func_sym = nullptr;
    bool calls_present = false;
    bool calls_present_direct = false;
    bool calls_present_proc_arg = false;
    bool calls_in_loop_condition = false;

    void mark_nested_procedure_arg(ASR::expr_t *arg_expr) {
        if (!arg_expr) {
            return;
        }
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_expr)) {
            arg_expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_expr)->m_arg;
        }
            if (ASR::is_a<ASR::Var_t>(*arg_expr)) {
            ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(arg_expr);
            if (is_nested_call_symbol(current_scope, var->m_v)) {
                calls_present = true;
                calls_present_proc_arg = true;
            }
        }
    }

    AssignNestedVars(Allocator &al_,
    std::map<ASR::symbol_t*, std::pair<std::string, ASR::symbol_t*>> &nv,
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &nm,
    std::map<ASR::symbol_t*, ASR::symbol_t*> &stack_vars,
    std::map<ASR::symbol_t*, ASR::symbol_t*> &stack_ptrs,
    std::map<ASR::symbol_t*, ASR::symbol_t*> &stack_next,
    std::map<ASR::symbol_t*, ASR::symbol_t*> &proc_ctx_vars) :
    PassVisitor(al_, nullptr), nested_var_to_ext_var(nv), nesting_map(nm),
    ext_var_to_stack_var(stack_vars), func_to_stack_ptr(stack_ptrs),
    func_to_stack_next(stack_next),
    nested_proc_ctx_var(proc_ctx_vars) { }

    ASR::symbol_t *get_or_import_external_symbol(ASR::symbol_t *sym, const Location &loc) {
        ASR::symbol_t *sym_past = ASRUtils::symbol_get_past_external(sym);
        auto it_ext = module_var_to_external.find(sym_past);
        if (it_ext != module_var_to_external.end() &&
                ASRUtils::symbol_parent_symtab(it_ext->second) == current_scope) {
            return it_ext->second;
        }
        for (auto &it_scope : current_scope->get_scope()) {
            ASR::symbol_t *cand = it_scope.second;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*cand) &&
                    ASRUtils::symbol_get_past_external(cand) == sym_past) {
                module_var_to_external[sym_past] = cand;
                module_var_to_external[sym] = cand;
                return cand;
            }
        }
        std::string original_name = ASRUtils::symbol_name(sym_past);
        ASR::symbol_t *existing = current_scope->get_symbol(original_name);
        ASR::symbol_t *ext_sym = nullptr;
        if (existing != nullptr && ASR::is_a<ASR::ExternalSymbol_t>(*existing) &&
                ASRUtils::symbol_get_past_external(existing) == sym_past) {
            ext_sym = existing;
        } else {
            std::string unique_name = original_name;
            if (existing != nullptr) {
                unique_name = current_scope->get_unique_name(original_name, false);
            }
            ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                al, loc, current_scope, s2c(al, unique_name), sym_past,
                ASRUtils::symbol_name(ASRUtils::get_asr_owner(sym_past)),
                nullptr, 0, s2c(al, original_name), ASR::accessType::Public);
            ext_sym = ASR::down_cast<ASR::symbol_t>(fn);
            current_scope->add_symbol(unique_name, ext_sym);
        }
        module_var_to_external[sym_past] = ext_sym;
        module_var_to_external[sym] = ext_sym;
        return ext_sym;
    }

    ASR::expr_t *make_int32_constant(int64_t n, const Location &loc) {
        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, n, int_type));
    }

    ASR::expr_t *make_stack_item_expr(ASR::symbol_t *stack_ext_sym, ASR::expr_t *idx_expr,
            const Location &loc) {
        ASR::expr_t *stack_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, stack_ext_sym));
        Vec<ASR::array_index_t> args;
        args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = idx_expr;
        ai.m_step = nullptr;
        args.push_back(al, ai);
        ASR::ttype_t *elem_type = ASRUtils::type_get_past_array(
            ASRUtils::expr_type(stack_var));
        return ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(
            al, loc, stack_var, args.p, args.size(), elem_type,
            ASR::arraystorageType::RowMajor, nullptr));
    }

    ASR::symbol_t *find_stack_var_symbol(ASR::symbol_t *captured_ext_sym) {
        auto it = ext_var_to_stack_var.find(captured_ext_sym);
        if (it != ext_var_to_stack_var.end()) {
            return it->second;
        }
        std::string captured_name = ASRUtils::symbol_name(captured_ext_sym);
        for (auto &kv : ext_var_to_stack_var) {
            if (ASRUtils::symbol_name(kv.first) == captured_name) {
                return kv.second;
            }
        }
        return nullptr;
    }

    ASR::symbol_t *resolve_capture_key(ASR::symbol_t *sym, ASR::symbol_t *capture_owner) {
        auto it = nesting_map.find(capture_owner);
        if (it == nesting_map.end()) return nullptr;
        ASR::symbol_t *sym_past = ASRUtils::symbol_get_past_external(sym);
        ASR::symbol_t *sym_decl = nullptr;
        if (ASR::is_a<ASR::Variable_t>(*sym_past)) {
            ASR::Variable_t *sym_var = ASR::down_cast<ASR::Variable_t>(sym_past);
            if (sym_var->m_type_declaration) {
                sym_decl = ASRUtils::symbol_get_past_external(sym_var->m_type_declaration);
            }
        }
        std::string sym_name = ASRUtils::symbol_name(sym);
        std::string sym_name_past = ASRUtils::symbol_name(sym_past);
        std::string sym_original_name = sym_name;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
            sym_original_name = std::string(ASR::down_cast<ASR::ExternalSymbol_t>(
                sym)->m_original_name);
        }
        auto canonical_name = [](const std::string &name) -> std::string {
            size_t pos = name.find('.');
            if (pos != std::string::npos) {
                return name.substr(0, pos);
            }
            return name;
        };
        std::string sym_canon = canonical_name(sym_name);
        std::string sym_past_canon = canonical_name(sym_name_past);
        std::string sym_original_canon = canonical_name(sym_original_name);
        for (auto &cand : it->second) {
            ASR::symbol_t *cand_past = ASRUtils::symbol_get_past_external(cand);
            if (cand == sym) return cand;
            if (cand_past == sym_past) return cand;
            if (sym_decl != nullptr && cand_past == sym_decl) return cand;
            std::string cand_name = ASRUtils::symbol_name(cand);
            std::string cand_past_name = ASRUtils::symbol_name(cand_past);
            std::string cand_canon = canonical_name(cand_name);
            std::string cand_past_canon = canonical_name(cand_past_name);
            if (cand_name == sym_name ||
                    cand_name == sym_name_past ||
                    cand_name == sym_original_name ||
                    cand_past_name == sym_name ||
                    cand_past_name == sym_name_past ||
                    cand_past_name == sym_original_name ||
                    cand_canon == sym_canon ||
                    cand_canon == sym_past_canon ||
                    cand_canon == sym_original_canon ||
                    cand_past_canon == sym_canon ||
                    cand_past_canon == sym_past_canon ||
                    cand_past_canon == sym_original_canon) {
                return cand;
            }
            if (sym_decl != nullptr && ASR::is_a<ASR::Variable_t>(*cand_past)) {
                ASR::Variable_t *cand_var = ASR::down_cast<ASR::Variable_t>(cand_past);
                if (cand_var->m_type_declaration &&
                        ASRUtils::symbol_get_past_external(cand_var->m_type_declaration) ==
                            sym_decl) {
                    return cand;
                }
            }
        }
        return nullptr;
    }

    template <typename TBody>
    void remap_direct_host_call_proc_context(ASR::stmt_t *stmt, ASR::symbol_t *capture_owner,
            ASR::symbol_t *stack_ptr_ext, TBody &body, bool strict_direct_host_call=false,
            bool use_previous_stack_ctx=false, ASR::expr_t *previous_stack_ctx_expr=nullptr) {
        SymbolTable *capture_owner_symtab = nullptr;
        if (capture_owner != nullptr &&
                ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(capture_owner))) {
            capture_owner_symtab = ASR::down_cast<ASR::Function_t>(
                ASRUtils::symbol_get_past_external(capture_owner))->m_symtab;
        }
        auto make_current_stack_ctx_expr = [&](const Location &loc) -> ASR::expr_t* {
            if (stack_ptr_ext == nullptr) {
                return nullptr;
            }
            ASR::expr_t *ctx_expr = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, stack_ptr_ext));
            if (use_previous_stack_ctx) {
                if (previous_stack_ctx_expr != nullptr) {
                    ctx_expr = previous_stack_ctx_expr;
                } else {
                    ASR::expr_t *one = make_int32_constant(1, loc);
                    ctx_expr = PassUtils::create_binop_helper(
                        al, loc, ctx_expr, one, ASR::binopType::Sub);
                }
            }
            return ctx_expr;
        };
        auto needs_current_stack_ctx = [&](ASR::symbol_t *sym_past) {
            if (sym_past == nullptr) {
                return false;
            }
            if (ASR::is_a<ASR::Function_t>(*sym_past)) {
                ASR::Function_t *afn = ASR::down_cast<ASR::Function_t>(sym_past);
                return ASRUtils::get_FunctionType(afn)->m_deftype == ASR::deftypeType::Implementation &&
                    stack_ptr_ext != nullptr && capture_owner_symtab != nullptr &&
                    afn->m_symtab->parent == capture_owner_symtab;
            }
            if (ASR::is_a<ASR::Variable_t>(*sym_past)) {
                ASR::Variable_t *avar = ASR::down_cast<ASR::Variable_t>(sym_past);
                if (avar->m_type_declaration &&
                        ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                            avar->m_type_declaration))) {
                    ASR::Function_t *afn = ASR::down_cast<ASR::Function_t>(
                        ASRUtils::symbol_get_past_external(avar->m_type_declaration));
                    return ASRUtils::get_FunctionType(afn)->m_deftype == ASR::deftypeType::Implementation &&
                        stack_ptr_ext != nullptr && capture_owner_symtab != nullptr &&
                        afn->m_symtab->parent == capture_owner_symtab;
                }
            }
            return false;
        };
        struct ProcRemapEntry {
            size_t arg_index = 0;
            ASR::symbol_t *formal_ptr_ext = nullptr;
            ASR::symbol_t *formal_ctx_ext = nullptr;
            ASR::expr_t *actual_ptr_expr = nullptr;
            ASR::expr_t *actual_ctx_expr = nullptr;
            ASR::ttype_t *ptr_type = nullptr;
            ASR::symbol_t *ptr_type_decl = nullptr;
        };
        ASR::symbol_t *callee = nullptr;
        ASR::call_arg_t *args = nullptr;
        size_t n_args = 0;
        if (ASR::is_a<ASR::Assignment_t>(*stmt)) {
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value)) return;
            ASR::FunctionCall_t *fc = ASR::down_cast<ASR::FunctionCall_t>(asgn->m_value);
            callee = fc->m_name;
            args = fc->m_args;
            n_args = fc->n_args;
        } else if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
            ASR::SubroutineCall_t *sc = ASR::down_cast<ASR::SubroutineCall_t>(stmt);
            callee = sc->m_name;
            args = sc->m_args;
            n_args = sc->n_args;
        } else {
            return;
        }
        ASR::symbol_t *callee_past = ASRUtils::symbol_get_past_external(callee);
        if (!ASR::is_a<ASR::Function_t>(*callee_past)) return;
        if (callee_past != ASRUtils::symbol_get_past_external(capture_owner)) return;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(callee_past);
        size_t n = std::min(n_args, fn->n_args);
        size_t proc_formal_count = 0;
        size_t proc_formal_remapped_count = 0;
        std::vector<ProcRemapEntry> remaps;
        remaps.reserve(n);
        for (size_t j = 0; j < n; j++) {
            if (!ASR::is_a<ASR::Var_t>(*fn->m_args[j])) continue;
            ASR::symbol_t *formal = ASR::down_cast<ASR::Var_t>(fn->m_args[j])->m_v;
            ASR::symbol_t *formal_key = resolve_capture_key(formal, capture_owner);
            if (formal_key == nullptr) continue;
            auto it_formal = nested_var_to_ext_var.find(formal_key);
            if (it_formal == nested_var_to_ext_var.end()) {
                if (strict_direct_host_call) {
                    throw LCompilersException(
                        "Direct recursive nested call remap failed: no external capture mapping for procedure formal '" +
                        std::string(ASRUtils::symbol_name(formal)) + "' in host '" +
                        std::string(ASRUtils::symbol_name(capture_owner)) + "'.");
                }
                continue;
            }
            ASR::symbol_t *formal_ext = get_or_import_external_symbol(
                it_formal->second.second, stmt->base.loc);
            ASR::symbol_t *formal_past = ASRUtils::symbol_get_past_external(formal_ext);
            if (!ASR::is_a<ASR::Variable_t>(*formal_past)) {
                if (strict_direct_host_call) {
                    throw LCompilersException(
                        "Direct recursive nested call remap failed: non-variable external capture for procedure formal '" +
                        std::string(ASRUtils::symbol_name(formal)) + "'.");
                }
                continue;
            }
            ASR::Variable_t *formal_var = ASR::down_cast<ASR::Variable_t>(formal_past);
            bool formal_is_proc = formal_var->m_type_declaration &&
                ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(formal_var->m_type_declaration));
            if (!formal_is_proc) continue;
            proc_formal_count++;
            if (find_stack_var_symbol(it_formal->second.second) == nullptr) {
                if (strict_direct_host_call) {
                    throw LCompilersException(
                        "Direct recursive nested call remap failed: missing stack slot for procedure capture '" +
                        std::string(ASRUtils::symbol_name(formal)) + "' in host '" +
                        std::string(ASRUtils::symbol_name(capture_owner)) + "'.");
                }
                continue;
            }
            if (args[j].m_value == nullptr || !ASR::is_a<ASR::Var_t>(*args[j].m_value)) {
                if (strict_direct_host_call) {
                    throw LCompilersException(
                        "Direct recursive nested call remap failed: procedure formal '" +
                        std::string(ASRUtils::symbol_name(formal)) + "' is not passed as a variable expression.");
                }
                continue;
            }

            ASR::symbol_t *actual_sym = ASR::down_cast<ASR::Var_t>(args[j].m_value)->m_v;
            ASR::symbol_t *actual_sym_past = ASRUtils::symbol_get_past_external(actual_sym);
            ASR::symbol_t *actual_key = resolve_capture_key(actual_sym, capture_owner);
            ASR::expr_t *actual_ptr_expr = nullptr;
            ASR::expr_t *actual_ctx_expr = nullptr;
            if (actual_key != nullptr && nested_var_to_ext_var.find(actual_key) != nested_var_to_ext_var.end()) {
                ASR::symbol_t *actual_ext = get_or_import_external_symbol(
                    nested_var_to_ext_var[actual_key].second, stmt->base.loc);
                actual_ptr_expr = ASRUtils::EXPR(ASR::make_Var_t(al, stmt->base.loc, actual_ext));
                auto it_act_ctx = nested_proc_ctx_var.find(actual_key);
                if (it_act_ctx != nested_proc_ctx_var.end()) {
                    ASR::symbol_t *actual_ctx_ext = get_or_import_external_symbol(
                        it_act_ctx->second, stmt->base.loc);
                    actual_ctx_expr = ASRUtils::EXPR(ASR::make_Var_t(al, stmt->base.loc, actual_ctx_ext));
                }
                if (needs_current_stack_ctx(actual_sym_past) &&
                        (actual_ctx_expr == nullptr ||
                            ASR::is_a<ASR::Function_t>(*actual_sym_past))) {
                    actual_ctx_expr = make_current_stack_ctx_expr(stmt->base.loc);
                }
            } else {
                actual_ptr_expr = ASRUtils::EXPR(ASR::make_Var_t(al, stmt->base.loc, actual_sym));
                if (needs_current_stack_ctx(actual_sym_past)) {
                    actual_ctx_expr = make_current_stack_ctx_expr(stmt->base.loc);
                }
            }
            if (actual_ptr_expr == nullptr) {
                if (strict_direct_host_call) {
                    throw LCompilersException(
                        "Direct recursive nested call remap failed: could not resolve actual procedure for formal '" +
                        std::string(ASRUtils::symbol_name(formal)) + "'.");
                }
                continue;
            }
            ProcRemapEntry entry;
            entry.arg_index = j;
            entry.formal_ptr_ext = formal_ext;
            entry.actual_ptr_expr = actual_ptr_expr;
            entry.ptr_type = formal_var->m_type;
            entry.ptr_type_decl = formal_var->m_type_declaration;
            auto it_formal_ctx = nested_proc_ctx_var.find(formal_key);
            if (it_formal_ctx != nested_proc_ctx_var.end()) {
                entry.formal_ctx_ext = get_or_import_external_symbol(
                    it_formal_ctx->second, stmt->base.loc);
                if (actual_ctx_expr == nullptr) {
                    actual_ctx_expr = make_int32_constant(0, stmt->base.loc);
                }
                entry.actual_ctx_expr = actual_ctx_expr;
            }
            remaps.push_back(entry);
            proc_formal_remapped_count++;
        }
        if (strict_direct_host_call && proc_formal_count > 0 &&
                proc_formal_remapped_count != proc_formal_count) {
            throw LCompilersException(
                "Direct recursive nested call remap failed: only " +
                std::to_string(proc_formal_remapped_count) + " of " +
                std::to_string(proc_formal_count) +
                " procedure captures were remapped in host '" +
                std::string(ASRUtils::symbol_name(capture_owner)) + "'.");
        }
        struct ProcRemapPreparedEntry {
            size_t arg_index = 0;
            ASR::symbol_t *formal_ptr_ext = nullptr;
            ASR::symbol_t *formal_ctx_ext = nullptr;
            ASR::expr_t *ptr_tmp_expr = nullptr;
            ASR::expr_t *ctx_tmp_expr = nullptr;
        };
        // Direct recursive host calls already pass procedure pointers as explicit call
        // arguments; remapping pointer globals here shifts the argument chain.
        const bool remap_ptr_values = false;
        std::vector<ProcRemapPreparedEntry> prepared_remaps;
        prepared_remaps.reserve(remaps.size());
        for (auto &entry : remaps) {
            ProcRemapPreparedEntry prepared_entry;
            prepared_entry.arg_index = entry.arg_index;
            prepared_entry.formal_ctx_ext = entry.formal_ctx_ext;
            if (remap_ptr_values) {
                std::string ptr_tmp_name = current_scope->get_unique_name(
                    "__lfortran_nested_ctx_arg_" + std::string(ASRUtils::symbol_name(entry.formal_ptr_ext)),
                    false);
                ASR::expr_t *ptr_tmp_expr = PassUtils::create_auxiliary_variable(
                    stmt->base.loc, ptr_tmp_name, al, current_scope,
                    entry.ptr_type, ASR::intentType::Local, entry.ptr_type_decl, nullptr);
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, stmt->base.loc, ptr_tmp_expr, entry.actual_ptr_expr, nullptr, false, false)));
                prepared_entry.formal_ptr_ext = entry.formal_ptr_ext;
                prepared_entry.ptr_tmp_expr = ptr_tmp_expr;
            }
            if (entry.formal_ctx_ext != nullptr) {
                std::string ctx_tmp_name = current_scope->get_unique_name(
                    "__lfortran_nested_ctx_arg_ctx_" + std::string(ASRUtils::symbol_name(entry.formal_ptr_ext)),
                    false);
                ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, stmt->base.loc, 4));
                ASR::expr_t *ctx_tmp_expr = PassUtils::create_auxiliary_variable(
                    stmt->base.loc, ctx_tmp_name, al, current_scope,
                    int_type, ASR::intentType::Local, nullptr, nullptr);
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, stmt->base.loc, ctx_tmp_expr, entry.actual_ctx_expr, nullptr, false, false)));
                prepared_entry.ctx_tmp_expr = ctx_tmp_expr;
            }
            prepared_remaps.push_back(prepared_entry);
        }
        for (auto &prepared_entry : prepared_remaps) {
            if (remap_ptr_values && prepared_entry.formal_ptr_ext != nullptr &&
                    prepared_entry.ptr_tmp_expr != nullptr) {
                ASR::expr_t *formal_ptr_expr = ASRUtils::EXPR(
                    ASR::make_Var_t(al, stmt->base.loc, prepared_entry.formal_ptr_ext));
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, stmt->base.loc, formal_ptr_expr, prepared_entry.ptr_tmp_expr,
                    nullptr, false, false)));
            }
            if (prepared_entry.formal_ctx_ext != nullptr) {
                ASR::expr_t *formal_ctx_expr = ASRUtils::EXPR(
                    ASR::make_Var_t(al, stmt->base.loc, prepared_entry.formal_ctx_ext));
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, stmt->base.loc, formal_ctx_expr, prepared_entry.ctx_tmp_expr, nullptr, false, false)));
            }
        }
    }

    bool is_direct_call_to_capture_owner(ASR::stmt_t *stmt, ASR::symbol_t *capture_owner) {
        if (capture_owner == nullptr) {
            return false;
        }
        ASR::symbol_t *callee = nullptr;
        if (ASR::is_a<ASR::Assignment_t>(*stmt)) {
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value)) return false;
            callee = ASR::down_cast<ASR::FunctionCall_t>(asgn->m_value)->m_name;
        } else if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
            callee = ASR::down_cast<ASR::SubroutineCall_t>(stmt)->m_name;
        } else {
            return false;
        }
        ASR::symbol_t *callee_past = ASRUtils::symbol_get_past_external(callee);
        return callee_past == ASRUtils::symbol_get_past_external(capture_owner);
    }

    ASR::symbol_t *resolve_capture_owner_for_scope(ASR::symbol_t *owner) {
        ASR::symbol_t *capture_owner = owner;
        if (nesting_map.find(capture_owner) == nesting_map.end() &&
                current_scope && current_scope->parent &&
                current_scope->parent->asr_owner &&
                ASR::is_a<ASR::symbol_t>(*current_scope->parent->asr_owner)) {
            ASR::symbol_t *parent_owner =
                ASR::down_cast<ASR::symbol_t>(current_scope->parent->asr_owner);
            if (nesting_map.find(parent_owner) != nesting_map.end()) {
                capture_owner = parent_owner;
            } else {
                std::string parent_name = ASRUtils::symbol_name(parent_owner);
                for (auto &it_nm : nesting_map) {
                    if (ASRUtils::symbol_name(it_nm.first) == parent_name) {
                        capture_owner = it_nm.first;
                        break;
                    }
                }
            }
        }
        return capture_owner;
    }

    bool is_plain_scalar_capture_symbol(ASR::symbol_t *sym) {
        ASR::symbol_t *sym_past = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Variable_t>(*sym_past)) {
            return false;
        }
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym_past);
        if (var->m_type_declaration &&
                ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(var->m_type_declaration))) {
            return false;
        }
        ASR::ttype_t *type = var->m_type;
        return !ASRUtils::is_array(type) &&
               !ASRUtils::is_pointer(type) &&
               !ASRUtils::is_allocatable(type);
    }

    ASR::symbol_t *prefer_scope_capture_symbol(ASR::symbol_t *sym,
            ASR::symbol_t *capture_owner) {
        if (sym == nullptr || current_scope == nullptr) {
            return sym;
        }
        std::string sym_name = ASRUtils::symbol_name(sym);
        ASR::symbol_t *scope_sym = current_scope->get_symbol(sym_name);
        if (scope_sym == nullptr) {
            return sym;
        }
        ASR::symbol_t *scope_key = resolve_capture_key(scope_sym, capture_owner);
        ASR::symbol_t *sym_key = resolve_capture_key(sym, capture_owner);
        if (scope_key != nullptr && sym_key != nullptr &&
                ASRUtils::symbol_get_past_external(scope_key) ==
                    ASRUtils::symbol_get_past_external(sym_key)) {
            return scope_sym;
        }
        return sym;
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        std::vector<ASR::stmt_t*> assigns_at_end;
        std::vector<ASR::stmt_t*> loop_end_syncs;
        for (size_t i=0; i<n_body; i++) {
            calls_present = false;
            calls_present_direct = false;
            calls_present_proc_arg = false;
            calls_in_loop_condition = false;
            bool is_do_loop_sync = false;
            std::vector<ASR::stmt_t*> context_restore_stmts;
            std::vector<ASR::stmt_t*> captured_scalar_writeback_stmts;
            if (ASR::is_a<ASR::WhileLoop_t>(*m_body[i]) || 
                (ASR::is_a<ASR::DoLoop_t>(*m_body[i]))) {
                is_do_loop_sync = true;
            }
            assigns_at_end.clear();
            loop_end_syncs.clear();
            ASR::symbol_t *capture_owner = cur_func_sym;
            visit_stmt(*m_body[i]);
            if (cur_func_sym != nullptr &&
                    ASR::is_a<ASR::Assignment_t>(*m_body[i])) {
                ASR::symbol_t *assignment_capture_owner =
                    resolve_capture_owner_for_scope(cur_func_sym);
                if (assignment_capture_owner != nullptr &&
                        nesting_map.find(assignment_capture_owner) != nesting_map.end()) {
                    bool in_nested_child = false;
                    bool in_capture_owner_scope = false;
                    if (ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                            assignment_capture_owner))) {
                        ASR::Function_t *capture_fn = ASR::down_cast<ASR::Function_t>(
                            ASRUtils::symbol_get_past_external(assignment_capture_owner));
                        in_capture_owner_scope = current_scope &&
                            current_scope == capture_fn->m_symtab;
                        in_nested_child = current_scope && current_scope->parent &&
                            current_scope->parent == capture_fn->m_symtab;
                    }
                    if (in_nested_child || in_capture_owner_scope) {
                        ASR::Assignment_t *assign_stmt = ASR::down_cast<ASR::Assignment_t>(m_body[i]);
                        if (ASR::is_a<ASR::Var_t>(*assign_stmt->m_target)) {
                            ASR::symbol_t *lhs_sym = ASR::down_cast<ASR::Var_t>(
                                assign_stmt->m_target)->m_v;
                            ASR::symbol_t *lhs_key = resolve_capture_key(
                                lhs_sym, assignment_capture_owner);
                            if (lhs_key != nullptr &&
                                    is_plain_scalar_capture_symbol(lhs_key) &&
                                    nested_var_to_ext_var.find(lhs_key) != nested_var_to_ext_var.end()) {
                                auto it_sp = func_to_stack_ptr.find(assignment_capture_owner);
                                ASR::symbol_t *stack_ptr_ext = nullptr;
                                if (it_sp != func_to_stack_ptr.end()) {
                                    stack_ptr_ext = get_or_import_external_symbol(
                                        it_sp->second, m_body[i]->base.loc);
                                }
                                ASR::symbol_t *captured_ext = nested_var_to_ext_var[lhs_key].second;
                                ASR::symbol_t *stack_var_sym = find_stack_var_symbol(captured_ext);
                                if (stack_ptr_ext != nullptr && stack_var_sym != nullptr) {
                                    ASR::symbol_t *ext_sym = get_or_import_external_symbol(
                                        captured_ext, m_body[i]->base.loc);
                                    ASR::symbol_t *stack_ext_sym = get_or_import_external_symbol(
                                        stack_var_sym, stack_var_sym->base.loc);
                                    ASR::expr_t *stack_ptr_var = ASRUtils::EXPR(
                                        ASR::make_Var_t(al, m_body[i]->base.loc, stack_ptr_ext));
                                    ASR::expr_t *zero = make_int32_constant(0, m_body[i]->base.loc);
                                    ASR::expr_t *has_ctx = PassUtils::create_compare_helper(
                                        al, m_body[i]->base.loc, stack_ptr_var, zero,
                                        ASR::cmpopType::Gt);
                                    ASR::expr_t *stack_item = make_stack_item_expr(
                                        stack_ext_sym, stack_ptr_var, m_body[i]->base.loc);
                                    ASR::expr_t *ext_val = ASRUtils::EXPR(
                                        ASR::make_Var_t(al, m_body[i]->base.loc, ext_sym));
                                    Vec<ASR::stmt_t*> if_body;
                                    if_body.reserve(al, 1);
                                    if_body.push_back(al, ASRUtils::STMT(
                                        ASRUtils::make_Assignment_t_util(al, m_body[i]->base.loc,
                                            stack_item, ext_val, nullptr, false, false)));
                                    captured_scalar_writeback_stmts.push_back(
                                        ASRUtils::STMT(ASR::make_If_t(al, m_body[i]->base.loc,
                                            nullptr, has_ctx, if_body.p, if_body.size(),
                                            nullptr, 0)));
                                }
                            }
                        }
                    }
                }
            }
            if (cur_func_sym != nullptr && (calls_present || calls_present_proc_arg || calls_in_loop_condition)) {
                capture_owner = resolve_capture_owner_for_scope(capture_owner);
                if (nesting_map.find(capture_owner) != nesting_map.end()) {
                    bool direct_host_recursive_call = false;
                    if (is_direct_call_to_capture_owner(m_body[i], capture_owner) &&
                            capture_owner != nullptr &&
                            ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(capture_owner))) {
                        ASR::Function_t *capture_fn = ASR::down_cast<ASR::Function_t>(
                            ASRUtils::symbol_get_past_external(capture_owner));
                        direct_host_recursive_call = current_scope && current_scope->parent &&
                            current_scope->parent == capture_fn->m_symtab;
                    }
                    ASR::symbol_t *stack_ptr_ext = nullptr;
                    auto it_sp = func_to_stack_ptr.find(capture_owner);
                    if (it_sp != func_to_stack_ptr.end()) {
                        stack_ptr_ext = get_or_import_external_symbol(it_sp->second, m_body[i]->base.loc);
                    }
                    ASR::symbol_t *stack_next_ext = nullptr;
                    auto it_sn = func_to_stack_next.find(capture_owner);
                    if (it_sn != func_to_stack_next.end()) {
                        stack_next_ext = get_or_import_external_symbol(it_sn->second, m_body[i]->base.loc);
                    }
                    std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> stack_backup_pairs;
                    std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> stack_ctx_backup_pairs;
                    std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> proc_local_backup_pairs;
                    std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> proc_local_ctx_backup_pairs;
                    if (stack_ptr_ext != nullptr) {
                        for (auto &sym: nesting_map[capture_owner]) {
                            ASR::symbol_t *sym_past = ASRUtils::symbol_get_past_external(sym);
                            bool is_proc_capture = ASR::is_a<ASR::Function_t>(*sym_past) ||
                                (ASR::is_a<ASR::Variable_t>(*sym_past) &&
                                 ASR::down_cast<ASR::Variable_t>(sym_past)->m_type_declaration &&
                                 ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                                     ASR::down_cast<ASR::Variable_t>(sym_past)->m_type_declaration)));
                            if (direct_host_recursive_call &&
                                    capture_owner != cur_func_sym &&
                                    !is_proc_capture &&
                                    is_plain_scalar_capture_symbol(sym)) {
                                // Mutable scalar captures in nested children represent closure state.
                                // Do not restore them from pre-call snapshots in direct recursive calls.
                                continue;
                            }
                            bool need_scalar_sync = direct_host_recursive_call ||
                                calls_present || calls_present_direct ||
                                calls_present_proc_arg || calls_in_loop_condition;
                            if (!need_scalar_sync && !is_proc_capture) {
                                continue;
                            }
                            ASR::symbol_t *t = nested_var_to_ext_var[sym].second;
                            ASR::symbol_t *stack_var_sym = find_stack_var_symbol(t);
                            ASR::symbol_t *ext_sym = get_or_import_external_symbol(t, t->base.loc);
                            if (stack_var_sym == nullptr) {
                                ASR::symbol_t *ext_sym_past = ASRUtils::symbol_get_past_external(ext_sym);
                                if (is_proc_capture &&
                                        ASR::is_a<ASR::Variable_t>(*ext_sym_past)) {
                                    ASR::Variable_t *ext_var =
                                        ASR::down_cast<ASR::Variable_t>(ext_sym_past);
                                    ASR::symbol_t *proc_decl = ext_var->m_type_declaration;
                                    std::string backup_name = current_scope->get_unique_name(
                                        "__lfortran_nested_ctx_saved_" + std::string(
                                            ASRUtils::symbol_name(ext_sym)), false);
                                    ASR::expr_t *backup_expr = PassUtils::create_auxiliary_variable(
                                        t->base.loc, backup_name, al, current_scope,
                                        ext_var->m_type,
                                        ASR::intentType::Local, proc_decl, nullptr);
                                    ASR::symbol_t *backup_sym =
                                        ASR::down_cast<ASR::Var_t>(backup_expr)->m_v;
                                    proc_local_backup_pairs.push_back({ext_sym, backup_sym});

                                    auto it_ctx = nested_proc_ctx_var.find(sym);
                                    if (it_ctx != nested_proc_ctx_var.end()) {
                                        ASR::symbol_t *ext_ctx_sym = get_or_import_external_symbol(
                                            it_ctx->second, it_ctx->second->base.loc);
                                        ASR::symbol_t *ctx_stack_sym = find_stack_var_symbol(
                                            it_ctx->second);
                                        if (ctx_stack_sym != nullptr) {
                                            ASR::symbol_t *ctx_stack_ext_sym =
                                                get_or_import_external_symbol(
                                                    ctx_stack_sym, ctx_stack_sym->base.loc);
                                            stack_ctx_backup_pairs.push_back(
                                                {ext_ctx_sym, ctx_stack_ext_sym});
                                        }
                                        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(
                                            al, t->base.loc, 4));
                                        std::string ctx_backup_name = current_scope->get_unique_name(
                                            "__lfortran_nested_ctx_saved_ctx_" + std::string(
                                                ASRUtils::symbol_name(ext_sym)), false);
                                        ASR::expr_t *ctx_backup_expr = PassUtils::create_auxiliary_variable(
                                            t->base.loc, ctx_backup_name, al, current_scope, int_type,
                                            ASR::intentType::Local, nullptr, nullptr);
                                        ASR::symbol_t *ctx_backup_sym =
                                            ASR::down_cast<ASR::Var_t>(ctx_backup_expr)->m_v;
                                        proc_local_ctx_backup_pairs.push_back({ext_ctx_sym, ctx_backup_sym});
                                    }
                                }
                                continue;
                            }
                            ASR::symbol_t *stack_ext_sym = get_or_import_external_symbol(
                                stack_var_sym, stack_var_sym->base.loc);
                            stack_backup_pairs.push_back({ext_sym, stack_ext_sym});
                            auto it_ctx = nested_proc_ctx_var.find(sym);
                            if (is_proc_capture && it_ctx != nested_proc_ctx_var.end()) {
                                ASR::symbol_t *ext_ctx_sym = get_or_import_external_symbol(
                                    it_ctx->second, it_ctx->second->base.loc);
                                ASR::symbol_t *ctx_stack_sym = find_stack_var_symbol(it_ctx->second);
                                if (ctx_stack_sym != nullptr) {
                                    ASR::symbol_t *ctx_stack_ext_sym = get_or_import_external_symbol(
                                        ctx_stack_sym, ctx_stack_sym->base.loc);
                                    stack_ctx_backup_pairs.push_back({ext_ctx_sym, ctx_stack_ext_sym});
                                }
                            }
                        }
                    }
                    bool pushed_context_frame = stack_ptr_ext != nullptr &&
                        (!stack_backup_pairs.empty() || !stack_ctx_backup_pairs.empty());
                    ASR::expr_t *saved_stack_ptr_expr = nullptr;
                    if (pushed_context_frame) {
                        ASR::expr_t *stack_ptr_var = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, stack_ptr_ext));
                        ASR::expr_t *alloc_ptr_var = stack_ptr_var;
                        if (stack_next_ext != nullptr) {
                            std::string saved_ctx_name = current_scope->get_unique_name(
                                "__lfortran_nested_saved_ctx", false);
                            ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(
                                al, m_body[i]->base.loc, 4));
                            saved_stack_ptr_expr = PassUtils::create_auxiliary_variable(
                                m_body[i]->base.loc, saved_ctx_name, al, current_scope,
                                int_type, ASR::intentType::Local, nullptr, nullptr);
                            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, m_body[i]->base.loc, saved_stack_ptr_expr, stack_ptr_var,
                                nullptr, false, false)));
                            alloc_ptr_var = ASRUtils::EXPR(
                                ASR::make_Var_t(al, m_body[i]->base.loc, stack_next_ext));
                        }
                        ASR::expr_t *stack_cap = make_int32_constant(
                            NESTED_CONTEXT_STACK_SIZE, m_body[i]->base.loc);
                        ASR::expr_t *in_bounds = PassUtils::create_compare_helper(
                            al, m_body[i]->base.loc, alloc_ptr_var,
                            stack_cap, ASR::cmpopType::Lt);
                        body.push_back(al, ASRUtils::STMT(
                            ASR::make_Assert_t(al, m_body[i]->base.loc, in_bounds, nullptr)));
                        ASR::expr_t *one = make_int32_constant(1, m_body[i]->base.loc);
                        ASR::expr_t *push_idx = PassUtils::create_binop_helper(
                            al, m_body[i]->base.loc, alloc_ptr_var, one, ASR::binopType::Add);
                        if (stack_next_ext != nullptr) {
                            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, m_body[i]->base.loc, alloc_ptr_var, push_idx,
                                nullptr, false, false)));
                        }
                        for (auto &pair : stack_backup_pairs) {
                            ASR::expr_t *stack_item = make_stack_item_expr(
                                pair.second, push_idx, m_body[i]->base.loc);
                            ASR::expr_t *ext_val = ASRUtils::EXPR(
                                ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, m_body[i]->base.loc, stack_item, ext_val, nullptr, false, false)));
                        }
                        for (auto &pair : stack_ctx_backup_pairs) {
                            ASR::expr_t *stack_item = make_stack_item_expr(
                                pair.second, push_idx, m_body[i]->base.loc);
                            ASR::expr_t *ext_val = ASRUtils::EXPR(
                                ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, m_body[i]->base.loc, stack_item, ext_val, nullptr, false, false)));
                        }
                        body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                            al, m_body[i]->base.loc, stack_ptr_var, push_idx, nullptr, false, false)));

                        ASR::expr_t *stack_ptr_var2 = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, stack_ptr_ext));
                        for (auto &pair : stack_backup_pairs) {
                            ASR::expr_t *stack_item = make_stack_item_expr(
                                pair.second, stack_ptr_var2, m_body[i]->base.loc);
                            ASR::expr_t *ext_target = ASRUtils::EXPR(
                                ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                            context_restore_stmts.push_back(ASRUtils::STMT(
                                ASRUtils::make_Assignment_t_util(
                                    al, m_body[i]->base.loc, ext_target, stack_item,
                                    nullptr, false, false)));
                        }
                        for (auto &pair : stack_ctx_backup_pairs) {
                            ASR::expr_t *stack_item = make_stack_item_expr(
                                pair.second, stack_ptr_var2, m_body[i]->base.loc);
                            ASR::expr_t *ext_target = ASRUtils::EXPR(
                                ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                            context_restore_stmts.push_back(ASRUtils::STMT(
                                ASRUtils::make_Assignment_t_util(
                                    al, m_body[i]->base.loc, ext_target, stack_item,
                                    nullptr, false, false)));
                        }
                        ASR::expr_t *stack_ptr_var3 = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, stack_ptr_ext));
                        ASR::expr_t *pop_idx = nullptr;
                        if (saved_stack_ptr_expr != nullptr) {
                            pop_idx = saved_stack_ptr_expr;
                        } else {
                            pop_idx = PassUtils::create_binop_helper(
                                al, m_body[i]->base.loc, stack_ptr_var3, one, ASR::binopType::Sub);
                        }
                        context_restore_stmts.push_back(ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                            al, m_body[i]->base.loc, stack_ptr_var3, pop_idx, nullptr, false, false)));
                    }
                    for (auto &pair : proc_local_backup_pairs) {
                        ASR::expr_t *backup_target = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.second));
                        ASR::expr_t *ext_val = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                        body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                            al, m_body[i]->base.loc, backup_target, ext_val, nullptr, false, false)));
                        ASR::expr_t *ext_target = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                        ASR::expr_t *backup_val = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.second));
                        context_restore_stmts.push_back(ASRUtils::STMT(
                            ASRUtils::make_Assignment_t_util(al, m_body[i]->base.loc,
                                ext_target, backup_val, nullptr, false, false)));
                    }
                    for (auto &pair : proc_local_ctx_backup_pairs) {
                        ASR::expr_t *backup_target = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.second));
                        ASR::expr_t *ext_val = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                        body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                            al, m_body[i]->base.loc, backup_target, ext_val, nullptr, false, false)));
                        ASR::expr_t *ext_target = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                        ASR::expr_t *backup_val = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, pair.second));
                        context_restore_stmts.push_back(ASRUtils::STMT(
                            ASRUtils::make_Assignment_t_util(al, m_body[i]->base.loc,
                                ext_target, backup_val, nullptr, false, false)));
                    }
                    remap_direct_host_call_proc_context(
                        m_body[i], capture_owner, stack_ptr_ext, body,
                        direct_host_recursive_call, pushed_context_frame,
                        saved_stack_ptr_expr);
                    for (auto &sym: nesting_map[capture_owner]) {
                        ASR::symbol_t *sym_past = ASRUtils::symbol_get_past_external(sym);
                        bool is_proc_capture = ASR::is_a<ASR::Function_t>(*sym_past) ||
                            (ASR::is_a<ASR::Variable_t>(*sym_past) &&
                             ASR::down_cast<ASR::Variable_t>(sym_past)->m_type_declaration &&
                             ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                                 ASR::down_cast<ASR::Variable_t>(sym_past)->m_type_declaration)));
                        bool need_scalar_sync = direct_host_recursive_call ||
                            calls_present || calls_present_direct ||
                            calls_present_proc_arg || calls_in_loop_condition;
                        if (!need_scalar_sync && !is_proc_capture) {
                            continue;
                        }
                        if (direct_host_recursive_call && !is_proc_capture &&
                                !calls_present_direct && !calls_present_proc_arg &&
                                !calls_in_loop_condition) {
                            continue;
                        }
                        std::string m_name = nested_var_to_ext_var[sym].first;
                        ASR::symbol_t *t = nested_var_to_ext_var[sym].second;
                        ASR::symbol_t *ext_sym = get_or_import_external_symbol(t, t->base.loc);
                        if (ASR::is_a<ASR::Variable_t>(
                                       *ASRUtils::symbol_get_past_external(ext_sym))
                                   && ASR::is_a<ASR::StructType_t>(*ASRUtils::type_get_past_array(
                                       ASRUtils::type_get_past_allocatable_pointer(
                                           ASR::down_cast<ASR::Variable_t>(
                                               ASRUtils::symbol_get_past_external(ext_sym))->m_type)))
                                    && !ASRUtils::is_class_type(
                                        ASRUtils::type_get_past_allocatable_pointer(
                                            ASR::down_cast<ASR::Variable_t>(
                                                ASRUtils::symbol_get_past_external(ext_sym))->m_type))
                                   && ASR::is_a<ASR::Program_t>(*ASRUtils::get_asr_owner((ext_sym)))) {
                            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(
                                    ASRUtils::symbol_get_past_external(ext_sym));
                            // Import the Struct as an `ExternalSymbol` into `Program`
                            std::string sym_name, module_name = "";
                            ASR::symbol_t* m_external = nullptr;
                            if (ASR::is_a<ASR::ExternalSymbol_t>(*var->m_type_declaration)) {
                                sym_name = ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(var->m_type_declaration));
                                m_external = ASRUtils::symbol_get_past_external(var->m_type_declaration);
                                module_name = ASR::down_cast<ASR::ExternalSymbol_t>(
                                                            var->m_type_declaration)->m_module_name;
                            } else {
                                sym_name = ASRUtils::symbol_name(var->m_type_declaration);
                                m_external = var->m_type_declaration;
                                module_name = ASR::down_cast<ASR::ExternalSymbol_t>(ext_sym)->m_module_name;
                            }
                            ASR::symbol_t* st_sym = ASR::down_cast<ASR::symbol_t>(
                                                    ASR::make_ExternalSymbol_t(
                                                        al,
                                                        var->base.base.loc,
                                                        current_scope,
                                                        s2c(al, sym_name),
                                                        m_external,
                                                        s2c(al, module_name),
                                                        nullptr,
                                                        0,
                                                        ASRUtils::symbol_name(var->m_type_declaration),
                                                        ASR::accessType::Public));
                            if (!current_scope->get_symbol(ASRUtils::symbol_name(var->m_type_declaration))) {
                                current_scope->add_symbol(ASRUtils::symbol_name(var->m_type_declaration), st_sym);
                            }
                        }
                        ASR::symbol_t* sym_ = sym;
                        if (capture_owner != cur_func_sym) {
                            sym_ = prefer_scope_capture_symbol(sym, capture_owner);
                        }
                        SymbolTable *sym_parent = ASRUtils::symbol_parent_symtab(sym_);
                        if (!is_sym_in_scope_chain(current_scope, sym_parent)) {
                            std::string sym_name = ASRUtils::symbol_name(sym_);
                            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
                            ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                                al, t->base.loc,
                                /* a_symtab */ current_scope,
                                /* a_name */ s2c(al, current_scope->get_unique_name(sym_name, false)),
                                s, ASRUtils::symbol_name(ASRUtils::get_asr_owner(s)),
                                nullptr, 0, ASRUtils::symbol_name(s), ASR::accessType::Public
                            );
                            sym_ = ASR::down_cast<ASR::symbol_t>(fn);
                            current_scope->add_symbol(sym_name, sym_);
                        }
                        ASR::symbol_t *sym_capture_past = ASRUtils::symbol_get_past_external(sym);
                        bool is_proc_capture_sym = ASR::is_a<ASR::Function_t>(*sym_capture_past) ||
                            (ASR::is_a<ASR::Variable_t>(*sym_capture_past) &&
                             ASR::down_cast<ASR::Variable_t>(sym_capture_past)->m_type_declaration &&
                             ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                                 ASR::down_cast<ASR::Variable_t>(sym_capture_past)->m_type_declaration)));
                        if (direct_host_recursive_call && is_proc_capture_sym) {
                            continue;
                        }
                        if (capture_owner != cur_func_sym && is_proc_capture_sym) {
                            // In nested child procedures, route procedure captures through
                            // the parent's context globals, not interface symbols.
                            sym_ = ext_sym;
                        }
                        LCOMPILERS_ASSERT(ext_sym != nullptr);
                        LCOMPILERS_ASSERT(sym_ != nullptr);
                        ASR::expr_t *target = ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, ext_sym));
                        ASR::expr_t *val = ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, sym_));
                        ASR::symbol_t *sym_past_for_types = ASRUtils::symbol_get_past_external(sym);
                        ASR::symbol_t *ext_sym_past = ASRUtils::symbol_get_past_external(ext_sym);
                        bool is_sym_variable = ASR::is_a<ASR::Variable_t>(*sym_past_for_types);
                        bool is_ext_sym_variable = ASR::is_a<ASR::Variable_t>(*ext_sym_past);
                        bool is_sym_allocatable_or_pointer = is_sym_variable &&
                            (ASRUtils::is_pointer(ASRUtils::symbol_type(sym)) ||
                             ASRUtils::is_allocatable(ASRUtils::symbol_type(sym)));
                        bool is_ext_sym_allocatable_or_pointer = is_ext_sym_variable &&
                            (ASRUtils::is_pointer(ASRUtils::symbol_type(ext_sym)) ||
                             ASRUtils::is_allocatable(ASRUtils::symbol_type(ext_sym)));
                        bool is_procedure_variable = ASR::is_a<ASR::Variable_t>(*sym_) &&
                            ASR::down_cast<ASR::Variable_t>(sym_)->m_type_declaration &&
                            ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external
                            (ASR::down_cast<ASR::Variable_t>(sym_)->m_type_declaration));
                        bool is_procedure_symbol = ASR::is_a<ASR::Function_t>(
                            *ASRUtils::symbol_get_past_external(sym_));
                        ASR::symbol_t *ext_ctx_sym = nullptr;
                        auto it_ctx = nested_proc_ctx_var.find(sym);
                        if (it_ctx != nested_proc_ctx_var.end()) {
                            ext_ctx_sym = get_or_import_external_symbol(
                                it_ctx->second, it_ctx->second->base.loc);
                        }
                        bool is_sym_array = is_sym_variable && ASRUtils::is_array(ASRUtils::symbol_type(sym));
                        bool is_ext_sym_pointer = is_ext_sym_variable && ASRUtils::is_pointer(ASRUtils::symbol_type(ext_sym));
                        if( is_sym_array || is_ext_sym_pointer ) {
                            ASR::stmt_t *associate = ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, t->base.loc,
                                                        target, val));
                            body.push_back(al, associate);
                            // TODO : Remove the following if block (See integration test `arrays_87.f90`)
                            if(is_sym_array &&
                                is_ext_sym_allocatable_or_pointer && is_sym_allocatable_or_pointer
                                && ASRUtils::EXPR2VAR(val)->m_storage != ASR::storage_typeType::Parameter ) {
                                associate = ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, t->base.loc,
                                    val, target));
                                assigns_at_end.push_back(associate);
                            }
                        } else if (is_procedure_variable || is_procedure_symbol) {
                            body.push_back(al, ASRUtils::STMT(ASR::make_Associate_t(al, t->base.loc, target, val)));
                            if (ext_ctx_sym != nullptr) {
                                ASR::expr_t *ctx_target = ASRUtils::EXPR(
                                    ASR::make_Var_t(al, t->base.loc, ext_ctx_sym));
                                ASR::expr_t *ctx_value = nullptr;
                                if (is_procedure_symbol && stack_ptr_ext != nullptr) {
                                    ASR::symbol_t *proc_sym_past = ASRUtils::symbol_get_past_external(sym_);
                                    ASR::Function_t *proc_fn = ASR::down_cast<ASR::Function_t>(proc_sym_past);
                                    if (ASRUtils::get_FunctionType(proc_fn)->m_deftype ==
                                            ASR::deftypeType::Implementation &&
                                        proc_fn->m_symtab->parent == current_scope) {
                                        ctx_value = ASRUtils::EXPR(
                                            ASR::make_Var_t(al, t->base.loc, stack_ptr_ext));
                                    } else {
                                        ctx_value = ASRUtils::EXPR(
                                            ASR::make_Var_t(al, t->base.loc, ext_ctx_sym));
                                    }
                                } else {
                                    SymbolTable *src_parent = ASRUtils::symbol_parent_symtab(sym_);
                                    if (!is_sym_in_scope_chain(current_scope, src_parent)) {
                                        auto it_src_ctx = nested_proc_ctx_var.find(sym);
                                        if (it_src_ctx != nested_proc_ctx_var.end()) {
                                            ASR::symbol_t *src_ctx_ext = get_or_import_external_symbol(
                                                it_src_ctx->second, it_src_ctx->second->base.loc);
                                            ctx_value = ASRUtils::EXPR(
                                                ASR::make_Var_t(al, t->base.loc, src_ctx_ext));
                                        }
                                    }
                                }
                                if (ctx_value == nullptr) {
                                    ctx_value = make_int32_constant(0, t->base.loc);
                                }
                                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                    al, t->base.loc, ctx_target, ctx_value, nullptr, false, false)));
                            }
                        } else {
                            ASR::stmt_t *assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                                        target, val, nullptr, false, false));
                            /* Allocatable RHS Needs A Check `IF allocated --> assign` (Based On Fortran Standards) */
                            if(ASRUtils::is_allocatable(ASRUtils::expr_type(val))){
                                body.push_back(al, create_if_allocated_block(val, assignment));
                            } else { // Otherwise Assign Directly.
                                body.push_back(al, assignment);
                            }
                            // Keep the active closure frame in sync for scalar captures.
                            // Calls through procedure variables may restore from stack slots.
                            if (stack_ptr_ext != nullptr &&
                                    is_plain_scalar_capture_symbol(sym)) {
                                ASR::symbol_t *stack_var_sym = find_stack_var_symbol(ext_sym);
                                if (stack_var_sym != nullptr) {
                                    ASR::symbol_t *stack_ext_sym = get_or_import_external_symbol(
                                        stack_var_sym, stack_var_sym->base.loc);
                                    ASR::expr_t *stack_ptr_var = ASRUtils::EXPR(
                                        ASR::make_Var_t(al, t->base.loc, stack_ptr_ext));
                                    ASR::expr_t *zero = make_int32_constant(0, t->base.loc);
                                    ASR::expr_t *has_ctx = PassUtils::create_compare_helper(
                                        al, t->base.loc, stack_ptr_var, zero,
                                        ASR::cmpopType::Gt);
                                    ASR::expr_t *stack_item = make_stack_item_expr(
                                        stack_ext_sym, stack_ptr_var, t->base.loc);
                                    ASR::expr_t *ext_val = ASRUtils::EXPR(
                                        ASR::make_Var_t(al, t->base.loc, ext_sym));
                                    Vec<ASR::stmt_t*> if_body;
                                    if_body.reserve(al, 1);
                                    if_body.push_back(al, ASRUtils::STMT(
                                        ASRUtils::make_Assignment_t_util(
                                            al, t->base.loc, stack_item, ext_val,
                                            nullptr, false, false)));
                                    body.push_back(al, ASRUtils::STMT(
                                        ASR::make_If_t(al, t->base.loc, nullptr, has_ctx,
                                            if_body.p, if_body.size(), nullptr, 0)));
                                }
                            }
                            if (ASRUtils::EXPR2VAR(val)->m_storage != ASR::storage_typeType::Parameter &&
                                    ASRUtils::EXPR2VAR(val)->m_intent != ASR::intentType::In) {
                                assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                                val, target, nullptr, false, false));
                                /* Allocatable RHS Needs A Check `IF allocated --> assign` (Based On Fortran Standards) */
                                if(ASRUtils::is_allocatable(ASRUtils::expr_type(target))){
                                    assigns_at_end.push_back(create_if_allocated_block(target, assignment));
                                } else { // Otherwise Assign Directly.
                                    assigns_at_end.push_back(assignment);
                                }
                            }
                            // For do-loop cycle statements, put a sync for scalar variables
                            // Note: Arrays are synced during loop entries, no need to sync again
                            // If not inside do-loop, bypass this step
                            if (is_do_loop_sync && calls_in_loop_condition && 
                                ASRUtils::EXPR2VAR(val)->m_storage != ASR::storage_typeType::Parameter &&
                                ASRUtils::EXPR2VAR(val)->m_intent != ASR::intentType::In && 
                                !ASRUtils::is_array(ASRUtils::symbol_type(sym))){
                                ASR::stmt_t *loop_sync = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                                target, val, nullptr, false, false));
                                if(ASRUtils::is_allocatable(ASRUtils::expr_type(val))){
                                    loop_end_syncs.push_back(create_if_allocated_block(val, loop_sync));
                                } else {
                                    loop_end_syncs.push_back(loop_sync);
                                }
                            }
                        }
                    }
                    if (stack_ptr_ext != nullptr &&
                            (!stack_backup_pairs.empty() || !stack_ctx_backup_pairs.empty())) {
                        ASR::expr_t *stack_ptr_now = ASRUtils::EXPR(
                            ASR::make_Var_t(al, m_body[i]->base.loc, stack_ptr_ext));
                        for (auto &pair : stack_backup_pairs) {
                            ASR::expr_t *stack_item = make_stack_item_expr(
                                pair.second, stack_ptr_now, m_body[i]->base.loc);
                            ASR::expr_t *ext_val = ASRUtils::EXPR(
                                ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, m_body[i]->base.loc, stack_item, ext_val, nullptr, false, false)));
                        }
                        for (auto &pair : stack_ctx_backup_pairs) {
                            ASR::expr_t *stack_item = make_stack_item_expr(
                                pair.second, stack_ptr_now, m_body[i]->base.loc);
                            ASR::expr_t *ext_val = ASRUtils::EXPR(
                                ASR::make_Var_t(al, m_body[i]->base.loc, pair.first));
                            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, m_body[i]->base.loc, stack_item, ext_val, nullptr, false, false)));
                        }
                    }
                }
            }

            // Inject syncs before cycle/exit if this is a loop statement
            if (!loop_end_syncs.empty()) {
                if (ASR::is_a<ASR::WhileLoop_t>(*m_body[i])) {
                    ASR::WhileLoop_t* loop = ASR::down_cast<ASR::WhileLoop_t>(m_body[i]);
                    inject_before_cycle(al, loop->m_body, loop->n_body, loop_end_syncs);
                } else if (ASR::is_a<ASR::DoLoop_t>(*m_body[i])) {
                    ASR::DoLoop_t* loop = ASR::down_cast<ASR::DoLoop_t>(m_body[i]);
                    inject_before_cycle(al, loop->m_body, loop->n_body, loop_end_syncs);
                }
            }
            // Handling for loop condition having function calls
            if (calls_in_loop_condition && is_do_loop_sync) { 
                ASR::WhileLoop_t* loop = ASR::down_cast<ASR::WhileLoop_t>(m_body[i]);
                Vec<ASR::stmt_t*> new_body;
                new_body.reserve(al, loop->n_body + assigns_at_end.size() * 2);
                // LOOP START:  Handle Assignments to main from function temporaries
                for (auto &stm: assigns_at_end) {
                    new_body.push_back(al, stm);
                }
                // Original loop body
                for (size_t j = 0; j < loop->n_body; j++) {
                    new_body.push_back(al, loop->m_body[j]);
                }
                // LOOP END: Handle Assignments from main to temporaries 
                // (for next iteration's condition)
                if (nesting_map.find(capture_owner) != nesting_map.end()) {
                    for (auto &sym: nesting_map[capture_owner]) {
                        ASR::symbol_t *sym_past = ASRUtils::symbol_get_past_external(sym);
                        bool is_proc_capture = ASR::is_a<ASR::Function_t>(*sym_past) ||
                            (ASR::is_a<ASR::Variable_t>(*sym_past) &&
                             ASR::down_cast<ASR::Variable_t>(sym_past)->m_type_declaration &&
                             ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                                 ASR::down_cast<ASR::Variable_t>(sym_past)->m_type_declaration)));
                        bool need_scalar_sync = calls_present || calls_present_direct ||
                            calls_present_proc_arg || calls_in_loop_condition;
                        if (!need_scalar_sync && !is_proc_capture) {
                            continue;
                        }
                        ASR::symbol_t *t = nested_var_to_ext_var[sym].second;
                        ASR::symbol_t *ext_sym = get_or_import_external_symbol(t, t->base.loc);

                        ASR::symbol_t* sym_ = sym;
                        if (capture_owner != cur_func_sym) {
                            sym_ = prefer_scope_capture_symbol(sym, capture_owner);
                        }
                        SymbolTable *sym_parent = ASRUtils::symbol_parent_symtab(sym_);
                        if (!is_sym_in_scope_chain(current_scope, sym_parent)) {
                            std::string sym_name = ASRUtils::symbol_name(sym_);
                            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
                            ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                                al, t->base.loc,
                                /* a_symtab */ current_scope,
                                /* a_name */ s2c(al, current_scope->get_unique_name(sym_name, false)),
                                s, ASRUtils::symbol_name(ASRUtils::get_asr_owner(s)),
                                nullptr, 0, ASRUtils::symbol_name(s), ASR::accessType::Public
                            );
                            sym_ = ASR::down_cast<ASR::symbol_t>(fn);
                            current_scope->add_symbol(sym_name, sym_);
                        }
                        ASR::symbol_t *sym_capture_past = ASRUtils::symbol_get_past_external(sym);
                        bool is_proc_capture_sym = ASR::is_a<ASR::Function_t>(*sym_capture_past) ||
                            (ASR::is_a<ASR::Variable_t>(*sym_capture_past) &&
                             ASR::down_cast<ASR::Variable_t>(sym_capture_past)->m_type_declaration &&
                             ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                                 ASR::down_cast<ASR::Variable_t>(sym_capture_past)->m_type_declaration)));
                        if (capture_owner != cur_func_sym && is_proc_capture_sym) {
                            sym_ = ext_sym;
                        }

                        if (ext_sym && sym_ &&
                            ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(sym)) &&
                            !ASRUtils::is_array(ASRUtils::symbol_type(sym)) &&
                            ASRUtils::EXPR2VAR(ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, sym_)))->m_storage
                                != ASR::storage_typeType::Parameter &&
                            ASRUtils::EXPR2VAR(ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, sym_)))->m_intent
                                != ASR::intentType::In) {
                            ASR::expr_t *target = ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, ext_sym));
                            ASR::expr_t *val = ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, sym_));
                            ASR::stmt_t *assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, t->base.loc, target, val, nullptr, false, false));
                            new_body.push_back(al, assignment);
                        }
                    }
                }
                loop->m_body = new_body.p;
                loop->n_body = new_body.size();
                new_body.n = 0;  // Clear size
                new_body.p = nullptr;  // Clear pointer after ownership transfer
                body.push_back(al, m_body[i]);
                calls_in_loop_condition = false;  // Reset flag

            } else if (calls_in_loop_condition && ASR::is_a<ASR::If_t>(*m_body[i])) {
                // Handling for IF condition having function calls
                // Handle assignments from function temporaries to main.
                // We need the pre-sync before entering each arm (state produced by
                // calls in condition) and post-sync at arm end (state produced by
                // nested calls in the arm body).
                ASR::If_t* if_stmt = ASR::down_cast<ASR::If_t>(m_body[i]);
                std::vector<ASR::stmt_t*> sync_back_to_context;
                sync_back_to_context.reserve(assigns_at_end.size());
                for (auto &stm: assigns_at_end) {
                    if (ASR::is_a<ASR::Assignment_t>(*stm)) {
                        ASR::Assignment_t *assign = ASR::down_cast<ASR::Assignment_t>(stm);
                        sync_back_to_context.push_back(
                            ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                                al, assign->base.base.loc, assign->m_value,
                                assign->m_target, nullptr, false, false)));
                    } else {
                        sync_back_to_context.push_back(stm);
                    }
                }
                Vec<ASR::stmt_t*> new_body;
                new_body.reserve(al, if_stmt->n_body + assigns_at_end.size() * 2);
                for (auto &stm: assigns_at_end) {
                    new_body.push_back(al, stm);
                }
                // Original if body
                for (size_t j = 0; j < if_stmt->n_body; j++) {
                    new_body.push_back(al, if_stmt->m_body[j]);
                }
                for (auto &stm: sync_back_to_context) {
                    new_body.push_back(al, stm);
                }
                if_stmt->m_body = new_body.p;
                if_stmt->n_body = new_body.size();

                // Also handle reassingment in else block if cond is false
                Vec<ASR::stmt_t*> else_new_body;
                else_new_body.reserve(al, if_stmt->n_orelse + assigns_at_end.size() * 2);
                for (auto &stm: assigns_at_end) {
                    else_new_body.push_back(al, stm);
                }
                // Original else body
                for (size_t j = 0; j < if_stmt->n_orelse; j++) {
                    else_new_body.push_back(al, if_stmt->m_orelse[j]);
                }
                for (auto &stm: sync_back_to_context) {
                    else_new_body.push_back(al, stm);
                }
                if_stmt->m_orelse = else_new_body.p;
                if_stmt->n_orelse = else_new_body.size();

                new_body.n = 0;
                new_body.p = nullptr;
                else_new_body.n = 0;
                else_new_body.p = nullptr;
                body.push_back(al, m_body[i]);
                calls_in_loop_condition = false;  // Reset flag
            } else {
                // Original behavior: append loop, then syncs after loop
                body.push_back(al, m_body[i]);
                for (auto &stm: loop_end_syncs) {
                    body.push_back(al, stm);
                }
                for (auto &stm: assigns_at_end) {
                    body.push_back(al, stm);
                }
                if (is_do_loop_sync) {
                    calls_in_loop_condition = false;  // Reset flag
                }
            }
            for (auto &stm: context_restore_stmts) {
                body.push_back(al, stm);
            }
            for (auto &stm: captured_scalar_writeback_stmts) {
                body.push_back(al, stm);
            }
            calls_in_loop_condition = false;  // Reset flag
            calls_present = false;
        }
        m_body = body.p;
        n_body = body.size();
    }

    void visit_Function(const ASR::Function_t &x) {
        ASR::Function_t &xx = const_cast<ASR::Function_t&>(x);
        SymbolTable* current_scope_copy = current_scope;
        ASR::symbol_t *sym_copy = cur_func_sym;
        cur_func_sym = (ASR::symbol_t*)&xx;
        current_scope = xx.m_symtab;
        transform_stmts(xx.m_body, xx.n_body);

        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(item.second);
                visit_Function(*s);
            }
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                ASR::Block_t *s = ASR::down_cast<ASR::Block_t>(item.second);
                visit_Block(*s);
            }
            if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                ASR::AssociateBlock_t *s = ASR::down_cast<ASR::AssociateBlock_t>(item.second);
                visit_AssociateBlock(*s);
            }
        }
        cur_func_sym = sym_copy;
        current_scope = current_scope_copy;
    }

    void visit_Program(const ASR::Program_t &x) {
        ASR::Program_t &xx = const_cast<ASR::Program_t&>(x);
        SymbolTable* current_scope_copy = current_scope;
        current_scope = xx.m_symtab;
        ASR::symbol_t *sym_copy = cur_func_sym;
        cur_func_sym = (ASR::symbol_t*)&xx;
        transform_stmts(xx.m_body, xx.n_body);

        // Transform nested functions and subroutines
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(item.second);
                visit_Function(*s);
            }
            if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                ASR::AssociateBlock_t *s = ASR::down_cast<ASR::AssociateBlock_t>(item.second);
                visit_AssociateBlock(*s);
            }
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                ASR::Block_t *s = ASR::down_cast<ASR::Block_t>(item.second);
                visit_Block(*s);
            }
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
                if (ASR::is_a<ASR::StructType_t>(*ASRUtils::type_get_past_array(
                        ASRUtils::type_get_past_allocatable_pointer(v->m_type)))) {
                    // Fix the type_declaration of variables to point to the imported Struct (as ExternalSymbol)
                    ASR::symbol_t* type_decl = v->m_type_declaration;
                    if ( current_scope->get_symbol(ASRUtils::symbol_name(type_decl)) ) {
                        ASR::down_cast<ASR::Variable_t>(item.second)->m_type_declaration = current_scope->get_symbol(
                                                                                                ASRUtils::symbol_name(type_decl));
                    }
                }
            }
        }
        current_scope = current_scope_copy;
        cur_func_sym = sym_copy;
    }

    void visit_Module(const ASR::Module_t &x) {
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(item.second);
                visit_Function(*s);
            }
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                ASR::Block_t *s = ASR::down_cast<ASR::Block_t>(item.second);
                visit_Block(*s);
            }
            if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                ASR::AssociateBlock_t *s = ASR::down_cast<ASR::AssociateBlock_t>(item.second);
                visit_AssociateBlock(*s);
            }
        }
        current_scope = current_scope_copy;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        bool is_nested_call = is_nested_call_symbol(current_scope, x.m_name);
        calls_present = true;
        if (is_nested_call) {
            ASR::symbol_t *call_target = ASRUtils::symbol_get_past_external(x.m_name);
            if (ASR::is_a<ASR::Function_t>(*call_target)) {
                ASR::Function_t *call_fn = ASR::down_cast<ASR::Function_t>(call_target);
                if (ASRUtils::get_FunctionType(call_fn)->m_deftype ==
                        ASR::deftypeType::Implementation) {
                    calls_present_direct = true;
                }
            } else if (ASR::is_a<ASR::Variable_t>(*call_target)) {
                ASR::Variable_t *call_var = ASR::down_cast<ASR::Variable_t>(call_target);
                if (call_var->m_type_declaration &&
                        ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                            call_var->m_type_declaration))) {
                    ASR::Function_t *decl_fn = ASR::down_cast<ASR::Function_t>(
                        ASRUtils::symbol_get_past_external(call_var->m_type_declaration));
                    if (ASRUtils::get_FunctionType(decl_fn)->m_deftype ==
                            ASR::deftypeType::Implementation) {
                        calls_present_direct = true;
                    }
                }
            }
        }
        for (size_t i=0; i<x.n_args; i++) {
            mark_nested_procedure_arg(x.m_args[i].m_value);
            visit_call_arg(x.m_args[i]);
        }
        visit_ttype(*x.m_type);
        if (x.m_value)
            visit_expr(*x.m_value);
        if (x.m_dt)
            visit_expr(*x.m_dt);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        bool is_nested_call = is_nested_call_symbol(current_scope, x.m_name);
        calls_present = true;
        if (is_nested_call) {
            ASR::symbol_t *call_target = ASRUtils::symbol_get_past_external(x.m_name);
            if (ASR::is_a<ASR::Function_t>(*call_target)) {
                ASR::Function_t *call_fn = ASR::down_cast<ASR::Function_t>(call_target);
                if (ASRUtils::get_FunctionType(call_fn)->m_deftype ==
                        ASR::deftypeType::Implementation) {
                    calls_present_direct = true;
                }
            } else if (ASR::is_a<ASR::Variable_t>(*call_target)) {
                ASR::Variable_t *call_var = ASR::down_cast<ASR::Variable_t>(call_target);
                if (call_var->m_type_declaration &&
                        ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                            call_var->m_type_declaration))) {
                    ASR::Function_t *decl_fn = ASR::down_cast<ASR::Function_t>(
                        ASRUtils::symbol_get_past_external(call_var->m_type_declaration));
                    if (ASRUtils::get_FunctionType(decl_fn)->m_deftype ==
                            ASR::deftypeType::Implementation) {
                        calls_present_direct = true;
                    }
                }
            }
        }
        for (size_t i=0; i<x.n_args; i++) {
            mark_nested_procedure_arg(x.m_args[i].m_value);
            visit_call_arg(x.m_args[i]);
        }
        if (x.m_dt)
            visit_expr(*x.m_dt);
    }

    void visit_Array(const ASR::Array_t& /*x*/) {
        return ;
    }

    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t& x) {
        visit_expr(*x.m_array);
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        // Step 1: Detect calls in loop condition
        calls_present = false;
        calls_present_direct = false;
        calls_present_proc_arg = false;
        visit_expr(*x.m_test);
        bool has_calls_in_condition = calls_present;
        // Step 2: Visit body to detect calls there
        calls_present = false;
        calls_present_direct = false;
        calls_present_proc_arg = false;
        // Now calls_present = true if body has calls
        PassUtils::PassVisitor<AssignNestedVars>::visit_WhileLoop(x);
        // Step 3: Set the condition flag (transform_stmts may 
        // reset the flag for calls_in_loop_condition while visiting body)
        calls_in_loop_condition = has_calls_in_condition;
    }

    void visit_If(const ASR::If_t &x) {
        // Step 1: Detect calls in condition
        calls_present = false;
        calls_present_direct = false;
        calls_present_proc_arg = false;
        visit_expr(*x.m_test);
        bool has_calls_in_condition = calls_present;
        // Step 2: Visit body to detect calls there
        calls_present = false;
        calls_present_direct = false;
        calls_present_proc_arg = false;
        // Now calls_present = true if body has calls
        PassUtils::PassVisitor<AssignNestedVars>::visit_If(x);
        // Step 3: Set the condition flag (transform_stmts may 
        // reset the flag for calls_in_loop_condition while visiting body)
        calls_in_loop_condition = has_calls_in_condition;
    }
};

void pass_nested_vars(Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    NestedVarVisitor v(al);
    v.visit_TranslationUnit(unit);
    ReplaceNestedVisitor w(al, v.nesting_map);
    w.visit_TranslationUnit(unit);
    AssignNestedVars z(al, w.nested_var_to_ext_var, w.nesting_map,
        w.ext_var_to_stack_var, w.func_to_stack_ptr, w.func_to_stack_next,
        w.nested_proc_ctx_var);
    z.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor x(al);
    x.visit_TranslationUnit(unit);
}


} // namespace LCompilers
