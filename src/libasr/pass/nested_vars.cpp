#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/nested_vars.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <set>
#include <functional>

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

// `program_interfaces` maps the copy of the interface of a program procedure
// in a context module to that procedure: a procedure variable declared with
// the copy is called like one declared with the procedure.
static bool is_nested_call_symbol(SymbolTable *current_scope, ASR::symbol_t *sym,
        const std::map<ASR::symbol_t*, ASR::symbol_t*> *program_interfaces = nullptr) {
    SymbolTable *host_scope = get_host_scope(current_scope);
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
    if (ASR::is_a<ASR::Function_t>(*s)) {
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
        return fn->m_symtab->parent == host_scope;
    }
    if (ASR::is_a<ASR::Variable_t>(*s)) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        ASR::symbol_t *decl = v->m_type_declaration
            ? ASRUtils::symbol_get_past_external(v->m_type_declaration) : nullptr;
        if (decl && program_interfaces) {
            auto it = program_interfaces->find(decl);
            if (it != program_interfaces->end()) {
                decl = it->second;
            }
        }
        if (decl && ASR::is_a<ASR::Function_t>(*decl)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(decl);
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

// Resolve an ExternalSymbol for `target` in `scope`, or create one if missing
// or if the name resolves to a different symbol.
static ASR::symbol_t *resolve_or_create_external_symbol(Allocator &al,
        SymbolTable *scope, ASR::symbol_t *target,
        const std::string &module_name) {
    std::string name(ASRUtils::symbol_name(target));
    ASR::symbol_t *ext = scope->resolve_symbol(name);
    if (ext != nullptr &&
            ASR::is_a<ASR::ExternalSymbol_t>(*ext) &&
            ASRUtils::symbol_get_past_external(ext) == target) {
        return ext;
    }
    std::string unique_name = name;
    if (ext != nullptr) {
        unique_name = scope->get_unique_name(name, false);
    }
    return make_external_symbol(al, scope, target, unique_name,
        module_name, name, ASR::accessType::Public);
}

static ASR::ttype_t *duplicate_type_for_nested_context(Allocator &al,
        ASR::ttype_t *var_type) {
    ASR::ttype_t *array_type = ASRUtils::type_get_past_allocatable_pointer(var_type);
    if (ASR::is_a<ASR::Array_t>(*array_type) &&
            ASR::down_cast<ASR::Array_t>(array_type)->m_physical_type ==
                ASR::array_physical_typeType::UnboundedPointerArray) {
        return ASRUtils::duplicate_type_with_empty_dims(al, var_type,
            ASR::array_physical_typeType::UnboundedPointerArray, true);
    }
    return ASRUtils::duplicate_type_with_empty_dims(al, var_type);
}

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
                if ( ASRUtils::is_entry_initialized_local(*v) ) {
                    // E.g. an automatic array bound that uses a host variable.
                    visit_expr(*v->m_symbolic_value);
                }
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
            } else if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(
                    item.second);
                if (!is_func_visited) {
                    is_func_visited = true;
                    for (size_t i = 0; i < x.n_body; i++) {
                        visit_stmt(*x.m_body[i]);
                    }
                }

                ASR::symbol_t* par_func_sym_copy = par_func_sym;
                par_func_sym = cur_func_sym;
                visit_Function(*s);
                par_func_sym = par_func_sym_copy;
            } else {
                visit_symbol(*item.second);
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
        ASR::symbol_t *cur_func_sym_copy = cur_func_sym;
        cur_func_sym = (ASR::symbol_t*)(&x);
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        visit_procedure(x);
        current_scope = current_scope_copy;
        cur_func_sym = cur_func_sym_copy;
    }

    // A template, e.g. the copy of a nested template in the procedure that
    // instantiates the enclosing one, is not code: only its instantiations
    // are, and they have their own variables.
    void visit_Template(const ASR::Template_t &/*x*/) {}

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
    /// Is a variable declared in a module scope or global translation unit scope
    bool is_module_or_global_variable(ASR::Variable_t* const v){
        LCOMPILERS_ASSERT(v->m_parent_symtab && v->m_parent_symtab->asr_owner)
        ASR::asr_t* const asr_owner = v->m_parent_symtab->asr_owner;
        const bool is_global_scope = ASR::is_a<ASR::unit_t>(*asr_owner) 
                                    && ASR::is_a<ASR::TranslationUnit_t>(*(ASR::unit_t*)asr_owner);
        const bool is_module_scoped = ASR::is_a<ASR::symbol_t>(*asr_owner) 
                                    && ASR::is_a<ASR::Module_t>(*(ASR::symbol_t*)asr_owner);
        return is_global_scope || is_module_scoped;
    }

    void visit_Var(const ASR::Var_t &x) {
        // Only attempt if we are actually in a nested function
        if (nesting_depth > 1) {
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_v);
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                visit_symbol(*sym);
            } else {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                // A variable in a common block lives in the block's static
                // storage, which every procedure reaches directly.
                if(is_module_or_global_variable(v) || v->m_common_block != nullptr) return;
                // A component of a derived type, e.g. the `=> null()`
                // initialization of `next` in `type(node), pointer :: next`
                // reached through the type's default initialization, is
                // not host storage.
                ASR::symbol_t* owner = ASRUtils::get_asr_owner(sym);
                if (owner && ASR::is_a<ASR::Struct_t>(*owner)) return;
                visit_ttype(*v->m_type);
                // If the variable is not defined in the current scope
                // (or a child scope such as an associate block), it is a
                // "needed global" since we need to be able to access it
                // from the nested procedure.
                if ( current_scope && par_func_sym &&
                    !is_sym_in_scope_chain(v->m_parent_symtab, current_scope)) {
                    nesting_map[get_declaring_procedure(v)].insert(x.m_v);
                }
            }
        }
    }

    // The procedure (or program) that declares `v`. It owns the context
    // for `v` and synchronizes it around its calls. Usually this is the
    // parent of the current procedure, but a procedure nested more than
    // one level deep (e.g. a template instantiated inside an internal
    // procedure) can reference a variable of a more distant ancestor.
    ASR::symbol_t* get_declaring_procedure(ASR::Variable_t* v) {
        SymbolTable* host_scope = get_host_scope(v->m_parent_symtab);
        if (host_scope->asr_owner != nullptr
                && ASR::is_a<ASR::symbol_t>(*host_scope->asr_owner)) {
            ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(host_scope->asr_owner);
            if (ASR::is_a<ASR::Function_t>(*owner) || ASR::is_a<ASR::Program_t>(*owner)) {
                return owner;
            }
        }
        return par_func_sym;
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        LCOMPILERS_ASSERT(ASR::is_a<ASR::AssociateBlock_t>(*x.m_m));
        ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        // Do NOT change current_scope here — the associate block is
        // within the same function.  The scope-chain check in visit_Var
        // correctly handles variables from child scopes.
        // Visit selector expressions (m_symbolic_value / m_value) to
        // detect host-associated variables used in associate selectors.
        for (auto &item : ab->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
                if (v->m_symbolic_value) {
                    visit_expr(*v->m_symbolic_value);
                }
                if (v->m_value) {
                    visit_expr(*v->m_value);
                }
            }
        }
        // Visit body statements.
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
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
            if (is_module_or_global_variable(v) || v->m_common_block != nullptr) {
                continue;
            }
            if (current_scope && par_func_sym &&
                !is_sym_in_scope_chain(v->m_parent_symtab, current_scope)) {
                nesting_map[par_func_sym].insert(nml->m_var_list[i]);
            }
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


// Finds a reference to a host variable that is replaced by its context
// variable in an expression, including in the types of its subexpressions
class NestedVarReferenceFinder: public ASR::BaseWalkVisitor<NestedVarReferenceFinder> {
public:
    const std::map<ASR::symbol_t*, std::pair<std::string, ASR::symbol_t*>> &nested_var_to_ext_var;
    bool found = false;

    NestedVarReferenceFinder(const std::map<ASR::symbol_t*,
            std::pair<std::string, ASR::symbol_t*>> &nested_var_to_ext_var_)
        : nested_var_to_ext_var(nested_var_to_ext_var_) {}

    void visit_Var(const ASR::Var_t &x) {
        if (nested_var_to_ext_var.find(x.m_v) != nested_var_to_ext_var.end()) {
            found = true;
        }
    }
};

class ReplacerNestedVars: public ASR::BaseExprReplacer<ReplacerNestedVars> {
private:
    Allocator &al;
public:
    SymbolTable *current_scope;
    SymbolTable *current_function_scope;
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
            ASR::symbol_t *existing = current_function_scope->get_symbol(sym_name);
            if (existing != nullptr &&
                    ASR::is_a<ASR::ExternalSymbol_t>(*existing) &&
                    ASRUtils::symbol_get_past_external(existing) == t) {
                x->m_v = existing;
                return;
            }
            std::string unique_name = sym_name;
            if (existing != nullptr) {
                unique_name = current_function_scope->get_unique_name(sym_name, false);
            }
            ASR::symbol_t *ext_sym = make_external_symbol(al, current_function_scope, t, unique_name,
                m_name, sym_name, ASR::accessType::Public);
            x->m_v = ext_sym;
        }
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        ASR::BaseExprReplacer<ReplacerNestedVars>::replace_ArrayPhysicalCast(x);
        x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
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
    std::map<std::pair<ASR::symbol_t*, ASR::symbol_t*>, ASR::symbol_t*> nested_namelists;
    std::map<ASR::symbol_t*, ASR::symbol_t*> assumed_length_ctx_var_len;
    // A derived type declared in a program and moved into a context module
    // -> its ExternalSymbol left in the program
    std::map<ASR::symbol_t*, ASR::symbol_t*> moved_program_structs;
    // (context module scope, procedure or imported symbol of a program)
    // -> the symbol that stands for it in that context module
    std::map<std::pair<SymbolTable*, ASR::symbol_t*>, ASR::symbol_t*> imported_program_symbols;
    // Copy of the interface of a program procedure -> that procedure
    std::map<ASR::symbol_t*, ASR::symbol_t*> program_procedure_interfaces;

    ReplaceNestedVisitor(Allocator& al_,
        std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &n_map) : al(al_),
        replacer(al_), nesting_map(n_map) {}

    // Skip replacing original variables with context variables
    // in the parent function except for function/subroutine calls
    bool replaces_nested_vars() {
        return nesting_depth != 1 || is_in_call;
    }

    void call_replacer() {
        bool skip_replace = !replaces_nested_vars();
        replacer.current_expr = current_expr;
        replacer.current_scope = current_scope;
        if (!func_stack.empty()) {
            ASR::symbol_t *fn_sym = func_stack.back();
            if (ASR::is_a<ASR::Function_t>(*fn_sym)) {
                replacer.current_function_scope = ASR::down_cast<ASR::Function_t>(fn_sym)->m_symtab;
            } else if (ASR::is_a<ASR::Program_t>(*fn_sym)) {
                replacer.current_function_scope = ASR::down_cast<ASR::Program_t>(fn_sym)->m_symtab;
            } else {
                replacer.current_function_scope = current_scope;
            }
        } else {
            replacer.current_function_scope = current_scope;
        }
        replacer.skip_replace = skip_replace;
        if (!skip_replace && *current_expr && !nested_var_to_ext_var.empty()) {
            NestedVarReferenceFinder finder(nested_var_to_ext_var);
            finder.visit_expr(**current_expr);
            if (finder.found) {
                // The expression can share nodes, notably its type, with a
                // declaration of the host: `2*s` has the type of the host's
                // `s(size(a))`. Replace in a copy, so that the declaration
                // keeps referring to the host's own variables.
                ASRUtils::ExprStmtDuplicator duplicator(al);
                *current_expr = duplicator.duplicate_expr(*current_expr);
            }
        }
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

    static bool is_program_symbol(ASR::symbol_t *sym) {
        ASR::symbol_t *owner = ASRUtils::get_asr_owner(sym);
        return owner != nullptr && ASR::is_a<ASR::Program_t>(*owner);
    }

    // The symbol through which the context module `scope` refers to `sym`,
    // which a derived type or a procedure interface used in the module
    // depends on. A module cannot refer into a program, so a symbol of a
    // program is replaced by a symbol of the module.
    ASR::symbol_t *import_dependency(ASR::symbol_t *sym, SymbolTable *scope,
            const std::string &module_name) {
        if (sym == nullptr || !is_program_symbol(sym)) {
            return sym;
        }
        return import_program_symbol(sym, scope, module_name);
    }

    ASR::symbol_t *import_program_symbol(ASR::symbol_t *sym, SymbolTable *scope,
            const std::string &module_name) {
        auto moved = moved_program_structs.find(sym);
        if (moved != moved_program_structs.end()) {
            ASR::symbol_t *copy = ASRUtils::symbol_get_past_external(moved->second);
            if (ASRUtils::symbol_parent_symtab(copy) == scope) {
                return copy;
            }
            return resolve_or_create_external_symbol(al, scope, copy,
                ASRUtils::symbol_name(ASRUtils::get_asr_owner(copy)));
        }
        auto imported = imported_program_symbols.find({scope, sym});
        if (imported != imported_program_symbols.end()) {
            return imported->second;
        }
        if (ASR::is_a<ASR::Struct_t>(*sym)) {
            return move_program_struct(sym, scope, module_name);
        } else if (ASR::is_a<ASR::Function_t>(*sym)) {
            return copy_program_procedure_interface(
                ASR::down_cast<ASR::Function_t>(sym), scope, module_name);
        } else if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
            ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(sym);
            std::string name = scope->get_unique_name(ext->m_name, false);
            ASR::symbol_t *copy = ASR::down_cast<ASR::symbol_t>(
                ASR::make_ExternalSymbol_t(al, ext->base.base.loc, scope,
                    s2c(al, name), ext->m_external, ext->m_module_name,
                    ext->m_scope_names, ext->n_scope_names,
                    ext->m_original_name, ext->m_access));
            scope->add_symbol(name, copy);
            imported_program_symbols[{scope, sym}] = copy;
            return copy;
        }
        return sym;
    }

    // Move the derived type `sym` of a program into the context module
    // `scope`, import it back into the program under its own name, and
    // make the types and procedure interfaces it depends on available in
    // the module.
    ASR::symbol_t *move_program_struct(ASR::symbol_t *sym, SymbolTable *scope,
            const std::string &module_name) {
        std::string struct_name = ASRUtils::symbol_name(sym);
        SymbolTable *program_scope = ASRUtils::symbol_parent_symtab(sym);
        ASRUtils::SymbolDuplicator sd(al);
        sd.duplicate_symbol(sym, scope);
        program_scope->erase_symbol(struct_name);
        ASR::symbol_t *copy = scope->get_symbol(struct_name);
        moved_program_structs[sym] = make_external_symbol(al, program_scope,
            copy, struct_name, module_name, struct_name,
            ASR::accessType::Public);
        ASR::Struct_t *struct_t = ASR::down_cast<ASR::Struct_t>(copy);
        struct_t->m_parent = import_dependency(struct_t->m_parent, scope,
            module_name);
        for (auto &item: struct_t->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *component = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                component->m_type_declaration = import_dependency(
                    component->m_type_declaration, scope, module_name);
            }
        }
        return copy;
    }

    // An interface in the context module `scope` for the procedure `fn` of a
    // program: a copy of its dummy arguments and result, without its body,
    // which may refer to the program.
    ASR::symbol_t *copy_program_procedure_interface(ASR::Function_t *fn,
            SymbolTable *scope, const std::string &module_name) {
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        ASRUtils::SymbolDuplicator sd(al);
        std::vector<ASR::symbol_t*> dummies;
        for (size_t i = 0; i < fn->n_args; i++) {
            dummies.push_back(ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
        }
        if (fn->m_return_var) {
            dummies.push_back(ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v);
        }
        std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> variables;
        for (ASR::symbol_t *dummy: dummies) {
            sd.duplicate_symbol(dummy, fn_symtab);
            ASR::symbol_t *copy = fn_symtab->get_symbol(ASRUtils::symbol_name(dummy));
            if (copy == nullptr) {
                return (ASR::symbol_t*) fn;
            }
            if (ASR::is_a<ASR::Variable_t>(*dummy) && ASR::is_a<ASR::Variable_t>(*copy)) {
                variables.push_back({ASR::down_cast<ASR::Variable_t>(dummy),
                    ASR::down_cast<ASR::Variable_t>(copy)});
            }
        }
        sd.remap_local_references(variables, fn_symtab);
        Vec<ASR::expr_t*> args;
        args.reserve(al, fn->n_args);
        for (size_t i = 0; i < fn->n_args; i++) {
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al,
                fn->m_args[i]->base.loc, fn_symtab->get_symbol(
                    ASRUtils::symbol_name(dummies[i])))));
        }
        ASR::expr_t *return_var = nullptr;
        if (fn->m_return_var) {
            return_var = ASRUtils::EXPR(ASR::make_Var_t(al,
                fn->m_return_var->base.loc, fn_symtab->get_symbol(
                    ASRUtils::symbol_name(dummies.back()))));
        }
        ASR::FunctionType_t *fn_type = ASRUtils::get_FunctionType(fn);
        std::string name = scope->get_unique_name(fn->m_name, false);
        ASR::symbol_t *iface = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, fn->base.base.loc, fn_symtab,
                s2c(al, name), nullptr, 0, args.p, args.size(), nullptr, 0,
                return_var, fn_type->m_abi, fn->m_access,
                ASR::deftypeType::Interface, fn_type->m_bindc_name,
                fn_type->m_elemental, fn_type->m_pure, fn_type->m_module,
                fn_type->m_inline, fn_type->m_static, fn_type->m_restrictions,
                fn_type->n_restrictions, fn_type->m_is_restriction,
                fn->m_deterministic, fn->m_side_effect_free, nullptr, nullptr,
                nullptr, fn_type->m_exec_space));
        scope->add_symbol(name, iface);
        imported_program_symbols[{scope, (ASR::symbol_t*) fn}] = iface;
        program_procedure_interfaces[iface] = (ASR::symbol_t*) fn;
        // The dummy arguments may be declared with symbols of the procedure
        // itself (e.g. the interface of a dummy procedure, or a type it
        // uses from a module) or of the program.
        for (auto &pair: variables) {
            ASR::symbol_t *decl = pair.second->m_type_declaration;
            if (decl != nullptr &&
                    ASRUtils::symbol_parent_symtab(decl) == fn->m_symtab) {
                std::string decl_name = ASRUtils::symbol_name(decl);
                sd.duplicate_symbol(decl, fn_symtab);
                pair.second->m_type_declaration = fn_symtab->get_symbol(decl_name);
            } else {
                pair.second->m_type_declaration = import_dependency(decl,
                    scope, module_name);
            }
        }
        return iface;
    }

    void create_context_modules(const ASR::TranslationUnit_t &x) {
        current_scope = x.m_symtab;
        SymbolTable* current_scope_copy = current_scope;

        // Add the nested vars by creating a new module

        for (auto &it: nesting_map) {
            // Iterate on each function with nested vars and create a context in
            // a new module.
            current_scope = al.make_new<SymbolTable>(current_scope_copy);
            std::string parent_names = "";
            SymbolTable *parent_scope = ASRUtils::symbol_parent_symtab(it.first);
            while (parent_scope != nullptr) {
                if (parent_scope->asr_owner != nullptr && ASR::is_a<ASR::symbol_t>(*parent_scope->asr_owner)) {
                    ASR::symbol_t *parent_sym = ASR::down_cast<ASR::symbol_t>(parent_scope->asr_owner);
                    parent_names = std::string(ASRUtils::symbol_name(parent_sym)) + "__" + parent_names;
                }
                parent_scope = parent_scope->parent;
            }
            std::string module_name = "__lcompilers_created__nested_context__" + parent_names + 
                std::string(ASRUtils::symbol_name(it.first)) + "_"; 
            bool is_any_variable_externally_defined = false;
            std::map<ASR::symbol_t*, std::string> sym_to_name;
            module_name = x.m_symtab->get_unique_name(module_name, false);
            for (auto &it2: it.second) {
                ASR::symbol_t* pending_length_ctx_sym = nullptr;
                std::string new_ext_var = std::string(ASRUtils::symbol_name(it2));
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(
                            ASRUtils::symbol_get_past_external(it2));
                if (!is_any_variable_externally_defined && is_externally_defined(var)) {
                    is_any_variable_externally_defined = true;
                }
                new_ext_var = current_scope->get_unique_name(new_ext_var, false);
                bool is_allocatable = ASRUtils::is_allocatable(var->m_type);
                bool is_pointer = ASRUtils::is_pointer(var->m_type);
                LCOMPILERS_ASSERT(!(is_allocatable && is_pointer));
                // For intent(IN) allocatable arrays, use a Pointer in the
                // context module instead of Allocatable. This ensures
                // the sync uses Associate (pointer association) which
                // preserves the original array bounds. Assignment would
                // reset lower bounds to 1.
                if (is_allocatable && var->m_intent == ASR::intentType::In) {
                    is_allocatable = false;
                }
                ASR::ttype_t* var_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(var->m_type));
                ASR::ttype_t* var_type_ = ASRUtils::type_get_past_array(var_type);
                if ( var->m_type_declaration &&
                     ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(var->m_type_declaration)) ) {
                    ASRUtils::SymbolDuplicator sd(al);
                    ASR::Variable_t* dup_var = ASR::down_cast<ASR::Variable_t>(sd.duplicate_Variable(var, current_scope));
                    dup_var->m_name = s2c(al, new_ext_var);
                    // Clear initialization expressions since they may
                    // reference symbols outside this module scope
                    // (e.g., `procedure(...), pointer :: p => f` where
                    // f lives in the program scope). The nested_vars
                    // pass synchronises the value via assignments.
                    dup_var->m_symbolic_value = nullptr;
                    dup_var->m_value = nullptr;
                    // Import m_type_declaration into the module scope
                    // when it lives in a different scope
                    ASR::symbol_t* type_decl = var->m_type_declaration;
                    if (type_decl && current_scope->get_counter() !=
                            ASRUtils::symbol_parent_symtab(type_decl)->get_counter()) {
                        std::string td_name = std::string(ASRUtils::symbol_name(type_decl));
                        ASR::symbol_t* existing_td = current_scope->get_symbol(td_name);
                        if (existing_td == nullptr) {
                            ASR::symbol_t* original = ASRUtils::symbol_get_past_external(type_decl);
                            ASR::symbol_t* owner_sym = ASRUtils::get_asr_owner(original);
                            if (ASR::is_a<ASR::Program_t>(*owner_sym)) {
                                // A module cannot refer into a program, so
                                // declare the variable with a copy of the
                                // interface of the program's procedure.
                                existing_td = import_program_symbol(original,
                                    current_scope, module_name);
                            } else {
                                std::string owner_name = std::string(ASRUtils::symbol_name(owner_sym));
                                ASR::asr_t *ext = ASR::make_ExternalSymbol_t(
                                    al, type_decl->base.loc,
                                    current_scope,
                                    s2c(al, td_name),
                                    original,
                                    s2c(al, owner_name),
                                    nullptr, 0,
                                    ASRUtils::symbol_name(original),
                                    ASR::accessType::Public
                                );
                                existing_td = ASR::down_cast<ASR::symbol_t>(ext);
                                current_scope->add_symbol(td_name, existing_td);
                            }
                        }
                        dup_var->m_type_declaration = existing_td;
                    }
                    ASR::symbol_t* dup_sym = (ASR::symbol_t*) dup_var;
                    current_scope->add_symbol(new_ext_var, dup_sym);
                    nested_var_to_ext_var[it2] = std::make_pair(module_name, dup_sym);
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
                                // A module cannot refer into a program, so
                                // move the program's type into the context
                                // module and import it back into the program
                                // under its own name.
                                m_derived_type_or_class_type = import_program_symbol(
                                    derived_type_or_class_type, current_scope,
                                    module_name);
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
                        } else if (is_allocatable) {
                            var_type = var_type_;
                        } else {
                            auto const pointer_var_type_ = ASRUtils::TYPE(ASR::make_Pointer_t(al, var_type->base.loc, var_type_));
                            var_type = pointer_var_type_;
                        }
                    }
                }
                if( (ASRUtils::is_array(var_type) && !is_pointer) ) {
                    bool is_unbounded_pointer_array =
                        ASRUtils::extract_physical_type(var_type) ==
                            ASR::array_physical_typeType::UnboundedPointerArray;
                    var_type = duplicate_type_for_nested_context(al, var_type);
                    if (is_allocatable) {
                        var_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, var_type->base.loc, var_type));
                    } else if (!is_unbounded_pointer_array) {
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
                                    ASRUtils::TYPE(ASR::make_String_t(al, str->base.base.loc, str->m_kind,
                                        nullptr, ASR::DeferredLength, ASR::DescriptorString))));
                    }
                } else if(ASRUtils::is_array_of_strings(var_type)){ // e.g -> `character(len=foo()) :: str(10)`
                    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(var_type));
                    ASR::String_t* string_t = ASRUtils::get_string_type(var_type);
                    bool non_constant_len = string_t->m_len &&
                        !ASRUtils::is_value_constant(string_t->m_len);
                    if(string_t->m_len_kind == ASR::AssumedLength || non_constant_len){
                        // Rewriting to deferred-length requires a companion
                        // length context var for allocatable arrays so nested
                        // allocate statements can provide m_len_expr.
                        if (is_allocatable) {
                            std::string len_name = current_scope->get_unique_name(
                                new_ext_var + "__lc_slen", false);
                            ASR::ttype_t* int8_type = ASRUtils::TYPE(
                                ASR::make_Integer_t(al, it2->base.loc, 8));
                            ASR::expr_t* len_expr_var = PassUtils::create_auxiliary_variable(
                                it2->base.loc, len_name, al, current_scope, int8_type,
                                ASR::intentType::Local, nullptr, nullptr);
                            pending_length_ctx_sym = ASR::down_cast<ASR::Var_t>(len_expr_var)->m_v;
                        }
                        // Create a new ASR::String node, To avoid using the original one.
                        array_t->m_type = ASRUtils::TYPE(ASR::make_String_t(al, string_t->base.base.loc, string_t->m_kind,
                                            nullptr, ASR::DeferredLength, ASR::DescriptorString));
                    }
                }
                if (is_allocatable && !ASRUtils::is_allocatable(var_type)) {
                    var_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, var_type->base.loc, var_type));
                }
                if (is_pointer && !ASRUtils::is_pointer(var_type)) {
                    var_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, var_type->base.loc, var_type));
                }
                ASR::symbol_t* type_decl = nullptr;
                if (m_derived_type_or_class_type) {
                    type_decl = m_derived_type_or_class_type;
                }
                ASR::expr_t *sym_expr = PassUtils::create_auxiliary_variable(
                    it2->base.loc, new_ext_var, al, current_scope, var_type,
                    ASR::intentType::Local, type_decl, nullptr);
                ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(sym_expr)->m_v;
                nested_var_to_ext_var[it2] = std::make_pair(module_name, sym);
                if (pending_length_ctx_sym) {
                    assumed_length_ctx_var_len[sym] = pending_length_ctx_sym;
                    pending_length_ctx_sym = nullptr;
                }
            }
            ASR::asr_t *tmp = ASR::make_Module_t(al, x.base.base.loc,
                                            /* a_symtab */ current_scope,
                                            /* a_name */ s2c(al, module_name),
                                            nullptr,
                                            nullptr,
                                            0,
                                            false, false, false, nullptr);
            if (is_any_variable_externally_defined) {
                // this module is externally defined, so we mark it as external
                current_scope->mark_all_variables_external(al);
            }
            ASR::symbol_t* mod_sym = ASR::down_cast<ASR::symbol_t>(tmp);
            x.m_symtab->add_symbol(module_name, mod_sym);
            func_to_nested_module[it.first] = mod_sym;
        }
        replacer.nested_var_to_ext_var = nested_var_to_ext_var;
        current_scope = current_scope_copy;
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        SymbolTable* current_scope_copy = current_scope;
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
        if (replaces_nested_vars() && !nested_var_to_ext_var.empty()) {
            NestedVarReferenceFinder finder(nested_var_to_ext_var);
            finder.visit_ttype(*xx.m_type);
            if (finder.found) {
                // A variable of a nested procedure can share its type with
                // a declaration of the host, e.g. a temporary created for
                // `v = v*v + 1` has the type of the host's `v(n)`. Replace
                // in a copy, so that the declaration keeps referring to
                // the host's own variables.
                ASRUtils::ExprStmtDuplicator duplicator(al);
                xx.m_type = duplicator.duplicate_ttype(xx.m_type);
            }
        }
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
                        ASR::symbol_t* new_sym = ASR::down_cast<ASR::Var_t>(m_dims[i].m_length)->m_v;
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
            if (it_ext != nested_var_to_ext_var.end()) {
                var_list.push_back(al, it_ext->second.second);
            } else {
                var_list.push_back(al, orig_sym);
            }
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
        
        ASR::symbol_t *nml_sym_past = ASRUtils::symbol_get_past_external(nml_sym);
        if (!ASR::is_a<ASR::Namelist_t>(*nml_sym_past)) return;
        ASR::Namelist_t *nml = ASR::down_cast<ASR::Namelist_t>(nml_sym_past);

        bool is_local_namelist = is_sym_in_scope_chain(nml->m_parent_symtab, current_scope);
        
        if (!is_local_namelist) {
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
                std::string unique_name = sym_name;
                if (current_scope->get_symbol(sym_name)) {
                    unique_name = current_scope->get_unique_name(sym_name, false);
                }
                ext_sym = make_external_symbol(al, current_scope, nml_new_sym, unique_name,
                    owner_name, sym_name, ASR::accessType::Public);
            }
            nml_sym = ext_sym;
        } else {
            for (size_t i = 0; i < nml->n_var_list; i++) {
                ASR::symbol_t *orig_sym = nml->m_var_list[i];
                auto it_ext = nested_var_to_ext_var.find(orig_sym);
                if (it_ext != nested_var_to_ext_var.end()) {
                    ASR::symbol_t *t = it_ext->second.second;
                    std::string &m_name = it_ext->second.first;
                    nml->m_var_list[i] = resolve_or_create_external_symbol(al, current_scope, t, m_name);
                }
            }
        }
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

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        bool is_in_call_copy = is_in_call;
        is_in_call = is_nested_call_symbol(current_scope, x.m_name,
            &program_procedure_interfaces);
        ASR::FunctionCall_t& xx = const_cast<ASR::FunctionCall_t&>(x);
        if (nested_var_to_ext_var.find(x.m_name) != nested_var_to_ext_var.end()) {
            std::string m_name = nested_var_to_ext_var[x.m_name].first;
            ASR::symbol_t *t = nested_var_to_ext_var[x.m_name].second;
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
            nullptr, false);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t& xx = const_cast<ASR::SubroutineCall_t&>(x);
        bool is_in_call_copy = is_in_call;
        is_in_call = is_nested_call_symbol(current_scope, x.m_name,
            &program_procedure_interfaces);
        if (nested_var_to_ext_var.find(x.m_name) != nested_var_to_ext_var.end()) {
            std::string m_name = nested_var_to_ext_var[x.m_name].first;
            ASR::symbol_t *t = nested_var_to_ext_var[x.m_name].second;
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
            nullptr, false);
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

    void visit_Allocate(const ASR::Allocate_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ReplaceNestedVisitor>::visit_Allocate(x);
        ASR::Allocate_t &xx = const_cast<ASR::Allocate_t&>(x);
        // Length comes from the source expression when present; do not inject.
        if (xx.m_source) return;
        for (size_t i = 0; i < xx.n_args; i++) {
            ASR::alloc_arg_t &a = xx.m_args[i];
            if (a.m_len_expr) continue;
            if (!a.m_a) continue;
            if (!ASR::is_a<ASR::Var_t>(*a.m_a)) continue;
            ASR::symbol_t *v_sym = ASR::down_cast<ASR::Var_t>(a.m_a)->m_v;
            ASR::symbol_t *v_target = ASRUtils::symbol_get_past_external(v_sym);
            auto it_len = assumed_length_ctx_var_len.find(v_target);
            if (it_len == assumed_length_ctx_var_len.end()) continue;
            ASR::ttype_t *var_type = ASRUtils::expr_type(a.m_a);
            if (!ASRUtils::is_character(*var_type)) continue;
            ASR::String_t *st = ASRUtils::get_string_type(var_type);
            if (st->m_len_kind != ASR::DeferredLength) continue;
            ASR::symbol_t *len_sym = it_len->second;
            std::string module_name(ASRUtils::symbol_name(
                ASRUtils::get_asr_owner(len_sym)));
            ASR::symbol_t *len_ext = resolve_or_create_external_symbol(
                al, current_scope, len_sym, module_name);
            a.m_len_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, len_ext));
        }
    }

};

// Copies a statement, using context variables in place of the host variables
class HostToContextVarDuplicator: public ASR::BaseExprStmtDuplicator<HostToContextVarDuplicator> {
public:
    // host variable -> its context variable, as seen from the statement
    std::map<ASR::symbol_t*, ASR::symbol_t*> ctx_vars;

    HostToContextVarDuplicator(Allocator &al): BaseExprStmtDuplicator(al) {}

    ASR::asr_t* duplicate_Var(ASR::Var_t* x) {
        auto it = ctx_vars.find(ASRUtils::symbol_get_past_external(x->m_v));
        return ASR::make_Var_t(al, x->base.base.loc,
            it == ctx_vars.end() ? x->m_v : it->second);
    }
};

class FunctionCallCollector: public ASR::BaseWalkVisitor<FunctionCallCollector> {
public:
    std::vector<ASR::FunctionCall_t*> calls;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        calls.push_back(const_cast<ASR::FunctionCall_t*>(&x));
        ASR::BaseWalkVisitor<FunctionCallCollector>::visit_FunctionCall(x);
    }
};

// Whether an entity of the derived type `struct_sym` can invoke a procedure
// that is not called explicitly: the type, a type it extends or the type of
// a component has a final subroutine (F2018 7.5.6.3) or a type-bound generic,
// such as a defined assignment of a component (10.2.1.3) or a defined
// input/output procedure (12.6.4.8). A polymorphic component can have any
// dynamic type.
static bool struct_may_invoke_procedures(ASR::symbol_t *struct_sym,
        std::set<ASR::symbol_t*> &visited) {
    if (struct_sym == nullptr) {
        return false;
    }
    struct_sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*struct_sym) || !visited.insert(struct_sym).second) {
        return false;
    }
    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(struct_sym);
    if (st->n_member_functions > 0) {
        return true;
    }
    for (auto &item: st->m_symtab->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::GenericProcedure_t>(*sym) ||
                ASR::is_a<ASR::CustomOperator_t>(*sym)) {
            return true;
        }
        if (!ASR::is_a<ASR::Variable_t>(*sym)) {
            continue;
        }
        ASR::Variable_t *member = ASR::down_cast<ASR::Variable_t>(sym);
        ASR::ttype_t *type = ASRUtils::extract_type(member->m_type);
        if (ASR::is_a<ASR::StructType_t>(*type) && (ASRUtils::is_class_type(type) ||
                struct_may_invoke_procedures(member->m_type_declaration, visited))) {
            return true;
        }
    }
    return struct_may_invoke_procedures(st->m_parent, visited);
}

// Whether a procedure can be invoked when the variable `v` is referenced or
// goes out of scope: it is finalizable, or it is of a derived type with a
// defined assignment of a component. A polymorphic allocatable variable, or
// a polymorphic intent(out) dummy, which is finalized on entry, can have any
// dynamic type. A polymorphic pointer is finalized only when it is
// deallocated (see ContainedReentryFinder); the context variable of a
// polymorphic dummy is such a pointer.
static bool variable_may_invoke_procedures(ASR::Variable_t *v) {
    ASR::ttype_t *type = ASRUtils::extract_type(v->m_type);
    if (!ASR::is_a<ASR::StructType_t>(*type)) {
        return false;
    }
    if (ASRUtils::is_class_type(type) && (ASRUtils::is_allocatable(v->m_type) ||
            v->m_intent == ASR::intentType::Out)) {
        return true;
    }
    std::set<ASR::symbol_t*> visited;
    return struct_may_invoke_procedures(v->m_type_declaration, visited);
}

// Finds whether a host can be entered again while a procedure contained in it runs:
// a contained procedure calls a procedure that is not contained in the host,
// explicitly or implicitly (by finalization, a defined assignment of a component
// or a defined input/output), or references one, which an intrinsic such as
// `reduce` or a procedure pointer can call, or a contained procedure is passed
// as an argument or associated with a pointer
class ContainedReentryFinder: public ASR::BaseWalkVisitor<ContainedReentryFinder> {
public:
    const ASR::Function_t *host = nullptr;
    bool in_contained = false;
    bool may_reenter = false;

    void check_variable(ASR::symbol_t *sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        may_reenter = may_reenter || (in_contained && ASR::is_a<ASR::Variable_t>(*sym) &&
            variable_may_invoke_procedures(ASR::down_cast<ASR::Variable_t>(sym)));
    }

    // A derived-type item, also of a namelist group, can be transferred by a
    // defined input/output procedure that is not bound to its type
    // (F2018 12.6.4.8.3)
    void check_io_items(ASR::expr_t **items, size_t n_items, ASR::symbol_t *nml) {
        if (!in_contained) {
            return;
        }
        for (size_t i = 0; i < n_items; i++) {
            may_reenter = may_reenter || ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(ASRUtils::expr_type(items[i])));
        }
        if (nml == nullptr) {
            return;
        }
        ASR::Namelist_t *group = ASR::down_cast<ASR::Namelist_t>(
            ASRUtils::symbol_get_past_external(nml));
        for (size_t i = 0; i < group->n_var_list; i++) {
            may_reenter = may_reenter || ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(ASRUtils::symbol_type(
                    ASRUtils::symbol_get_past_external(group->m_var_list[i]))));
        }
    }

    void visit_Variable(const ASR::Variable_t &x) {
        check_variable((ASR::symbol_t*)&x);
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_Variable(x);
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        check_variable(x.m_m);
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_StructInstanceMember(x);
    }

    void visit_StructConstructor(const ASR::StructConstructor_t &x) {
        std::set<ASR::symbol_t*> visited;
        may_reenter = may_reenter || (in_contained &&
            struct_may_invoke_procedures(x.m_dt_sym, visited));
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_StructConstructor(x);
    }

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        may_reenter = may_reenter || (in_contained && x.n_dtio_bindings > 0);
        check_io_items(x.m_args, x.n_args, nullptr);
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_StringFormat(x);
    }

    // Deallocating a polymorphic pointer finalizes its dynamic type
    void check_deallocated(ASR::expr_t **vars, size_t n_vars) {
        for (size_t i = 0; i < n_vars && in_contained; i++) {
            may_reenter = may_reenter ||
                ASRUtils::is_class_type(ASRUtils::extract_type(ASRUtils::expr_type(vars[i])));
        }
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
        check_deallocated(x.m_vars, x.n_vars);
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_ExplicitDeallocate(x);
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &x) {
        check_deallocated(x.m_vars, x.n_vars);
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_ImplicitDeallocate(x);
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        check_io_items(x.m_values, x.n_values, x.m_nml);
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_FileWrite(x);
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        check_io_items(x.m_values, x.n_values, x.m_nml);
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_FileRead(x);
    }

    bool is_contained(ASR::symbol_t *sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        return ASR::is_a<ASR::Function_t>(*sym) &&
            ASRUtils::symbol_parent_symtab(sym) == host->m_symtab &&
            ASRUtils::get_FunctionType(ASR::down_cast<ASR::Function_t>(sym))->m_deftype ==
                ASR::deftypeType::Implementation;
    }

    void visit_Function(const ASR::Function_t &x) {
        bool in_contained_copy = in_contained;
        in_contained = &x != host;
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_Function(x);
        in_contained = in_contained_copy;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        may_reenter = may_reenter || (in_contained && !is_contained(x.m_name));
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        may_reenter = may_reenter || (in_contained && !is_contained(x.m_name));
        ASR::BaseWalkVisitor<ContainedReentryFinder>::visit_FunctionCall(x);
    }

    static bool is_procedure(ASR::symbol_t *sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        return ASR::is_a<ASR::Function_t>(*sym) || ASRUtils::is_symbol_procedure_variable(sym);
    }

    void visit_Var(const ASR::Var_t &x) {
        may_reenter = may_reenter || is_contained(x.m_v) ||
            (in_contained && is_procedure(x.m_v));
        check_variable(x.m_v);
    }

    bool find(const ASR::Function_t &x) {
        host = &x;
        may_reenter = false;
        visit_Function(x);
        return may_reenter;
    }
};

// Puts the statements made by `make_restores` before every return of a procedure
class ReturnRestorer: public PassUtils::PassVisitor<ReturnRestorer> {
public:
    std::function<std::vector<ASR::stmt_t*>()> make_restores;

    ReturnRestorer(Allocator &al_, std::function<std::vector<ASR::stmt_t*>()> make_restores_):
        PassVisitor(al_, nullptr), make_restores(make_restores_) {
        pass_result.reserve(al, 1);
    }

    void visit_Return(const ASR::Return_t &/*x*/) {
        for (ASR::stmt_t *stmt: make_restores()) {
            pass_result.push_back(al, stmt);
        }
        retain_original_stmt = true;
    }

    // A return in a contained procedure returns from that procedure
    void visit_Function(const ASR::Function_t &/*x*/) { }

    void restore_before_returns(ASR::Function_t &x) {
        current_scope = x.m_symtab;
        transform_stmts(x.m_body, x.n_body);
        for (auto &item: x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                visit_Block(*ASR::down_cast<ASR::Block_t>(item.second));
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                visit_AssociateBlock(*ASR::down_cast<ASR::AssociateBlock_t>(item.second));
            }
        }
    }
};

class AssignNestedVars: public PassUtils::PassVisitor<AssignNestedVars> {
private : 

    /*
    Creates an if block with call to `allocated` intrinsic
    to check if RHS is allocated or not before doing an assignment.
    That's needed with allocatable RHS as the Fortran standard requires

    #### Example:

    ```fortran
        if(allocated(RHS)) then
            LHS = RHS ! assignment_stmt
        else if (allocated(LHS)) then
            deallocate(LHS)
        end if
    ```
    */
    ASR::stmt_t* create_if_allocated_block(ASR::expr_t* RHS, ASR::stmt_t* assignment_stmt) {
        /* Some Assertions */
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Assignment_t>(*assignment_stmt))
        LCOMPILERS_ASSERT(ASR::down_cast<ASR::Assignment_t>(assignment_stmt)->m_value == RHS)

        ASR::expr_t* LHS = ASR::down_cast<ASR::Assignment_t>(assignment_stmt)->m_target;

        /* Create Call To ImpureIntrinsic `allocated(RHS)` */
        ASR::expr_t* allocated_rhs_call {};
        {
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, RHS);
            diag::Diagnostics diag_instance;
            allocated_rhs_call = ASRUtils::EXPR(ASRUtils::Allocated::create_Allocated(al, RHS->base.loc, args, diag_instance));
            if(diag_instance.has_error()) throw diag_instance;
        }
        /* Create If Body: LHS = RHS */
        Vec<ASR::stmt_t*> if_body {};
        {
            if_body.reserve(al, 1);
            if_body.push_back(al, assignment_stmt);
        }

        Vec<ASR::stmt_t*> else_body {};
        else_body.reserve(al, 1);
        if (ASRUtils::is_allocatable(ASRUtils::expr_type(LHS))) {
            ASR::expr_t* allocated_lhs_call {};
            {
                Vec<ASR::expr_t*> args;
                args.reserve(al, 1);
                args.push_back(al, LHS);
                diag::Diagnostics diag_instance;
                allocated_lhs_call = ASRUtils::EXPR(ASRUtils::Allocated::create_Allocated(al, LHS->base.loc, args, diag_instance));
                if(diag_instance.has_error()) throw diag_instance;
            }
            Vec<ASR::expr_t*> dealloc_args;
            dealloc_args.reserve(al, 1);
            dealloc_args.push_back(al, LHS);
            ASR::stmt_t* dealloc_stmt = ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(al,
                LHS->base.loc, dealloc_args.p, dealloc_args.n));

            Vec<ASR::stmt_t*> inner_if_body;
            inner_if_body.reserve(al, 1);
            inner_if_body.push_back(al, dealloc_stmt);
            ASR::stmt_t* inner_if = ASRUtils::STMT(ASR::make_If_t(al, LHS->base.loc,
                nullptr, allocated_lhs_call, inner_if_body.p, inner_if_body.size(),
                nullptr, 0));
            else_body.push_back(al, inner_if);
        }

        return ASRUtils::STMT(ASR::make_If_t(al, RHS->base.loc, nullptr,
            allocated_rhs_call, if_body.p, if_body.size(),
            else_body.p, else_body.size()));
    }

public:
    std::map<ASR::symbol_t*, std::pair<std::string, ASR::symbol_t*>> &nested_var_to_ext_var;
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &nesting_map;
    std::map<ASR::symbol_t*, ASR::symbol_t*> &assumed_length_ctx_var_len;
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &program_procedure_interfaces;
    std::map<ASR::symbol_t*, ASR::symbol_t*> module_var_to_external;

    ASR::symbol_t *cur_func_sym = nullptr;
    bool calls_present = false;
    bool calls_in_loop_condition = false;
    std::set<ASR::symbol_t*> nested_proc_dispatch_hosts;
    struct ContextVarCopy {
        ASR::symbol_t *ctx_var;
        std::string module_name;
        ASR::symbol_t *type_declaration;
        bool associate;
        bool move;
    };
    // Procedure host -> the context variables it syncs, in the order of their first sync
    std::map<ASR::symbol_t*, std::vector<ContextVarCopy>> host_context_vars;
    // Context variable -> the host variable it stands for
    std::map<ASR::symbol_t*, ASR::symbol_t*> ctx_var_to_host_var;

    ASR::stmt_t* copy_context_var(ASR::expr_t *target, ASR::expr_t *value,
            bool associate, bool move) {
        const Location &loc = value->base.loc;
        if (associate) {
            ASR::stmt_t *associate_stmt = ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, loc,
                target, value));
            if (!ASRUtils::is_pointer(ASRUtils::expr_type(value)) ||
                    !ASRUtils::is_pointer(ASRUtils::expr_type(target))) {
                return associate_stmt;
            }
            // The context variable is disassociated until the first host instance syncs it
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *is_associated = ASRUtils::EXPR(ASR::make_PointerAssociated_t(al, loc,
                value, nullptr, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
            Vec<ASR::expr_t*> nullify_args;
            nullify_args.reserve(al, 1);
            nullify_args.push_back(al, target);
            return b.If(is_associated, {associate_stmt},
                {ASRUtils::STMT(ASR::make_Nullify_t(al, loc, nullify_args.p, nullify_args.size()))});
        }
        ASR::ttype_t *target_type = ASRUtils::expr_type(target);
        bool realloc = move || (ASRUtils::is_array(target_type) &&
            ASRUtils::is_allocatable(target_type));
        ASR::stmt_t *assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc,
            target, value, nullptr, realloc, move));
        if (ASRUtils::is_allocatable(ASRUtils::expr_type(value))) {
            return create_if_allocated_block(value, assignment);
        }
        return assignment;
    }

    ASR::symbol_t* create_context_var_copy(SymbolTable *scope, const std::string &prefix,
            ASR::symbol_t *ctx_var, ASR::symbol_t *type_declaration) {
        std::string name = scope->get_unique_name(prefix +
            std::string(ASRUtils::symbol_name(ctx_var)), false);
        ASR::ttype_t *type = ASRUtils::duplicate_type(al, ASRUtils::symbol_type(ctx_var));
        // A local procedure that is not a pointer would declare a procedure
        if (ASR::is_a<ASR::FunctionType_t>(*type)) {
            type = ASRUtils::TYPE(ASR::make_Pointer_t(al, type->base.loc, type));
        }
        ASR::symbol_t *copy = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al,
            ctx_var->base.loc, scope, s2c(al, name), nullptr, 0,
            ASR::intentType::Local, nullptr, nullptr, ASR::storage_typeType::Default,
            type, type_declaration, ASR::abiType::Source, ASR::accessType::Public,
            ASR::presenceType::Required, false));
        scope->add_symbol(name, copy);
        return copy;
    }

    void record_context_var(ASR::symbol_t *ctx_var, const std::string &module_name,
            ASR::symbol_t *type_declaration, bool associate, bool move) {
        if (!ASR::is_a<ASR::Function_t>(*cur_func_sym)) {
            return;
        }
        std::vector<ContextVarCopy> &vars = host_context_vars[cur_func_sym];
        for (auto &v: vars) {
            if (v.ctx_var == ctx_var) {
                return;
            }
        }
        vars.push_back({ctx_var, module_name, type_declaration, associate, move});
    }

    /*
    A procedure can be entered again while a procedure contained in it runs,
    and the new instance syncs its own variables into the shared context
    variables. An instance of a procedure host that is entered while another
    instance of it is active therefore saves the context variables when it
    starts and restores them before it returns, so that after a call that
    entered the host again a contained procedure called directly by the host
    sees the variables of the instance that called it (Fortran 2018,
    15.6.2.4). Within an instance the context variables keep the values of
    the last sync, as a contained procedure called through a procedure
    pointer is not synced. A flag in the context module tells whether an
    instance is active, so a host that is not entered again only sets and
    resets the flag. A host is left unchanged when its contained procedures
    explicitly call only each other, are only called directly, name
    procedures only to call them (an intrinsic such as `reduce` calls a
    procedure it is passed), and invoke no procedure implicitly: they reference no entity that can be
    finalized or assigned by a defined assignment of a component, and
    transfer no derived-type item, which could use defined input/output (see
    ContainedReentryFinder). Such a host cannot be entered again while one of
    them runs. A program cannot be entered again and is left unchanged too.

    ```fortran
        reentered = active
        active = .true.
        if (reentered) saved_x = ctx_x
        ...
        ctx_x = x
        call inner()
        x = ctx_x
        ...
        if (reentered) ctx_x = saved_x
        active = reentered
    ```
    */
    void save_and_restore_context_vars(ASR::Function_t &x) {
        auto it = host_context_vars.find((ASR::symbol_t*)&x);
        // A host defined in another compilation unit is not compiled here
        if (it == host_context_vars.end() ||
                ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::ExternalUndefined) {
            return;
        }
        ContainedReentryFinder reentry_finder;
        if (!reentry_finder.find(x)) {
            return;
        }
        std::vector<ContextVarCopy> &vars = it->second;
        const Location &loc = x.base.base.loc;
        ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
        SymbolTable *module_scope = ASRUtils::symbol_parent_symtab(vars[0].ctx_var);
        std::string active_name = module_scope->get_unique_name(
            "__libasr_active_" + std::string(x.m_name), false);
        ASR::expr_t *inactive = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, false,
            logical_type));
        ASR::symbol_t *active_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, module_scope, s2c(al, active_name), nullptr, 0,
            ASR::intentType::Local, inactive, inactive, ASR::storage_typeType::Default,
            logical_type, nullptr, ASR::abiType::Source, ASR::accessType::Public,
            ASR::presenceType::Required, false));
        module_scope->add_symbol(active_name, active_sym);
        ASR::symbol_t *active_ext = resolve_or_create_external_symbol(al, x.m_symtab,
            active_sym, vars[0].module_name);
        SymbolTable *scope = x.m_symtab;
        std::string reentered_name = scope->get_unique_name("__libasr_reentered", false);
        ASR::symbol_t *reentered_sym = ASR::down_cast<ASR::Var_t>(
            PassUtils::create_auxiliary_variable(loc, reentered_name, al, scope, logical_type,
            ASR::intentType::Local, nullptr, nullptr))->m_v;
        auto var = [&](ASR::symbol_t *sym) {
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        };
        auto assign = [&](ASR::expr_t *target, ASR::expr_t *value) {
            return ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, target, value,
                nullptr, false, false));
        };

        std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> ctx_and_saved;
        Vec<ASR::stmt_t*> saves;
        saves.reserve(al, vars.size());
        for (auto &v: vars) {
            ASR::symbol_t *ext_sym = nullptr;
            auto it_ext = module_var_to_external.find(v.ctx_var);
            if (it_ext != module_var_to_external.end() &&
                    ASRUtils::symbol_parent_symtab(it_ext->second) == x.m_symtab) {
                ext_sym = it_ext->second;
            } else {
                ext_sym = resolve_or_create_external_symbol(al, x.m_symtab, v.ctx_var,
                    v.module_name);
            }
            ASR::symbol_t *saved_sym = create_context_var_copy(x.m_symtab, "__libasr_saved_",
                v.ctx_var, v.type_declaration);
            ctx_and_saved.push_back({ext_sym, saved_sym});
            saves.push_back(al, copy_context_var(var(saved_sym), var(ext_sym),
                v.associate, v.move));
        }
        auto make_restores = [&]() {
            Vec<ASR::stmt_t*> restores;
            restores.reserve(al, vars.size());
            for (size_t i = 0; i < vars.size(); i++) {
                restores.push_back(al, copy_context_var(var(ctx_and_saved[i].first),
                    var(ctx_and_saved[i].second), vars[i].associate, vars[i].move));
            }
            return std::vector<ASR::stmt_t*>{
                ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr, var(reentered_sym),
                    restores.p, restores.size(), nullptr, 0)),
                assign(var(active_ext), var(reentered_sym))};
        };
        ReturnRestorer restorer(al, make_restores);
        restorer.restore_before_returns(x);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body + 5);
        body.push_back(al, assign(var(reentered_sym), var(active_ext)));
        body.push_back(al, assign(var(active_ext), ASRUtils::EXPR(ASR::make_LogicalConstant_t(
            al, loc, true, logical_type))));
        body.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr, var(reentered_sym),
            saves.p, saves.size(), nullptr, 0)));
        for (size_t i = 0; i < x.n_body; i++) {
            body.push_back(al, x.m_body[i]);
        }
        if (x.n_body == 0 || !ASR::is_a<ASR::Return_t>(*x.m_body[x.n_body - 1])) {
            for (ASR::stmt_t *stmt: make_restores()) {
                body.push_back(al, stmt);
            }
        }
        x.m_body = body.p;
        x.n_body = body.size();
    }

    struct ContextArgCopy {
        // The context variable as the statement refers to it
        ASR::symbol_t *ctx_var_ref;
        ASR::symbol_t *copy;
        bool associate;
        bool allocatable_array;
    };

    // Finds the context variables that a call passes to the host that owns their variables
    bool find_context_args(ASR::symbol_t *callee, ASR::call_arg_t *args, size_t n_args,
            std::vector<std::pair<ASR::symbol_t*, ContextArgCopy>> &copies) {
        callee = ASRUtils::symbol_get_past_external(callee);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) {
            callee = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(callee)->m_proc);
        }
        if (!ASR::is_a<ASR::Function_t>(*callee)) {
            return false;
        }
        ASR::Function_t *callee_fn = ASR::down_cast<ASR::Function_t>(callee);
        bool found = false;
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t *arg = args[i].m_value;
            if (arg == nullptr || i >= callee_fn->n_args) {
                continue;
            }
            arg = ASRUtils::get_past_array_physical_cast(arg);
            if (!ASR::is_a<ASR::Var_t>(*arg)) {
                continue;
            }
            ASR::symbol_t *ctx_var_ref = ASR::down_cast<ASR::Var_t>(arg)->m_v;
            ASR::symbol_t *ctx_var = ASRUtils::symbol_get_past_external(ctx_var_ref);
            auto it = ctx_var_to_host_var.find(ctx_var);
            if (it == ctx_var_to_host_var.end() ||
                    ASRUtils::symbol_parent_symtab(it->second)->asr_owner != &callee->base ||
                    !ASR::is_a<ASR::Var_t>(*callee_fn->m_args[i])) {
                continue;
            }
            ASR::symbol_t *dummy = ASR::down_cast<ASR::Var_t>(callee_fn->m_args[i])->m_v;
            ASR::ttype_t *type = ASRUtils::symbol_type(ctx_var);
            // A pointer to a dummy argument with the target attribute stays associated with the actual argument
            if (!ASR::is_a<ASR::Variable_t>(*dummy) ||
                    ASR::down_cast<ASR::Variable_t>(dummy)->m_target_attr ||
                    ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(type))) {
                continue;
            }
            found = true;
            bool copied = false;
            for (auto &c: copies) {
                copied = copied || c.first == ctx_var;
            }
            if (copied) {
                continue;
            }
            bool allocatable_array = ASRUtils::is_array(type) && ASRUtils::is_allocatable(type);
            ASR::symbol_t *copy = create_context_var_copy(current_scope, "__libasr_created_arg_",
                ctx_var, ASR::down_cast<ASR::Variable_t>(it->second)->m_type_declaration);
            copies.push_back({ctx_var, {ctx_var_ref, copy,
                !allocatable_array && (ASRUtils::is_array(type) || ASRUtils::is_pointer(type)),
                allocatable_array}});
        }
        return found;
    }

    /*
    A contained procedure that passes a variable of its host to the host
    itself would pass the context variable, which the new instance of the
    host saves and syncs with its own variables. The statement (or the test
    of an if) uses a copy of the context variable instead, in every reference
    to it, so that the other actual arguments and the target see the same
    object, and the copy is copied back after it (at the start of each branch
    of an if). An allocatable array is moved, unless the statement also calls
    another procedure, which could read the variable by host association.

    ```fortran
        tmp = ctx_x
        call host(tmp, tmp(1))
        ctx_x = tmp
    ```
    */
    void copy_context_args(ASR::stmt_t *&stmt, std::vector<ASR::stmt_t*> &copies_in,
            std::vector<ASR::stmt_t*> &copies_out) {
        FunctionCallCollector collector;
        ASR::If_t *if_stmt = nullptr;
        if (ASR::is_a<ASR::If_t>(*stmt)) {
            if_stmt = ASR::down_cast<ASR::If_t>(stmt);
            collector.visit_expr(*if_stmt->m_test);
        } else if (ASR::is_a<ASR::SubroutineCall_t>(*stmt) || ASR::is_a<ASR::Assignment_t>(*stmt) ||
                ASR::is_a<ASR::Print_t>(*stmt) || ASR::is_a<ASR::FileWrite_t>(*stmt)) {
            collector.visit_stmt(*stmt);
        } else {
            return;
        }
        std::vector<std::pair<ASR::symbol_t*, ContextArgCopy>> copies;
        bool calls_other = false;
        if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
            ASR::SubroutineCall_t *call = ASR::down_cast<ASR::SubroutineCall_t>(stmt);
            calls_other = !find_context_args(call->m_name, call->m_args, call->n_args, copies);
        }
        for (ASR::FunctionCall_t *call: collector.calls) {
            if (!find_context_args(call->m_name, call->m_args, call->n_args, copies)) {
                calls_other = true;
            }
        }
        if (copies.empty()) {
            return;
        }
        HostToContextVarDuplicator duplicator(al);
        for (auto &item: copies) {
            ContextArgCopy &c = item.second;
            duplicator.ctx_vars[item.first] = c.copy;
            const Location &loc = c.copy->base.loc;
            copies_in.push_back(copy_context_var(ASRUtils::EXPR(ASR::make_Var_t(al, loc, c.copy)),
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, c.ctx_var_ref)), c.associate,
                c.allocatable_array && !calls_other));
        }
        auto copy_back = [&](std::vector<ASR::stmt_t*> &stmts) {
            for (auto &item: copies) {
                ContextArgCopy &c = item.second;
                const Location &loc = c.copy->base.loc;
                stmts.push_back(copy_context_var(
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, c.ctx_var_ref)),
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, c.copy)), c.associate,
                    c.allocatable_array));
            }
        };
        if (if_stmt == nullptr) {
            stmt = duplicator.duplicate_stmt(stmt);
            copy_back(copies_out);
            return;
        }
        if_stmt->m_test = duplicator.duplicate_expr(if_stmt->m_test);
        auto prepend_copy_back = [&](ASR::stmt_t **&m_body, size_t &n_body) {
            std::vector<ASR::stmt_t*> stmts;
            copy_back(stmts);
            stmts.insert(stmts.end(), m_body, m_body + n_body);
            Vec<ASR::stmt_t*> body;
            body.from_pointer_n_copy(al, stmts.data(), stmts.size());
            m_body = body.p;
            n_body = body.size();
        };
        prepend_copy_back(if_stmt->m_body, if_stmt->n_body);
        prepend_copy_back(if_stmt->m_orelse, if_stmt->n_orelse);
    }

    static ASR::symbol_t* get_root_host_symbol(ASR::expr_t* expr) {
        expr = ASRUtils::get_past_array_physical_cast(expr);
        while (expr) {
            if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
                expr = ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v;
                expr = ASRUtils::get_past_array_physical_cast(expr);
                continue;
            }
            if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
                expr = ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v;
                expr = ASRUtils::get_past_array_physical_cast(expr);
                continue;
            }
            if (ASR::is_a<ASR::ArraySection_t>(*expr)) {
                expr = ASR::down_cast<ASR::ArraySection_t>(expr)->m_v;
                expr = ASRUtils::get_past_array_physical_cast(expr);
                continue;
            }
            if (ASR::is_a<ASR::StringSection_t>(*expr)) {
                expr = ASR::down_cast<ASR::StringSection_t>(expr)->m_arg;
                expr = ASRUtils::get_past_array_physical_cast(expr);
                continue;
            }
            if (ASR::is_a<ASR::Var_t>(*expr)) {
                return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(expr)->m_v);
            }
            break;
        }
        return nullptr;
    }

    bool value_is_nested_procedure(ASR::expr_t* value) {
        value = ASRUtils::get_past_array_physical_cast(value);
        if (value && ASR::is_a<ASR::Var_t>(*value)) {
            ASR::symbol_t* v = ASR::down_cast<ASR::Var_t>(value)->m_v;
            return is_nested_call_symbol(current_scope, v,
                &program_procedure_interfaces);
        }
        return false;
    }

    void record_nested_dispatch_host(ASR::expr_t* target, ASR::expr_t* value) {
        if (!target || !value || !value_is_nested_procedure(value)) {
            return;
        }
        ASR::symbol_t* host = get_root_host_symbol(target);
        if (host) {
            nested_proc_dispatch_hosts.insert(host);
        }
    }

    void mark_nested_procedure_arg(ASR::expr_t *arg_expr) {
        if (!arg_expr) {
            return;
        }
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_expr)) {
            arg_expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_expr)->m_arg;
        }

        if (ASR::is_a<ASR::Var_t>(*arg_expr)) {
            ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(arg_expr);
            if (is_nested_call_symbol(current_scope, var->m_v,
                    &program_procedure_interfaces) ||
                nested_proc_dispatch_hosts.find(get_root_host_symbol(arg_expr)) != nested_proc_dispatch_hosts.end()) {
                calls_present = true;
            }
        }
    }

    AssignNestedVars(Allocator &al_,
    std::map<ASR::symbol_t*, std::pair<std::string, ASR::symbol_t*>> &nv,
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> &nm,
    std::map<ASR::symbol_t*, ASR::symbol_t*> &al_map,
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &pi_map) :
    PassVisitor(al_, nullptr), nested_var_to_ext_var(nv), nesting_map(nm),
    assumed_length_ctx_var_len(al_map), program_procedure_interfaces(pi_map) {
        for (auto &item: nested_var_to_ext_var) {
            ctx_var_to_host_var[item.second.second] = item.first;
        }
    }

    void visit_Associate(const ASR::Associate_t &x) {
        record_nested_dispatch_host(x.m_target, x.m_value);
        PassUtils::PassVisitor<AssignNestedVars>::visit_Associate(x);
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        record_nested_dispatch_host(x.m_target, x.m_value);
        PassUtils::PassVisitor<AssignNestedVars>::visit_Assignment(x);
    }

    bool calls_nested_procedure(ASR::expr_t *expr) {
        bool calls_present_copy = calls_present;
        calls_present = false;
        visit_expr(*expr);
        std::swap(calls_present, calls_present_copy);
        return calls_present_copy;
    }

    void hoist_loop_bound(ASR::expr_t *&bound, std::vector<ASR::stmt_t*> &stmts) {
        if (bound == nullptr || !calls_nested_procedure(bound)) {
            return;
        }
        std::string name = current_scope->get_unique_name("__libasr_created_loop_bound");
        ASR::stmt_t *assign = nullptr;
        bound = PassUtils::create_auxiliary_variable_for_expr(bound, name, al,
            current_scope, assign);
        stmts.push_back(assign);
    }

    // do while (.true.); tmp = test; if (.not. tmp) exit; body; end do
    void move_test_into_body(ASR::WhileLoop_t &loop) {
        // The exit would skip an else part
        if (loop.n_orelse > 0 || !calls_nested_procedure(loop.m_test)) {
            return;
        }
        Location loc = loop.m_test->base.loc;
        std::string name = current_scope->get_unique_name("__libasr_created_while_test");
        ASR::stmt_t *assign = nullptr;
        ASR::expr_t *test = PassUtils::create_auxiliary_variable_for_expr(loop.m_test, name, al,
            current_scope, assign);
        ASR::ttype_t *logical_type = ASRUtils::expr_type(test);
        Vec<ASR::stmt_t*> exit_body;
        exit_body.reserve(al, 1);
        exit_body.push_back(al, ASRUtils::STMT(ASR::make_Exit_t(al, loc, loop.m_name)));
        Vec<ASR::stmt_t*> body;
        body.reserve(al, loop.n_body + 2);
        body.push_back(al, assign);
        body.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr,
            ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc, test, logical_type, nullptr)),
            exit_body.p, exit_body.size(), nullptr, 0)));
        for (size_t i=0; i<loop.n_body; i++) {
            body.push_back(al, loop.m_body[i]);
        }
        loop.m_body = body.p;
        loop.n_body = body.size();
        loop.m_test = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, true, logical_type));
    }

    // A bound or test of loop `i` that calls a nested procedure gets a statement of its own
    void split_loop_header(ASR::stmt_t **&m_body, size_t &n_body, size_t i) {
        std::vector<ASR::stmt_t*> stmts;
        if (ASR::is_a<ASR::DoLoop_t>(*m_body[i])) {
            ASR::do_loop_head_t &head = ASR::down_cast<ASR::DoLoop_t>(m_body[i])->m_head;
            hoist_loop_bound(head.m_start, stmts);
            hoist_loop_bound(head.m_end, stmts);
            hoist_loop_bound(head.m_increment, stmts);
        } else if (ASR::is_a<ASR::WhileLoop_t>(*m_body[i])) {
            move_test_into_body(*ASR::down_cast<ASR::WhileLoop_t>(m_body[i]));
        }
        if (stmts.empty()) {
            return;
        }
        stmts.insert(stmts.begin(), m_body, m_body + i);
        stmts.insert(stmts.end(), m_body + i, m_body + n_body);
        Vec<ASR::stmt_t*> body;
        body.from_pointer_n_copy(al, stmts.data(), stmts.size());
        m_body = body.p;
        n_body = body.size();
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        std::vector<ASR::stmt_t*> assigns_at_end;
        HostToContextVarDuplicator ctx_duplicator(al);
        for (size_t i=0; i<n_body; i++) {
            if (nesting_map.find(cur_func_sym) != nesting_map.end()) {
                split_loop_header(m_body, n_body, i);
            }
            calls_present = false;
            calls_in_loop_condition = false;
            assigns_at_end.clear();
            ctx_duplicator.ctx_vars.clear();
            ASR::symbol_t *assigned_target_sym = nullptr;
            bool assigns_part = false;
            if (ASR::is_a<ASR::Assignment_t>(*m_body[i])) {
                ASR::Assignment_t *assignment = ASR::down_cast<ASR::Assignment_t>(m_body[i]);
                assigned_target_sym = get_root_host_symbol(assignment->m_target);
                ASR::expr_t *target = ASRUtils::get_past_array_physical_cast(assignment->m_target);
                // The value keeps a part of the target in `z = cmplx(h(), aimag(z))`, made from `z%re = h()`
                assigns_part = !ASR::is_a<ASR::Var_t>(*target) || ASRUtils::expr_references_symbol(
                    assignment->m_value, ASR::down_cast<ASR::Var_t>(target)->m_v);
            }
            std::vector<ASR::stmt_t*> arg_copies_in, arg_copies_out;
            copy_context_args(m_body[i], arg_copies_in, arg_copies_out);
            for (auto &stm: arg_copies_in) {
                body.push_back(al, stm);
            }
            visit_stmt(*m_body[i]);
            if (cur_func_sym != nullptr && (calls_present || calls_in_loop_condition)) {
                if (nesting_map.find(cur_func_sym) != nesting_map.end()) {
                    for (auto &sym: nesting_map[cur_func_sym]) {
                        bool skip_sync_back = ASR::is_a<ASR::Assignment_t>(*m_body[i]) &&
                            assigned_target_sym == ASRUtils::symbol_get_past_external(sym);
                        std::string m_name = nested_var_to_ext_var[sym].first;
                        ASR::symbol_t *t = nested_var_to_ext_var[sym].second;
                        ASR::symbol_t *ext_sym = nullptr;
                        auto it_ext = module_var_to_external.find(t);
                        if (it_ext != module_var_to_external.end() &&
                                is_sym_in_scope_chain(current_scope,ASRUtils::symbol_parent_symtab(it_ext->second))) {
                            ext_sym = it_ext->second;
                        } else {
                            std::string original_name = ASRUtils::symbol_name(t);
                            ASR::symbol_t *existing = current_scope->resolve_symbol(original_name);
                            if (existing != nullptr && ASR::is_a<ASR::ExternalSymbol_t>(*existing) &&
                                    ASRUtils::symbol_get_past_external(existing) == t) {
                                ext_sym = existing;
                            } else {
                                std::string unique_name = original_name;
                                if (existing != nullptr) {
                                    unique_name = current_scope->get_unique_name(original_name + "_nested_ctx", false);
                                }
                                ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                                    al, t->base.loc,
                                    /* a_symtab */ current_scope,
                                    /* a_name */ s2c(al, unique_name),
                                    t,
                                    s2c(al, m_name), nullptr, 0, s2c(al, original_name),
                                    ASR::accessType::Public
                                );
                                ext_sym = ASR::down_cast<ASR::symbol_t>(fn);
                                current_scope->add_symbol(unique_name, ext_sym);
                            }
                            module_var_to_external[t] = ext_sym;
                        }
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
                        SymbolTable *sym_parent = ASRUtils::symbol_parent_symtab(sym_);
                        if (!is_sym_in_scope_chain(current_scope, sym_parent)) {
                            std::string sym_name = ASRUtils::symbol_name(sym_);
                            std::string unique_name = current_scope->get_unique_name(sym_name, false);
                            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
                            ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                                al, t->base.loc,
                                /* a_symtab */ current_scope,
                                /* a_name */ s2c(al, unique_name),
                                s, ASRUtils::symbol_name(ASRUtils::get_asr_owner(s)),
                                nullptr, 0, ASRUtils::symbol_name(s), ASR::accessType::Public
                            );
                            sym_ = ASR::down_cast<ASR::symbol_t>(fn);
                            current_scope->add_symbol(unique_name, sym_);
                        }
                        LCOMPILERS_ASSERT(ext_sym != nullptr);
                        LCOMPILERS_ASSERT(sym_ != nullptr);
                        ASR::expr_t *target = ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, ext_sym));
                        ASR::expr_t *val = ASRUtils::EXPR(ASR::make_Var_t(al, t->base.loc, sym_));
                        bool is_sym_allocatable_or_pointer = (ASRUtils::is_pointer(ASRUtils::symbol_type(sym)) ||
                            ASRUtils::is_allocatable(ASRUtils::symbol_type(sym)));
                        bool is_ext_sym_allocatable_or_pointer = (ASRUtils::is_pointer(ASRUtils::symbol_type(ext_sym)) ||
                            ASRUtils::is_allocatable(ASRUtils::symbol_type(ext_sym)));
                        bool is_procedure_variable = ASR::is_a<ASR::Variable_t>(*sym_) &&
                            ASR::down_cast<ASR::Variable_t>(sym_)->m_type_declaration &&
                            ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external
                            (ASR::down_cast<ASR::Variable_t>(sym_)->m_type_declaration));
                        if (sym_ == sym) {
                            bool by_reference = ASRUtils::is_array(ASRUtils::symbol_type(sym)) ||
                                ASRUtils::is_pointer(ASRUtils::symbol_type(ext_sym));
                            bool moves = by_reference && ASRUtils::is_allocatable(ASRUtils::symbol_type(sym)) &&
                                ASRUtils::is_allocatable(ASRUtils::symbol_type(ext_sym));
                            record_context_var(t, m_name, ASR::down_cast<ASR::Variable_t>(
                                ASRUtils::symbol_get_past_external(sym))->m_type_declaration,
                                (by_reference && !moves) || (!by_reference && is_procedure_variable),
                                moves);
                        }
                        if( ASRUtils::is_array(ASRUtils::symbol_type(sym)) || ASRUtils::is_pointer(ASRUtils::symbol_type(ext_sym)) ) {
                            if( ASRUtils::is_allocatable(ASRUtils::symbol_type(sym)) && ASRUtils::is_allocatable(ASRUtils::symbol_type(ext_sym)) ) {
                                auto it_len = assumed_length_ctx_var_len.find(t);
                                if (it_len != assumed_length_ctx_var_len.end()) {
                                    ASR::symbol_t* len_sym_target = it_len->second;
                                    ASR::symbol_t* len_ext_sym = resolve_or_create_external_symbol(
                                        al, current_scope, len_sym_target, m_name);
                                    ASR::expr_t* len_target = ASRUtils::EXPR(
                                        ASR::make_Var_t(al, t->base.loc, len_ext_sym));
                                    ASR::ttype_t* int8_ty = ASRUtils::TYPE(
                                        ASR::make_Integer_t(al, t->base.loc, 8));
                                    ASR::expr_t* len_call = ASRUtils::EXPR(
                                        ASR::make_StringLen_t(al, t->base.loc, val,
                                            int8_ty, nullptr));
                                    ASR::stmt_t* len_assign = ASRUtils::STMT(
                                        ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                            len_target, len_call, nullptr, false, false));
                                    body.push_back(al, len_assign);
                                    if (sym_ == sym) {
                                        record_context_var(len_sym_target, m_name, nullptr,
                                            false, false);
                                    }
                                }
                                // For allocatable arrays, use Assignment instead of Associate
                                // to properly handle reallocation in nested functions
                                ASR::stmt_t *assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                                            target, val, nullptr, true, true));
                                body.push_back(al, create_if_allocated_block(val, assignment));
                                ctx_duplicator.ctx_vars[ASRUtils::symbol_get_past_external(sym)] = ext_sym;
                                if( ASRUtils::EXPR2VAR(val)->m_storage != ASR::storage_typeType::Parameter &&
                                        ASRUtils::EXPR2VAR(val)->m_intent != ASR::intentType::In) {
                                    assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                        val, target, nullptr, true, true));
                                    assigns_at_end.push_back(create_if_allocated_block(target, assignment));
                                }
                            } else {
                                ASR::stmt_t *associate = ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, t->base.loc,
                                                            target, val));
                                body.push_back(al, associate);
                                if((ASRUtils::is_array(ASRUtils::symbol_type(sym)) ||
                                    is_sym_allocatable_or_pointer)
                                    && is_ext_sym_allocatable_or_pointer && is_sym_allocatable_or_pointer
                                    && ASRUtils::EXPR2VAR(val)->m_storage != ASR::storage_typeType::Parameter
                                    && ASRUtils::EXPR2VAR(val)->m_intent != ASR::intentType::In
                                    && !skip_sync_back) {
                                    associate = ASRUtils::STMT(ASRUtils::make_Associate_t_util(al, t->base.loc,
                                        val, target));
                                    assigns_at_end.push_back(associate);
                                }
                            }
                        } else if (is_procedure_variable) {
                            body.push_back(al, ASRUtils::STMT(ASR::make_Associate_t(al, t->base.loc, target, val, ASR::pointer_bounds_formType::NoPointerBounds)));
                        } else {
                            ASR::stmt_t *assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                                        target, val, nullptr, false, false));
                            /* Allocatable RHS Needs A Check `IF allocated --> assign` (Based On Fortran Standards) */
                            if(ASRUtils::is_allocatable(ASRUtils::expr_type(val))){
                                body.push_back(al, create_if_allocated_block(val, assignment));
                            } else { // Otherwise Assign Directly.
                                body.push_back(al, assignment);
                            }
                            // Assign the part in the context variable, so that copying it back keeps what the call wrote
                            bool assigns_part_of_sym = skip_sync_back && assigns_part;
                            if (ASRUtils::EXPR2VAR(val)->m_storage != ASR::storage_typeType::Parameter &&
                                    ASRUtils::EXPR2VAR(val)->m_intent != ASR::intentType::In &&
                                    (!skip_sync_back || assigns_part_of_sym)) {
                                assignment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, t->base.loc,
                                                val, target, nullptr, false, false));
                                /* Allocatable RHS Needs A Check `IF allocated --> assign` (Based On Fortran Standards) */
                                if(ASRUtils::is_allocatable(ASRUtils::expr_type(target))){
                                    assigns_at_end.push_back(create_if_allocated_block(target, assignment));
                                } else { // Otherwise Assign Directly.
                                    assigns_at_end.push_back(assignment);
                                }
                                // A later statement of the where construct must see what the call wrote
                                if (ASR::is_a<ASR::Where_t>(*m_body[i]) || assigns_part_of_sym) {
                                    ctx_duplicator.ctx_vars[ASRUtils::symbol_get_past_external(sym)] = ext_sym;
                                }
                            }
                        }
                    }
                }
            }

            // A moved array is unallocated in the host until it is moved back
            if (!ctx_duplicator.ctx_vars.empty()) {
                if (ASR::is_a<ASR::If_t>(*m_body[i])) {
                    ASR::If_t *if_stmt = ASR::down_cast<ASR::If_t>(m_body[i]);
                    if_stmt->m_test = ctx_duplicator.duplicate_expr(if_stmt->m_test);
                } else {
                    m_body[i] = ctx_duplicator.duplicate_stmt(m_body[i]);
                }
            }

            if (calls_in_loop_condition && ASR::is_a<ASR::If_t>(*m_body[i])) {
                // Handling for IF condition having function calls
                // Handle assignments from function temporaries to main
                ASR::If_t* if_stmt = ASR::down_cast<ASR::If_t>(m_body[i]);
                Vec<ASR::stmt_t*> new_body;
                new_body.reserve(al, if_stmt->n_body + assigns_at_end.size());
                for (auto &stm: assigns_at_end) {
                    new_body.push_back(al, stm);
                }
                // Original if body
                for (size_t j = 0; j < if_stmt->n_body; j++) {
                    new_body.push_back(al, if_stmt->m_body[j]);
                }
                if_stmt->m_body = new_body.p;
                if_stmt->n_body = new_body.size();

                // Also handle reassingment in else block if cond is false
                Vec<ASR::stmt_t*> else_new_body;
                else_new_body.reserve(al, if_stmt->n_orelse + assigns_at_end.size());
                for (auto &stm: assigns_at_end) {
                    else_new_body.push_back(al, stm);
                }
                // Original else body
                for (size_t j = 0; j < if_stmt->n_orelse; j++) {
                    else_new_body.push_back(al, if_stmt->m_orelse[j]);
                }
                if_stmt->m_orelse = else_new_body.p;
                if_stmt->n_orelse = else_new_body.size();

                new_body.n = 0;
                new_body.p = nullptr;
                else_new_body.n = 0;
                else_new_body.p = nullptr;
                body.push_back(al, m_body[i]);
            } else {
                // Append loop, then syncs after loop
                body.push_back(al, m_body[i]);
                for (auto &stm: assigns_at_end) {
                    body.push_back(al, stm);
                }
            }
            for (auto &stm: arg_copies_out) {
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
        std::set<ASR::symbol_t*> nested_proc_dispatch_hosts_copy = nested_proc_dispatch_hosts;
        nested_proc_dispatch_hosts.clear();
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
        save_and_restore_context_vars(xx);
        cur_func_sym = sym_copy;
        current_scope = current_scope_copy;
        nested_proc_dispatch_hosts = nested_proc_dispatch_hosts_copy;
    }

    void visit_Program(const ASR::Program_t &x) {
        ASR::Program_t &xx = const_cast<ASR::Program_t&>(x);
        SymbolTable* current_scope_copy = current_scope;
        current_scope = xx.m_symtab;
        ASR::symbol_t *sym_copy = cur_func_sym;
        std::set<ASR::symbol_t*> nested_proc_dispatch_hosts_copy = nested_proc_dispatch_hosts;
        nested_proc_dispatch_hosts.clear();
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
        nested_proc_dispatch_hosts = nested_proc_dispatch_hosts_copy;
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
        bool is_nested_dispatch_call = false;
        ASR::symbol_t* call_sym = ASRUtils::symbol_get_past_external(x.m_name);
        if (nested_proc_dispatch_hosts.find(call_sym) != nested_proc_dispatch_hosts.end()) {
            is_nested_dispatch_call = true;
        }
        if (x.m_dt) {
            ASR::symbol_t* dt_host = get_root_host_symbol(x.m_dt);
            if (dt_host && nested_proc_dispatch_hosts.find(dt_host) != nested_proc_dispatch_hosts.end()) {
                is_nested_dispatch_call = true;
            }
        }
        calls_present = calls_present || is_nested_call_symbol(current_scope, x.m_name,
            &program_procedure_interfaces)
            || is_nested_dispatch_call;
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
        bool is_nested_dispatch_call = false;
        ASR::symbol_t* call_sym = ASRUtils::symbol_get_past_external(x.m_name);
        if (nested_proc_dispatch_hosts.find(call_sym) != nested_proc_dispatch_hosts.end()) {
            is_nested_dispatch_call = true;
        }
        if (x.m_dt) {
            ASR::symbol_t* dt_host = get_root_host_symbol(x.m_dt);
            if (dt_host && nested_proc_dispatch_hosts.find(dt_host) != nested_proc_dispatch_hosts.end()) {
                is_nested_dispatch_call = true;
            }
        }
        calls_present = calls_present || is_nested_call_symbol(current_scope, x.m_name,
            &program_procedure_interfaces)
            || is_nested_dispatch_call;
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

    // A where construct is synced as one statement: its body holds only masked assignments
    void visit_Where(const ASR::Where_t &x) {
        visit_expr(*x.m_test);
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }
        for (size_t i = 0; i < x.n_orelse; i++) {
            visit_stmt(*x.m_orelse[i]);
        }
    }

    void visit_If(const ASR::If_t &x) {
        // Step 1: Detect calls in condition
        calls_present = false;
        visit_expr(*x.m_test);
        bool has_calls_in_condition = calls_present;
        // Step 2: Visit body to detect calls there
        calls_present = false;
        // Now calls_present = true if body has calls
        PassUtils::PassVisitor<AssignNestedVars>::visit_If(x);
        // Step 3: Set the condition flag (transform_stmts may 
        // reset the flag for calls_in_loop_condition while visiting body)
        calls_in_loop_condition = has_calls_in_condition;
    }
};

/*
A derived type declared in a program and used by a contained procedure is
moved into the context module, and the program imports it back with an
ExternalSymbol. This visitor rewrites every remaining reference to the moved
(and now erased) type or to its components, so that each resolves in its own
scope: variable declarations of contained procedures, structure constructors
and constants, type guards and casts, allocation type specs and component
accesses.
*/
class ReplaceMovedProgramStructs:
    public ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs> {
private:
    Allocator &al;
    // moved type -> its copy in the context module
    std::map<ASR::symbol_t*, ASR::symbol_t*> moved;

public:
    ReplaceMovedProgramStructs(Allocator &al_,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &moved_program_structs)
            : al(al_) {
        for (auto &it: moved_program_structs) {
            moved[it.first] = ASRUtils::symbol_get_past_external(it.second);
        }
    }

    // The copy of a moved type, as seen from `scope`
    ASR::symbol_t *replace_type(ASR::symbol_t *sym, SymbolTable *scope) {
        if (sym == nullptr) return sym;
        auto it = moved.find(sym);
        if (it == moved.end()) return sym;
        ASR::symbol_t *new_struct = it->second;
        std::string name = ASRUtils::symbol_name(new_struct);
        ASR::symbol_t *visible = scope->resolve_symbol(name);
        if (visible != nullptr &&
                ASRUtils::symbol_get_past_external(visible) == new_struct) {
            return visible;
        }
        std::string unique_name = name;
        if (visible != nullptr) {
            unique_name = scope->get_unique_name(name, false);
        }
        return make_external_symbol(al, scope, new_struct, unique_name,
            ASRUtils::symbol_name(ASRUtils::get_asr_owner(new_struct)), name,
            ASR::accessType::Public);
    }

    // A component of a moved type -> the same component of its copy
    ASR::symbol_t *replace_component(ASR::symbol_t *sym) {
        ASR::asr_t *owner = ASRUtils::symbol_parent_symtab(sym)->asr_owner;
        if (owner == nullptr || !ASR::is_a<ASR::symbol_t>(*owner)) return sym;
        auto it = moved.find(ASR::down_cast<ASR::symbol_t>(owner));
        if (it == moved.end()) return sym;
        ASR::symbol_t *component = ASR::down_cast<ASR::Struct_t>(it->second)
            ->m_symtab->get_symbol(ASRUtils::symbol_name(sym));
        return component != nullptr ? component : sym;
    }

    void visit_ExternalSymbol(const ASR::ExternalSymbol_t &x) {
        ASR::ExternalSymbol_t &xx = const_cast<ASR::ExternalSymbol_t&>(x);
        xx.m_external = replace_component(xx.m_external);
    }

    void visit_Struct(const ASR::Struct_t &x) {
        ASR::Struct_t &xx = const_cast<ASR::Struct_t&>(x);
        xx.m_parent = replace_type(xx.m_parent, xx.m_symtab->parent);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_Struct(x);
    }

    void visit_Variable(const ASR::Variable_t &x) {
        ASR::Variable_t &xx = const_cast<ASR::Variable_t&>(x);
        xx.m_type_declaration = replace_type(xx.m_type_declaration,
            xx.m_parent_symtab);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_Variable(x);
    }

    // A type named by an expression, such as the destination type of a
    // class-to-type cast in a type guard
    void visit_Var(const ASR::Var_t &x) {
        ASR::Var_t &xx = const_cast<ASR::Var_t&>(x);
        xx.m_v = replace_type(xx.m_v, current_scope);
    }

    void visit_StructConstructor(const ASR::StructConstructor_t &x) {
        ASR::StructConstructor_t &xx = const_cast<ASR::StructConstructor_t&>(x);
        xx.m_dt_sym = replace_type(xx.m_dt_sym, current_scope);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_StructConstructor(x);
    }

    void visit_StructConstant(const ASR::StructConstant_t &x) {
        ASR::StructConstant_t &xx = const_cast<ASR::StructConstant_t&>(x);
        xx.m_dt_sym = replace_type(xx.m_dt_sym, current_scope);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_StructConstant(x);
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        ASR::StructInstanceMember_t &xx = const_cast<ASR::StructInstanceMember_t&>(x);
        xx.m_m = replace_component(xx.m_m);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_StructInstanceMember(x);
    }

    void visit_alloc_arg(const ASR::alloc_arg_t &x) {
        ASR::alloc_arg_t &xx = const_cast<ASR::alloc_arg_t&>(x);
        xx.m_sym_subclass = replace_type(xx.m_sym_subclass, current_scope);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_alloc_arg(x);
    }

    void visit_dtio_binding(const ASR::dtio_binding_t &x) {
        ASR::dtio_binding_t &xx = const_cast<ASR::dtio_binding_t&>(x);
        xx.m_type_declaration = replace_type(xx.m_type_declaration, current_scope);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_dtio_binding(x);
    }

    void visit_TypeStmtName(const ASR::TypeStmtName_t &x) {
        ASR::TypeStmtName_t &xx = const_cast<ASR::TypeStmtName_t&>(x);
        xx.m_sym = replace_type(xx.m_sym, current_scope);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_TypeStmtName(x);
    }

    void visit_ClassStmt(const ASR::ClassStmt_t &x) {
        ASR::ClassStmt_t &xx = const_cast<ASR::ClassStmt_t&>(x);
        xx.m_sym = replace_type(xx.m_sym, current_scope);
        ASR::ASRPassBaseWalkVisitor<ReplaceMovedProgramStructs>::visit_ClassStmt(x);
    }
};

void pass_nested_vars(Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    NestedVarVisitor v(al);
    v.visit_TranslationUnit(unit);
    ReplaceNestedVisitor w(al, v.nesting_map);
    w.create_context_modules(unit);
    // Re-point references to the moved program types before replacing the
    // captured variables, so that a context variable passed to a contained
    // procedure has the same type as its dummy argument.
    if (!w.moved_program_structs.empty()) {
        ReplaceMovedProgramStructs r(al, w.moved_program_structs);
        r.visit_TranslationUnit(unit);
    }
    w.visit_TranslationUnit(unit);
    AssignNestedVars z(al, w.nested_var_to_ext_var, w.nesting_map,
        w.assumed_length_ctx_var_len, w.program_procedure_interfaces);
    z.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor x(al);
    x.visit_TranslationUnit(unit);
}


} // namespace LCompilers
