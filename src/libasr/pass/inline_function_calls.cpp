#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/inline_function_calls.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/scoped_inlining.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/symbol_expr_substitution.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <set>
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

    // Whether every lower bound of the array type `type` is one: stated
    // as one, or not stated by an array that is neither allocatable nor a
    // pointer.
    static bool lower_bounds_are_one(ASR::ttype_t* type) {
        if( ASRUtils::is_allocatable(type) || ASRUtils::is_pointer(type) ) {
            return false;
        }
        ASR::dimension_t* dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, dims);
        for( size_t d = 0; d < n_dims; d++ ) {
            int64_t start = 1;
            if( dims[d].m_start != nullptr &&
                !(ASRUtils::extract_value(ASRUtils::expr_value(dims[d].m_start),
                    start) && start == 1) ) {
                return false;
            }
        }
        return true;
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

        // Inlining not possible as target dummy is copied into a local variable
        // before the inlined body runs.
        for( size_t i = 0; i < function->n_args; i++ ) {
            if( ASR::is_a<ASR::Var_t>(*function->m_args[i]) ) {
                ASR::symbol_t* sym =
                    ASR::down_cast<ASR::Var_t>(function->m_args[i])->m_v;
                if( ASR::is_a<ASR::Variable_t>(*sym) &&
                    ASR::down_cast<ASR::Variable_t>(sym)->m_target_attr ) {
                    return false;
                }
            }
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
            // A raw pointer to an array element is sequence associated with
            // the dummy, which a pointer association cannot express.
            if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*func_call->m_args[i].m_value) ||
                ASR::is_a<ASR::GetPointer_t>(*func_call->m_args[i].m_value) ||
                ASRUtils::is_pointer(ASRUtils::expr_type(func_call->m_args[i].m_value)) ||
                ASRUtils::is_allocatable(ASRUtils::expr_type(func_call->m_args[i].m_value)) ) {
                return false;
            }
            // An array dummy is bound to its actual by pointer association,
            // which gives it the bounds of the actual, while a dummy that
            // is neither allocatable nor a pointer has the lower bounds it
            // declares. The two agree when both are one.
            ASR::ttype_t* dummy_type = ASRUtils::expr_type(function->m_args[i]);
            if( ASRUtils::is_array(dummy_type) &&
                !(lower_bounds_are_one(dummy_type) &&
                  (ASR::is_a<ASR::ArraySection_t>(*func_call->m_args[i].m_value) ||
                   lower_bounds_are_one(ASRUtils::expr_type(
                        func_call->m_args[i].m_value)))) ) {
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

        // The variables of the host that an internal procedure refers to
        // are bound when it is entered, through its frame dummy argument.
        for( auto& item: function->m_symtab->get_scope() ) {
            if( ASR::is_a<ASR::Variable_t>(*item.second) &&
                    ASRUtils::is_host_associated(
                        *ASR::down_cast<ASR::Variable_t>(item.second)) ) {
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

    // See InlineFunctionCallsVisitor::visit_FunctionType.
    void replace_FunctionType(ASR::FunctionType_t* /*x*/) {
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

    // A procedure type is never evaluated where it appears and must stay
    // free of scoped references, so calls in it (e.g. in the bounds of a
    // dummy argument) are not inlined.
    void visit_FunctionType(const ASR::FunctionType_t& /*x*/) {
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        ASR::WhileLoop_t& xx = const_cast<ASR::WhileLoop_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
    }

    // The bounds and the step of a DO loop are evaluated once, before it
    // runs: a call in them is inlined before the loop, into the statement
    // list that contains it. Before the body, since an expression of the
    // head can be shared with the body, as the target `t(g(i))%v` of an
    // assignment that array_op made a loop over its elements is: the
    // body then reads the result too, instead of reading a result that is
    // computed only inside the body after the head has read it.
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        ASR::DoLoop_t& xx = const_cast<ASR::DoLoop_t&>(x);
        ASR::expr_t** current_expr_copy = current_expr;
        for (ASR::expr_t** e : {&xx.m_head.m_start, &xx.m_head.m_end,
                &xx.m_head.m_increment}) {
            if (*e != nullptr && current_body != nullptr) {
                current_expr = e;
                call_replacer();
            }
        }
        current_expr = current_expr_copy;
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


/*
Inlining of calls of small subroutines in loops.

A call in a loop of a small subroutine whose body the compiler sees, an
internal procedure of the caller or a procedure of the caller's module,

    do i = 1, n
        call step()         ! c = a + b, with host associated c, a, b
    end do

becomes the statements of the subroutine, with the names of its dummy
arguments replaced by its actual arguments:

    do i = 1, n
        c = a + b
    end do

The statements are then statements of the loop, which the passes below
treat as such: with reallocation on assignment, hoist_realloc can move the
reallocation of `c` out of the loop, which it cannot do across a call.

Only what the inlined statements do exactly as the call does is inlined:

* the subroutine calls no procedure (so it is not recursive), has no
  nested scopes, labels, named loops or alternate returns, returns only at
  its end and has at most a few statements;
* a dummy argument that is allocatable stands for its actual argument, a
  whole variable of the same type, rank and kind, which it is; a scalar
  dummy argument stands for its actual argument when that is a whole
  variable, and an INTENT(IN) or VALUE one for a copy of any other actual
  argument; an assumed-shape dummy argument stands for a whole array of the
  same type, rank and kind with a descriptor, when the subroutine does not
  depend on its lower bounds (it reads only its whole value, elementally,
  and its size) and does not change it. Optional, pointer and TARGET dummy
  arguments, derived types, characters and procedures are not inlined;
* a local variable that is a scalar or an array of constant size becomes a
  local variable of the caller, which is undefined on entry just the same;
  a local variable with the SAVE attribute moves to the scope that contains
  the subroutine, the module or the host, renamed, so that the subroutine
  and every inlined copy of it share it, as the calls share it. Local
  allocatable, pointer and automatic arrays are not inlined.
*/
namespace {

class LoopSubroutineInliner {
public:
    explicit LoopSubroutineInliner(Allocator &al_) : al(al_) {}

    // Inlines the calls in the loops of the procedure or program whose
    // symbol table is `scope` and body `body`.
    void procedure(SymbolTable *scope, ASR::asr_t *owner, ASR::stmt_t **&body,
            size_t &n_body) {
        caller_scope = scope;
        caller = owner;
        rewrite(body, n_body, 0);
        caller_scope = nullptr;
        caller = nullptr;
    }

private:
    Allocator &al;
    SymbolTable *caller_scope = nullptr;
    ASR::asr_t *caller = nullptr;
    static constexpr size_t max_statements = 32;

    void rewrite(ASR::stmt_t **&body, size_t &n_body, int loops) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    rewrite(x->m_body, x->n_body, loops + 1);
                    rewrite(x->m_orelse, x->n_orelse, loops);
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    rewrite(x->m_body, x->n_body, loops + 1);
                    rewrite(x->m_orelse, x->n_orelse, loops);
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    rewrite(x->m_body, x->n_body, loops);
                    rewrite(x->m_orelse, x->n_orelse, loops);
                    break;
                }
                case ASR::stmtType::SubroutineCall: {
                    if (loops > 0 && inline_call(
                            *ASR::down_cast<ASR::SubroutineCall_t>(s), out)) {
                        continue;
                    }
                    break;
                }
                default:
                    break;
            }
            out.push_back(al, s);
        }
        body = out.p;
        n_body = out.size();
    }

    // Whether `f` can be seen from the caller: an internal procedure of it,
    // or a procedure of the module that contains it.
    bool visible(const ASR::Function_t &f) {
        SymbolTable *parent = f.m_symtab->parent;
        if (parent == caller_scope) return true;
        if (parent != caller_scope->parent || parent->asr_owner == nullptr ||
                !ASR::is_a<ASR::symbol_t>(*parent->asr_owner)) {
            return false;
        }
        return ASR::is_a<ASR::Module_t>(
            *ASR::down_cast<ASR::symbol_t>(parent->asr_owner));
    }

    static bool simple_type(ASR::ttype_t *type) {
        ASR::ttype_t *element = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(type));
        return ASR::is_a<ASR::Integer_t>(*element) ||
            ASR::is_a<ASR::UnsignedInteger_t>(*element) ||
            ASR::is_a<ASR::Real_t>(*element) ||
            ASR::is_a<ASR::Complex_t>(*element) ||
            ASR::is_a<ASR::Logical_t>(*element);
    }

    static bool same_type(ASR::ttype_t *a, ASR::ttype_t *b) {
        ASR::ttype_t *ea = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(a));
        ASR::ttype_t *eb = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(b));
        return ea->type == eb->type &&
            ASRUtils::extract_kind_from_ttype_t(ea) ==
                ASRUtils::extract_kind_from_ttype_t(eb) &&
            ASRUtils::extract_n_dims_from_ttype(a) ==
                ASRUtils::extract_n_dims_from_ttype(b);
    }

    static ASR::Variable_t* variable(ASR::expr_t *e) {
        if (!ASR::is_a<ASR::Var_t>(*e)) return nullptr;
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        return ASR::is_a<ASR::Variable_t>(*s)
            ? ASR::down_cast<ASR::Variable_t>(s) : nullptr;
    }

    // The statements of the body of a subroutine that can be inlined, and
    // how many there are, nested ones included.
    class BodyCheck : public ASR::BaseWalkVisitor<BodyCheck> {
    public:
        bool ok = true;
        size_t count = 0;

        void visit_stmt(const ASR::stmt_t &x) {
            count++;
            switch (x.type) {
                case ASR::stmtType::Assignment:
                case ASR::stmtType::If:
                case ASR::stmtType::Allocate:
                case ASR::stmtType::ExplicitDeallocate:
                case ASR::stmtType::Print:
                case ASR::stmtType::Exit:
                case ASR::stmtType::Cycle:
                case ASR::stmtType::Stop:
                case ASR::stmtType::ErrorStop:
                    break;
                case ASR::stmtType::DoLoop:
                    if (ASR::down_cast<ASR::DoLoop_t>(&x)->m_name != nullptr) {
                        ok = false;
                    }
                    break;
                case ASR::stmtType::WhileLoop:
                    if (ASR::down_cast<ASR::WhileLoop_t>(&x)->m_name != nullptr) {
                        ok = false;
                    }
                    break;
                default:
                    ok = false;
                    return;
            }
            ASR::BaseWalkVisitor<BodyCheck>::visit_stmt(x);
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            if (x.m_overloaded != nullptr || x.m_move_allocation) ok = false;
            ASR::BaseWalkVisitor<BodyCheck>::visit_Assignment(x);
        }

        void visit_Allocate(const ASR::Allocate_t &x) {
            if (x.m_source != nullptr) ok = false;
            ASR::BaseWalkVisitor<BodyCheck>::visit_Allocate(x);
        }

        void visit_FunctionCall(const ASR::FunctionCall_t &/*x*/) { ok = false; }
        void visit_IntrinsicImpureFunction(
                const ASR::IntrinsicImpureFunction_t &x) {
            if (x.m_impure_intrinsic_id != static_cast<int64_t>(
                    ASRUtils::IntrinsicImpureFunctions::Allocated)) {
                ok = false;
            }
            ASR::BaseWalkVisitor<BodyCheck>::visit_IntrinsicImpureFunction(x);
        }
    };

    // Whether the subroutine uses the assumed-shape dummy argument `d` only
    // in ways that do not depend on its lower bounds and do not change it:
    // as a whole in elemental expressions and in inquiries about its size.
    class ShapeOnlyUse : public ASR::BaseWalkVisitor<ShapeOnlyUse> {
    public:
        ASR::symbol_t *d;
        bool ok = true;
        explicit ShapeOnlyUse(ASR::symbol_t *d_) : d(d_) {}

        bool names(ASR::expr_t *e) {
            e = ASRUtils::get_past_array_physical_cast(e);
            return ASR::is_a<ASR::Var_t>(*e) && ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v) == d;
        }
        void visit_ArrayItem(const ASR::ArrayItem_t &x) {
            if (names(x.m_v)) ok = false;
            ASR::BaseWalkVisitor<ShapeOnlyUse>::visit_ArrayItem(x);
        }
        void visit_ArraySection(const ASR::ArraySection_t &x) {
            if (names(x.m_v)) ok = false;
            ASR::BaseWalkVisitor<ShapeOnlyUse>::visit_ArraySection(x);
        }
        void visit_ArrayBound(const ASR::ArrayBound_t &x) {
            if (names(x.m_v)) ok = false;
            ASR::BaseWalkVisitor<ShapeOnlyUse>::visit_ArrayBound(x);
        }
        void visit_IntrinsicArrayFunction(
                const ASR::IntrinsicArrayFunction_t &x) {
            for (size_t i = 0; i < x.n_args; i++) {
                if (x.m_args[i] != nullptr) {
                    ShapeOnlyUse inner(d);
                    inner.visit_expr(*x.m_args[i]);
                    // Any use inside, `maxloc(d)` returns a subscript.
                    class Mentions : public ASR::BaseWalkVisitor<Mentions> {
                    public:
                        ASR::symbol_t *d;
                        bool found = false;
                        explicit Mentions(ASR::symbol_t *d_) : d(d_) {}
                        void visit_Var(const ASR::Var_t &v) {
                            if (ASRUtils::symbol_get_past_external(v.m_v) == d) {
                                found = true;
                            }
                        }
                    };
                    Mentions m(d);
                    m.visit_expr(*x.m_args[i]);
                    if (m.found) ok = false;
                }
            }
        }
        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::expr_t *t = x.m_target;
            while (true) {
                t = ASRUtils::get_past_array_physical_cast(t);
                if (ASR::is_a<ASR::ArrayItem_t>(*t)) {
                    t = ASR::down_cast<ASR::ArrayItem_t>(t)->m_v;
                } else if (ASR::is_a<ASR::ArraySection_t>(*t)) {
                    t = ASR::down_cast<ASR::ArraySection_t>(t)->m_v;
                } else {
                    break;
                }
            }
            // `w = d` gives an allocatable `w` the bounds of `d`.
            if (names(t) || names(x.m_value)) ok = false;
            ASR::BaseWalkVisitor<ShapeOnlyUse>::visit_Assignment(x);
        }
        void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
            for (size_t i = 0; i < x.n_vars; i++) {
                if (names(x.m_vars[i])) ok = false;
            }
        }
    };

    ASR::expr_t* new_local(const Location &loc, const std::string &name,
            ASR::ttype_t *type) {
        std::string unique = PassUtils::inline_local_name(caller_scope,
            "__inlined_" + name);
        ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, caller_scope,
                s2c(al, unique), nullptr, 0, ASR::intentType::Local, nullptr,
                nullptr, ASR::storage_typeType::Default,
                ASRUtils::duplicate_type(al, type), nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        caller_scope->add_symbol(unique, v);
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, v));
    }

    bool inline_call(const ASR::SubroutineCall_t &call, Vec<ASR::stmt_t*> &out) {
        if (call.m_dt != nullptr ||
                ASR::is_a<ASR::ExternalSymbol_t>(*call.m_name) ||
                !ASR::is_a<ASR::Function_t>(*call.m_name)) {
            return false;
        }
        ASR::Function_t &f = *ASR::down_cast<ASR::Function_t>(call.m_name);
        ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(f);
        if (ft->m_abi != ASR::abiType::Source ||
                ft->m_deftype != ASR::deftypeType::Implementation ||
                ft->m_elemental || f.m_return_var != nullptr ||
                f.n_body == 0 || f.m_gpu != nullptr ||
                ASRUtils::runs_on_device(f) ||
                &f.base.base == caller || !visible(f) ||
                f.n_args != call.n_args) {
            return false;
        }
        BodyCheck check;
        for (size_t i = 0; i < f.n_body; i++) {
            if (i + 1 == f.n_body && ASR::is_a<ASR::Return_t>(*f.m_body[i])) {
                break;
            }
            check.visit_stmt(*f.m_body[i]);
        }
        if (!check.ok || check.count > max_statements) return false;

        const Location &loc = call.base.base.loc;
        std::set<ASR::symbol_t*> dummies;
        std::map<ASR::symbol_t*, ASR::expr_t*> substitutions;
        // The dummy arguments that stand for copies of their actual
        // arguments, created once everything else is known to be inlined.
        std::vector<std::pair<ASR::Variable_t*, ASR::expr_t*>> copied;
        for (size_t i = 0; i < f.n_args; i++) {
            ASR::Variable_t *d = variable(f.m_args[i]);
            ASR::expr_t *actual = call.m_args[i].m_value;
            if (d == nullptr || actual == nullptr ||
                    d->m_presence == ASR::presenceType::Optional ||
                    d->m_target_attr || d->m_is_volatile ||
                    d->m_is_asynchronous || d->n_codims != 0 ||
                    ASRUtils::is_pointer(d->m_type) ||
                    !simple_type(d->m_type)) {
                return false;
            }
            dummies.insert(&d->base);
            ASR::Variable_t *a = variable(actual);
            ASR::ttype_t *actual_type = ASRUtils::expr_type(actual);
            if (ASRUtils::is_allocatable(d->m_type)) {
                if (a == nullptr || !ASRUtils::is_allocatable(a->m_type) ||
                        ASRUtils::is_pointer(a->m_type) ||
                        !same_type(d->m_type, a->m_type) || d->m_value_attr) {
                    return false;
                }
            } else if (ASRUtils::is_array(d->m_type)) {
                ShapeOnlyUse use(&d->base);
                for (size_t k = 0; k < f.n_body; k++) use.visit_stmt(*f.m_body[k]);
                if (a == nullptr || !use.ok || d->m_value_attr ||
                        ASRUtils::is_pointer(a->m_type) ||
                        !ASRUtils::is_array(a->m_type) ||
                        !same_type(d->m_type, a->m_type) ||
                        ASRUtils::extract_physical_type(d->m_type) !=
                            ASR::array_physical_typeType::DescriptorArray ||
                        ASRUtils::extract_physical_type(a->m_type) !=
                            ASR::array_physical_typeType::DescriptorArray) {
                    return false;
                }
            } else if (ASRUtils::is_array(actual_type) ||
                    ASRUtils::is_allocatable(actual_type) ||
                    ASRUtils::is_pointer(actual_type) ||
                    !same_type(d->m_type, actual_type)) {
                return false;
            } else if (a == nullptr || d->m_value_attr) {
                if (d->m_intent != ASR::intentType::In && !d->m_value_attr) {
                    return false;
                }
                copied.push_back({d, actual});
                continue;
            }
            substitutions[&d->base] = actual;
        }
        // The local variables: SAVE ones move to the scope around the
        // subroutine, the others to the caller.
        std::vector<ASR::Variable_t*> saved, locals;
        for (auto &item : f.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) return false;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
            if (dummies.count(item.second)) continue;
            if (v->n_codims != 0 || v->m_common_block != nullptr ||
                    v->m_is_volatile || v->m_is_asynchronous ||
                    !ASRUtils::has_own_storage(*v) ||
                    ASRUtils::is_pointer(v->m_type) || v->m_target_attr ||
                    !simple_type(v->m_type)) {
                return false;
            }
            if (v->m_storage == ASR::storage_typeType::Save) {
                saved.push_back(v);
            } else if (v->m_symbolic_value != nullptr || v->m_value != nullptr ||
                    ASRUtils::is_allocatable(v->m_type) ||
                    (ASRUtils::is_array(v->m_type) &&
                        !ASRUtils::is_fixed_size_array(v->m_type))) {
                return false;
            } else {
                locals.push_back(v);
            }
        }
        for (ASR::Variable_t *v : locals) {
            substitutions[&v->base] = new_local(loc, v->m_name, v->m_type);
        }
        SymbolTable *around = f.m_symtab->parent;
        for (ASR::Variable_t *v : saved) {
            std::string name = PassUtils::inline_local_name(around,
                std::string(f.m_name) + "_" + v->m_name);
            f.m_symtab->erase_symbol(v->m_name);
            v->m_name = s2c(al, name);
            v->m_access = ASR::accessType::Private;
            v->m_parent_symtab = around;
            around->add_symbol(name, &v->base);
        }
        ASRUtils::ASRBuilder b(al, loc);
        for (auto &c : copied) {
            ASR::expr_t *copy = new_local(loc, c.first->m_name, c.first->m_type);
            out.push_back(al, b.Assignment(copy, c.second));
            substitutions[&c.first->base] = copy;
        }
        AssociateVarResolverVisitor replace(al, substitutions);
        ASRUtils::ExprStmtDuplicator duplicator(al);
        for (size_t i = 0; i < f.n_body; i++) {
            if (i + 1 == f.n_body && ASR::is_a<ASR::Return_t>(*f.m_body[i])) {
                break;
            }
            duplicator.success = true;
            ASR::stmt_t *copy = duplicator.duplicate_stmt(f.m_body[i]);
            LCOMPILERS_ASSERT(duplicator.success && copy);
            replace.visit_stmt(*copy);
            out.push_back(al, copy);
        }
        return true;
    }
};

class LoopSubroutineInlinerVisitor
        : public ASR::BaseWalkVisitor<LoopSubroutineInlinerVisitor> {
public:
    LoopSubroutineInliner inliner;
    explicit LoopSubroutineInlinerVisitor(Allocator &al) : inliner(al) {}

    void visit_Program(const ASR::Program_t &x) {
        ASR::Program_t &xx = const_cast<ASR::Program_t&>(x);
        inliner.procedure(xx.m_symtab, &xx.base.base, xx.m_body, xx.n_body);
        for (auto &item : x.m_symtab->get_scope()) visit_symbol(*item.second);
    }

    void visit_Function(const ASR::Function_t &x) {
        ASR::Function_t &xx = const_cast<ASR::Function_t&>(x);
        if (ASRUtils::runs_on_device(x) || x.m_gpu != nullptr) return;
        inliner.procedure(xx.m_symtab, &xx.base.base, xx.m_body, xx.n_body);
        for (auto &item : x.m_symtab->get_scope()) visit_symbol(*item.second);
    }
};

} // namespace

void pass_inline_loop_subroutine_calls(Allocator &al,
        ASR::TranslationUnit_t &unit, const LCompilers::PassOptions &pass_options) {
    if (!pass_options.fast) return;
    LoopSubroutineInlinerVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor update_dependencies(al);
    update_dependencies.visit_TranslationUnit(unit);
}

} // namespace LCompilers
