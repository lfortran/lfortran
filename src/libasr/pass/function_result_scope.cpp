#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/function_result_scope.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <vector>
#include <unordered_map>
#include <unordered_set>

/*
 * F2018 7.5.6.3 p5: the result of a nonpointer function referenced by an
 * executable construct is finalized after the execution of the innermost
 * executable construct containing the reference.
 *
 * The passes that run before subroutine_from_function (implied_do_loops,
 * array_struct_temporary, where, array_op, ...) split a statement into
 * several: fills of compiler temporaries (`tmp(k) = f(k)`), per-element
 * loops, the statement itself. Once that is done, nothing tells which of
 * them were one statement. This pass runs before all of them and gives each
 * statement that references such a function a scope of its own, a BLOCK
 * made of the statement:
 *
 *     call show(g(f(1)), [f(2), f(3)])
 *
 * becomes
 *
 *     block
 *         type(t), pointer :: r
 *         r => f(1)
 *         call show(g(r), [f(2), f(3)])
 *     end block
 *
 * - A reference that is evaluated once per execution of the statement is
 *   associated with a pointer of the BLOCK. subroutine_from_function returns
 *   the result into a new variable of the BLOCK (as for the associate name of
 *   an ASSOCIATE construct), which is finalized when the BLOCK completes,
 *   after everything the statement was split into.
 * - A reference that is an element of an array constructor is left in place:
 *   its result is returned straight into the element of the temporary that
 *   holds the constructor, which is not a copy of it. So are references in
 *   an implied DO loop or in a branch of a conditional expression, which are
 *   evaluated a number of times not known here. The temporaries later passes
 *   make for the statement are variables of the BLOCK, so they too are
 *   finalized when the statement is done, and not when the procedure returns.
 * - An allocatable trait result is captured into an owning local with an
 *   allocation-moving Assignment, not a pointer association. Result lowering
 *   returns directly into that slot. This internal capture is not the user's
 *   value assignment, which still copies and preserves result finalization.
 * - An elemental reference with an array argument and a finalizable result
 *   is an array. It is finalized (7.5.6.2) only if its type has an elemental
 *   final subroutine, and then its temporary is made a variable of the
 *   BLOCK, for the same reason.
 *
 * The header of an IF, DO, DO CONCURRENT, FORALL or SELECT CASE construct
 * is evaluated into variables at the start of a BLOCK that then executes
 * the construct, so that its results are finalized after the construct
 * (only the results are, for a character selector of SELECT CASE).
 * The condition of a DO WHILE is evaluated, and tested, by a BLOCK at the
 * start of each iteration, whose results are finalized before the body
 * executes (F2018 11.1.7.4.1 p2).
 * The results referenced by the selector of an ASSOCIATE construct are
 * associated with pointers of the construct, so they are finalized when it
 * completes. A construct left by EXIT, CYCLE, RETURN or GO TO completes as
 * well, and the backend finalizes the variables of the BLOCK or ASSOCIATE
 * constructs that such a branch leaves. The header of an arithmetic IF, a
 * statement that branches, is evaluated by a BLOCK of its own before the
 * branch. A WHERE construct holds only assignments and WHERE constructs, so
 * it is made a BLOCK as a whole.
 */

namespace LCompilers {

namespace {

// A reference whose result is finalized after the statement, and that
// subroutine_from_function returns into a variable.
bool is_result_reference(ASR::expr_t* expr) {
    if (!ASRUtils::is_finalizable_function_reference(expr)) {
        return false;
    }
    ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(
        ASRUtils::get_past_array_physical_cast(expr));
    ASR::Function_t* func = ASRUtils::get_function(fc->m_name);
    return func != nullptr &&
        ASRUtils::get_FunctionType(func)->m_abi != ASR::abiType::BindC;
}

// A result already bound to this scope must not be captured a second time.
ASR::expr_t* bound_result_reference(ASR::stmt_t* statement) {
    ASR::expr_t* value = nullptr;
    if (ASR::is_a<ASR::Associate_t>(*statement)) {
        value = ASR::down_cast<ASR::Associate_t>(statement)->m_value;
    } else if (ASR::is_a<ASR::Assignment_t>(*statement)) {
        auto *assignment = ASR::down_cast<ASR::Assignment_t>(statement);
        if (assignment->m_move_allocation &&
                ASRUtils::is_trait_owner(ASRUtils::expr_type(assignment->m_target))) {
            value = assignment->m_value;
        }
    }
    return value && is_result_reference(value) ? value : nullptr;
}

// Whether the derived type `struct_sym`, or a type it extends, has an
// elemental final subroutine.
bool has_elemental_final_subroutine(ASR::symbol_t* struct_sym) {
    std::unordered_set<ASR::symbol_t*> visited;
    ASR::symbol_t* sym = struct_sym;
    while (sym != nullptr) {
        sym = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Struct_t>(*sym) ||
                visited.find(sym) != visited.end()) {
            return false;
        }
        visited.insert(sym);
        ASR::Struct_t* st = ASR::down_cast<ASR::Struct_t>(sym);
        for (size_t i = 0; i < st->n_member_functions; i++) {
            ASR::symbol_t* final_sym = st->m_symtab->parent->resolve_symbol(
                st->m_member_functions[i]);
            if (final_sym != nullptr && ASRUtils::is_elemental(final_sym)) {
                return true;
            }
        }
        sym = st->m_parent;
    }
    return false;
}

// An elemental reference with an array argument whose result is finalized
// element by element after the statement (7.5.6.2, 7.5.6.3 p5).
bool is_elemental_result_reference(ASR::expr_t* expr) {
    expr = ASRUtils::get_past_array_physical_cast(expr);
    if (!ASR::is_a<ASR::FunctionCall_t>(*expr)) {
        return false;
    }
    ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
    ASR::ttype_t* type = ASRUtils::expr_type(expr);
    if (!ASRUtils::is_array(type) || !ASRUtils::is_elemental(fc->m_name)) {
        return false;
    }
    ASR::ttype_t* element_type = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(type));
    if (!ASR::is_a<ASR::StructType_t>(*element_type) ||
            ASRUtils::is_class_type(element_type)) {
        return false;
    }
    return has_elemental_final_subroutine(
        ASRUtils::get_struct_sym_from_struct_expr(expr));
}

// The index of the argument of a call to `name` with `n_args` arguments that
// is the passed object of the type-bound procedure, or -1 if there is none.
int passed_object_index(ASR::symbol_t* name, size_t n_args) {
    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(name);
    if (!ASR::is_a<ASR::StructMethodDeclaration_t>(*sym)) {
        return -1;
    }
    ASR::StructMethodDeclaration_t* method =
        ASR::down_cast<ASR::StructMethodDeclaration_t>(sym);
    ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(method->m_proc);
    if (method->m_is_nopass || !ASR::is_a<ASR::Function_t>(*proc)) {
        return -1;
    }
    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
    size_t index = 0;
    if (method->m_self_argument != nullptr) {
        index = func->n_args;
        for (size_t i = 0; i < func->n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*func->m_args[i]) &&
                    std::string(ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(func->m_args[i])->m_v)) ==
                    method->m_self_argument) {
                index = i;
                break;
            }
        }
    }
    if (index >= func->n_args || index >= n_args) {
        return -1;
    }
    return static_cast<int>(index);
}

// Whether an expression or a statement references a function whose result
// this pass gives a scope (see is_result_reference and
// is_elemental_result_reference).
class ReferencesResults: public ASR::BaseWalkVisitor<ReferencesResults> {
public:
    bool found = false;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::expr_t* expr = const_cast<ASR::expr_t*>(&x.base);
        if (is_result_reference(expr) || is_elemental_result_reference(expr)) {
            found = true;
            return;
        }
        ASR::BaseWalkVisitor<ReferencesResults>::visit_FunctionCall(x);
    }

    void visit_ttype(const ASR::ttype_t & /*x*/) {}
};

bool references_results(ASR::expr_t* expr) {
    if (expr == nullptr) {
        return false;
    }
    ReferencesResults v;
    v.visit_expr(*expr);
    return v.found;
}

bool references_results(const ASR::stmt_t &stmt) {
    ReferencesResults v;
    v.visit_stmt(stmt);
    return v.found;
}

// Replaces each reference evaluated once per execution of the statement
// with a pointer declared in `scope`, and appends to `associations` the
// `p => f(...)` that associates the pointer with the result. Of a defined
// operation, assignment or input/output only the call is visited, and an
// operand that is an argument of it, or a copy of one, is made the replaced
// argument.
class AssociateResults: public ASR::BaseExprReplacer<AssociateResults> {
public:
    Allocator &al;
    SymbolTable* scope;
    Vec<ASR::stmt_t*> &associations;
    std::unordered_map<ASR::expr_t*, ASR::expr_t*> replaced;
    // Elements of array constructors, returned into the constructor.
    std::unordered_set<ASR::expr_t*> kept;

    AssociateResults(Allocator &al_, SymbolTable* scope_,
            Vec<ASR::stmt_t*> &associations_):
        al(al_), scope(scope_), associations(associations_) {
        call_replacer_on_value = false;
    }

    void replace_expr(ASR::expr_t* x) {
        if (x == nullptr) {
            return;
        }
        auto it = replaced.find(x);
        if (it != replaced.end()) {
            *current_expr = it->second;
            return;
        }
        ASR::BaseExprReplacer<AssociateResults>::replace_expr(x);
    }

    void replace_ttype(ASR::ttype_t* /*x*/) {}

    // The operands of a defined operation or assignment are the arguments
    // `args` of its call, in order. An operand that references such a
    // function is the same node as the argument, or, in an operation read
    // from a module file, a copy of it. Only the call is executed, so only
    // its references are replaced: `operand_is_copy` tells, before that,
    // which operands are copies, and `take_arguments` then makes each
    // operand the (replaced) argument, so that nothing evaluates the copy.
    // It returns false if an operand that references such a function is
    // neither (`args` is nullptr if the call is not of that form).
    bool operand_is_copy(const std::vector<ASR::expr_t**> &operands,
            ASR::call_arg_t* args, std::vector<bool> &copy) {
        copy.assign(operands.size(), false);
        bool matched = true;
        for (size_t i = 0; i < operands.size(); i++) {
            ASR::expr_t* operand = operands[i] ? *operands[i] : nullptr;
            if (operand == nullptr || !references_results(operand)) {
                continue;
            }
            ASR::expr_t* argument = args ? args[i].m_value : nullptr;
            copy[i] = argument != nullptr && operand != argument &&
                ASRUtils::types_equal(ASRUtils::expr_type(operand),
                    ASRUtils::expr_type(argument), operand, argument);
            matched = matched && (copy[i] || operand == argument);
        }
        return matched;
    }

    void take_arguments(const std::vector<ASR::expr_t**> &operands,
            ASR::call_arg_t* args, const std::vector<bool> &copy) {
        for (size_t i = 0; i < operands.size(); i++) {
            if (operands[i] == nullptr) {
                continue;
            }
            if (copy[i]) {
                *operands[i] = args[i].m_value;
                continue;
            }
            auto it = replaced.find(*operands[i]);
            if (it != replaced.end()) {
                *operands[i] = it->second;
            }
        }
    }

    // A defined operation is executed as its function call `m_overloaded`.
    // False if its operands are not the arguments of the call.
    template <typename T>
    bool replace_defined_operation(T* x,
            const std::vector<ASR::expr_t**> &operands) {
        ASR::expr_t* call = ASRUtils::get_past_array_physical_cast(
            x->m_overloaded);
        ASR::call_arg_t* args = nullptr;
        if (ASR::is_a<ASR::FunctionCall_t>(*call) &&
                ASR::down_cast<ASR::FunctionCall_t>(call)->n_args ==
                    operands.size()) {
            args = ASR::down_cast<ASR::FunctionCall_t>(call)->m_args;
        }
        std::vector<bool> copy;
        if (!operand_is_copy(operands, args, copy)) {
            return false;
        }
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &x->m_overloaded;
        replace_expr(x->m_overloaded);
        current_expr = current_expr_copy;
        take_arguments(operands, args, copy);
        return true;
    }

    void replace_OverloadedBinOp(ASR::OverloadedBinOp_t* x) {
        if (!replace_defined_operation(x, {&x->m_left, &x->m_right})) {
            ASR::BaseExprReplacer<AssociateResults>::replace_OverloadedBinOp(x);
        }
    }

    void replace_OverloadedCompare(ASR::OverloadedCompare_t* x) {
        if (!replace_defined_operation(x, {&x->m_left, &x->m_right})) {
            ASR::BaseExprReplacer<AssociateResults>::replace_OverloadedCompare(
                x);
        }
    }

    void replace_OverloadedBoolOp(ASR::OverloadedBoolOp_t* x) {
        if (!replace_defined_operation(x, {&x->m_left, &x->m_right})) {
            ASR::BaseExprReplacer<AssociateResults>::replace_OverloadedBoolOp(
                x);
        }
    }

    void replace_OverloadedStringConcat(ASR::OverloadedStringConcat_t* x) {
        if (!replace_defined_operation(x, {&x->m_left, &x->m_right})) {
            ASR::BaseExprReplacer<AssociateResults>
                ::replace_OverloadedStringConcat(x);
        }
    }

    void replace_OverloadedUnaryMinus(ASR::OverloadedUnaryMinus_t* x) {
        if (!replace_defined_operation(x, {&x->m_arg})) {
            ASR::BaseExprReplacer<AssociateResults>
                ::replace_OverloadedUnaryMinus(x);
        }
    }

    // Evaluated once for each iteration.
    void replace_ImpliedDoLoop(ASR::ImpliedDoLoop_t* /*x*/) {}

    // Only one branch is evaluated.
    void replace_IfExp(ASR::IfExp_t* x) {
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &x->m_test;
        replace_expr(x->m_test);
        current_expr = current_expr_copy;
    }

    void replace_ArrayConstructor(ASR::ArrayConstructor_t* x) {
        for (size_t i = 0; i < x->n_args; i++) {
            if (is_result_reference(x->m_args[i])) {
                kept.insert(ASRUtils::get_past_array_physical_cast(
                    x->m_args[i]));
            }
        }
        ASR::BaseExprReplacer<AssociateResults>::replace_ArrayConstructor(x);
    }

    // Replaces the references in the arguments of the call `x` and in the
    // object `m_dt` of a type-bound call. The object is the same expression
    // as the passed-object argument, but it is a copy of it, not the same
    // node, in a call read from a module file (a type-bound defined
    // operation or input/output), and replacing the references of that copy
    // too would evaluate them twice: it is made the replaced argument.
    template <typename T>
    void replace_call_arguments(T* x) {
        int index = x->m_dt == nullptr ? -1 :
            passed_object_index(x->m_name, x->n_args);
        ASR::expr_t* passed_object = index >= 0 ?
            x->m_args[index].m_value : nullptr;
        bool copy_of_passed_object = passed_object != nullptr &&
            passed_object != x->m_dt && references_results(x->m_dt) &&
            references_results(passed_object) &&
            ASRUtils::types_equal(ASRUtils::expr_type(x->m_dt),
                ASRUtils::expr_type(passed_object), x->m_dt, passed_object);
        ASR::expr_t** current_expr_copy = current_expr;
        for (size_t i = 0; i < x->n_args; i++) {
            if (x->m_args[i].m_value != nullptr) {
                current_expr = &x->m_args[i].m_value;
                replace_expr(x->m_args[i].m_value);
            }
        }
        if (copy_of_passed_object) {
            x->m_dt = x->m_args[index].m_value;
        } else if (x->m_dt != nullptr) {
            current_expr = &x->m_dt;
            replace_expr(x->m_dt);
        }
        current_expr = current_expr_copy;
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        // The references in the arguments are evaluated first.
        replace_call_arguments(x);
        ASR::expr_t* expr = &x->base;
        if (!is_result_reference(expr) || kept.find(expr) != kept.end()) {
            return;
        }
        const Location &loc = expr->base.loc;
        std::string name = scope->get_unique_name("__libasr_function_result");
        bool owning_result = ASRUtils::is_trait_owner(x->m_type);
        ASR::ttype_t* result_type = ASRUtils::duplicate_type(al, x->m_type);
        if (!owning_result) {
            result_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, result_type));
        }
        ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                nullptr, 0, ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, result_type,
                owning_result ? nullptr : ASRUtils::get_struct_sym_from_struct_expr(expr),
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        scope->add_symbol(name, sym);
        ASR::expr_t* var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        // An allocatable result is captured into its own slot, not associated
        // through a pointer or copied. Result lowering turns this move into
        // the callee's hidden OUT argument; the using construct owns cleanup.
        associations.push_back(al, ASRUtils::STMT(owning_result
            ? ASR::make_Assignment_t(al, loc, var, expr, nullptr, false, true)
            : ASR::make_Associate_t(al, loc, var, expr)));
        replaced[expr] = var;
        *current_expr = var;
    }
};

// Applies AssociateResults to every expression of a statement, and of the
// statements it contains (those of a WHERE construct).
class AssociateStatementResults:
    public ASR::CallReplacerOnExpressionsVisitor<AssociateStatementResults> {
public:
    AssociateResults &replacer;

    AssociateStatementResults(AssociateResults &replacer_):
        replacer(replacer_) {
        visit_expr_after_replacement = false;
    }

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    // A defined assignment is executed as its subroutine call, whose second
    // argument is the value (see AssociateResults::operand_is_copy).
    void visit_Assignment(const ASR::Assignment_t &x) {
        if (x.m_overloaded == nullptr) {
            ASR::CallReplacerOnExpressionsVisitor<AssociateStatementResults>
                ::visit_Assignment(x);
            return;
        }
        ASR::Assignment_t &xx = const_cast<ASR::Assignment_t&>(x);
        ASR::call_arg_t* args = nullptr;
        if (ASR::is_a<ASR::SubroutineCall_t>(*x.m_overloaded) &&
                ASR::down_cast<ASR::SubroutineCall_t>(
                    x.m_overloaded)->n_args == 2) {
            args = ASR::down_cast<ASR::SubroutineCall_t>(
                x.m_overloaded)->m_args;
        }
        std::vector<ASR::expr_t**> operands = {nullptr, &xx.m_value};
        std::vector<bool> copy;
        replacer.operand_is_copy(operands, args, copy);
        visit_stmt(*x.m_overloaded);
        replacer.take_arguments(operands, args, copy);
    }

    // A defined input/output statement is executed as its subroutine call
    // (and the unit), whose first argument is the one item read or written.
    // False if it is not of that form.
    template <typename T>
    bool visit_defined_input_output(T &x) {
        if (x.m_overloaded == nullptr || x.n_values != 1 ||
                !ASR::is_a<ASR::SubroutineCall_t>(*x.m_overloaded)) {
            return false;
        }
        ASR::SubroutineCall_t* call = ASR::down_cast<ASR::SubroutineCall_t>(
            x.m_overloaded);
        ASR::expr_t** item = &x.m_values[0];
        if (ASR::is_a<ASR::StringFormat_t>(**item)) {
            ASR::StringFormat_t* format = ASR::down_cast<ASR::StringFormat_t>(
                *item);
            item = format->n_args == 1 ? &format->m_args[0] : nullptr;
        }
        std::vector<ASR::expr_t**> operands = {item};
        ASR::call_arg_t* args = call->n_args >= 1 ? call->m_args : nullptr;
        std::vector<bool> copy;
        if (item == nullptr ||
                !replacer.operand_is_copy(operands, args, copy)) {
            return false;
        }
        if (x.m_unit != nullptr) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = &x.m_unit;
            call_replacer();
            current_expr = current_expr_copy;
        }
        visit_stmt(*x.m_overloaded);
        replacer.take_arguments(operands, args, copy);
        return true;
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        if (!visit_defined_input_output(const_cast<ASR::FileWrite_t&>(x))) {
            ASR::CallReplacerOnExpressionsVisitor<AssociateStatementResults>
                ::visit_FileWrite(x);
        }
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        if (!visit_defined_input_output(const_cast<ASR::FileRead_t&>(x))) {
            ASR::CallReplacerOnExpressionsVisitor<AssociateStatementResults>
                ::visit_FileRead(x);
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        replacer.replace_call_arguments(const_cast<ASR::SubroutineCall_t*>(&x));
    }

    void visit_ttype(const ASR::ttype_t & /*x*/) {}
};

// The BLOCK of `scope` made of `block_body`, with the symbol table
// `block_scope`, and the statement that executes it.
ASR::stmt_t* make_block(Allocator &al, SymbolTable* scope, const Location &loc,
        SymbolTable* block_scope, Vec<ASR::stmt_t*> &block_body) {
    std::string block_name = scope->get_unique_name(
        "__libasr_function_result_block");
    ASR::asr_t* block = ASR::make_Block_t(al, loc, block_scope,
        s2c(al, block_name), block_body.p, block_body.size());
    block_scope->asr_owner = block;
    ASR::symbol_t* block_sym = ASR::down_cast<ASR::symbol_t>(block);
    scope->add_symbol(block_name, block_sym);
    return ASRUtils::STMT(ASR::make_BlockCall_t(al, loc, -1, block_sym));
}

// A variable of `scope` that holds the value of `value`.
ASR::expr_t* create_header_variable(Allocator &al, SymbolTable* scope,
        ASR::expr_t* value) {
    std::string name = scope->get_unique_name(
        "__libasr_function_result_header");
    ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Variable_t_util(al, value->base.loc,
            scope, s2c(al, name), nullptr, 0,
            ASR::intentType::Local, nullptr, nullptr,
            ASR::storage_typeType::Default,
            ASRUtils::duplicate_type(al, ASRUtils::expr_type(value)),
            nullptr, ASR::abiType::Source, ASR::accessType::Public,
            ASR::presenceType::Required, false));
    scope->add_symbol(name, sym);
    return ASRUtils::EXPR(ASR::make_Var_t(al, value->base.loc, sym));
}

// The BLOCK of `scope` that evaluates the header values `header` of a
// statement into variables of `scope`, which the statement then uses, and
// then executes `rest`, which is the statement itself if it is a construct:
// the results are finalized when the BLOCK completes, after the construct.
// Unless `into_variables` is set, only the references to such functions are
// evaluated first, and the statement evaluates the rest of each value.
// nullptr if none of the values references such a function.
ASR::stmt_t* make_header_block(Allocator &al, SymbolTable* scope,
        const Location &loc, const std::vector<ASR::expr_t**> &header,
        const std::vector<ASR::stmt_t*> &rest, bool into_variables = true) {
    // No SymbolTable is made for a header without such a reference:
    // each one takes a number from the global counter, which names
    // things in the generated code.
    bool references = false;
    for (ASR::expr_t** value : header) {
        references = references || references_results(*value);
    }
    if (!references) {
        return nullptr;
    }
    SymbolTable* block_scope = al.make_new<SymbolTable>(scope);
    Vec<ASR::stmt_t*> block_body;
    block_body.reserve(al, header.size() + rest.size() + 1);
    AssociateResults replacer(al, block_scope, block_body);
    for (ASR::expr_t** value : header) {
        if (!references_results(*value)) {
            continue;
        }
        replacer.current_expr = value;
        replacer.replace_expr(*value);
        if (!into_variables) {
            continue;
        }
        ASR::expr_t* variable = create_header_variable(al, scope, *value);
        block_body.push_back(al, ASRUtils::STMT(
            ASRUtils::make_Assignment_t_util(al, (*value)->base.loc,
                variable, *value, nullptr, false, false)));
        *value = variable;
    }
    if (block_body.size() == 0) {
        return nullptr;
    }
    for (ASR::stmt_t* stmt : rest) {
        block_body.push_back(al, stmt);
    }
    return make_block(al, scope, loc, block_scope, block_body);
}

class FunctionResultScopeVisitor:
    public ASR::CallReplacerOnExpressionsVisitor<FunctionResultScopeVisitor> {
public:
    Allocator &al;
    const PassOptions &pass_options;

    FunctionResultScopeVisitor(Allocator &al_, const PassOptions &pass_options_):
        al(al_), pass_options(pass_options_) {}

    void call_replacer() {}

    // A WHERE construct is given a scope as a whole (see transform_stmts).
    void visit_Where(const ASR::Where_t & /*x*/) {}

    // The body of a parallel region that the openmp or gpu_offload pass
    // moves into a procedure of its own is left as it is: those passes do
    // not move the BLOCKs of the body along with it. subroutine_from_function
    // gives each statement there a BLOCK of its own after they have run.
    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        bool outlined = x.m_exec_target == ASR::exec_targetType::ExecDevice ||
            (pass_options.openmp &&
                x.m_exec_target != ASR::exec_targetType::ExecSerial);
        if (!outlined) {
            ASR::CallReplacerOnExpressionsVisitor<FunctionResultScopeVisitor>
                ::visit_OMPRegion(x);
        }
    }

    ASR::stmt_t* make_block(const Location &loc, SymbolTable* block_scope,
            Vec<ASR::stmt_t*> &block_body) {
        return LCompilers::make_block(al, current_scope, loc, block_scope,
            block_body);
    }

    ASR::stmt_t* make_header_block(const Location &loc,
            const std::vector<ASR::expr_t**> &header,
            const std::vector<ASR::stmt_t*> &rest,
            bool into_variables = true) {
        return LCompilers::make_header_block(al, current_scope, loc, header,
            rest, into_variables);
    }

    // The BLOCK made of `x`, a statement that references such functions.
    ASR::stmt_t* make_statement_block(ASR::stmt_t* x) {
        SymbolTable* block_scope = al.make_new<SymbolTable>(current_scope);
        Vec<ASR::stmt_t*> block_body;
        block_body.reserve(al, 2);
        // `target = f(...)` with an allocatable scalar target. The
        // intrinsic assignment finalizes the target only if it is allocated
        // (7.5.6.3 p1), and semantics does not allocate it beforehand for
        // such a reference; an unallocated target is given the value by
        // sourced allocation, which finalizes nothing. A polymorphic target
        // is allocated by semantics beforehand, as for any other reference.
        bool allocate_unallocated_target = false;
        if (ASR::is_a<ASR::Assignment_t>(*x)) {
            ASR::Assignment_t* assignment = ASR::down_cast<ASR::Assignment_t>(x);
            ASR::ttype_t* target_type = ASRUtils::expr_type(
                assignment->m_target);
            allocate_unallocated_target = assignment->m_overloaded == nullptr &&
                ASRUtils::is_allocatable(target_type) &&
                !ASRUtils::is_array(target_type) &&
                !ASRUtils::is_class_type(
                    ASRUtils::type_get_past_allocatable_pointer(target_type)) &&
                is_result_reference(assignment->m_value);
        }
        AssociateResults replacer(al, block_scope, block_body);
        AssociateStatementResults statement_replacer(replacer);
        statement_replacer.current_scope = block_scope;
        statement_replacer.visit_stmt(*x);
        ASR::stmt_t* statement = x;
        if (allocate_unallocated_target) {
            ASR::Assignment_t* assignment = ASR::down_cast<ASR::Assignment_t>(x);
            ASR::expr_t* target = assignment->m_target;
            const Location &loc = x->base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::alloc_arg_t alloc_arg;
            alloc_arg.loc = loc;
            alloc_arg.m_a = target;
            alloc_arg.m_dims = nullptr;
            alloc_arg.n_dims = 0;
            alloc_arg.m_len_expr = nullptr;
            alloc_arg.m_type = nullptr;
            alloc_arg.m_sym_subclass = nullptr;
            alloc_arg.m_codims = nullptr;
            alloc_arg.n_codims = 0;
            Vec<ASR::alloc_arg_t> alloc_args;
            alloc_args.reserve(al, 1);
            alloc_args.push_back(al, alloc_arg);
            ASR::stmt_t* sourced_allocate = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.n, nullptr, nullptr,
                assignment->m_value));
            Vec<ASR::expr_t*> allocated_args;
            allocated_args.reserve(al, 1);
            allocated_args.push_back(al, target);
            ASR::expr_t* is_allocated = ASRUtils::EXPR(
                ASR::make_IntrinsicImpureFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicImpureFunctions::Allocated),
                    allocated_args.p, allocated_args.n, 0,
                    ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
            statement = b.If(is_allocated, {x}, {sourced_allocate});
        }
        block_body.push_back(al, statement);
        return make_block(x->base.loc, block_scope, block_body);
    }

    // The header values of the construct `x` that are evaluated once, before
    // the construct is executed, and that can be held by a variable.
    static std::vector<ASR::expr_t**> construct_header(ASR::stmt_t &x) {
        std::vector<ASR::expr_t**> header;
        switch (x.type) {
            case ASR::stmtType::If: {
                header.push_back(&ASR::down_cast<ASR::If_t>(&x)->m_test);
                break;
            }
            case ASR::stmtType::IfArithmetic: {
                header.push_back(
                    &ASR::down_cast<ASR::IfArithmetic_t>(&x)->m_test);
                break;
            }
            case ASR::stmtType::Select: {
                header.push_back(&ASR::down_cast<ASR::Select_t>(&x)->m_test);
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t* loop = ASR::down_cast<ASR::DoLoop_t>(&x);
                header.push_back(&loop->m_head.m_start);
                header.push_back(&loop->m_head.m_end);
                header.push_back(&loop->m_head.m_increment);
                break;
            }
            case ASR::stmtType::ForAllSingle: {
                ASR::ForAllSingle_t* forall =
                    ASR::down_cast<ASR::ForAllSingle_t>(&x);
                header.push_back(&forall->m_head.m_start);
                header.push_back(&forall->m_head.m_end);
                header.push_back(&forall->m_head.m_increment);
                break;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t* loop =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(&x);
                for (size_t i = 0; i < loop->n_head; i++) {
                    header.push_back(&loop->m_head[i].m_start);
                    header.push_back(&loop->m_head[i].m_end);
                    header.push_back(&loop->m_head[i].m_increment);
                }
                break;
            }
            default:
                break;
        }
        return header;
    }

    // do while (c) ... end do
    //   becomes
    // do while (.true.); block; h = c; if (.not. h) exit; end block; ...
    //
    // The effect of DO WHILE is that of a DO construct whose block starts
    // with `if (.not. (c)) exit` (F2018 11.1.7.4.1 p2), so the results
    // of the condition are those of that statement: they are finalized
    // after it (or when EXIT leaves the BLOCK), before the body executes.
    void evaluate_condition_in_block(ASR::WhileLoop_t &x) {
        ASRUtils::ASRBuilder b(al, x.base.base.loc);
        std::vector<ASR::expr_t**> header = {&x.m_test};
        ASR::stmt_t* condition_block = make_header_block(x.base.base.loc,
            header, {});
        LCOMPILERS_ASSERT(condition_block != nullptr);
        // The BLOCK holds the evaluation of the condition, into the
        // variable that is now the test, and the test.
        ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(
            ASR::down_cast<ASR::BlockCall_t>(condition_block)->m_m);
        Vec<ASR::stmt_t*> block_body;
        block_body.reserve(al, block->n_body + 1);
        for (size_t i = 0; i < block->n_body; i++) {
            block_body.push_back(al, block->m_body[i]);
        }
        block_body.push_back(al, b.If(b.Not(x.m_test), {b.Exit()}, {}));
        block->m_body = block_body.p;
        block->n_body = block_body.size();
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body + 1);
        body.push_back(al, condition_block);
        for (size_t i = 0; i < x.n_body; i++) {
            body.push_back(al, x.m_body[i]);
        }
        x.m_body = body.p;
        x.n_body = body.size();
        x.m_test = b.logical_true();
    }

    // Whether `x` gives its value to an associate name of the ASSOCIATE
    // construct whose body is being transformed. Semantics makes these the
    // first statements of the body, one for each associate name in order:
    // an association with the selector, or an assignment of the value of an
    // expression selector. `associated` holds the associate names given
    // their value by the statements before `x`; a statement that gives a
    // value to one of them again is a statement of the construct.
    bool is_selector_statement(ASR::stmt_t* x,
            std::unordered_set<ASR::symbol_t*> &associated) {
        if (current_scope->asr_owner == nullptr ||
                !ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner)) {
            return false;
        }
        ASR::expr_t* target = nullptr;
        if (ASR::is_a<ASR::Associate_t>(*x)) {
            target = ASR::down_cast<ASR::Associate_t>(x)->m_target;
        } else if (ASR::is_a<ASR::Assignment_t>(*x)) {
            target = ASR::down_cast<ASR::Assignment_t>(x)->m_target;
        }
        if (target == nullptr || !ASR::is_a<ASR::Var_t>(*target)) {
            return false;
        }
        ASR::symbol_t* name = ASR::down_cast<ASR::Var_t>(target)->m_v;
        if (!ASR::is_a<ASR::AssociateBlock_t>(*ASR::down_cast<ASR::symbol_t>(
                    current_scope->asr_owner)) &&
                !ASRUtils::association_variable(target)) return false;
        return current_scope->get_symbol(ASRUtils::symbol_name(name)) == name &&
            associated.insert(name).second;
    }

    // The references in the selector statement `x` (see
    // is_selector_statement) are associated with pointers of the ASSOCIATE
    // construct, so that their results are finalized after the construct
    // (F2018 7.5.6.3 p5), not after the statement that evaluates the
    // selector. A selector that is itself such a reference is associated
    // with the associate name by semantics already, and only the
    // references in its arguments are replaced.
    void associate_selector_results(ASR::stmt_t* x, Vec<ASR::stmt_t*> &body) {
        AssociateResults replacer(al, current_scope, body);
        ASR::expr_t* value = bound_result_reference(x);
        if (value != nullptr) {
            replacer.replace_call_arguments(ASR::down_cast<ASR::FunctionCall_t>(
                ASRUtils::get_past_array_physical_cast(value)));
        } else {
            AssociateStatementResults statement_replacer(replacer);
            statement_replacer.current_scope = current_scope;
            statement_replacer.visit_stmt(*x);
        }
        body.push_back(al, x);
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        bool selectors = true;
        std::unordered_set<ASR::symbol_t*> associated;
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t* x = m_body[i];
            visit_stmt(*x);
            selectors = selectors && is_selector_statement(x, associated);
            if (selectors) {
                if (references_results(*x)) {
                    associate_selector_results(x, body);
                } else {
                    body.push_back(al, x);
                }
                continue;
            }
            if (ASR::is_a<ASR::Where_t>(*x) || ASRUtils::is_single_statement(*x)) {
                // The associate name of an ASSOCIATE construct is associated
                // with the result by semantics already.
                bool associates_result = bound_result_reference(x) != nullptr;
                if (!associates_result && references_results(*x)) {
                    body.push_back(al, make_statement_block(x));
                    continue;
                }
            } else if (ASR::is_a<ASR::WhileLoop_t>(*x)) {
                ASR::WhileLoop_t* loop = ASR::down_cast<ASR::WhileLoop_t>(x);
                if (references_results(loop->m_test)) {
                    evaluate_condition_in_block(*loop);
                }
            } else {
                std::vector<ASR::expr_t**> header = construct_header(*x);
                if (!header.empty()) {
                    // An arithmetic IF is a statement that branches: its
                    // results are finalized before the branch. The results
                    // in the header of a construct are finalized after it.
                    bool construct = !ASR::is_a<ASR::IfArithmetic_t>(*x);
                    std::vector<ASR::stmt_t*> rest;
                    if (construct) {
                        rest.push_back(x);
                    }
                    // The length of a character selector may be known only
                    // once it is evaluated. It is left to the SELECT CASE
                    // construct, which evaluates it once, and only the
                    // results it references are evaluated before.
                    bool into_variables = !(ASR::is_a<ASR::Select_t>(*x) &&
                        ASRUtils::is_character(*ASRUtils::expr_type(
                            ASR::down_cast<ASR::Select_t>(x)->m_test)));
                    ASR::stmt_t* header_block = make_header_block(
                        x->base.loc, header, rest, into_variables);
                    if (header_block != nullptr) {
                        body.push_back(al, header_block);
                        if (construct) {
                            continue;
                        }
                    }
                }
            }
            body.push_back(al, x);
        }
        m_body = body.p;
        n_body = body.size();
    }
};

} // namespace

bool references_function_results(ASR::expr_t* expr) {
    return references_results(expr);
}

ASR::stmt_t* make_function_result_header_block(Allocator &al,
        SymbolTable* scope, const Location &loc,
        const std::vector<ASR::expr_t**> &header,
        const std::vector<ASR::stmt_t*> &rest) {
    return make_header_block(al, scope, loc, header, rest);
}

void pass_function_result_scope(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    FunctionResultScopeVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
