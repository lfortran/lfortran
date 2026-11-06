#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_call_guards.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

/*
A call that its subroutine would return from at once is skipped.

A subroutine whose first statement is `if (c) return` does nothing when `c`
holds, but its call still evaluates the actual arguments, and the
`gpu_launch_sync` pass has to wait for the device before a call handed an
array a pending launch writes. The debug hooks of a program are like that:

    call dump('x', x)      ! dump: if (len_trim(directory) == 0) return

So the call tests `c` itself first:

    if (.not. (len_trim(directory) == 0)) call dump('x', x)

and the wait goes into that branch. This runs before temporaries are made
for the actual arguments (`reshape(...)`), so building them moves into the
branch too.

The call is rewritten only when that changes nothing but the time it takes:

* the subroutine is a procedure with a body, called by name (not through a
  procedure pointer or a binding), and has no `intent(out)` dummy, which a
  call defines (deallocates, default-initializes) even when the body returns
  at once;
* `c` calls no function, impure intrinsics included, and names no dummy or
  local of the subroutine, only variables the call can name as they are --
  of a scope the call is in, such as the procedure both are contained in or
  the module;
* the actual arguments call no function: skipping their evaluation is then
  not observable, and evaluating them cannot change `c`.

Only with a GPU device selected, where the waits are what this saves.
*/

namespace {

// Whether an expression has no effect and reads only what `visible` allows.
class Inert : public ASR::BaseWalkVisitor<Inert> {
public:
    bool ok = true;
    // The scope the expression is evaluated in at the call, or nullptr to
    // allow any variable.
    SymbolTable *scope = nullptr;
    // A scope whose variables the expression must not name.
    SymbolTable *callee = nullptr;

    static bool within(SymbolTable *s, SymbolTable *outer) {
        for (; s != nullptr; s = s->parent) {
            if (s == outer) return true;
        }
        return false;
    }

    void visit_Var(const ASR::Var_t &x) {
        if (scope == nullptr) return;
        ASR::symbol_t *s = x.m_v;
        SymbolTable *owner = ASRUtils::symbol_parent_symtab(s);
        if (!ASR::is_a<ASR::Variable_t>(*s) || within(owner, callee)
                || !within(scope, owner)) {
            ok = false;
        }
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &/*x*/) {
        ok = false;
    }
    void visit_IntrinsicImpureFunction(
            const ASR::IntrinsicImpureFunction_t &/*x*/) {
        ok = false;
    }
};

class CallGuards : public PassUtils::PassVisitor<CallGuards> {
public:
    CallGuards(Allocator &al_) : PassVisitor(al_, nullptr) {}

    // The condition `c` of the leading `if (c) return` of `fn`, when a call
    // may test it instead.
    ASR::expr_t *guard(ASR::Function_t *fn) {
        ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);
        if (type->m_deftype != ASR::deftypeType::Implementation
                || fn->m_return_var != nullptr || fn->n_body == 0
                || !ASR::is_a<ASR::If_t>(*fn->m_body[0])) {
            return nullptr;
        }
        ASR::If_t *test = ASR::down_cast<ASR::If_t>(fn->m_body[0]);
        if (test->n_orelse != 0 || test->n_body != 1
                || !ASR::is_a<ASR::Return_t>(*test->m_body[0])) {
            return nullptr;
        }
        for (size_t i = 0; i < fn->n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) return nullptr;
            ASR::symbol_t *d = ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v;
            if (ASR::is_a<ASR::Variable_t>(*d)
                    && ASR::down_cast<ASR::Variable_t>(d)->m_intent
                        == ASR::intentType::Out) {
                return nullptr;
            }
        }
        Inert inert;
        inert.scope = current_scope;
        inert.callee = fn->m_symtab;
        inert.visit_expr(*test->m_test);
        return inert.ok ? test->m_test : nullptr;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        if (x.m_dt != nullptr || current_scope == nullptr) return;
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(x.m_name);
        if (!ASR::is_a<ASR::Function_t>(*callee)) return;
        ASR::expr_t *c = guard(ASR::down_cast<ASR::Function_t>(callee));
        if (c == nullptr) return;
        Inert args;
        for (size_t i = 0; i < x.n_args && args.ok; i++) {
            if (x.m_args[i].m_value) args.visit_expr(*x.m_args[i].m_value);
        }
        if (!args.ok) return;
        const Location &loc = x.base.base.loc;
        ASRUtils::ExprStmtDuplicator dup(al);
        ASR::expr_t *test = dup.duplicate_expr(c);
        ASR::expr_t *run = ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc,
            test, ASRUtils::expr_type(test), nullptr));
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, const_cast<ASR::stmt_t*>(&x.base));
        pass_result.reserve(al, 1);
        pass_result.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc,
            nullptr, run, body.p, body.n, nullptr, 0)));
    }
};

} // namespace

void pass_gpu_call_guards(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) return;
    CallGuards v(al);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
