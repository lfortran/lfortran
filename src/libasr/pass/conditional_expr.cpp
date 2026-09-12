#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/pass/array_struct_temporary.h>
#include <libasr/pass/conditional_expr.h>
#include <libasr/pass/pass_utils.h>

/*
Lowers an IfExp whose result is an array, a derived type or a polymorphic
entity into a temporary and an If statement:

    x = ( c ? a : b )       =>      if (c) then
                                        allocate(tmp, source shape of a)
                                        tmp = a
                                    else
                                        allocate(tmp, source shape of b)
                                        tmp = b
                                    end if
                                    x = tmp

A conditional expression takes its shape, its length type parameters and its
dynamic type from the arm that is chosen (Fortran 2023, 10.1.4 p22-23), so
these results cannot be joined by the backends: there is no single descriptor
they could write into before knowing which arm runs. Assigning into an
allocatable temporary inside the branch gets all three right.

Scalars of intrinsic type, including character, are left as IfExp. The
backends lower those directly, and they must: the standard requires that the
arm which is not chosen is never evaluated (10.1.4 NOTE 3), which the
generated If statement preserves here and a real branch preserves there.
*/

namespace LCompilers {

// The backends can join a result that is a plain scalar of intrinsic type by
// value. Everything else is described by a descriptor whose contents are only
// known once an arm has been chosen.
static bool requires_temporary(ASR::ttype_t* type) {
    return ASRUtils::is_array(type)
        || ASRUtils::is_struct(*type)
        || ASRUtils::is_class_type(ASRUtils::extract_type(type));
}

class ConditionalExprCollector:
        public ASR::BaseWalkVisitor<ConditionalExprCollector> {
public:
    bool found;

    ConditionalExprCollector(): found(false) {}

    void visit_IfExp(const ASR::IfExp_t& x) {
        if (requires_temporary(x.m_type)) {
            found = true;
            return;
        }
        ASR::BaseWalkVisitor<ConditionalExprCollector>::visit_IfExp(x);
    }
};

static bool contains_lowered_conditional_expr(ASR::expr_t* x) {
    if (x == nullptr) {
        return false;
    }
    ConditionalExprCollector collector;
    collector.visit_expr(*x);
    return collector.found;
}

class ConditionalExprReplacer:
        public ASR::BaseExprReplacer<ConditionalExprReplacer> {
private:
    Allocator& al;
    int counter;

public:
    Vec<ASR::stmt_t*>* current_body;
    SymbolTable* current_scope;

    ConditionalExprReplacer(Allocator& al_): al(al_), counter(0),
        current_body(nullptr), current_scope(nullptr) {}

    void replace_ttype(ASR::ttype_t* /*x*/) {
        // Do nothing
    }

    // An arm is only evaluated when its branch is taken, so anything hoisted
    // out of it has to be hoisted into that branch rather than in front of
    // the whole statement.
    void replace_in_branch(ASR::expr_t*& arm, Vec<ASR::stmt_t*>* branch_body) {
        Vec<ASR::stmt_t*>* current_body_copy = current_body;
        ASR::expr_t** current_expr_copy = current_expr;
        current_body = branch_body;
        current_expr = &arm;
        replace_expr(arm);
        current_expr = current_expr_copy;
        current_body = current_body_copy;
    }

    void build_branch(ASR::expr_t* temporary_var, ASR::expr_t*& arm,
            Vec<ASR::stmt_t*>& branch_body) {
        branch_body.reserve(al, 2);
        replace_in_branch(arm, &branch_body);
        if (ASRUtils::is_array(ASRUtils::expr_type(arm))) {
            insert_allocate_stmt_for_array(al, temporary_var, arm, &branch_body);
        }
        branch_body.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
            al, arm->base.loc, temporary_var, arm, nullptr, true, false)));
    }

    void replace_IfExp(ASR::IfExp_t* x) {
        // The condition is evaluated unconditionally, so anything hoisted out
        // of it belongs in front of the whole statement.
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &(x->m_test);
        replace_expr(x->m_test);
        current_expr = current_expr_copy;

        // A scalar result that holds no lowered conditional expression in its
        // arms stays an IfExp for the backends to branch on. So does anything
        // outside a statement body, such as a specification expression: there
        // is no statement list the If could be inserted into, and a backend
        // reporting that it cannot lower the node beats a crash here.
        if (current_body == nullptr
                || (!requires_temporary(x->m_type)
                    && !contains_lowered_conditional_expr(x->m_body)
                    && !contains_lowered_conditional_expr(x->m_orelse))) {
            ASR::expr_t** arms_copy = current_expr;
            current_expr = &(x->m_body);
            replace_expr(x->m_body);
            current_expr = &(x->m_orelse);
            replace_expr(x->m_orelse);
            current_expr = arms_copy;
            return;
        }

        const Location& loc = x->base.base.loc;
        ASR::ttype_t* temporary_type = ASRUtils::type_get_past_allocatable_pointer(
            x->m_type);
        if (ASRUtils::is_array(temporary_type)) {
            temporary_type = ASRUtils::duplicate_type_with_empty_dims(al,
                temporary_type);
        }
        temporary_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al,
            loc, temporary_type));
        ASR::expr_t* temporary_var = PassUtils::create_var(counter++,
            "conditional_expr", loc, temporary_type, al, current_scope,
            x->m_body);

        Vec<ASR::stmt_t*> then_body, else_body;
        build_branch(temporary_var, x->m_body, then_body);
        build_branch(temporary_var, x->m_orelse, else_body);
        current_body->push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc,
            nullptr, x->m_test, then_body.p, then_body.size(),
            else_body.p, else_body.size())));
        *current_expr = temporary_var;
    }
};

class ConditionalExprVisitor:
        public ASR::CallReplacerOnExpressionsVisitor<ConditionalExprVisitor> {
private:
    Allocator& al;
    ConditionalExprReplacer replacer;
    Vec<ASR::stmt_t*>* current_body;

public:
    ConditionalExprVisitor(Allocator& al_): al(al_), replacer(al_),
        current_body(nullptr) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_body = current_body;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
    }

    void transform_stmts(ASR::stmt_t**& m_body, size_t& n_body) {
        Vec<ASR::stmt_t*>* current_body_copy = current_body;
        Vec<ASR::stmt_t*> current_body_vec;
        current_body_vec.reserve(al, n_body);
        current_body = &current_body_vec;
        for (size_t i = 0; i < n_body; i++) {
            visit_stmt(*m_body[i]);
            current_body->push_back(al, m_body[i]);
        }
        m_body = current_body_vec.p;
        n_body = current_body_vec.size();
        current_body = current_body_copy;
    }
};

void pass_replace_conditional_expr(Allocator &al, ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions& /*pass_options*/) {
    ConditionalExprVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
