#include <limits>

#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_offload_overwrite.h>

namespace LCompilers {

/*
Which arrays a launch need not copy to the device first, because the kernel
assigns all of them before it reads them. That is a property of the loop the
kernel runs, so it is decided here, from the loop as the source wrote it,
and handed to the runtime as an access mode by `device_launch_expand`.

The answer is "yes" only when it holds for every element and every
iteration, which is shown for one shape: an assignment whose subscripts are
the loop variables, one loop per dimension, at the top of the iteration or
inside IFs that compare loop variables with values the loop cannot change.
Nothing of the iteration may name the array before it, and a procedure the
iteration calls before it must not be able to see the array other than
through its arguments. Whether the loops, narrowed by those IFs, run over
exactly the bounds of the array is only known when the launch is made, so
that part is the condition the launch evaluates.
*/

namespace {

ASR::symbol_t* past_external(ASR::symbol_t *s) {
    return ASRUtils::symbol_get_past_external(s);
}

// A variable, or a component of a designator that is one.
bool is_designator_chain(ASR::expr_t *x) {
    while (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
        x = ASR::down_cast<ASR::StructInstanceMember_t>(x)->m_v;
    }
    return ASR::is_a<ASR::Var_t>(*x);
}

ASR::symbol_t* designator_root(ASR::expr_t *x) {
    while (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
        x = ASR::down_cast<ASR::StructInstanceMember_t>(x)->m_v;
    }
    return past_external(ASR::down_cast<ASR::Var_t>(x)->m_v);
}

// Whether `x` is `prefix` or a component of it, at any depth.
bool designator_starts_with(ASR::expr_t *x, ASR::expr_t *prefix) {
    while (true) {
        if (gpu_same_designator(x, prefix)) return true;
        if (!ASR::is_a<ASR::StructInstanceMember_t>(*x)) return false;
        x = ASR::down_cast<ASR::StructInstanceMember_t>(x)->m_v;
    }
}

// The loop variable a subscript is, alone, or nullptr.
ASR::symbol_t* plain_subscript(const ASR::array_index_t &index) {
    if (index.m_left || index.m_step || !index.m_right) return nullptr;
    if (!ASR::is_a<ASR::Var_t>(*index.m_right)) return nullptr;
    return past_external(ASR::down_cast<ASR::Var_t>(index.m_right)->m_v);
}

// What a part of an iteration does with the array: whether it names it
// other than as the element `element` (the subscripts of the assignment,
// when the part follows it), whether it holds statements whose effects are
// out of sight here, and the procedures it calls.
class ArrayUses : public ASR::BaseWalkVisitor<ArrayUses> {
public:
    ASR::expr_t *array;
    ASR::symbol_t *root;
    const std::vector<ASR::symbol_t*> *element = nullptr;
    bool named = false;
    bool opaque = false;
    std::vector<ASR::symbol_t*> &callees;

    ArrayUses(ASR::expr_t *array, std::vector<ASR::symbol_t*> &callees)
        : array(array), root(designator_root(array)), callees(callees) {}

    void visit_Var(const ASR::Var_t &x) {
        if (past_external(x.m_v) == root) named = true;
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
        if (!is_designator_chain(e)) {
            ASR::BaseWalkVisitor<ArrayUses>::visit_StructInstanceMember(x);
            return;
        }
        // The array itself, or something it is a component of, which
        // carries it along. Another component of the same variable is
        // another object.
        if (designator_starts_with(array, e)
                || designator_starts_with(e, array)) {
            named = true;
        }
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        if (!gpu_same_designator(x.m_v, array)) {
            ASR::BaseWalkVisitor<ArrayUses>::visit_ArrayItem(x);
            return;
        }
        bool same = element != nullptr && x.n_args == element->size();
        for (size_t k = 0; same && k < x.n_args; k++) {
            same = plain_subscript(x.m_args[k]) == (*element)[k];
        }
        if (!same) named = true;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        callees.push_back(past_external(x.m_name));
        ASR::BaseWalkVisitor<ArrayUses>::visit_FunctionCall(x);
    }

    // Their bodies are not walked here.
    void visit_SubroutineCall(const ASR::SubroutineCall_t &) {
        opaque = true;
    }
    void visit_BlockCall(const ASR::BlockCall_t &) { opaque = true; }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &) {
        opaque = true;
    }
};

class OverwriteSearch {
public:
    OverwriteSearch(Allocator &al, const Location &loc,
            const ParallelLoopNest &loop, ASR::expr_t *array)
        : al(al), loc(loc), b(al, loc), array(array) {
        for (size_t d = 0; d < loop.n_heads(); d++) {
            ASR::expr_t *v = loop.head(d).m_v;
            loop_vars.push_back(v && ASR::is_a<ASR::Var_t>(*v)
                ? past_external(ASR::down_cast<ASR::Var_t>(v)->m_v)
                : nullptr);
        }
        wide = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
    }

    // The limits of each loop, narrowed by the guards of the assignment.
    struct Limits {
        std::vector<ASR::expr_t*> start, end;
        std::vector<ASR::expr_t*> nonempty;
    };

    enum Result { NotFound, Found, Fails };

    Allocator &al;
    const Location &loc;
    ASRUtils::ASRBuilder b;
    ASR::expr_t *array;
    ASR::ttype_t *wide;
    std::vector<ASR::symbol_t*> loop_vars;
    // The loop variable that subscripts each dimension of the array in the
    // assignment, and the loop it belongs to.
    std::vector<ASR::symbol_t*> element;
    std::vector<size_t> loop_of_dimension;
    std::vector<ASR::symbol_t*> callees;
    Limits found;

    ASR::expr_t* dup(ASR::expr_t *x) {
        ASRUtils::ExprStmtDuplicator d(al);
        d.success = true;
        return d.duplicate_expr(x);
    }

    // The larger and the smaller of two values, which share no node.
    ASR::expr_t* max(ASR::expr_t *x, ASR::expr_t *y) {
        return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc, b.Gt(x, y),
            dup(x), dup(y), wide, nullptr));
    }

    ASR::expr_t* min(ASR::expr_t *x, ASR::expr_t *y) {
        return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc, b.Lt(x, y),
            dup(x), dup(y), wide, nullptr));
    }

    // Whether nothing in `x` names the array (other than as the element
    // the assignment wrote, when `after` is set) or hides what it does.
    bool clean(ASR::stmt_t &x, bool after) {
        ArrayUses uses(array, callees);
        if (after) uses.element = &element;
        uses.visit_stmt(x);
        return !uses.named && !uses.opaque;
    }

    bool clean_expr(ASR::expr_t &x) {
        ArrayUses uses(array, callees);
        uses.visit_expr(x);
        return !uses.named && !uses.opaque;
    }

    // A statement before the assignment: one that assigns something else,
    // or an IF of such statements. Anything that could leave the iteration
    // early or reach the array out of sight is not.
    bool before(ASR::stmt_t *s) {
        if (ASR::is_a<ASR::Assignment_t>(*s)) {
            if (ASR::down_cast<ASR::Assignment_t>(s)->m_overloaded) {
                return false;
            }
            return clean(*s, false);
        }
        if (ASR::is_a<ASR::If_t>(*s)) {
            ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
            if (!clean_expr(*x->m_test)) return false;
            for (size_t i = 0; i < x->n_body; i++) {
                if (!before(x->m_body[i])) return false;
            }
            for (size_t i = 0; i < x->n_orelse; i++) {
                if (!before(x->m_orelse[i])) return false;
            }
            return true;
        }
        return false;
    }

    int loop_index(ASR::expr_t *x) {
        // A loop variable converted to a kind at least as large keeps its
        // value.
        while (ASR::is_a<ASR::Cast_t>(*x)) {
            ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(x);
            if (c->m_kind != ASR::cast_kindType::IntegerToInteger
                    || ASRUtils::extract_kind_from_ttype_t(c->m_type)
                        < ASRUtils::extract_kind_from_ttype_t(
                            ASRUtils::expr_type(c->m_arg))) {
                return -1;
            }
            x = c->m_arg;
        }
        if (!ASR::is_a<ASR::Var_t>(*x)) return -1;
        ASR::symbol_t *s = past_external(ASR::down_cast<ASR::Var_t>(x)->m_v);
        for (size_t d = 0; d < loop_vars.size(); d++) {
            if (loop_vars[d] == s) return (int)d;
        }
        return -1;
    }

    // A value no iteration can change, which the host computes as the
    // kernel does: constants and integer scalars the procedure cannot
    // assign, combined by arithmetic.
    bool invariant(ASR::expr_t *x) {
        switch (x->type) {
            case ASR::exprType::IntegerConstant:
                return true;
            case ASR::exprType::Var: {
                ASR::symbol_t *s = past_external(
                    ASR::down_cast<ASR::Var_t>(x)->m_v);
                if (!s || !ASR::is_a<ASR::Variable_t>(*s)) return false;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                return ASRUtils::is_integer(*v->m_type)
                    && !ASRUtils::is_array(v->m_type)
                    && (v->m_intent == ASR::intentType::In
                        || v->m_storage == ASR::storage_typeType::Parameter);
            }
            case ASR::exprType::IntegerBinOp: {
                ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(x);
                return (op->m_op == ASR::binopType::Add
                        || op->m_op == ASR::binopType::Sub
                        || op->m_op == ASR::binopType::Mul)
                    && invariant(op->m_left) && invariant(op->m_right);
            }
            case ASR::exprType::IntegerUnaryMinus:
                return invariant(
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(x)->m_arg);
            case ASR::exprType::Cast: {
                ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(x);
                return c->m_kind == ASR::cast_kindType::IntegerToInteger
                    && invariant(c->m_arg);
            }
            default:
                return false;
        }
    }

    // Narrows `limits` by an IF test made of comparisons of a loop
    // variable with an invariant, joined by .and.; false for any other.
    bool guard(ASR::expr_t *test, Limits &limits) {
        if (ASR::is_a<ASR::LogicalBinOp_t>(*test)) {
            ASR::LogicalBinOp_t *x = ASR::down_cast<ASR::LogicalBinOp_t>(test);
            return x->m_op == ASR::logicalbinopType::And
                && guard(x->m_left, limits) && guard(x->m_right, limits);
        }
        if (!ASR::is_a<ASR::IntegerCompare_t>(*test)) return false;
        ASR::IntegerCompare_t *x = ASR::down_cast<ASR::IntegerCompare_t>(test);
        ASR::cmpopType op = x->m_op;
        int d = loop_index(x->m_left);
        ASR::expr_t *bound = x->m_right;
        if (d < 0) {
            d = loop_index(x->m_right);
            bound = x->m_left;
            switch (op) {
                case ASR::cmpopType::Lt: op = ASR::cmpopType::Gt; break;
                case ASR::cmpopType::LtE: op = ASR::cmpopType::GtE; break;
                case ASR::cmpopType::Gt: op = ASR::cmpopType::Lt; break;
                case ASR::cmpopType::GtE: op = ASR::cmpopType::LtE; break;
                default: break;
            }
        }
        if (d < 0 || !invariant(bound)) return false;
        ASR::expr_t *e = b.i2i_t(dup(bound), wide);
        ASR::expr_t *&start = limits.start[d];
        ASR::expr_t *&end = limits.end[d];
        bool wide_bound = ASRUtils::extract_kind_from_ttype_t(
            ASRUtils::expr_type(bound)) == 8;
        // Strict endpoints of smaller kinds fit after widening. For kind 8,
        // clamp before +/- 1 and separately exclude the empty range: the
        // conjunction in the coverage condition need not short-circuit.
        switch (op) {
            case ASR::cmpopType::Eq:
                start = max(start, e);
                end = min(end, b.i2i_t(dup(bound), wide));
                return true;
            case ASR::cmpopType::LtE:
                end = min(end, e);
                return true;
            case ASR::cmpopType::Lt:
                if (wide_bound) {
                    constexpr int64_t lowest = std::numeric_limits<int64_t>::min();
                    limits.nonempty.push_back(b.Gt(dup(e), b.i64(lowest)));
                    e = max(e, b.i64(lowest + 1));
                }
                end = min(end, b.Sub(e, b.i64(1)));
                return true;
            case ASR::cmpopType::GtE:
                start = max(start, e);
                return true;
            case ASR::cmpopType::Gt:
                if (wide_bound) {
                    constexpr int64_t highest = std::numeric_limits<int64_t>::max();
                    limits.nonempty.push_back(b.Lt(dup(e), b.i64(highest)));
                    e = min(e, b.i64(highest - 1));
                }
                start = max(start, b.Add(e, b.i64(1)));
                return true;
            default:
                return false;
        }
    }

    // Whether `s` is an assignment to an element of the array.
    bool assigns_array(ASR::stmt_t *s) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::expr_t *target = ASR::down_cast<ASR::Assignment_t>(s)->m_target;
        return ASR::is_a<ASR::ArrayItem_t>(*target) && gpu_same_designator(
            ASR::down_cast<ASR::ArrayItem_t>(target)->m_v, array);
    }

    // The assignment that overwrites the array: every dimension subscripted
    // by a loop variable of its own, and a value that does not read it.
    bool overwrites(ASR::stmt_t *s) {
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded) return false;
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(a->m_target);
        if (item->n_args != loop_vars.size()) return false;
        std::vector<bool> used(loop_vars.size(), false);
        for (size_t k = 0; k < item->n_args; k++) {
            ASR::symbol_t *v = plain_subscript(item->m_args[k]);
            int d = -1;
            for (size_t j = 0; v && j < loop_vars.size(); j++) {
                if (loop_vars[j] == v) d = (int)j;
            }
            if (d < 0 || used[d]) return false;
            used[d] = true;
            element.push_back(v);
            loop_of_dimension.push_back((size_t)d);
        }
        return clean_expr(*a->m_value);
    }

    bool rest_clean(ASR::stmt_t **stmts, size_t from, size_t n, bool after) {
        for (size_t k = from; k < n; k++) {
            if (!clean(*stmts[k], after)) return false;
        }
        return true;
    }

    Result scan(ASR::stmt_t **stmts, size_t n, const Limits &limits,
            bool guarded) {
        for (size_t k = 0; k < n; k++) {
            ASR::stmt_t *s = stmts[k];
            if (assigns_array(s)) {
                if (!overwrites(s)) return Fails;
                found = limits;
                // An iteration the guards keep out does not write its
                // element, so then nothing after may name the array.
                return rest_clean(stmts, k + 1, n, !guarded) ? Found : Fails;
            }
            if (ASR::is_a<ASR::If_t>(*s)) {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                Limits inner = limits;
                if (guard(x->m_test, inner)) {
                    Result r = scan(x->m_body, x->n_body, inner, true);
                    if (r == Fails) return Fails;
                    if (r == Found) {
                        for (size_t i = 0; i < x->n_orelse; i++) {
                            if (!before(x->m_orelse[i])) return Fails;
                        }
                        return rest_clean(stmts, k + 1, n, false)
                            ? Found : Fails;
                    }
                }
            }
            if (!before(s)) return Fails;
        }
        return NotFound;
    }

    // Whether `callee` sees the array only through its arguments: it is
    // not contained in the procedure the array belongs to, and the array
    // is not a variable of a module.
    bool cannot_reach(ASR::symbol_t *callee) {
        if (!callee || !ASR::is_a<ASR::Function_t>(*callee)) return false;
        ASR::symbol_t *root = designator_root(array);
        if (!ASR::is_a<ASR::Variable_t>(*root)) return false;
        SymbolTable *owner = ASR::down_cast<ASR::Variable_t>(root)
            ->m_parent_symtab;
        if (!owner->asr_owner
                || owner->asr_owner->type != ASR::asrType::symbol) {
            return false;
        }
        ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(
            owner->asr_owner);
        if (!ASR::is_a<ASR::Function_t>(*owner_sym)
                && !ASR::is_a<ASR::Program_t>(*owner_sym)
                && !ASR::is_a<ASR::Block_t>(*owner_sym)) {
            return false;
        }
        for (SymbolTable *s = ASR::down_cast<ASR::Function_t>(callee)
                ->m_symtab->parent; s; s = s->parent) {
            if (s == owner) return false;
        }
        return true;
    }
};

// An array of intrinsic elements whose bounds the host knows.
bool overwritable_array(ASR::expr_t *array) {
    if (!is_designator_chain(array)) return false;
    ASR::ttype_t *type = ASRUtils::expr_type(array);
    if (!ASRUtils::is_array(type) || ASRUtils::is_pointer(type)) {
        return false;
    }
    ASR::ttype_t *element = ASRUtils::extract_type(type);
    if (!(ASR::is_a<ASR::Integer_t>(*element)
            || ASR::is_a<ASR::Real_t>(*element)
            || ASR::is_a<ASR::Complex_t>(*element)
            || ASR::is_a<ASR::Logical_t>(*element))) {
        return false;
    }
    ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(
        ASRUtils::type_get_past_allocatable_pointer(type));
    if (a->m_physical_type ==
            ASR::array_physical_typeType::UnboundedPointerArray) {
        return false;
    }
    // An assumed-size array has no upper bound in its last dimension.
    return ASRUtils::is_allocatable(type) || a->n_dims == 0
        || a->m_physical_type ==
            ASR::array_physical_typeType::DescriptorArray
        || a->m_dims[a->n_dims - 1].m_length != nullptr;
}

} // namespace

ASR::expr_t* gpu_overwrite_condition(Allocator &al, const Location &loc,
        const ParallelLoopNest &loop, const std::vector<ASR::expr_t*> &starts,
        const std::vector<ASR::expr_t*> &ends, ASR::expr_t *array) {
    if (!overwritable_array(array)) return nullptr;
    size_t n = loop.n_heads();
    if (n == 0 || starts.size() != n || ends.size() != n
            || static_cast<size_t>(ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(array))) != n) {
        return nullptr;
    }
    OverwriteSearch search(al, loc, loop, array);
    OverwriteSearch::Limits limits;
    for (size_t d = 0; d < n; d++) {
        ASR::expr_t *step = loop.head(d).m_increment;
        int64_t step_value = 0;
        if (search.loop_vars[d] == nullptr || (step != nullptr
                && !(ASRUtils::extract_value(ASRUtils::expr_value(step),
                    step_value) && step_value == 1))) {
            return nullptr;
        }
        for (size_t e = 0; e < d; e++) {
            if (search.loop_vars[e] == search.loop_vars[d]) return nullptr;
        }
        limits.start.push_back(search.b.i2i_t(search.dup(starts[d]),
            search.wide));
        limits.end.push_back(search.b.i2i_t(search.dup(ends[d]),
            search.wide));
    }
    if (search.scan(loop.body, loop.n_body, limits, false)
            != OverwriteSearch::Found) {
        return nullptr;
    }
    for (ASR::symbol_t *callee : search.callees) {
        if (!search.cannot_reach(callee)) return nullptr;
    }
    ASRUtils::ASRBuilder &b = search.b;
    ASR::expr_t *condition = nullptr;
    for (size_t k = 0; k < n; k++) {
        size_t d = search.loop_of_dimension[k];
        ASR::expr_t *dim = b.And(
            b.Eq(search.found.start[d],
                b.ArrayLBound(search.dup(array), k + 1, 8)),
            b.Eq(search.found.end[d],
                b.ArrayUBound(search.dup(array), k + 1, 8)));
        condition = condition ? b.And(condition, dim) : dim;
    }
    for (ASR::expr_t *nonempty : search.found.nonempty) {
        condition = b.And(condition, nonempty);
    }
    return condition;
}

} // namespace LCompilers
