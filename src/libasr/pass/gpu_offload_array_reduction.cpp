#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_array_reduction.h>

#include <algorithm>

namespace LCompilers {

namespace {

ASR::symbol_t* variable_root(ASR::expr_t *e) {
    while (e != nullptr) {
        switch (e->type) {
            case ASR::exprType::Var:
                return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
            case ASR::exprType::ArrayItem:
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                break;
            case ASR::exprType::ArraySection:
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                break;
            case ASR::exprType::StructInstanceMember:
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                break;
            case ASR::exprType::ArrayPhysicalCast:
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                break;
            default:
                return nullptr;
        }
    }
    return nullptr;
}

// Whether `e` is the array `array` itself.
bool names_array(ASR::expr_t *e, ASR::symbol_t *array) {
    e = ASRUtils::get_past_array_physical_cast(e);
    return ASR::is_a<ASR::Var_t>(*e) && ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v) == array;
}

// The scalars the statements may change: what they assign, the variables
// of their loops, what they pass to a procedure or read into, and the
// locals of their BLOCKs, which each iteration has its own of.
class ChangedScalars : public ASR::BaseWalkVisitor<ChangedScalars> {
public:
    std::set<ASR::symbol_t*> changed;
    std::set<SymbolTable*> block_scopes;
    std::set<ASR::symbol_t*> blocks;

    void mark(ASR::expr_t *e) {
        ASR::symbol_t *r = variable_root(e);
        if (r != nullptr) changed.insert(r);
    }
    void visit_Assignment(const ASR::Assignment_t &x) {
        mark(x.m_target);
        BaseWalkVisitor::visit_Assignment(x);
    }
    void visit_Associate(const ASR::Associate_t &x) {
        mark(x.m_target);
        BaseWalkVisitor::visit_Associate(x);
    }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        mark(x.m_head.m_v);
        BaseWalkVisitor::visit_DoLoop(x);
    }
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) mark(x.m_head[i].m_v);
        BaseWalkVisitor::visit_DoConcurrentLoop(x);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        for (size_t i = 0; i < x.n_args; i++) mark(x.m_args[i].m_value);
        BaseWalkVisitor::visit_SubroutineCall(x);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        for (size_t i = 0; i < x.n_args; i++) mark(x.m_args[i].m_value);
        BaseWalkVisitor::visit_FunctionCall(x);
    }
    void visit_FileRead(const ASR::FileRead_t &x) {
        for (size_t i = 0; i < x.n_values; i++) mark(x.m_values[i]);
        BaseWalkVisitor::visit_FileRead(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::Block_t>(*b) || !blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        block_scopes.insert(block->m_symtab);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::AssociateBlock_t>(*b)
                || !blocks.insert(b).second) {
            return;
        }
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(b);
        block_scopes.insert(block->m_symtab);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Whether every reference to `array` is an element of it whose subscripts
// are the same in every iteration.
class InvariantElements : public ASR::BaseWalkVisitor<InvariantElements> {
public:
    ASR::symbol_t *array;
    const std::set<ASR::symbol_t*> &indices;
    const ChangedScalars &changes;
    std::set<ASR::symbol_t*> blocks;
    bool ok = true;

    InvariantElements(ASR::symbol_t *array_,
            const std::set<ASR::symbol_t*> &indices_,
            const ChangedScalars &changes_)
        : array(array_), indices(indices_), changes(changes_) {}

    bool invariant(ASR::expr_t *e) const {
        if (!ASRUtils::is_integer(*ASRUtils::expr_type(e))) return false;
        int64_t value = 0;
        if (ASRUtils::extract_value(ASRUtils::expr_value(e), value)) {
            return true;
        }
        switch (e->type) {
            case ASR::exprType::IntegerConstant:
                return true;
            case ASR::exprType::Var: {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                return !ASRUtils::is_array(v->m_type)
                    && !ASRUtils::is_pointer(v->m_type)
                    && indices.count(s) == 0 && changes.changed.count(s) == 0
                    && changes.block_scopes.count(v->m_parent_symtab) == 0;
            }
            case ASR::exprType::IntegerBinOp: {
                auto *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
                return invariant(b->m_left) && invariant(b->m_right);
            }
            case ASR::exprType::IntegerUnaryMinus:
                return invariant(
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
            case ASR::exprType::Cast: {
                auto *c = ASR::down_cast<ASR::Cast_t>(e);
                return c->m_kind == ASR::cast_kindType::IntegerToInteger
                    && invariant(c->m_arg);
            }
            default:
                return false;
        }
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        if (!names_array(x.m_v, array)) {
            BaseWalkVisitor::visit_ArrayItem(x);
            return;
        }
        for (size_t i = 0; i < x.n_args; i++) {
            const ASR::array_index_t &index = x.m_args[i];
            if (index.m_left != nullptr || index.m_step != nullptr
                    || index.m_right == nullptr
                    || !invariant(index.m_right)) {
                ok = false;
                return;
            }
        }
    }
    void visit_Var(const ASR::Var_t &x) {
        if (ASRUtils::symbol_get_past_external(x.m_v) == array) ok = false;
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::Block_t>(*b) || !blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::AssociateBlock_t>(*b)
                || !blocks.insert(b).second) {
            return;
        }
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

bool same_element(const ASR::ArrayItem_t &a, const ASR::ArrayItem_t &b) {
    if (a.n_args != b.n_args) return false;
    for (size_t i = 0; i < a.n_args; i++) {
        if (!ASRUtils::expr_equal(a.m_args[i].m_right, b.m_args[i].m_right)) {
            return false;
        }
    }
    return true;
}

class ElementReplacer : public ASR::BaseExprReplacer<ElementReplacer> {
public:
    Allocator &al;
    ASR::symbol_t *array;
    std::vector<ASR::ArrayItem_t*> &elements;
    std::vector<ASR::expr_t*> &accumulators;
    const std::function<ASR::expr_t*(size_t)> &new_accumulator;

    ElementReplacer(Allocator &al_, ASR::symbol_t *array_,
            std::vector<ASR::ArrayItem_t*> &elements_,
            std::vector<ASR::expr_t*> &accumulators_,
            const std::function<ASR::expr_t*(size_t)> &new_accumulator_)
        : al(al_), array(array_), elements(elements_),
          accumulators(accumulators_), new_accumulator(new_accumulator_) {}

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        if (!names_array(x->m_v, array)) {
            ASR::BaseExprReplacer<ElementReplacer>::replace_ArrayItem(x);
            return;
        }
        size_t j = 0;
        while (j < elements.size() && !same_element(*elements[j], *x)) j++;
        if (j == elements.size()) {
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.success = true;
            ASR::expr_t *copy = dup.duplicate_expr(
                ASRUtils::EXPR((ASR::asr_t*)x));
            LCOMPILERS_ASSERT(dup.success && copy != nullptr);
            elements.push_back(ASR::down_cast<ASR::ArrayItem_t>(copy));
            accumulators.push_back(new_accumulator(j));
        }
        ASR::Var_t *acc = ASR::down_cast<ASR::Var_t>(accumulators[j]);
        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
            x->base.base.loc, acc->m_v));
    }
};

class ElementReplacerVisitor
        : public ASR::CallReplacerOnExpressionsVisitor<ElementReplacerVisitor> {
public:
    ElementReplacer replacer;

    ElementReplacerVisitor(Allocator &al, ASR::symbol_t *array,
            std::vector<ASR::ArrayItem_t*> &elements,
            std::vector<ASR::expr_t*> &accumulators,
            const std::function<ASR::expr_t*(size_t)> &new_accumulator)
        : replacer(al, array, elements, accumulators, new_accumulator) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        visit_symbol(*x.m_m);
    }
};

// The elements of `array` the statements name, BLOCKs included.
class ArrayElements : public ASR::BaseWalkVisitor<ArrayElements> {
public:
    ASR::symbol_t *array;
    std::vector<ASR::ArrayItem_t*> elements;
    std::set<ASR::symbol_t*> blocks;
    // The names of the array, elements' included.
    size_t names = 0;

    explicit ArrayElements(ASR::symbol_t *array_) : array(array_) {}

    void visit_Var(const ASR::Var_t &x) {
        if (ASRUtils::symbol_get_past_external(x.m_v) == array) names++;
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        if (names_array(x.m_v, array)) {
            elements.push_back(const_cast<ASR::ArrayItem_t*>(&x));
        }
        BaseWalkVisitor::visit_ArrayItem(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::Block_t>(*b) || !blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

bool constant_subscripts(const ASR::ArrayItem_t &x) {
    for (size_t i = 0; i < x.n_args; i++) {
        int64_t value = 0;
        if (x.m_args[i].m_left != nullptr || x.m_args[i].m_step != nullptr
                || x.m_args[i].m_right == nullptr
                || !ASRUtils::extract_value(
                    ASRUtils::expr_value(x.m_args[i].m_right), value)) {
            return false;
        }
    }
    return true;
}

// Whether two elements at constant subscripts are the same element.
bool same_constant_element(const ASR::ArrayItem_t &a,
        const ASR::ArrayItem_t &b) {
    if (a.n_args != b.n_args) return false;
    for (size_t i = 0; i < a.n_args; i++) {
        int64_t x = 0, y = 0;
        if (!ASRUtils::extract_value(ASRUtils::expr_value(
                    a.m_args[i].m_right), x)
                || !ASRUtils::extract_value(ASRUtils::expr_value(
                    b.m_args[i].m_right), y)
                || x != y) {
            return false;
        }
    }
    return true;
}

// Whether the heads of the loop `s` name `array`.
bool heads_name(ASR::stmt_t *s, ASR::symbol_t *array) {
    class Names : public ASR::BaseWalkVisitor<Names> {
    public:
        ASR::symbol_t *array;
        bool found = false;
        explicit Names(ASR::symbol_t *array_) : array(array_) {}
        void visit_Var(const ASR::Var_t &x) {
            if (ASRUtils::symbol_get_past_external(x.m_v) == array) {
                found = true;
            }
        }
    } names(array);
    auto head = [&](const ASR::do_loop_head_t &h) {
        for (ASR::expr_t *e : {h.m_start, h.m_end, h.m_increment}) {
            if (e != nullptr) names.visit_expr(*e);
        }
    };
    if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
        ASR::DoConcurrentLoop_t *d = ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
        for (size_t i = 0; i < d->n_head; i++) head(d->m_head[i]);
        for (size_t i = 0; i < d->n_body; i++) {
            if (heads_name(d->m_body[i], array)) return true;
        }
    } else if (ASR::is_a<ASR::DoLoop_t>(*s)) {
        ASR::DoLoop_t *d = ASR::down_cast<ASR::DoLoop_t>(s);
        head(d->m_head);
        for (size_t i = 0; i < d->n_body; i++) {
            if (heads_name(d->m_body[i], array)) return true;
        }
    }
    return names.found;
}

class ReductionInitializers {
public:
    std::map<const ASR::OMPRegion_t*, std::vector<GpuReductionInitializer>>
        &initializers;
    std::set<ASR::symbol_t*> blocks;

    explicit ReductionInitializers(std::map<const ASR::OMPRegion_t*,
            std::vector<GpuReductionInitializer>> &initializers_)
        : initializers(initializers_) {}

    void list(ASR::stmt_t **body, size_t &n_body) {
        for (size_t i = 0; i < n_body; i++) nested(body[i]);
        size_t kept = 0;
        for (size_t i = 0; i < n_body; i++) {
            body[kept++] = body[i];
            if (!ASR::is_a<ASR::OMPRegion_t>(*body[i])) continue;
            const ASR::OMPRegion_t *region =
                ASR::down_cast<ASR::OMPRegion_t>(body[i]);
            if (region->m_exec_target != ASR::exec_targetType::ExecDevice) {
                continue;
            }
            // The assignments right before the loop, latest first, kept
            // as long as each is one the loop can start from.
            std::vector<GpuReductionInitializer> taken;
            while (kept >= 2) {
                GpuReductionInitializer init;
                if (!initializer(*region, body[kept - 2], taken, init)) break;
                taken.push_back(init);
                body[kept - 2] = body[kept - 1];
                kept--;
            }
            if (!taken.empty()) {
                std::reverse(taken.begin(), taken.end());
                initializers[region] = taken;
            }
        }
        n_body = kept;
    }

private:
    // Whether `s` assigns a constant to an element, at constant subscripts,
    // of an array of a REDUCE of `region` that the loop names, which none
    // of `taken` assigns.
    bool initializer(const ASR::OMPRegion_t &region, ASR::stmt_t *s,
            const std::vector<GpuReductionInitializer> &taken,
            GpuReductionInitializer &init) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded != nullptr || a->m_realloc_lhs
                || !ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
            return false;
        }
        ASR::ArrayItem_t *element = ASR::down_cast<ASR::ArrayItem_t>(
            a->m_target);
        if (!constant_subscripts(*element)) return false;
        ASR::symbol_t *array = nullptr;
        ASR::reduction_opType op = ASR::reduction_opType::ReduceAdd;
        for (size_t c = 0; c < region.n_clauses && array == nullptr; c++) {
            if (region.m_clauses[c]->type
                    != ASR::omp_clauseType::OMPReduction) {
                continue;
            }
            ASR::OMPReduction_t *clause = ASR::down_cast<ASR::OMPReduction_t>(
                region.m_clauses[c]);
            if (clause->m_operator != ASR::reduction_opType::ReduceAdd
                    && clause->m_operator != ASR::reduction_opType::ReduceSub
                    && clause->m_operator != ASR::reduction_opType::ReduceMul
                    && clause->m_operator != ASR::reduction_opType::ReduceMIN
                    && clause->m_operator != ASR::reduction_opType::ReduceMAX) {
                continue;
            }
            for (size_t v = 0; v < clause->n_vars; v++) {
                if (ASR::is_a<ASR::Var_t>(*clause->m_vars[v])
                        && ASRUtils::is_array(ASRUtils::expr_type(
                            clause->m_vars[v]))
                        && names_array(element->m_v,
                            ASRUtils::symbol_get_past_external(
                                ASR::down_cast<ASR::Var_t>(
                                    clause->m_vars[v])->m_v))) {
                    array = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(clause->m_vars[v])->m_v);
                    op = clause->m_operator;
                }
            }
        }
        if (array == nullptr) return false;
        // An element combined with atomic updates starts from what it holds,
        // which the host assigns before the launch.
        if (gpu_reduction_order_free(op, ASRUtils::expr_type(a->m_target))) {
            return false;
        }
        // The host evaluates the bounds of the loop before the launch, after
        // the assignment it no longer runs.
        for (size_t i = 0; i < region.n_body; i++) {
            if (heads_name(region.m_body[i], array)) return false;
        }
        ASR::expr_t *value = ASRUtils::expr_value(a->m_value);
        if (value == nullptr || !ASRUtils::check_equal_type(
                ASRUtils::expr_type(value), ASRUtils::expr_type(a->m_target),
                value, a->m_target)) {
            return false;
        }
        for (const GpuReductionInitializer &t : taken) {
            if (t.array == array && same_constant_element(*t.element,
                    *element)) {
                return false;
            }
        }
        // The loop names the array only as elements at constant subscripts,
        // the element assigned written one way: the reduction then has one
        // item for it, which starts from the value, and no other item
        // combines into it after that one.
        ArrayElements named(array);
        for (size_t i = 0; i < region.n_body; i++) {
            named.visit_stmt(*region.m_body[i]);
        }
        if (named.names != named.elements.size()) return false;
        bool found = false;
        for (ASR::ArrayItem_t *e : named.elements) {
            if (!constant_subscripts(*e)) return false;
            if (same_constant_element(*e, *element)) {
                if (!same_element(*e, *element)) return false;
                found = true;
            }
        }
        if (!found) return false;
        init.assignment = s;
        init.array = array;
        init.element = element;
        init.value = value;
        return true;
    }

    void nested(ASR::stmt_t *s) {
        switch (s->type) {
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                list(x->m_body, x->n_body);
                list(x->m_orelse, x->n_orelse);
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                list(x->m_body, x->n_body);
                list(x->m_orelse, x->n_orelse);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                list(x->m_body, x->n_body);
                list(x->m_orelse, x->n_orelse);
                break;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                for (size_t i = 0; i < x->n_body; i++) {
                    ASR::case_stmt_t *c = x->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(c);
                        list(cs->m_body, cs->n_body);
                    } else if (ASR::is_a<ASR::CaseStmt_Range_t>(*c)) {
                        ASR::CaseStmt_Range_t *cs =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        list(cs->m_body, cs->n_body);
                    }
                }
                list(x->m_default, x->n_default);
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b) || !blocks.insert(b).second) {
                    break;
                }
                ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(b);
                list(x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*b)
                        || !blocks.insert(b).second) {
                    break;
                }
                ASR::AssociateBlock_t *x =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                list(x->m_body, x->n_body);
                break;
            }
            default:
                // The body of a parallel loop is device code, whose
                // assignments the device runs anyway.
                break;
        }
    }
};

void take_initializers_of_scope(SymbolTable *scope,
        ReductionInitializers &taker) {
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
            taker.list(fn->m_body, fn->n_body);
            take_initializers_of_scope(fn->m_symtab, taker);
        } else if (ASR::is_a<ASR::Program_t>(*sym)) {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
            taker.list(p->m_body, p->n_body);
            take_initializers_of_scope(p->m_symtab, taker);
        } else if (ASR::is_a<ASR::Module_t>(*sym)) {
            take_initializers_of_scope(
                ASR::down_cast<ASR::Module_t>(sym)->m_symtab, taker);
        }
    }
}

} // namespace

void gpu_take_reduction_initializers(ASR::TranslationUnit_t &unit,
        std::map<const ASR::OMPRegion_t*, std::vector<GpuReductionInitializer>>
            &initializers) {
    ReductionInitializers taker(initializers);
    take_initializers_of_scope(unit.m_symtab, taker);
}

bool gpu_array_reduction_elements_invariant(ASR::symbol_t *array,
        ASR::stmt_t **body, size_t n_body,
        const std::set<ASR::symbol_t*> &indices) {
    array = ASRUtils::symbol_get_past_external(array);
    ChangedScalars changes;
    for (size_t i = 0; i < n_body; i++) changes.visit_stmt(*body[i]);
    InvariantElements elements(array, indices, changes);
    for (size_t i = 0; i < n_body && elements.ok; i++) {
        elements.visit_stmt(*body[i]);
    }
    return elements.ok;
}

void gpu_replace_array_reduction_elements(Allocator &al,
        ASR::symbol_t *array, ASR::stmt_t **body, size_t n_body,
        std::vector<ASR::ArrayItem_t*> &elements,
        std::vector<ASR::expr_t*> &accumulators,
        const std::function<ASR::expr_t*(size_t)> &new_accumulator) {
    ElementReplacerVisitor v(al, ASRUtils::symbol_get_past_external(array),
        elements, accumulators, new_accumulator);
    for (size_t i = 0; i < n_body; i++) v.visit_stmt(*body[i]);
}

} // namespace LCompilers
