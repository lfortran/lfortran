#include <deque>

#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_rewrite.h>

namespace LCompilers {

bool expr_has_function_call(ASR::expr_t *expr) {
    if (!expr) return false;
    ContainsFunctionCall checker;
    checker.visit_expr(*expr);
    return checker.found;
}

bool gpu_is_array_function_result(ASR::expr_t *e) {
    if (!e) return false;
    e = ASRUtils::get_past_array_physical_cast(e);
    if (!ASR::is_a<ASR::FunctionCall_t>(*e)) return false;
    ASR::FunctionCall_t *fc = ASR::down_cast<ASR::FunctionCall_t>(e);
    if (ASRUtils::is_elemental(fc->m_name)) return false;
    if (!ASRUtils::is_array(fc->m_type)) return false;
    ASR::ttype_t *elem = ASRUtils::extract_type(fc->m_type);
    return ASR::is_a<ASR::Integer_t>(*elem) || ASR::is_a<ASR::Real_t>(*elem)
        || ASR::is_a<ASR::Complex_t>(*elem)
        || ASR::is_a<ASR::Logical_t>(*elem)
        || ASR::is_a<ASR::StructType_t>(*elem);
}

void gpu_check_host_expr_index_free(ASR::expr_t *host_expr,
        const std::set<std::string> &index_names, const std::string &what) {
    if (!host_expr || index_names.empty()) return;
    GpuVarNameCollector c;
    c.visit_expr(*host_expr);
    for (const std::string &n : c.names) {
        if (index_names.count(n)) {
            throw LCompilersException(
                "GPU offload: the host expression for " + what +
                " refers to the loop index '" + n + "', which has no "
                "value outside the loop");
        }
    }
}

bool is_single_assignment_binding(ASR::symbol_t *sym,
        ASR::stmt_t **body, size_t n_body) {
    AssignmentTargetCounter counter(sym);
    for (size_t i = 0; i < n_body; i++) {
        counter.visit_stmt(*body[i]);
    }
    return counter.count == 1;
}

std::vector<ASR::Function_t*> reachable_routines(ASR::stmt_t **body,
        size_t n_body, GpuProcedureImplementations &implementations) {
    std::vector<ASR::Function_t*> order;
    std::set<ASR::Function_t*> seen;
    std::deque<ASR::Function_t*> work;
    auto reach = [&](ASR::Function_t *fn) {
        fn = implementations.implementation(fn);
        if (seen.insert(fn).second) work.push_back(fn);
    };
    // A routine handed over as an actual argument is reached too: whoever
    // receives it calls it.
    for (ASR::Function_t *fn : gpu_callees(body, n_body, true)) reach(fn);
    while (!work.empty()) {
        ASR::Function_t *fn = work.front();
        work.pop_front();
        order.push_back(fn);
        for (ASR::Function_t *callee : gpu_callees(fn->m_body,
                fn->n_body, true)) {
            reach(callee);
        }
    }
    return order;
}

ASR::expr_t* gpu_liveout_slot(Allocator &al, const Location &loc,
        ASR::symbol_t *buffer, ASR::ttype_t *scalar_type) {
    ASR::array_index_t index;
    index.loc = loc;
    index.m_left = nullptr;
    index.m_right = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
        ASR::integerbozType::Decimal));
    index.m_step = nullptr;
    Vec<ASR::array_index_t> indices;
    indices.reserve(al, 1);
    indices.push_back(al, index);
    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
        ASRUtils::EXPR(ASR::make_Var_t(al, loc, buffer)), indices.p, 1,
        ASRUtils::duplicate_type(al, scalar_type),
        ASR::arraystorageType::ColMajor, nullptr, false));
}

namespace {

// gpu_call_dummy for a call to `name` with `n_args` actual arguments and
// the object `dt` of a type-bound call.
ASR::Variable_t *gpu_call_dummy_of(ASR::symbol_t *name, size_t n_args,
        ASR::expr_t *dt, size_t i, bool *not_passed) {
    if (not_passed != nullptr) *not_passed = false;
    ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
    // A type-bound call passes its object as the dummy argument the
    // binding names, the first one unless PASS names another.
    bool passes = false, nopass = false;
    const char *self_name = nullptr;
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) {
        ASR::StructMethodDeclaration_t *m =
            ASR::down_cast<ASR::StructMethodDeclaration_t>(callee);
        passes = dt != nullptr && !m->m_is_nopass;
        nopass = m->m_is_nopass;
        self_name = m->m_self_argument;
        callee = ASRUtils::symbol_get_past_external(m->m_proc);
    }
    if (callee == nullptr || !ASR::is_a<ASR::Function_t>(*callee)) {
        return nullptr;
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(callee);
    size_t self = 0;
    if (passes && self_name != nullptr) {
        for (size_t j = 0; j < f->n_args; j++) {
            if (ASR::is_a<ASR::Var_t>(*f->m_args[j]) && std::string(
                    ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(
                        f->m_args[j])->m_v)) == self_name) {
                self = j;
            }
        }
    }
    size_t k;
    if (i == n_args) {
        if (!passes) {
            // The object of a NOPASS binding is not an argument.
            if (not_passed != nullptr && nopass && dt != nullptr) {
                *not_passed = true;
            }
            return nullptr;
        }
        k = self;
    } else if (n_args == f->n_args) {
        // The actual arguments include the passed object.
        k = i;
    } else if (passes && n_args + 1 == f->n_args) {
        k = i < self ? i : i + 1;
    } else {
        return nullptr;
    }
    if (k >= f->n_args || !ASR::is_a<ASR::Var_t>(*f->m_args[k])) {
        return nullptr;
    }
    ASR::symbol_t *dummy = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(f->m_args[k])->m_v);
    return ASR::is_a<ASR::Variable_t>(*dummy)
        ? ASR::down_cast<ASR::Variable_t>(dummy) : nullptr;
}

} // namespace

ASR::Variable_t *gpu_call_dummy(const ASR::SubroutineCall_t &x, size_t i,
        bool *not_passed) {
    return gpu_call_dummy_of(x.m_name, x.n_args, x.m_dt, i, not_passed);
}

ASR::Variable_t *gpu_call_dummy(const ASR::FunctionCall_t &x, size_t i,
        bool *not_passed) {
    return gpu_call_dummy_of(x.m_name, x.n_args, x.m_dt, i, not_passed);
}

bool gpu_call_object_is_argument(const ASR::SubroutineCall_t &x) {
    if (x.m_dt == nullptr) return false;
    ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(x.m_name);
    if (!ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) return false;
    ASR::StructMethodDeclaration_t *m =
        ASR::down_cast<ASR::StructMethodDeclaration_t>(callee);
    ASR::symbol_t *proc = ASRUtils::symbol_get_past_external(m->m_proc);
    return !m->m_is_nopass && proc != nullptr
        && ASR::is_a<ASR::Function_t>(*proc)
        && ASR::down_cast<ASR::Function_t>(proc)->n_args == x.n_args;
}

bool gpu_call_may_define(const ASR::SubroutineCall_t &x, size_t i) {
    bool not_passed;
    ASR::Variable_t *v = gpu_call_dummy(x, i, &not_passed);
    if (not_passed) return false;
    if (v == nullptr) return true;
    return v->m_intent != ASR::intentType::In && !v->m_value_attr;
}

bool gpu_call_defines(const ASR::SubroutineCall_t &x, size_t i) {
    ASR::Variable_t *v = gpu_call_dummy(x, i);
    return v != nullptr && v->m_intent == ASR::intentType::Out;
}

ASR::symbol_t *gpu_defined_variable(ASR::expr_t *e) {
    while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
    }
    // An allocatable or pointer array component handed over whole: what
    // is defined is its elements.
    if (!ASR::is_a<ASR::Var_t>(*e) && ASRUtils::is_array(
            ASRUtils::expr_type(e)) && ASRUtils::is_allocatable_or_pointer(
                ASRUtils::expr_type(e))) {
        return nullptr;
    }
    while (true) {
        ASR::expr_t *base = nullptr;
        if (ASR::is_a<ASR::Var_t>(*e)) {
            if (ASRUtils::is_array(ASRUtils::expr_type(e))) return nullptr;
            return ASR::down_cast<ASR::Var_t>(e)->m_v;
        } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            base = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            base = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
        } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            base = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
        } else {
            return nullptr;
        }
        // A part of what an allocatable or a pointer designates is not
        // stored in the variable.
        if (!ASR::is_a<ASR::Var_t>(*base) &&
                ASRUtils::is_allocatable_or_pointer(
                    ASRUtils::expr_type(base))) {
            return nullptr;
        }
        if (ASR::is_a<ASR::Var_t>(*base) && ASRUtils::is_array(
                ASRUtils::expr_type(base))) {
            return nullptr;
        }
        e = base;
    }
}

namespace {

// Where the base of a designator is: the operand a component, an element
// or a section is taken from.
ASR::expr_t **gpu_designator_base_slot(ASR::expr_t *e) {
    if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        return &ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
    } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
        return &ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
    } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        return &ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
    }
    return nullptr;
}

// An actual argument as the designator it is, past the change of
// descriptor it may be passed through.
ASR::expr_t *gpu_actual_designator(ASR::expr_t *e) {
    while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
    }
    return e;
}

} // namespace

GpuLiveoutWriteBack::GpuLiveoutWriteBack(Allocator &al_)
        : PassUtils::PassVisitor<GpuLiveoutWriteBack>(al_, nullptr) {
    pass_result.reserve(al, 4);
}

namespace {

// Whether a statement can be left before its end other than by finishing
// it.
class GpuEarlyExitFinder
        : public ASRUtils::BlockBodyWalkVisitor<GpuEarlyExitFinder> {
public:
    bool found = false;
    void visit_Cycle(const ASR::Cycle_t &) { found = true; }
    void visit_Exit(const ASR::Exit_t &) { found = true; }
    void visit_Return(const ASR::Return_t &) { found = true; }
    void visit_GoTo(const ASR::GoTo_t &) { found = true; }
    void visit_Stop(const ASR::Stop_t &) { found = true; }
    void visit_ErrorStop(const ASR::ErrorStop_t &) { found = true; }
};

// Whether a designator names the whole of what it is a part of or a
// component of it: no element or section, which can name another part on
// the next trip of a loop.
bool gpu_component_designator(ASR::expr_t *e) {
    while (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
    }
    return ASR::is_a<ASR::Var_t>(*e);
}

} // namespace

void GpuLiveoutWriteBack::run(ASR::Function_t &kernel) {
    kernel_scope = kernel.m_symtab;
    Vec<ASR::stmt_t*> body;
    body.reserve(al, kernel.n_body);
    for (size_t i = 0; i < kernel.n_body; i++) {
        ASR::stmt_t *s = kernel.m_body[i];
        GpuEarlyExitFinder exits;
        exits.visit_stmt(*s);
        deferring = !exits.found;
        loops = 0;
        deferred.clear();
        pass_result.n = 0;
        visit_stmt(*s);
        if (pass_result.size() > 0) {
            for (size_t j = 0; j < pass_result.size(); j++) {
                body.push_back(al, pass_result[j]);
            }
            pass_result.n = 0;
        } else {
            body.push_back(al, s);
        }
        if (deferred.empty()) continue;
        // Each flag is cleared before the statement and tested after it.
        Vec<ASR::stmt_t*> with_flags;
        with_flags.reserve(al, body.size() + 2 * deferred.size());
        const Location &loc = s->base.loc;
        ASR::ttype_t *logical = ASRUtils::TYPE(ASR::make_Logical_t(al, loc,
            4));
        for (size_t j = 0; j + 1 < body.size(); j++) {
            with_flags.push_back(al, body[j]);
        }
        for (auto &d : deferred) {
            with_flags.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
                al, loc, ASRUtils::EXPR(ASR::make_Var_t(al, loc, d.first)),
                ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, false,
                    logical)), nullptr, false, false, false)));
        }
        with_flags.push_back(al, body[body.size() - 1]);
        for (auto &d : deferred) {
            Vec<ASR::stmt_t*> then;
            then.reserve(al, 1);
            then.push_back(al, d.second);
            with_flags.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc,
                nullptr, ASRUtils::EXPR(ASR::make_Var_t(al, loc, d.first)),
                then.p, then.n, nullptr, 0)));
        }
        body = with_flags;
    }
    kernel.m_body = body.p;
    kernel.n_body = body.size();
    // The BLOCK and ASSOCIATE constructs of the kernel, whose statements
    // are not statements of the kernel body. A procedure the kernel calls
    // defines its own dummy arguments; the call is what is put back after.
    deferring = false;
    for (auto &item : kernel.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Block_t>(*item.second)) {
            visit_Block(*ASR::down_cast<ASR::Block_t>(item.second));
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
            visit_AssociateBlock(
                *ASR::down_cast<ASR::AssociateBlock_t>(item.second));
        }
    }
}

ASR::symbol_t *GpuLiveoutWriteBack::copy_of(ASR::expr_t *designator) {
    ASR::expr_t *root = designator;
    while (ASR::expr_t **base = gpu_designator_base_slot(root)) root = *base;
    if (!ASR::is_a<ASR::Var_t>(*root)) return nullptr;
    ASR::symbol_t *copy = ASR::down_cast<ASR::Var_t>(root)->m_v;
    return shared.count(copy) ? copy : nullptr;
}

ASR::stmt_t *GpuLiveoutWriteBack::put_back(const Location &loc,
        ASR::expr_t *designator, ASR::symbol_t *copy) {
    ASRUtils::ExprStmtDuplicator dup(al);
    ASR::expr_t *target = dup.duplicate_expr(designator);
    ASR::expr_t **at = &target;
    while (ASR::expr_t **base = gpu_designator_base_slot(*at)) at = base;
    *at = dup.duplicate_expr(shared[copy]);
    return ASRUtils::STMT(ASR::make_Assignment_t(al, loc, target,
        dup.duplicate_expr(designator), nullptr, false, false, false));
}

void GpuLiveoutWriteBack::define(const Location &loc,
        ASR::expr_t *designator, ASR::symbol_t *copy,
        std::vector<ASR::stmt_t*> &after) {
    if (!deferring || loops == 0 || kernel_scope == nullptr
            || !gpu_component_designator(designator)) {
        after.push_back(put_back(loc, designator, copy));
        return;
    }
    ASR::symbol_t *flag = gpu_new_variable(al, loc, kernel_scope,
        kernel_scope->get_unique_name("__gpu_defined"),
        ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)));
    after.push_back(ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
        ASRUtils::EXPR(ASR::make_Var_t(al, loc, flag)),
        ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, true,
            ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)))),
        nullptr, false, false, false)));
    deferred.push_back({flag, put_back(loc, designator, copy)});
}

void GpuLiveoutWriteBack::visit_Assignment(const ASR::Assignment_t &x) {
    if (entries.count(const_cast<ASR::stmt_t*>(&x.base))) {
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            shared[ASR::down_cast<ASR::Var_t>(x.m_target)->m_v] =
                x.m_value;
        }
        return;
    }
    ASR::symbol_t *copy = copy_of(x.m_target);
    if (copy == nullptr) return;
    std::vector<ASR::stmt_t*> after;
    define(x.base.base.loc, x.m_target, copy, after);
    pass_result.push_back(al, const_cast<ASR::stmt_t*>(&x.base));
    for (ASR::stmt_t *s : after) pass_result.push_back(al, s);
}

namespace {

// Whether a call changed the scalar `a`, which was `b` before it: `a /= b`,
// except that a NaN that stays a NaN is not a change, which every thread
// would otherwise put back over the value the defining iteration gave.
// A zero that changes sign compares equal and is not put back; the GPU
// code has no way of comparing the bits of a real yet. Device code compiled
// with fast math, as Metal compiles it by default, assumes there is no NaN,
// and may then take a NaN left as it was for a change.
ASR::expr_t *gpu_differs(Allocator &al, const Location &loc, ASR::expr_t *a,
        ASR::expr_t *b) {
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(a));
    ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc,
        4));
    ASRUtils::ExprStmtDuplicator dup(al);
    auto compare = [&](ASR::expr_t *x, ASR::cmpopType op, ASR::expr_t *y) {
        x = dup.duplicate_expr(x);
        y = dup.duplicate_expr(y);
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            return ASRUtils::EXPR(ASR::make_ComplexCompare_t(al, loc, x, op,
                y, logical_type, nullptr));
        }
        return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc, x, op, y,
            logical_type, nullptr));
    };
    ASR::cmpopType ne = ASR::cmpopType::NotEq;
    switch (t->type) {
        case ASR::ttypeType::Integer:
            return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc, a, ne,
                b, logical_type, nullptr));
        case ASR::ttypeType::Logical:
            return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al, loc, a, ne,
                b, logical_type, nullptr));
        default: {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*t)
                || ASR::is_a<ASR::Complex_t>(*t));
            // a /= b .and. (a == a .or. b == b)
            ASR::expr_t *not_both_nan = ASRUtils::EXPR(
                ASR::make_LogicalBinOp_t(al, loc,
                    compare(a, ASR::cmpopType::Eq, a),
                    ASR::logicalbinopType::Or,
                    compare(b, ASR::cmpopType::Eq, b), logical_type,
                    nullptr));
            return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
                compare(a, ne, b), ASR::logicalbinopType::And, not_both_nan,
                logical_type, nullptr));
        }
    }
}

// The scalar parts of `designator`: itself when it is a scalar of an
// intrinsic type, else each scalar component, or component of one. A
// thread keeps a copy of a whole derived-type value only of a type
// without array components that a procedure may define.
void gpu_scalar_parts(Allocator &al, ASR::expr_t *designator,
        std::vector<ASR::expr_t*> &parts) {
    ASR::ttype_t *t = ASRUtils::expr_type(designator);
    if (!ASR::is_a<ASR::StructType_t>(*t)) {
        parts.push_back(designator);
        return;
    }
    ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(
        ASRUtils::get_struct_sym_from_struct_expr(designator));
    for (ASR::Struct_t *st = gpu_struct_definition(struct_sym); st;
            st = st->m_parent ? gpu_struct_definition(st->m_parent)
                : nullptr) {
        for (size_t i = 0; i < st->n_members; i++) {
            ASR::symbol_t *member = st->m_symtab->get_symbol(
                st->m_members[i]);
            if (!member || !ASR::is_a<ASR::Variable_t>(*member)) continue;
            ASRUtils::ExprStmtDuplicator dup(al);
            gpu_scalar_parts(al, ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, designator->base.loc,
                    dup.duplicate_expr(designator), member,
                    ASRUtils::duplicate_type(al, ASRUtils::symbol_type(
                        member)), nullptr)), parts);
        }
    }
}

} // namespace

// A procedure may leave an actual argument it may define as it was, and
// another thread may define what it designates in the meantime, so each
// scalar of it is put back only when the call changed it; only what an
// `intent(out)` dummy argument is associated with is put back whole.
void GpuLiveoutWriteBack::visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
    const Location &loc = x.base.base.loc;
    std::vector<ASR::stmt_t*> before, after;
    auto actual = [&](ASR::expr_t *a, bool defined) {
        a = gpu_actual_designator(a);
        ASR::symbol_t *copy = copy_of(a);
        if (copy == nullptr) return;
        if (defined) {
            define(loc, a, copy, after);
            return;
        }
        std::vector<ASR::expr_t*> parts;
        gpu_scalar_parts(al, a, parts);
        for (ASR::expr_t *part : parts) {
            ASRUtils::ExprStmtDuplicator dup(al);
            ASR::symbol_t *saved = gpu_new_variable(al, loc, kernel_scope,
                kernel_scope->get_unique_name("__gpu_before"),
                ASRUtils::duplicate_type(al, ASRUtils::expr_type(part)));
            ASR::expr_t *saved_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                saved));
            before.push_back(ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
                saved_var, dup.duplicate_expr(part), nullptr, false, false,
                false)));
            std::vector<ASR::stmt_t*> changed;
            define(loc, part, copy, changed);
            Vec<ASR::stmt_t*> then;
            then.reserve(al, changed.size());
            for (ASR::stmt_t *s : changed) then.push_back(al, s);
            after.push_back(ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr,
                gpu_differs(al, loc, dup.duplicate_expr(part),
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, saved))),
                then.p, then.n, nullptr, 0)));
        }
    };
    for (size_t i = 0; i < x.n_args; i++) {
        if (x.m_args[i].m_value && gpu_call_may_define(x, i)) {
            actual(x.m_args[i].m_value, gpu_call_defines(x, i));
        }
    }
    if (x.m_dt != nullptr && !gpu_call_object_is_argument(x)
            && gpu_call_may_define(x, x.n_args)) {
        actual(x.m_dt, false);
    }
    if (after.empty()) return;
    for (ASR::stmt_t *s : before) pass_result.push_back(al, s);
    pass_result.push_back(al, const_cast<ASR::stmt_t*>(&x.base));
    for (ASR::stmt_t *s : after) pass_result.push_back(al, s);
}

void GpuLiveoutWriteBack::visit_DoLoop(const ASR::DoLoop_t &x) {
    loops++;
    PassUtils::PassVisitor<GpuLiveoutWriteBack>::visit_DoLoop(x);
    loops--;
}

void GpuLiveoutWriteBack::visit_WhileLoop(const ASR::WhileLoop_t &x) {
    loops++;
    PassUtils::PassVisitor<GpuLiveoutWriteBack>::visit_WhileLoop(x);
    loops--;
}

void GpuLiveoutWriteBack::visit_DoConcurrentLoop(
        const ASR::DoConcurrentLoop_t &x) {
    loops++;
    PassUtils::PassVisitor<GpuLiveoutWriteBack>::visit_DoConcurrentLoop(x);
    loops--;
}

namespace {

bool gpu_struct_has_derived_component(ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = gpu_struct_definition(struct_sym);
    if (!st) return false;
    if (st->m_parent && gpu_struct_has_derived_component(st->m_parent)) {
        return true;
    }
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) continue;
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(
                ASRUtils::symbol_type(member)))) {
            return true;
        }
    }
    return false;
}

} // namespace

bool gpu_no_kernel_local_struct(SymbolTable *scope,
        const std::string &name, ASR::ttype_t *type) {
    if (ASRUtils::is_array(type) || ASRUtils::is_class_type(type)
            || ASRUtils::is_allocatable_or_pointer(type)
            || !ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
        return false;
    }
    ASR::symbol_t *sym = scope->resolve_symbol(name);
    if (sym == nullptr) return false;
    sym = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
    ASR::symbol_t *struct_sym =
        ASR::down_cast<ASR::Variable_t>(sym)->m_type_declaration;
    return gpu_struct_has_allocatable_parts(struct_sym)
        && gpu_struct_has_derived_component(struct_sym);
}

namespace {

// The procedure or main program whose body holds the statements of
// `scope`, past the BLOCK and ASSOCIATE constructs around them.
ASR::symbol_t *gpu_procedure_of(SymbolTable *scope) {
    for (; scope && scope->asr_owner; scope = scope->parent) {
        if (scope->asr_owner->type != ASR::asrType::symbol) return nullptr;
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            scope->asr_owner);
        if (ASR::is_a<ASR::Program_t>(*owner)
                || ASR::is_a<ASR::Function_t>(*owner)) {
            return owner;
        }
        if (!ASR::is_a<ASR::Block_t>(*owner)
                && !ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
            return nullptr;
        }
    }
    return nullptr;
}

class GpuLoopFinder : public ASRUtils::BlockBodyWalkVisitor<GpuLoopFinder> {
public:
    const ASR::OMPRegion_t *loop;
    bool found = false;
    GpuLoopFinder(const ASR::OMPRegion_t *loop_) : loop(loop_) {}
    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (&x == loop) {
            found = true;
            return;
        }
        ASR::BaseWalkVisitor<GpuLoopFinder>::visit_OMPRegion(x);
    }
};

// Adds to `names` what may be referenced once `loop` has run, when one of
// `stmts` holds it, and says whether one does.
bool gpu_collect_after_loop(ASR::stmt_t **stmts, size_t n,
        const ASR::OMPRegion_t &loop, std::set<std::string> &names) {
    for (size_t i = 0; i < n; i++) {
        GpuLoopFinder finder(&loop);
        finder.visit_stmt(*stmts[i]);
        if (!finder.found) continue;
        ASR::stmt_t *s = stmts[i];
        if (s != &loop.base) {
            // Only the branch of an IF or the body of a BLOCK or ASSOCIATE
            // holding the loop is followed into; any other construct, a
            // loop around it in particular, can run its statements again
            // after the loop, so all of them count.
            bool inner = false;
            if (ASR::is_a<ASR::If_t>(*s)) {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                inner = gpu_collect_after_loop(x->m_body, x->n_body, loop,
                        names)
                    || gpu_collect_after_loop(x->m_orelse, x->n_orelse,
                        loop, names);
            } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*m)) {
                    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(m);
                    inner = gpu_collect_after_loop(b->m_body, b->n_body,
                        loop, names);
                }
            } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*s)) {
                ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::AssociateBlock_t>(*m)) {
                    ASR::AssociateBlock_t *b =
                        ASR::down_cast<ASR::AssociateBlock_t>(m);
                    inner = gpu_collect_after_loop(b->m_body, b->n_body,
                        loop, names);
                }
            }
            if (!inner) {
                PostLoopVarCollector c(names, &loop);
                c.visit_stmt(*s);
            }
        }
        PostLoopVarCollector c(names, &loop);
        for (size_t j = i + 1; j < n; j++) {
            c.visit_stmt(*stmts[j]);
        }
        return true;
    }
    return false;
}

class GpuGoToFinder : public ASRUtils::BlockBodyWalkVisitor<GpuGoToFinder> {
public:
    bool found = false;
    void visit_GoTo(const ASR::GoTo_t &) { found = true; }
};

// Adds to `names` what the procedures contained in `scope` reference,
// any of which a statement after a loop may call.
void gpu_collect_contained(SymbolTable *scope, std::set<std::string> &names) {
    for (auto &item : scope->get_scope()) {
        if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(item.second);
        PostLoopVarCollector c(names);
        for (size_t i = 0; i < f->n_body; i++) c.visit_stmt(*f->m_body[i]);
        gpu_collect_contained(f->m_symtab, names);
    }
}

} // namespace

std::set<std::string> gpu_names_referenced_after_loop(
        const ASR::OMPRegion_t &loop, SymbolTable *scope, bool &everything) {
    std::set<std::string> names;
    everything = false;
    ASR::symbol_t *proc = gpu_procedure_of(scope);
    if (proc == nullptr) return names;
    ASR::stmt_t **body;
    size_t n_body;
    if (ASR::is_a<ASR::Program_t>(*proc)) {
        ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(proc);
        body = p->m_body;
        n_body = p->n_body;
    } else {
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(proc);
        body = f->m_body;
        n_body = f->n_body;
    }
    GpuGoToFinder jumps;
    for (size_t i = 0; i < n_body && !jumps.found; i++) {
        jumps.visit_stmt(*body[i]);
    }
    if (jumps.found) {
        everything = true;
        return names;
    }
    gpu_collect_after_loop(body, n_body, loop, names);
    gpu_collect_contained(ASRUtils::symbol_symtab(proc), names);
    return names;
}

bool gpu_visible_outside_procedure(ASR::symbol_t *sym, SymbolTable *scope) {
    if (sym == nullptr) return false;
    if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) return true;
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
    if (v->m_intent != ASR::intentType::Local) return true;
    ASR::symbol_t *proc = gpu_procedure_of(scope);
    if (proc == nullptr) return true;
    if (v->m_storage == ASR::storage_typeType::Save
            && ASR::is_a<ASR::Function_t>(*proc)) {
        return true;
    }
    SymbolTable *proc_scope = ASRUtils::symbol_symtab(proc);
    for (SymbolTable *t = v->m_parent_symtab; t != proc_scope;
            t = t->parent) {
        if (t == nullptr || t->asr_owner == nullptr
                || t->asr_owner->type != ASR::asrType::symbol) {
            return true;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(t->asr_owner);
        if (!ASR::is_a<ASR::Block_t>(*owner)
                && !ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
            return true;
        }
    }
    return false;
}

std::set<ASR::Function_t*> gpu_parallel_loop_routines(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options) {
    std::set<ASR::Function_t*> routines;
    GpuParallelRegionCollector regions;
    regions.visit_TranslationUnit(unit);
    GpuProcedureImplementations implementations(al, unit, pass_options);
    for (ASR::OMPRegion_t *r : regions.loops) {
        for (ASR::Function_t *fn : reachable_routines(r->m_body, r->n_body,
                implementations)) {
            routines.insert(fn);
        }
    }
    return routines;
}

} // namespace LCompilers
