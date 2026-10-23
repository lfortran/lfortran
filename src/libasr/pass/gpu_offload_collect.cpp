#include <deque>

#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_offload_collect.h>

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
    for (ASR::Function_t *fn : gpu_callees(body, n_body, false)) reach(fn);
    while (!work.empty()) {
        ASR::Function_t *fn = work.front();
        work.pop_front();
        order.push_back(fn);
        for (ASR::Function_t *callee : gpu_callees(fn->m_body,
                fn->n_body, false)) {
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

bool gpu_call_may_define(const ASR::SubroutineCall_t &x, size_t i) {
    // Which dummy argument a type-bound call associates an actual
    // argument with depends on the passed object; it is not worked out.
    if (x.m_dt != nullptr) return true;
    ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(x.m_name);
    if (!ASR::is_a<ASR::Function_t>(*callee)) return true;
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(callee);
    if (i >= f->n_args || !ASR::is_a<ASR::Var_t>(*f->m_args[i])) return true;
    ASR::symbol_t *dummy = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*dummy)) return true;
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(dummy);
    return v->m_intent != ASR::intentType::In && !v->m_value_attr;
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
    pass_result.push_back(al, const_cast<ASR::stmt_t*>(&x.base));
    pass_result.push_back(al, put_back(x.base.base.loc, x.m_target, copy));
}

void GpuLiveoutWriteBack::visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
    std::vector<ASR::stmt_t*> after;
    for (size_t i = 0; i < x.n_args; i++) {
        ASR::expr_t *a = x.m_args[i].m_value;
        if (a == nullptr || !gpu_call_may_define(x, i)) continue;
        a = gpu_actual_designator(a);
        if (ASR::symbol_t *copy = copy_of(a)) {
            after.push_back(put_back(x.base.base.loc, a, copy));
        }
    }
    if (x.m_dt != nullptr) {
        ASR::expr_t *a = gpu_actual_designator(x.m_dt);
        if (ASR::symbol_t *copy = copy_of(a)) {
            after.push_back(put_back(x.base.base.loc, a, copy));
        }
    }
    if (after.empty()) return;
    pass_result.push_back(al, const_cast<ASR::stmt_t*>(&x.base));
    for (ASR::stmt_t *s : after) pass_result.push_back(al, s);
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
