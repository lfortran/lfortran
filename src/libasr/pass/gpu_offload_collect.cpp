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
        size_t n_body) {
    std::vector<ASR::Function_t*> order;
    std::set<ASR::Function_t*> seen;
    std::deque<ASR::Function_t*> work;
    for (ASR::Function_t *fn : gpu_callees(body, n_body, false)) {
        if (seen.insert(fn).second) work.push_back(fn);
    }
    while (!work.empty()) {
        ASR::Function_t *fn = work.front();
        work.pop_front();
        order.push_back(fn);
        for (ASR::Function_t *callee : gpu_callees(fn->m_body,
                fn->n_body, false)) {
            if (seen.insert(callee).second) work.push_back(callee);
        }
    }
    return order;
}

} // namespace LCompilers
