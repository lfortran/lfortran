// What a parallel loop defines and the host reads after it.
//
// A variable of unspecified locality that only one iteration defines has
// that value after the loop (Fortran 2018, 11.1.7.5), and one that an
// iteration defines before it references it is a temporary of that
// iteration. The threads of a kernel run the iterations at the same time,
// so a variable all of them read and write in place would serve neither:
// one thread could read what another wrote in between. Each thread keeps
// a private copy of the variable instead, which it takes from where the
// value is shared on entry, and after each statement that defines the
// copy or a part of it puts that part back (GpuLiveoutWriteBack). A
// scalar is shared in a one-element buffer the launch copies the value
// into and back out of.

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>

namespace LCompilers {

namespace {

// The kind of scalar a one-element buffer can carry back to the host, or
// what to call the one `type` is not.
std::string gpu_liveout_scalar_unsupported(ASR::ttype_t *type) {
    if (ASRUtils::is_allocatable(type)) return "allocatable";
    if (ASRUtils::is_pointer(type)) return "pointer";
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(type);
    if (ASR::is_a<ASR::Integer_t>(*t) || ASR::is_a<ASR::Real_t>(*t)
            || ASR::is_a<ASR::Logical_t>(*t)) {
        return "";
    }
    if (ASR::is_a<ASR::Complex_t>(*t)) return gpu_scalar_type_name(t);
    if (ASR::is_a<ASR::String_t>(*t)) return "character";
    return "non-numeric";
}

} // namespace

bool GpuOffloadVisitor::plan_liveouts(const Location &loc,
        const std::set<std::string> &assigned_vars,
        const std::set<std::string> &loop_var_set,
        const std::set<std::string> &reduction_names,
        std::set<std::string> &local_scalar_names,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>>
            &involved_syms,
        GpuLiveoutPlan &plan) {
    std::vector<std::string> scalars;
    for (auto &name : assigned_vars) {
        if (loop_var_set.count(name)) continue;
        if (local_scalar_names.count(name)) continue;
        // A reduction accumulator is written by every thread. One shared
        // slot would be a race, so it gets a slot per thread instead.
        if (reduction_names.count(name)) continue;
        auto it = involved_syms.find(name);
        if (it == involved_syms.end()) continue;
        ASR::ttype_t *type = it->second.first;
        if (ASRUtils::is_array(type)) continue;
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
            report_not_offloaded(loc, GpuDecline(
                GpuDeclineReason::LiveoutDerivedType, name));
            return false;
        }
        std::string unsupported = gpu_liveout_scalar_unsupported(type);
        if (!unsupported.empty()) {
            GpuDecline decline(GpuDeclineReason::LiveoutScalar, name);
            decline.type_name = unsupported;
            report_not_offloaded(loc, decline);
            return false;
        }
        scalars.push_back(name);
    }

    // The kernel takes the buffer, and the private copy is a local of it
    // under the scalar's own name, which the body names.
    ASR::ttype_t *int4_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    for (auto &name : scalars) {
        auto it = involved_syms.find(name);
        ASR::ttype_t *scalar_type = it->second.first;
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
            int4_type, ASR::integerbozType::Decimal));
        dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            1, int4_type, ASR::integerbozType::Decimal));
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        dims.push_back(al, dim);
        ASR::ttype_t *buffer_type = ASRUtils::TYPE(ASR::make_Array_t(al,
            loc, ASRUtils::duplicate_type(al, scalar_type), dims.p, 1,
            ASR::array_physical_typeType::FixedSizeArray,
            ASR::memory_spaceType::Global));
        std::string buf_name = current_scope->get_unique_name(
            "__gpu_buf_" + name);
        ASR::symbol_t *buf_sym = gpu_new_variable(al, loc, current_scope,
            buf_name, ASRUtils::duplicate_type(al, buffer_type));
        involved_syms.erase(it);
        involved_syms[buf_name] = {buffer_type,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, buf_sym))};
        plan.scalars.push_back({name, buf_name, buf_sym,
            current_scope->resolve_symbol(name), scalar_type});
    }
    return true;
}

void GpuOffloadVisitor::declare_liveout_copies(const Location &loc,
        SymbolTable *kernel_scope, GpuLiveoutPlan &plan) {
    for (auto &ls : plan.scalars) {
        gpu_new_variable(al, loc, kernel_scope, ls.orig_name,
            ASRUtils::duplicate_type(al, ls.scalar_type));
    }
}

void GpuOffloadVisitor::enter_liveouts(const Location &loc,
        SymbolTable *kernel_scope, Vec<ASR::stmt_t*> &kernel_body,
        GpuLiveoutPlan &plan) {
    for (auto &ls : plan.scalars) {
        ASR::stmt_t *entry = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                kernel_scope->get_symbol(ls.orig_name))),
            gpu_liveout_slot(al, loc, kernel_scope->get_symbol(ls.buf_name),
                ls.scalar_type),
            nullptr, false, false, false));
        plan.entries.insert(entry);
        kernel_body.push_back(al, entry);
    }
}

void GpuOffloadVisitor::write_back_liveouts(ASR::Function_t &kernel,
        GpuLiveoutPlan &plan) {
    if (plan.entries.empty()) return;
    GpuLiveoutWriteBack write_back(al);
    write_back.entries = plan.entries;
    write_back.visit_Function(kernel);
}

} // namespace LCompilers
