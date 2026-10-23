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
// into and back out of; a derived-type variable is a kernel argument,
// which the device writes in place. A thread keeps a copy of the whole
// value of a derived type with nothing allocatable or a pointer in it,
// and of each scalar component the loop defines of one of another type,
// whose allocatable and pointer array components stay shared.

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_data_layout.h>
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
        ASR::stmt_t **body, size_t n_body,
        const std::set<std::string> &assigned_vars,
        const std::set<std::string> &loop_var_set,
        const std::set<std::string> &reduction_names,
        std::set<std::string> &local_scalar_names,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>>
            &involved_syms,
        GpuLiveoutPlan &plan) {
    // Every question that can stop the offload is asked before anything
    // is created.
    std::vector<std::string> scalars, wholes;
    std::set<ASR::symbol_t*> partial;
    std::map<ASR::symbol_t*, ASR::symbol_t*> named;
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
            ASR::symbol_t *sym = current_scope->resolve_symbol(name);
            ASR::symbol_t *v = sym ? ASRUtils::symbol_get_past_external(sym)
                : nullptr;
            if (v == nullptr || !ASR::is_a<ASR::Variable_t>(*v)
                    || ASRUtils::is_class_type(type)
                    || ASRUtils::is_allocatable_or_pointer(type)) {
                report_not_offloaded(loc, GpuDecline(
                    GpuDeclineReason::LiveoutDerivedType, name));
                return false;
            }
            if (gpu_partially_private_struct(current_scope, name, type)) {
                partial.insert(v);
                named[v] = sym;
            } else {
                wholes.push_back(name);
            }
            continue;
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
    GpuPrivateComponentCollector components(partial);
    if (!partial.empty()) {
        for (size_t i = 0; i < n_body; i++) {
            components.visit_stmt(*body[i]);
        }
        for (ASR::symbol_t *v : partial) {
            if (components.unsafe.count(v)) {
                report_not_offloaded(loc, GpuDecline(
                    GpuDeclineReason::LiveoutDerivedType,
                    ASRUtils::symbol_name(v)));
                return false;
            }
        }
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

    // The kernel takes the variable under another name, and the private
    // copy is a local of it under the variable's own, which the body
    // names; it is declared with the kernel's other locals.
    for (auto &name : wholes) {
        auto it = involved_syms.find(name);
        ASR::ttype_t *type = it->second.first;
        std::string shared = "__gpu_shared_" + name;
        involved_syms.erase(it);
        involved_syms[shared] = {type, ASRUtils::EXPR(ASR::make_Var_t(al,
            loc, current_scope->resolve_symbol(name)))};
        local_scalar_names.insert(name);
        plan.wholes.push_back({name, shared});
    }

    for (auto &[key, member] : components.defined) {
        plan.components.push_back({key.first, named[key.first],
            member.first, member.second,
            "__gpu_private_" + std::string(ASRUtils::symbol_name(key.first))
                + "_" + key.second});
    }
    return true;
}

void GpuOffloadVisitor::declare_liveout_copies(const Location &loc,
        SymbolTable *kernel_scope, GpuLiveoutPlan &plan) {
    for (auto &ls : plan.scalars) {
        gpu_new_variable(al, loc, kernel_scope, ls.orig_name,
            ASRUtils::duplicate_type(al, ls.scalar_type));
    }
    for (auto &c : plan.components) {
        gpu_new_variable(al, loc, kernel_scope, c.copy,
            ASRUtils::duplicate_type(al, c.type));
    }
}

void GpuOffloadVisitor::privatize_liveout_block(SymbolTable *kernel_scope,
        ASR::stmt_t **body, size_t n_body, GpuLiveoutPlan &plan) {
    if (plan.components.empty()) return;
    GpuPrivateComponentVisitor replace(al);
    for (auto &c : plan.components) {
        std::string member = ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(c.member));
        ASR::symbol_t *copy = kernel_scope->get_symbol(c.copy);
        replace.replacer.copies[{c.variable, member}] = copy;
        // The kernel argument, which a remapped body names instead.
        if (ASR::symbol_t *argument = kernel_scope->get_symbol(
                ASRUtils::symbol_name(c.named))) {
            replace.replacer.copies[{argument, member}] = copy;
        }
    }
    for (size_t i = 0; i < n_body; i++) {
        replace.visit_stmt(*body[i]);
    }
}

void GpuOffloadVisitor::privatize_liveout_body(const Location &loc,
        SymbolTable *kernel_scope, Vec<ASR::stmt_t*> &body,
        GpuLiveoutPlan &plan) {
    if (plan.components.empty()) return;
    privatize_liveout_block(kernel_scope, body.p, body.n, plan);
    // Each copy starts as the component, read through the host's symbols
    // like the rest of the body, and remapped with it.
    Vec<ASR::stmt_t*> entered;
    entered.reserve(al, plan.components.size() + body.n);
    for (auto &c : plan.components) {
        ASR::stmt_t *entry = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                kernel_scope->get_symbol(c.copy))),
            ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, c.named)),
                c.member, ASRUtils::duplicate_type(al, c.type), nullptr)),
            nullptr, false, false, false));
        plan.entries.insert(entry);
        entered.push_back(al, entry);
    }
    for (size_t i = 0; i < body.n; i++) entered.push_back(al, body.p[i]);
    body = entered;
}

void GpuOffloadVisitor::enter_liveouts(const Location &loc,
        SymbolTable *kernel_scope, Vec<ASR::stmt_t*> &kernel_body,
        GpuLiveoutPlan &plan) {
    for (auto &w : plan.wholes) {
        ASR::stmt_t *entry = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                kernel_scope->get_symbol(w.name))),
            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                kernel_scope->get_symbol(w.shared))),
            nullptr, false, false, false));
        plan.entries.insert(entry);
        kernel_body.push_back(al, entry);
    }
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
    write_back.run(kernel);
}

} // namespace LCompilers
