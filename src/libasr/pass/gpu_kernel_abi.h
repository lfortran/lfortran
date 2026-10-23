#ifndef LIBASR_PASS_GPU_KERNEL_ABI_H
#define LIBASR_PASS_GPU_KERNEL_ABI_H

#include <map>
#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

inline std::string gpu_workspace_buffer_name(int slot) {
    return "__workspace_" + std::to_string(slot);
}

void pass_gpu_kernel_finalize(Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &options);

void pass_gpu_kernel_storage(Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &options);

bool gpu_create_kernel_layout(Allocator &al, ASR::Function_t &kernel,
    ASR::call_arg_t *args, size_t n_args, GpuDecline &decline);

// The variables a statement list may define: the target of an assignment,
// what an `allocate` or `deallocate` names, an actual argument of a dummy
// that may be defined, everything a statement not known to be harmless
// names, and the index of a DO loop, in the bodies of the BLOCK and
// ASSOCIATE constructs it runs as well. The procedures it calls are not
// looked into.
std::set<ASR::symbol_t*> gpu_written_variables(ASR::stmt_t **body,
    size_t n_body);

// The extent `dim` of a workspace of `kernel` has, written as the host
// evaluates it before the launch, or nullptr when the host cannot work it
// out: it reads a value the kernel computes. Such an array is allocated on
// the device heap instead (see gpu_device_heap.cpp).
ASR::expr_t* gpu_workspace_host_extent(Allocator &al,
    ASR::Function_t &kernel, const GpuVlaDim &dim);

ASR::expr_t* gpu_bind_kernel_expression(Allocator &al,
    const ASR::Function_t &kernel, ASR::call_arg_t *args, size_t n_args,
    ASR::expr_t *expression);

// What a kernel does ahead of its body, written over the kernel's
// parameters and its thread index: the tests under which a thread returns
// without running the body -- it is past the end of the iteration space --
// and the value each scalar it sets there has from then on, when the body
// does not set it again. Those scalars are the loop indices.
struct GpuKernelPrelude {
    std::vector<ASR::expr_t*> exits;
    std::map<ASR::symbol_t*, ASR::expr_t*> values;
};

GpuKernelPrelude gpu_kernel_prelude(Allocator &al,
    const ASR::Function_t &kernel);

// A workspace extent of the kernel as the thread that evaluates it has it,
// written over the kernel's parameters and its thread index alone, so the
// host can evaluate it for every thread before the launch and the device
// anywhere in the kernel. The loop indices are replaced by what the
// prelude gives them, and a scalar the kernel binds once by the value it
// binds. nullptr when that cannot be done, or when the extent reads a
// value the kernel writes, which the host cannot know before the launch.
ASR::expr_t* gpu_kernel_per_thread_extent(Allocator &al,
    const ASR::Function_t &kernel, ASR::expr_t *extent);

inline ASR::Variable_t* gpu_argument_variable(
        const ASR::gpu_kernel_argument_t &arg) {
    return ASR::down_cast<ASR::Variable_t>(arg.m_variable);
}

inline std::string gpu_argument_name(const ASR::gpu_kernel_argument_t &arg,
        const ASR::gpu_kernel_layout_t &layout) {
    const char *name = ASRUtils::symbol_name(arg.m_variable);
    const char *member = arg.m_member
        ? ASRUtils::symbol_name(arg.m_member) : "";
    switch (arg.m_kind) {
        case ASR::gpu_argument_kindType::GpuArray:
        case ASR::gpu_argument_kindType::GpuStruct:
        case ASR::gpu_argument_kindType::GpuClass:
        case ASR::gpu_argument_kindType::GpuScalar:
            return name;
        case ASR::gpu_argument_kindType::GpuMemberData:
            return GpuNames::member_data(name, member);
        case ASR::gpu_argument_kindType::GpuMemberOffsets:
            return GpuNames::member_offsets(name, member);
        case ASR::gpu_argument_kindType::GpuMemberSizes:
            return GpuNames::member_sizes(name, member);
        case ASR::gpu_argument_kindType::GpuArrayExtent:
            return GpuNames::dim_size(name, arg.m_dimension);
        case ASR::gpu_argument_kindType::GpuPackedOffset:
            LCOMPILERS_ASSERT(arg.m_dimension >= 0 &&
                (size_t)arg.m_dimension < layout.n_buffers);
            return "__offset_" + gpu_argument_name(
                layout.m_buffers[arg.m_dimension], layout);
    }
    LCOMPILERS_ASSERT(false);
    return "";
}

inline ASR::ttype_t* gpu_argument_type(
        const ASR::gpu_kernel_argument_t &arg) {
    return arg.m_type;
}

inline ASR::symbol_t* gpu_argument_type_declaration(
        const ASR::gpu_kernel_argument_t &arg) {
    return ASR::down_cast<ASR::Variable_t>(
        arg.m_member ? arg.m_member : arg.m_variable)->m_type_declaration;
}

inline size_t gpu_parameter_index(const ASR::Function_t &kernel,
        ASR::symbol_t *parameter) {
    for (size_t i = 0; i < kernel.n_args; i++) {
        if (ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v == parameter) {
            return i;
        }
    }
    LCOMPILERS_ASSERT(false);
    return kernel.n_args;
}

inline std::vector<GpuVlaWorkspace> gpu_kernel_workspaces(
        const ASR::Function_t &kernel) {
    LCOMPILERS_ASSERT(kernel.m_gpu != nullptr);
    std::vector<GpuVlaWorkspace> result;
    for (size_t i = 0; i < kernel.m_gpu->n_workspaces; i++) {
        const ASR::gpu_workspace_t &source = kernel.m_gpu->m_workspaces[i];
        GpuVlaWorkspace ws;
        ws.var = source.m_variable;
        ws.var_name = ASRUtils::symbol_name(source.m_variable);
        ws.elem_size = checked_integer_cast<int>(source.m_element_size);
        ws.buffer_index = checked_integer_cast<int>(source.m_buffer_index);
        for (size_t d = 0; d < source.n_dims; d++) {
            GpuVlaDim dim;
            dim.source_extent = source.m_dims[d].m_extent;
            dim.extent_parameter = source.m_dims[d].m_parameter;
            dim.is_constant = dim.extent_parameter == nullptr;
            dim.per_thread = gpu_reads_thread_index(dim.source_extent);
            if (dim.is_constant) {
                [[maybe_unused]] const bool constant = ASRUtils::extract_value(
                    ASRUtils::expr_value(dim.source_extent),
                    dim.constant_value);
                LCOMPILERS_ASSERT(constant);
            }
            ws.dims.push_back(dim);
        }
        result.push_back(std::move(ws));
    }
    return result;
}

} // namespace LCompilers

#endif
