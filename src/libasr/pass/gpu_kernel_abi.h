#ifndef LIBASR_PASS_GPU_KERNEL_ABI_H
#define LIBASR_PASS_GPU_KERNEL_ABI_H

#include <libasr/asr.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

inline std::string gpu_workspace_buffer_name(int slot) {
    return "__workspace_" + std::to_string(slot);
}

void pass_gpu_kernel_finalize(Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &options);

bool has_pending_gpu_offload(const ASR::TranslationUnit_t &unit);

bool gpu_create_kernel_layout(Allocator &al, ASR::Function_t &kernel,
    ASR::call_arg_t *args, size_t n_args, GpuDecline &decline);

ASR::expr_t* gpu_bind_kernel_expression(Allocator &al,
    const ASR::Function_t &kernel, ASR::call_arg_t *args, size_t n_args,
    ASR::expr_t *expression);

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
        ws.elem_size = source.m_element_size;
        ws.buffer_index = source.m_buffer_index;
        for (size_t d = 0; d < source.n_dims; d++) {
            GpuVlaDim dim;
            dim.source_extent = source.m_dims[d].m_extent;
            dim.extent_parameter = source.m_dims[d].m_parameter;
            dim.is_constant = dim.extent_parameter == nullptr;
            if (dim.is_constant) {
                bool constant = ASRUtils::extract_value(
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
