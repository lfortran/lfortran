#ifndef LFORTRAN_GPU_UTILS_H
#define LFORTRAN_GPU_UTILS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

#include <string>
#include <vector>

namespace LCompilers {

// Describes one dimension of a VLA workspace buffer.
struct GpuVlaDim {
    bool is_constant;
    int64_t constant_value;
    size_t call_arg_index;
    ASR::expr_t *dim_expr; // original ASR dimension expression
};

// Describes a VLA workspace buffer required by a GPU kernel.
struct GpuVlaWorkspace {
    std::string var_name;
    int buffer_index;
    int elem_size;
    std::vector<GpuVlaDim> dims;
};

// Analyze a GPU kernel function for variable-length arrays in blocks.
// Returns workspace metadata for each VLA found, with buffer indices
// assigned sequentially starting after the kernel's regular arguments.
inline std::vector<GpuVlaWorkspace> analyze_gpu_vla_workspaces(
        const ASR::GpuKernelFunction_t &kernel) {
    // Build the kernel argument name list for mapping dim vars to arg indices
    std::vector<std::string> arg_names;
    for (size_t i = 0; i < kernel.n_args; i++) {
        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kernel.m_args[i]);
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(v->m_v));
        arg_names.push_back(std::string(var->m_name));
    }

    // Buffer index starts after all regular kernel arguments
    int buffer_idx = static_cast<int>(kernel.n_args);

    std::vector<GpuVlaWorkspace> result;

    // Scan BlockCall statements in the kernel body for VLAs
    for (size_t i = 0; i < kernel.n_body; i++) {
        if (!ASR::is_a<ASR::BlockCall_t>(*kernel.m_body[i])) continue;
        ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
            kernel.m_body[i]);
        if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);

        for (auto &item : block->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                item.second);
            if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);

            bool has_vla = false;
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_length &&
                        !ASR::is_a<ASR::IntegerConstant_t>(
                            *arr->m_dims[d].m_length)) {
                    has_vla = true;
                    break;
                }
            }
            if (!has_vla) continue;

            GpuVlaWorkspace ws;
            ws.var_name = var->m_name;
            ws.buffer_index = buffer_idx++;

            // Compute element size from the array element type
            if (ASR::is_a<ASR::Real_t>(*arr->m_type)) {
                ws.elem_size = ASR::down_cast<ASR::Real_t>(
                    arr->m_type)->m_kind;
            } else if (ASR::is_a<ASR::Integer_t>(*arr->m_type)) {
                ws.elem_size = ASR::down_cast<ASR::Integer_t>(
                    arr->m_type)->m_kind;
            } else {
                ws.elem_size = 4;
            }

            // Process each dimension
            for (size_t d = 0; d < arr->n_dims; d++) {
                ASR::expr_t *dim = arr->m_dims[d].m_length;
                GpuVlaDim vd;
                vd.dim_expr = dim;
                if (dim && ASR::is_a<ASR::IntegerConstant_t>(*dim)) {
                    vd.is_constant = true;
                    vd.constant_value =
                        ASR::down_cast<ASR::IntegerConstant_t>(dim)->m_n;
                    vd.call_arg_index = 0;
                } else if (dim && ASR::is_a<ASR::Var_t>(*dim)) {
                    std::string dim_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(dim)->m_v);
                    vd.is_constant = false;
                    vd.constant_value = 0;
                    vd.call_arg_index = 0;
                    for (size_t a = 0; a < arg_names.size(); a++) {
                        if (arg_names[a] == dim_name) {
                            vd.call_arg_index = a;
                            break;
                        }
                    }
                } else {
                    vd.is_constant = true;
                    vd.constant_value = 1;
                    vd.call_arg_index = 0;
                }
                ws.dims.push_back(vd);
            }
            result.push_back(std::move(ws));
        }
    }
    return result;
}

} // namespace LCompilers

#endif // LFORTRAN_GPU_UTILS_H
