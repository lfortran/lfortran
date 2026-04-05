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

// Classify kernel arguments into buffer (array/struct) and scalar categories.
// Returns the count of buffer args and scalar args respectively.
inline std::pair<int, int> classify_gpu_kernel_args(
        const ASR::GpuKernelFunction_t &kernel) {
    int n_buffer = 0, n_scalar = 0;
    for (size_t i = 0; i < kernel.n_args; i++) {
        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kernel.m_args[i]);
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(v->m_v));
        ASR::ttype_t *type = var->m_type;
        if (ASRUtils::is_array(type) ||
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(type))) {
            n_buffer++;
        } else {
            n_scalar++;
        }
    }
    return {n_buffer, n_scalar};
}

// Helper to recursively find the first Allocate statement for a given
// variable name within a statement list.
inline ASR::Allocate_t* find_allocate_for_var(
        ASR::stmt_t **stmts, size_t n, const std::string &var_name) {
    for (size_t i = 0; i < n; i++) {
        if (ASR::is_a<ASR::Allocate_t>(*stmts[i])) {
            ASR::Allocate_t *alloc =
                ASR::down_cast<ASR::Allocate_t>(stmts[i]);
            for (size_t ai = 0; ai < alloc->n_args; ai++) {
                if (!alloc->m_args[ai].m_a) continue;
                if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a))
                    continue;
                std::string aname = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        alloc->m_args[ai].m_a)->m_v);
                if (aname == var_name) return alloc;
            }
        }
        if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
            ASR::WhileLoop_t *wl =
                ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
            auto *r = find_allocate_for_var(wl->m_body, wl->n_body,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
            ASR::DoLoop_t *dl =
                ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
            auto *r = find_allocate_for_var(dl->m_body, dl->n_body,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
            ASR::If_t *if_s =
                ASR::down_cast<ASR::If_t>(stmts[i]);
            auto *r = find_allocate_for_var(if_s->m_body, if_s->n_body,
                var_name);
            if (r) return r;
            r = find_allocate_for_var(if_s->m_orelse, if_s->n_orelse,
                var_name);
            if (r) return r;
        }
    }
    return nullptr;
}

// Helper to extract a kernel argument reference from a complex Allocate
// dimension expression.
inline bool find_arg_var_in_expr(ASR::expr_t *expr,
        const std::vector<std::string> &arg_names,
        size_t &arg_index) {
    if (!expr) return false;
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        for (size_t a = 0; a < arg_names.size(); a++) {
            if (arg_names[a] == name) {
                arg_index = a;
                return true;
            }
        }
        return false;
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        ASR::IntegerBinOp_t *op =
            ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        if (find_arg_var_in_expr(op->m_left, arg_names, arg_index))
            return true;
        return find_arg_var_in_expr(op->m_right, arg_names, arg_index);
    }
    if (ASR::is_a<ASR::IfExp_t>(*expr)) {
        ASR::IfExp_t *ie = ASR::down_cast<ASR::IfExp_t>(expr);
        if (find_arg_var_in_expr(ie->m_body, arg_names, arg_index))
            return true;
        return find_arg_var_in_expr(ie->m_orelse, arg_names, arg_index);
    }
    if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
        ASR::IntegerCompare_t *cmp =
            ASR::down_cast<ASR::IntegerCompare_t>(expr);
        if (find_arg_var_in_expr(cmp->m_left, arg_names, arg_index))
            return true;
        return find_arg_var_in_expr(cmp->m_right, arg_names, arg_index);
    }
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return find_arg_var_in_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg,
            arg_names, arg_index);
    }
    return false;
}

// Scan kernel-scope Allocatable(Array) variables for VLA workspaces.
inline void scan_kernel_scope_alloc_vlas(
        const ASR::GpuKernelFunction_t &kernel,
        const std::vector<std::string> &arg_names,
        int &buffer_idx,
        std::vector<GpuVlaWorkspace> &result) {
    for (auto &item : kernel.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var =
            ASR::down_cast<ASR::Variable_t>(item.second);
        if (!ASRUtils::is_allocatable(var->m_type)) continue;
        ASR::ttype_t *inner =
            ASRUtils::type_get_past_allocatable(var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Allocate_t *alloc = find_allocate_for_var(
            kernel.m_body, kernel.n_body, std::string(var->m_name));
        if (!alloc) continue;
        ASR::alloc_arg_t *target_arg = nullptr;
        for (size_t ai = 0; ai < alloc->n_args; ai++) {
            if (!alloc->m_args[ai].m_a) continue;
            if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a)) continue;
            std::string aname = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(
                    alloc->m_args[ai].m_a)->m_v);
            if (aname == std::string(var->m_name)) {
                target_arg = &alloc->m_args[ai];
                break;
            }
        }
        if (!target_arg) continue;
        bool has_runtime_dim = false;
        for (size_t d = 0; d < target_arg->n_dims; d++) {
            ASR::dimension_t &dim = target_arg->m_dims[d];
            if (dim.m_length &&
                    !ASR::is_a<ASR::IntegerConstant_t>(*dim.m_length)) {
                has_runtime_dim = true;
                break;
            }
        }
        if (!has_runtime_dim) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);
        GpuVlaWorkspace ws;
        ws.var_name = var->m_name;
        ws.buffer_index = buffer_idx++;
        if (ASR::is_a<ASR::Real_t>(*arr->m_type)) {
            ws.elem_size = ASR::down_cast<ASR::Real_t>(
                arr->m_type)->m_kind;
        } else if (ASR::is_a<ASR::Integer_t>(*arr->m_type)) {
            ws.elem_size = ASR::down_cast<ASR::Integer_t>(
                arr->m_type)->m_kind;
        } else {
            ws.elem_size = 4;
        }
        for (size_t d = 0; d < target_arg->n_dims; d++) {
            ASR::expr_t *dim = target_arg->m_dims[d].m_length;
            GpuVlaDim vd;
            vd.dim_expr = dim;
            if (dim && ASR::is_a<ASR::IntegerConstant_t>(*dim)) {
                vd.is_constant = true;
                vd.constant_value =
                    ASR::down_cast<ASR::IntegerConstant_t>(dim)->m_n;
                vd.call_arg_index = 0;
            } else if (dim) {
                vd.is_constant = false;
                vd.constant_value = 0;
                vd.call_arg_index = 0;
                size_t idx = 0;
                if (find_arg_var_in_expr(dim, arg_names, idx)) {
                    vd.call_arg_index = idx;
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

// Count VLA workspaces in a kernel without assigning buffer indices.
inline int count_gpu_vla_workspaces(const ASR::GpuKernelFunction_t &kernel) {
    int count = 0;
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
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_length &&
                        !ASR::is_a<ASR::IntegerConstant_t>(
                            *arr->m_dims[d].m_length)) {
                    count++;
                    break;
                }
            }
        }
    }
    // Also count kernel-scope Allocatable(Array) variables with
    // runtime-dependent Allocate dimensions
    for (auto &item : kernel.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var =
            ASR::down_cast<ASR::Variable_t>(item.second);
        if (!ASRUtils::is_allocatable(var->m_type)) continue;
        ASR::ttype_t *inner =
            ASRUtils::type_get_past_allocatable(var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Allocate_t *alloc = find_allocate_for_var(
            kernel.m_body, kernel.n_body, std::string(var->m_name));
        if (!alloc) continue;
        for (size_t ai = 0; ai < alloc->n_args; ai++) {
            if (!alloc->m_args[ai].m_a) continue;
            if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a))
                continue;
            std::string aname = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(
                    alloc->m_args[ai].m_a)->m_v);
            if (aname != std::string(var->m_name)) continue;
            bool has_runtime = false;
            for (size_t d = 0; d < alloc->m_args[ai].n_dims; d++) {
                if (alloc->m_args[ai].m_dims[d].m_length &&
                        !ASR::is_a<ASR::IntegerConstant_t>(
                            *alloc->m_args[ai].m_dims[d].m_length)) {
                    has_runtime = true;
                    break;
                }
            }
            if (has_runtime) count++;
            break;
        }
    }
    return count;
}

static const int MAX_METAL_BUFFERS = 31;
static const int PACKED_BUFFER_ALIGN = 16;

// Determine whether a kernel needs buffer packing because its total
// buffer count exceeds Metal's 31-slot limit.
inline bool gpu_kernel_needs_buffer_packing(
        const ASR::GpuKernelFunction_t &kernel) {
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    int n_vla = count_gpu_vla_workspaces(kernel);
    int total = n_buffer + (n_scalar > 0 ? 1 : 0) + n_vla;
    return total > MAX_METAL_BUFFERS;
}

// Compute the Metal buffer index where VLA workspace buffers start.
// Normal layout:  [buffer_args...] [scalar_struct?] [vla_workspaces...]
// Packed layout:  [packed_arrays(0)] [scalar_struct(1)] [vla_workspaces...]
inline int gpu_vla_buffer_start(const ASR::GpuKernelFunction_t &kernel) {
    if (gpu_kernel_needs_buffer_packing(kernel)) {
        return 2;
    }
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    return n_buffer + (n_scalar > 0 ? 1 : 0);
}

// Analyze a GPU kernel function for variable-length arrays in blocks.
// Returns workspace metadata for each VLA found, with buffer indices
// assigned sequentially starting after the kernel's packed arguments.
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

    // Buffer index starts after buffer args + optional scalar struct
    int buffer_idx = gpu_vla_buffer_start(kernel);

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

    // Also scan kernel-scope Allocatable(Array) variables whose Allocate
    // statements have runtime-dependent dimensions (e.g. temporaries from
    // subroutine_from_function pass with runtime-sized array sections).
    scan_kernel_scope_alloc_vlas(kernel, arg_names, buffer_idx, result);

    return result;
}

} // namespace LCompilers

#endif // LFORTRAN_GPU_UTILS_H
