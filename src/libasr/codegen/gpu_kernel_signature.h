#ifndef LIBASR_CODEGEN_GPU_KERNEL_SIGNATURE_H
#define LIBASR_CODEGEN_GPU_KERNEL_SIGNATURE_H

namespace LCompilers {

template <typename D>
void ASRToGpuCVisitor<D>::emit_kernel_signature(const ASR::Function_t &kernel) {
    LCOMPILERS_ASSERT(kernel.m_gpu != nullptr);
    const auto &layout = *kernel.m_gpu;
    std::string name = kernel.m_name;
    std::string scalar_struct = "__ScalarArgs_" + name;
    auto element_type = [&](const ASR::gpu_kernel_argument_t &arg) {
        if (ASR::is_a<ASR::StructType_t>(*arg.m_type)) {
            return get_struct_name(ASR::down_cast<ASR::Variable_t>(
                arg.m_member ? arg.m_member : arg.m_variable));
        }
        return gpu_type(arg.m_type);
    };
    auto is_reference = [](const ASR::gpu_kernel_argument_t &arg) {
        return arg.m_kind == ASR::gpu_argument_kindType::GpuStruct ||
            arg.m_kind == ASR::gpu_argument_kindType::GpuClass;
    };
    if (layout.n_scalars) {
        src << "struct " << scalar_struct << " {\n";
        for (size_t i = 0; i < layout.n_scalars; i++) {
            const auto &arg = layout.m_scalars[i];
            src << "    " << gpu_type(arg.m_type) << " "
                << gpu_argument_name(arg, layout) << ";\n";
        }
        src << "};\n\n";
    }
    src << dialect.kernel_qualifier() << name << "(\n";
    kernel_params.clear();
    int slot = 0;
    bool previous = false;
    auto separator = [&]() {
        if (previous) src << ",\n";
        previous = true;
    };
    if (layout.m_packed) {
        separator();
        src << "    " << global_prefix() << "char* __packed_arrays"
            << dialect.buffer_attr(slot++);
        kernel_params.push_back(
            {"char", "__packed_arrays", GpuKernelParamKind::Buffer});
    } else {
        for (size_t i = 0; i < layout.n_buffers; i++) {
            const auto &arg = layout.m_buffers[i];
            std::string type = element_type(arg);
            std::string parameter = gpu_argument_name(arg, layout);
            bool reference = is_reference(arg);
            separator();
            src << "    " << global_prefix() << type
                << (reference ? "& " : "* ") << parameter
                << dialect.buffer_attr(slot++);
            kernel_params.push_back({type, parameter, reference
                ? GpuKernelParamKind::StructReference
                : GpuKernelParamKind::Buffer});
        }
    }
    if (layout.n_scalars) {
        separator();
        dialect.emit_scalar_args_param(src, scalar_struct, slot++);
        kernel_params.push_back({scalar_struct, "__scalar_args",
            GpuKernelParamKind::ScalarStruct});
    }
    for (size_t i = 0; i < layout.n_workspaces; i++) {
        const auto &workspace = layout.m_workspaces[i];
        LCOMPILERS_ASSERT(workspace.m_buffer_index == slot);
        std::string type = gpu_type(ASRUtils::extract_type(
            ASRUtils::symbol_type(workspace.m_variable)));
        std::string parameter = gpu_workspace_buffer_name(workspace.m_buffer_index);
        separator();
        src << "    " << global_prefix() << type << "* " << parameter
            << dialect.buffer_attr(slot++);
        kernel_params.push_back({type, parameter, GpuKernelParamKind::Buffer});
    }
    dialect.emit_thread_id_param(src, previous);
    src << ")\n{\n";
    indent_level++;
    for (size_t i = 0; i < layout.n_scalars; i++) {
        const auto &arg = layout.m_scalars[i];
        if (arg.m_kind == ASR::gpu_argument_kindType::GpuPackedOffset) continue;
        std::string parameter = gpu_argument_name(arg, layout);
        src << get_indent() << gpu_type(arg.m_type) << " " << parameter
            << " = __scalar_args." << parameter << ";\n";
    }
    if (layout.m_packed) {
        for (size_t i = 0; i < layout.n_buffers; i++) {
            const auto &arg = layout.m_buffers[i];
            std::string type = element_type(arg);
            std::string parameter = gpu_argument_name(arg, layout);
            bool reference = is_reference(arg);
            src << get_indent() << global_prefix() << type
                << (reference ? "& " : "* ") << parameter << " = "
                << (reference ? "*" : "") << "(" << global_prefix()
                << type << "*)(__packed_arrays + __scalar_args.__offset_"
                << parameter << ");\n";
        }
    }
}

template <typename D>
void ASRToGpuCVisitor<D>::bind_kernel_arguments(const ASR::Function_t &kernel) {
    const auto &layout = *kernel.m_gpu;
    func_array_size_params.clear();
    func_array_data_params.clear();
    struct_array_offset_params.clear();
    struct_array_sizes_params.clear();
    struct_from_array_elem.clear();
    for (size_t i = 0; i < layout.n_buffers; i++) {
        const auto &arg = layout.m_buffers[i];
        std::string name = ASRUtils::symbol_name(arg.m_variable);
        if (arg.m_member) {
            std::string key = name + "." + ASRUtils::symbol_name(arg.m_member);
            std::string parameter = gpu_argument_name(arg, layout);
            switch (arg.m_kind) {
                case ASR::gpu_argument_kindType::GpuMemberData:
                    func_array_data_params[key] = parameter;
                    break;
                case ASR::gpu_argument_kindType::GpuMemberOffsets:
                    struct_array_offset_params[key] = parameter;
                    break;
                case ASR::gpu_argument_kindType::GpuMemberSizes:
                    struct_array_sizes_params[key] = parameter;
                    break;
                default: LCOMPILERS_ASSERT(false);
            }
            continue;
        }
        ASR::Variable_t *var = gpu_argument_variable(arg);
        if (arg.m_kind == ASR::gpu_argument_kindType::GpuArray) {
            if (array_extents_are_explicit(var->m_type)) {
                register_array_extents(name, var->m_type);
            }
            continue;
        }
        ASR::Struct_t *st = get_struct_decl(var);
        if (!st) continue;
        for (auto &entry : ASRUtils::collect_allocatable_array_members(st)) {
            std::string key = name + "." + entry.first;
            func_array_size_params[key] = GpuNames::member_size(name, entry.first);
            func_array_data_params[key] = GpuNames::member_data(name, entry.first);
            size_t rank = struct_member_rank(entry.second);
            if (rank > 1) {
                for (size_t d = 0; d < rank; d++) {
                    func_array_size_params[dim_size_key(key, d)] =
                        struct_member_dim_param(name, entry.first, d);
                }
            }
        }
    }
    for (size_t i = 0; i < layout.n_scalars; i++) {
        const auto &arg = layout.m_scalars[i];
        if (arg.m_kind != ASR::gpu_argument_kindType::GpuArrayExtent) continue;
        std::string name = ASRUtils::symbol_name(arg.m_variable);
        func_array_size_params[dim_size_key(name, arg.m_dimension)] =
            gpu_argument_name(arg, layout);
        if (arg.m_dimension != 0) continue;
        int rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::symbol_type(arg.m_variable));
        std::string total = GpuNames::dim_size(name, 0);
        for (int d = 1; d < rank; d++) {
            total += " * " + GpuNames::dim_size(name, d);
        }
        func_array_size_params[name] = "(" + total + ")";
    }
}

} // namespace LCompilers

#endif
