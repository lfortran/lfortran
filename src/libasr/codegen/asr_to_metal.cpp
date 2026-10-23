#include <libasr/codegen/asr_to_metal.h>
#include <libasr/codegen/asr_to_gpu_c.h>

namespace LCompilers {

// Metal Shading Language. The spellings the shared device code generator
// asks a dialect for; everything else it derives from the ASR itself.
struct MetalDialect {
    std::string addr_space(ASR::memory_spaceType space) const {
        switch (space) {
            case ASR::memory_spaceType::Global: return "device";
            case ASR::memory_spaceType::Shared: return "threadgroup";
            case ASR::memory_spaceType::Constant: return "constant";
            case ASR::memory_spaceType::Thread: return "thread";
        }
        return "device";
    }

    // Metal has no double precision. This silently narrows real(8), and is
    // only correct because pass_replace_gpu_offload refuses to offload a loop
    // that touches real(8) or integer(8) and reports it instead. Do not lift
    // that guard without also giving real(8) a correct lowering here.
    std::string real_type(int /*kind*/) const {
        return "float";
    }

    // The integer kind an element's offset in an array is computed in. The
    // offload pass hands Metal no integer(8) (see real_type), so its arrays
    // are indexed in 32 bits.
    int offset_kind() const {
        return 4;
    }

    void emit_prologue(std::stringstream &src) const {
        src << "#include <metal_stdlib>\n";
        src << "using namespace metal;\n\n";
    }

    std::string device_fn_qualifier() const {
        return "inline ";
    }

    std::string kernel_qualifier() const {
        return "kernel void ";
    }

    // Every kernel parameter is bound to an explicit buffer slot.
    std::string buffer_attr(int index) const {
        return " [[buffer(" + std::to_string(index) + ")]]";
    }

    // The Metal Shading Language has no way to say that a buffer is not
    // aliased.
    std::string noalias_qualifier() const {
        return "";
    }

    // The grid position is a parameter of the kernel rather than a built-in.
    void emit_thread_id_param(std::stringstream &src, bool has_prev) const {
        if (has_prev) src << ",\n";
        src << "    uint __thread_id [[thread_position_in_grid]]";
    }

    // A kernel whose iterations each run on several lanes (see
    // gpu_vector_lanes.cpp) takes its grid position under another name,
    // and its position within its SIMD-group too.
    void emit_lane_thread_params(std::stringstream &src, bool has_prev) const {
        if (has_prev) src << ",\n";
        src << "    uint __gpu_grid_thread [[thread_position_in_grid]],\n";
        src << "    uint __gpu_simd_lane [[thread_index_in_simdgroup]]";
    }

    // The lanes of an iteration are consecutive threads of one SIMD-group,
    // as `lanes` divides the SIMD-group width (the lanes a device's
    // capabilities give), and the threads of a threadgroup fill its
    // SIMD-groups in order.
    void emit_lane_prologue(std::stringstream &src, const std::string &indent,
            const std::string &lanes) const {
        src << indent << "uint __thread_id = __gpu_grid_thread / (uint)"
            << lanes << ";\n";
        src << indent << "int __gpu_lane = (int)(__gpu_grid_thread % (uint)"
            << lanes << ");\n";
    }

    std::string lane_index() const { return "__gpu_lane"; }

    // The value in the first lane of the iteration. With one lane per
    // iteration, that is the thread's own.
    std::string lane_broadcast(const std::string &value) const {
        return "simd_shuffle(" + value
            + ", (ushort)(__gpu_simd_lane - (uint)__gpu_lane))";
    }

    // With one lane per iteration, the threads of a SIMD-group run
    // different iterations, which need not reach the same barriers.
    void emit_lane_barrier(std::stringstream &src, const std::string &indent,
            const std::string &lanes) const {
        src << indent << "if (" << lanes << " > 1) simdgroup_barrier("
            "mem_flags::mem_device | mem_flags::mem_threadgroup);\n";
    }

    // Every real is single precision, so a literal needs no suffix to say
    // which precision it is.
    std::string real_literal(double value, int /*kind*/) const {
        return double_to_scientific(value);
    }

    // The grid position already accounts for the block, so a kernel that
    // asks for its block coordinates is asking about a grid Metal does not
    // divide up: the block is the whole grid and its index is zero.
    std::string thread_index() const { return "__thread_id"; }
    std::string global_thread_id() const { return "__thread_id"; }
    std::string block_index() const { return "0"; }
    std::string block_size() const { return "0"; }

    std::string uint_type() const {
        return "uint";
    }

    // A relaxed atomic add to an `int` in device memory, giving the old value.
    std::string atomic_fetch_add(const std::string &address,
            const std::string &increment) const {
        return "atomic_fetch_add_explicit((device atomic_int*)(" + address
            + "), " + increment + ", memory_order_relaxed)";
    }

    // The scalars a kernel takes travel together in one read-only buffer.
    void emit_scalar_args_param(std::stringstream &src,
            const std::string &struct_name, int buffer_index) const {
        src << "    " << addr_space(ASR::memory_spaceType::Constant) << " "
            << struct_name << "& __scalar_args"
            << buffer_attr(buffer_index);
    }

    void emit_kernel_epilogue(std::stringstream & /*src*/,
            const std::string & /*kernel_name*/,
            const std::vector<GpuKernelParam> & /*params*/) const {}

    void emit_translation_unit_epilogue(std::stringstream & /*src*/,
            const std::vector<std::string> & /*kernel_names*/) const {}
};

Result<std::string> asr_to_metal(Allocator & /*al*/, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co)
{
    ASRToGpuCVisitor<MetalDialect> v(co);
    try {
        v.visit_TranslationUnit(asr);
    } catch (const CodeGenError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    } catch (const Abort &) {
        return Error();
    }
    return v.src.str();
}

} // namespace LCompilers
