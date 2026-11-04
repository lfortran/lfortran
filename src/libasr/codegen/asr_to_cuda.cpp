#include <libasr/codegen/asr_to_cuda.h>
#include <libasr/codegen/asr_to_gpu_c.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

// CUDA C++, the CPU emulation of it that `--gpu=cuda_cpu` compiles as
// ordinary host code, and HIP (`--gpu=hip`), which spells everything this
// dialect emits as CUDA C++ does. The spellings the shared device code
// generator asks a dialect for; everything else it derives from the ASR
// itself.
struct CudaDialect {
    // Whether to emit the trailing kernel-registration shim, which is only
    // meaningful to the runtime's lookup-by-name launch path.
    bool emit_registrar = true;
    // Whether to emit the extra shims the CPU emulation needs. The real CUDA
    // output is left untouched.
    bool emulate_cpu = false;
    // Whether the source is compiled as HIP for an AMD GPU.
    bool hip = false;
    // The integer kind element offsets are computed in
    // (GpuDeviceCapabilities::offset_kind).
    int element_offset_kind = 4;

    CudaDialect(bool emit_registrar_, bool emulate_cpu_, bool hip_,
            int offset_kind_)
        : emit_registrar(emit_registrar_), emulate_cpu(emulate_cpu_),
          hip(hip_), element_offset_kind(offset_kind_) {}

    // CUDA has one flat address space for global memory, so a pointer into it
    // needs no qualifier at all; only the two special spaces are named.
    std::string addr_space(ASR::memory_spaceType space) const {
        switch (space) {
            case ASR::memory_spaceType::Global: return "";
            case ASR::memory_spaceType::Shared: return "__shared__";
            case ASR::memory_spaceType::Constant: return "__constant__";
            case ASR::memory_spaceType::Thread: return "";
        }
        return "";
    }

    std::string real_type(int kind) const {
        return (kind == 8) ? "double" : "float";
    }

    // The integer kind an element's offset in an array is computed in:
    // 8 with --descriptor-index-64, the flag that selects 64-bit array
    // indexing, and 4 otherwise, which is cheaper to compute in.
    int offset_kind() const {
        return element_offset_kind;
    }

    void emit_prologue(std::stringstream &src) const {
        src << "#include <stdint.h>\n";
        if (hip) {
            // nvcc includes the CUDA runtime header itself; the HIP driver
            // needs the HIP one named.
            src << "#include <hip/hip_runtime.h>\n";
        }
        if (emulate_cpu) {
            // Turns the CUDA execution space qualifiers and the built-in
            // thread coordinates into plain host C++.
            src << "#include \"cuda_cpu_device.h\"\n";
            src << "#include <algorithm>\n";
            src << "#include <cmath>\n";
            // The shared generator spells the maths unqualified, the way a
            // shading language does; on the host they live in namespace std.
            for (const char *fn : {"abs", "exp", "sqrt", "sin", "cos",
                    "fmod", "fma", "pow", "min", "max"}) {
                src << "using std::" << fn << ";\n";
            }
        }
        src << "\n";
    }

    std::string device_fn_qualifier() const {
        return "__device__ inline ";
    }

    std::string kernel_qualifier() const {
        return "extern \"C\" __global__ void ";
    }

    // A kernel parameter is bound by position, not to a named buffer slot.
    std::string buffer_attr(int /*index*/) const {
        return "";
    }

    // A buffer no other buffer of the launch aliases. nvcc then reads one
    // the kernel does not store to through the read-only data path, and
    // keeps a value it loaded across a store to another buffer.
    std::string noalias_qualifier() const {
        return "__restrict__ ";
    }

    // The grid position is a built-in rather than a parameter, so the kernel
    // takes none and derives it in its first statement instead.
    void emit_thread_id_param(std::stringstream & /*src*/,
            bool /*has_prev*/) const {}

    // A kernel whose iterations each run on several lanes (see
    // gpu_vector_lanes.cpp; only on an NVIDIA GPU, whose warps run them)
    // takes no parameters for it: the lanes of an iteration are
    // consecutive threads of a warp, `lanes` (1 or 32) dividing the block,
    // and the prologue numbers the iterations by dividing the thread
    // index and the block size by `lanes`.
    void emit_lane_thread_params(std::stringstream & /*src*/,
            bool /*has_prev*/) const {}
    void emit_lane_prologue(std::stringstream &src, const std::string &indent,
            const std::string &lanes) const {
        src << indent << "int __gpu_lanes_n = " << lanes << ";\n";
        src << indent << "int __gpu_lane = (int)(threadIdx.x % "
            "(unsigned)__gpu_lanes_n);\n";
        src << indent << "unsigned __gpu_iteration_thread = threadIdx.x / "
            "(unsigned)__gpu_lanes_n;\n";
        src << indent << "unsigned __gpu_iteration_block = blockDim.x / "
            "(unsigned)__gpu_lanes_n;\n";
    }
    std::string lane_index() const { return "__gpu_lane"; }
    std::string lane_thread_index() const { return "__gpu_iteration_thread"; }
    std::string lane_block_size() const { return "__gpu_iteration_block"; }
    // The value in the first lane of the iteration. With one lane per
    // iteration the threads of a warp run different iterations, which need
    // not reach the same shuffle, and the value is the thread's own.
    std::string lane_broadcast(const std::string &value) const {
        return "(__gpu_lanes_n > 1 ? __shfl_sync(0xffffffffu, " + value
            + ", 0, __gpu_lanes_n) : " + value + ")";
    }
    void emit_lane_barrier(std::stringstream &src, const std::string &indent,
            const std::string & /*lanes*/) const {
        src << indent << "if (__gpu_lanes_n > 1) __syncwarp();\n";
    }
    // What the lanes of the iteration hold, combined, in every lane
    // (__lfortran_lane_reduce in the prologue of the kernel source).
    std::string lane_reduce(const std::string &value, int op) const {
        return "__lfortran_lane_reduce<" + std::to_string(op) + ">(" + value
            + ", __gpu_lanes_n)";
    }
    // The definition lane_reduce names, written once ahead of the kernels.
    std::string lane_reduce_helper() const {
        return
            "// The combination of what the `lanes` lanes of an iteration hold,\n"
            "// in every lane: 0 adds, 1 multiplies, 2 takes the smallest, 3 the\n"
            "// largest. With one lane, the lane's own value.\n"
            "template <int op, typename T>\n"
            "__device__ inline T __lfortran_lane_reduce(T v, int lanes) {\n"
            "    for (int offset = lanes >> 1; offset > 0; offset >>= 1) {\n"
            "        T w = __shfl_xor_sync(0xffffffffu, v, offset, lanes);\n"
            "        v = op == 0 ? v + w : op == 1 ? v * w\n"
            "            : op == 2 ? (w < v ? w : v) : (w > v ? w : v);\n"
            "    }\n"
            "    return v;\n"
            "}\n\n";
    }

    // A bare literal is double precision, which would pick the wrong
    // overload of an intrinsic taking single precision arguments.
    std::string real_literal(double value, int kind) const {
        std::string literal = double_to_scientific(value);
        return (kind == 8) ? literal : literal + "f";
    }

    std::string thread_index() const { return "threadIdx.x"; }
    std::string block_index() const { return "blockIdx.x"; }
    std::string block_size() const { return "blockDim.x"; }

    // The grid divides into blocks, so a thread's position in the grid has to
    // be worked out from all three.
    std::string global_thread_id() const {
        return "(blockIdx.x * blockDim.x + threadIdx.x)";
    }

    std::string uint_type() const {
        return "unsigned int";
    }

    // The bits of `value` read as a `type` of the same width: a real of
    // kind `kind` when `to_real`, else an integer of that kind. The CPU
    // emulation defines these intrinsics in cuda_cpu_device.h.
    std::string bit_cast(const std::string & /*type*/, bool to_real,
            int kind, const std::string &value) const {
        if (kind == 8) {
            return std::string(to_real ? "__longlong_as_double("
                : "__double_as_longlong(") + value + ")";
        }
        return std::string(to_real ? "__int_as_float(" : "__float_as_int(")
            + value + ")";
    }

    // A relaxed atomic add to an `int` in device memory, giving the old
    // value. The CPU emulation defines atomicAdd in cuda_cpu_device.h.
    std::string atomic_fetch_add(const std::string &address,
            const std::string &increment) const {
        return "atomicAdd(" + address + ", " + increment + ")";
    }

    // Every scalar arrives in one struct, passed by value, which is what the
    // runtime's argument array holds a pointer to.
    void emit_scalar_args_param(std::stringstream &src,
            const std::string &struct_name, int /*buffer_index*/) const {
        src << "    " << struct_name << " __scalar_args";
    }

    // On the CPU there is no portable way to call a function pointer with a
    // dynamically built argument list, so every kernel gets a thunk that
    // unpacks the argument array the runtime passes.
    void emit_kernel_epilogue(std::stringstream &src,
            const std::string &kernel_name,
            const std::vector<GpuKernelParam> &params) const {
        if (!emulate_cpu) return;
        src << "extern \"C\" void __lf_thunk_" << kernel_name
            << "(void **a) {\n";
        src << "    " << kernel_name << "(";
        for (size_t slot = 0; slot < params.size(); slot++) {
            const GpuKernelParam &p = params[slot];
            if (slot > 0) src << ", ";
            switch (p.kind) {
                case GpuKernelParamKind::Buffer:
                    src << "*(" << p.type << " **)a[" << slot << "]";
                    break;
                case GpuKernelParamKind::StructReference:
                    src << "**(" << p.type << " **)a[" << slot << "]";
                    break;
                case GpuKernelParamKind::ScalarStruct:
                    src << "*(" << p.type << " *)a[" << slot << "]";
                    break;
            }
        }
        src << ");\n";
        src << "}\n\n";
    }

    void emit_translation_unit_epilogue(std::stringstream &src,
            const std::vector<std::string> &kernel_names) const {
        if (!emit_registrar) return;
        src << "\n// Auto-generated kernel registration\n";
        if (emulate_cpu) {
            if (kernel_names.empty()) return;
            // The thunks are registered instead of the kernels, with the
            // type of a kernel, and the emulated cudaLaunchKernel calls them.
            // A plain constructor attribute keeps the file free of any C++
            // runtime dependency. Under separate compilation the per-object
            // device sources are concatenated, so the registrar is named after
            // the first kernel to keep it unique per translation unit.
            src << "typedef void (*kernel_func_t)(void);\n";
            src << "extern \"C\" void lfortran_gpu_register_kernel("
                   "const char *name, kernel_func_t func);\n\n";
            src << "__attribute__((constructor)) static void "
                   "_lfortran_cuda_cpu_registrar_" << kernel_names[0]
                << "(void) {\n";
            for (auto &kname : kernel_names) {
                src << "    lfortran_gpu_register_kernel(\"" << kname
                    << "\", (kernel_func_t)__lf_thunk_" << kname << ");\n";
            }
            src << "}\n";
            return;
        }
        if (kernel_names.empty()) return;
        src << "typedef void (*kernel_func_t)(void);\n";
        src << "extern \"C\" void lfortran_gpu_register_kernel("
               "const char *name, kernel_func_t func);\n\n";
        src << "namespace {\n";
        src << "struct _lfortran_cuda_registrar {\n";
        src << "    _lfortran_cuda_registrar() {\n";
        for (auto &kname : kernel_names) {
            src << "        lfortran_gpu_register_kernel(\""
                << kname << "\", (kernel_func_t)" << kname << ");\n";
        }
        src << "    }\n";
        src << "} _lfortran_cuda_reg;\n";
        src << "}\n";
    }
};

Result<std::string> asr_to_cuda(Allocator & /*al*/, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co, bool emit_registrar)
{
    ASRToGpuCVisitor<CudaDialect> v(co,
        CudaDialect(emit_registrar, co.gpu_cpu_emulation, co.gpu_hip,
            gpu_device_capabilities(co.po).offset_kind));
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
