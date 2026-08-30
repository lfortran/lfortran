#include <libasr/codegen/asr_to_cuda.h>
#include <libasr/codegen/asr_to_gpu_c.h>

namespace LCompilers {

// CUDA C++, and the CPU emulation of it that `--gpu=cuda_cpu` compiles as
// ordinary host code. The spellings the shared device code generator asks a
// dialect for; everything else it derives from the ASR itself.
struct CudaDialect {
    // Whether to emit the trailing kernel-registration shim, which is only
    // meaningful to the runtime's lookup-by-name launch path.
    bool emit_registrar = true;
    // Whether to emit the extra shims the CPU emulation needs. The real CUDA
    // output is left untouched.
    bool emulate_cpu = false;

    CudaDialect(bool emit_registrar_, bool emulate_cpu_)
        : emit_registrar(emit_registrar_), emulate_cpu(emulate_cpu_) {}

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

    void emit_prologue(std::stringstream &src) const {
        src << "#include <stdint.h>\n";
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

    // The grid position is a built-in rather than a parameter, so the kernel
    // takes none and derives it in its first statement instead.
    void emit_thread_id_param(std::stringstream & /*src*/,
            bool /*has_prev*/) const {}

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
            // The thunks are registered instead of the kernels, and a plain
            // constructor attribute keeps the file free of any C++ runtime
            // dependency. Under separate compilation the per-object device
            // sources are concatenated, so the registrar is named after the
            // first kernel to keep it unique per translation unit.
            src << "typedef void (*kernel_func_t)(void **);\n";
            src << "extern \"C\" void lfortran_gpu_register_kernel("
                   "const char *name, kernel_func_t func);\n\n";
            src << "__attribute__((constructor)) static void "
                   "_lfortran_cuda_cpu_registrar_" << kernel_names[0]
                << "(void) {\n";
            for (auto &kname : kernel_names) {
                src << "    lfortran_gpu_register_kernel(\"" << kname
                    << "\", __lf_thunk_" << kname << ");\n";
            }
            src << "}\n";
            return;
        }
        src << "typedef void (*kernel_func_t)(void);\n";
        src << "extern \"C\" void lfortran_gpu_register_kernel("
               "const char *name, kernel_func_t func);\n\n";
        src << "struct _lfortran_cuda_registrar {\n";
        src << "    _lfortran_cuda_registrar() {\n";
        for (auto &kname : kernel_names) {
            src << "        lfortran_gpu_register_kernel(\""
                << kname << "\", (kernel_func_t)" << kname << ");\n";
        }
        src << "    }\n";
        src << "} _lfortran_cuda_reg;\n";
    }
};

Result<std::string> asr_to_cuda(Allocator & /*al*/, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co, bool emit_registrar)
{
    ASRToGpuCVisitor<CudaDialect> v(co,
        CudaDialect(emit_registrar, co.gpu_cpu_emulation));
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
