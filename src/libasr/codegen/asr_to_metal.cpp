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

    // The grid position is a parameter of the kernel rather than a built-in.
    void emit_thread_id_param(std::stringstream &src, bool has_prev) const {
        if (has_prev) src << ",\n";
        src << "    uint __thread_id [[thread_position_in_grid]]";
    }

    std::string uint_type() const {
        return "uint";
    }

    // The scalars a kernel takes travel together in one read-only buffer.
    void emit_scalar_args_param(std::stringstream &src,
            const std::string &struct_name, int buffer_index) const {
        src << "    " << addr_space(ASR::memory_spaceType::Constant) << " "
            << struct_name << "& __scalar_args"
            << buffer_attr(buffer_index);
    }

    void emit_kernel_body_prologue(std::stringstream & /*src*/,
            const std::string & /*indent*/) const {}

    void emit_kernel_epilogue(std::stringstream & /*src*/,
            const std::string & /*kernel_name*/,
            const std::vector<GpuKernelParam> & /*params*/,
            const std::string & /*scalar_struct_name*/) const {}

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
