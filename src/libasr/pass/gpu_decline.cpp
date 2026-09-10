#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_decline.h>
#include <iostream>

namespace LCompilers {

void report_gpu_decline(const PassOptions &options, const Location &where,
        const GpuDecline &decline, bool has_fallback) {
    LCOMPILERS_ASSERT(decline.declined());
    if (!options.diagnostics) return;
    GpuDeclineClass category = gpu_decline_class(decline,
        gpu_device_capabilities(options));
    std::string why = gpu_decline_message(decline);
    if (options.gpu_decline_stats) {
        std::cerr << "gpu-decline: " << gpu_decline_class_name(category)
            << ": " << why << std::endl;
    }
    bool fallback = has_fallback && options.gpu_allow_cpu_fallback;
    if (fallback) {
        options.diagnostics->message_label(
            "parallel loop not offloaded to the GPU, "
            "it runs on the CPU instead", {where}, why,
            diag::Level::Warning, diag::Stage::ASRPass);
    } else {
        options.diagnostics->message_label(
            "parallel loop cannot be offloaded to the GPU: " + why +
                (has_fallback
                    ? "; pass `--gpu-allow-cpu-fallback` to run it on the CPU instead"
                    : "; no CPU alternative is available for this launch"),
            {where}, why, diag::Level::Error, diag::Stage::ASRPass);
    }
}

GpuDevice gpu_device_selected(const PassOptions &pass_options) {
    if (pass_options.gpu_offload_metal) return GpuDevice::Metal;
    if (pass_options.gpu_offload_cuda) return GpuDevice::Cuda;
    return GpuDevice::None;
}

// Everything the offload machinery knows about a dialect, in one table. A
// pass that needs a new answer about a device grows a field here rather than
// a test of `device` where the question is asked.
GpuDeviceCapabilities gpu_device_capabilities(GpuDevice device) {
    GpuDeviceCapabilities caps;
    caps.device = device;
    switch (device) {
        case GpuDevice::Metal:
            // The Metal Shading Language has `float`, `half` and `bfloat`
            // but no 64-bit floating point type, and the emitter has no
            // 64-bit integer of its own either.
            caps.max_integer_kind = 4;
            caps.max_real_kind = 4;
            // Metal shaders have neither variable-length arrays nor a heap,
            // so a device function cannot declare a local whose extent is
            // only known once the kernel runs.
            caps.device_function_runtime_sized_locals = false;
            break;
        case GpuDevice::Cuda:
            // CUDA C++ has `double` and `long long`, so it narrows nothing
            // the shared width table permits.
            //
            // CUDA C++ has no variable-length arrays either -- a device
            // function's locals are laid out in registers and local memory
            // whose size the compiler has to know -- so a run-time sized
            // local has to be moved to kernel scope here too.
            caps.device_function_runtime_sized_locals = false;
            break;
        case GpuDevice::None:
            break;
    }
    return caps;
}

GpuDeviceCapabilities gpu_device_capabilities(const PassOptions &pass_options) {
    return gpu_device_capabilities(gpu_device_selected(pass_options));
}

bool GpuDeviceCapabilities::has_scalar_type(ASR::ttype_t *t) const {
    if (t == nullptr) return false;
    // The width table every device we emit for shares: a kind the table
    // turns down has no device type of the host's width anywhere.
    if (!gpu_scalar_width_supported(t)) return false;
    switch (t->type) {
        case ASR::ttypeType::Real:
            return ASR::down_cast<ASR::Real_t>(t)->m_kind <= max_real_kind;
        case ASR::ttypeType::Integer:
            return ASR::down_cast<ASR::Integer_t>(t)->m_kind
                <= max_integer_kind;
        default:
            return true;
    }
}

bool GpuDeviceCapabilities::narrows_scalar_type(ASR::ttype_t *t) const {
    if (t == nullptr) return false;
    return gpu_scalar_width_supported(t) && !has_scalar_type(t);
}

bool GpuDeviceCapabilities::narrows_scalar_types() const {
    return max_integer_kind < 8 || max_real_kind < 8;
}

GpuDeclineClass gpu_decline_class(const GpuDecline &decline,
        const GpuDeviceCapabilities &caps) {
    switch (decline.reason) {
        // Native floating-point width is a backend limit. Missing integer,
        // logical, character or aggregate lowering is compiler work, not a
        // reason to silently waive strict mode.
        case GpuDeclineReason::LocalTypeWidth:
        case GpuDeclineReason::SymbolTypeNotRepresentable:
        case GpuDeclineReason::WideTypeNotOnDevice:
        case GpuDeclineReason::StructMemberTypeWidth:
        case GpuDeclineReason::ArrayElementTypeWidth:
        case GpuDeclineReason::ScalarTypeWidth:
            if (decline.type && ASR::is_a<ASR::Real_t>(*decline.type) &&
                    ASR::down_cast<ASR::Real_t>(decline.type)->m_kind >
                        caps.max_real_kind) {
                return GpuDeclineClass::BackendCannot;
            }
            return GpuDeclineClass::NotImplemented;

        case GpuDeclineReason::StructMemberNotNumeric:
        case GpuDeclineReason::ArrayElementNotNumeric:
        case GpuDeclineReason::ScalarNotNumeric:
            return GpuDeclineClass::NotImplemented;

        // A statement the device has no way to run: there are no Fortran
        // units, formats or exit codes on a device.
        case GpuDeclineReason::StatementIo:
        case GpuDeclineReason::StatementStop:
            return GpuDeclineClass::BackendCannot;

        // Everything else is a lowering this pass has not written yet.
        case GpuDeclineReason::None:
        case GpuDeclineReason::ReductionClause:
        case GpuDeclineReason::LoopWithoutIndex:
        case GpuDeclineReason::IncompleteLoopHead:
        case GpuDeclineReason::StridedLoop:
        case GpuDeclineReason::StructElementGather:
        case GpuDeclineReason::UnsizedLocalArray:
        case GpuDeclineReason::AliasTemporaryRuntimeSized:
        case GpuDeclineReason::UngatherableStridedSection:
        case GpuDeclineReason::DeviceFunctionInlining:
        case GpuDeclineReason::DeviceFunctionImplementation:
        case GpuDeclineReason::RecursiveDeviceFunction:
        case GpuDeclineReason::FunctionResultAllocation:
        case GpuDeclineReason::NestedArraySection:
        case GpuDeclineReason::WorkspaceNotSizeableOnHost:
        case GpuDeclineReason::StructDeclarationUnknown:
        case GpuDeclineReason::StructNonDataMember:
        case GpuDeclineReason::StructPointerMember:
        case GpuDeclineReason::StructAllocatableArrayMember:
        case GpuDeclineReason::StructAllocatableScalarMember:
        case GpuDeclineReason::StructAssumedShapeArrayMember:
        case GpuDeclineReason::ClassComponentArrayRank:
        case GpuDeclineReason::ClassComponentArrayExtents:
        case GpuDeclineReason::ClassDeclarationUnknown:
        case GpuDeclineReason::ClassNonDataComponent:
        case GpuDeclineReason::ClassAllocatableComponent:
        case GpuDeclineReason::UnlimitedPolymorphicArgument:
        case GpuDeclineReason::PolymorphicArrayArgument:
        case GpuDeclineReason::WorkspaceStructElementShape:
        case GpuDeclineReason::LaunchVlaExtentNotRebuildable:
        case GpuDeclineReason::KernelArgumentCountMismatch:
        case GpuDeclineReason::NestedAllocatableComponent:
        case GpuDeclineReason::MissingArgument:
        case GpuDeclineReason::ScalarKindMismatch:
            return GpuDeclineClass::NotImplemented;
    }
    return GpuDeclineClass::NotImplemented;
}

// The routine an unsupported statement was found in, when it was reached
// through a call rather than written in the loop body.
static std::string in_routine(const GpuDecline &decline) {
    if (decline.name.empty()) return "";
    return " in '" + decline.name + "'";
}

std::string gpu_decline_message(const GpuDecline &decline) {
    // The kind as a user writes it, for the reasons that name one.
    std::string type_name = decline.type != nullptr
        ? gpu_scalar_type_name(decline.type) : std::string("that type");
    // The reasons that name a thing the device has no support for all read
    // the same way; the rest already state a fact of their own.
    std::string unsupported = "the gpu backend does not support ";
    switch (decline.reason) {
        case GpuDeclineReason::None:
            return "";

        case GpuDeclineReason::ReductionClause:
            return "a reduction has no gpu lowering yet";
        case GpuDeclineReason::LoopWithoutIndex:
            return "the loop has no index";
        case GpuDeclineReason::IncompleteLoopHead:
            return "the loop head is incomplete";
        case GpuDeclineReason::StridedLoop:
            return "the loop has a stride the gpu index arithmetic "
                "cannot express";

        case GpuDeclineReason::StructElementGather:
            return "a derived-type element cannot be gathered for the gpu";
        case GpuDeclineReason::UnsizedLocalArray:
            return "local array '" + decline.name +
                "' has no extent the gpu can use";
        case GpuDeclineReason::AliasTemporaryRuntimeSized:
            return "an aliased assignment needs a run-time sized temporary";
        case GpuDeclineReason::UngatherableStridedSection:
            return "a strided section cannot be gathered for the gpu";
        case GpuDeclineReason::DeviceFunctionInlining:
            return "a device function cannot be inlined";
        case GpuDeclineReason::DeviceFunctionImplementation:
            return "procedure '" + decline.name + "' has no device implementation";
        case GpuDeclineReason::RecursiveDeviceFunction:
            return "recursive device procedure '" + decline.name +
                "' has no gpu lowering yet";
        case GpuDeclineReason::FunctionResultAllocation:
            return "the allocatable array result of '" + decline.name +
                "' has no single allocation the gpu can use";
        case GpuDeclineReason::NestedArraySection:
            return "a nested array section cannot be addressed on the gpu";
        case GpuDeclineReason::WorkspaceNotSizeableOnHost:
            return "workspace '" + decline.name +
                "' cannot be sized on the host";

        case GpuDeclineReason::LocalTypeWidth:
            return "local '" + decline.name +
                "' has no gpu type of the same width";
        case GpuDeclineReason::SymbolTypeNotRepresentable:
            return "the type of '" + decline.name +
                "' is not representable on the gpu";
        case GpuDeclineReason::WideTypeNotOnDevice:
            return unsupported + type_name + ", used by '" +
                decline.name + "'";

        case GpuDeclineReason::StatementIo:
            return unsupported + "input or output" + in_routine(decline);
        case GpuDeclineReason::StatementStop:
            return unsupported + "stop" + in_routine(decline);

        case GpuDeclineReason::StructDeclarationUnknown:
            return unsupported +
                "a derived type whose declaration is not known";
        case GpuDeclineReason::StructNonDataMember:
            return unsupported + "a derived type with a non-data member";
        case GpuDeclineReason::StructPointerMember:
            return unsupported + "a derived type with a pointer member";
        case GpuDeclineReason::StructAllocatableArrayMember:
            return unsupported + "a derived type with an allocatable array "
                "member the gpu backend cannot decompose";
        case GpuDeclineReason::StructAllocatableScalarMember:
            return unsupported +
                "a derived type with an allocatable scalar member";
        case GpuDeclineReason::StructAssumedShapeArrayMember:
            return unsupported +
                "a derived type with an assumed shape array member";
        case GpuDeclineReason::StructMemberTypeWidth:
            return unsupported + "a derived type with a " + type_name +
                " member, which has no gpu type of the same width";
        case GpuDeclineReason::StructMemberNotNumeric:
            return unsupported +
                "a derived type with a member that is not a number";

        case GpuDeclineReason::ClassComponentArrayRank:
            return unsupported + "a polymorphic argument with a component "
                "array of no rank the gpu backend can copy";
        case GpuDeclineReason::ClassComponentArrayExtents:
            return unsupported + "a polymorphic argument with a component "
                "array whose extents are not known where it is passed";
        case GpuDeclineReason::ClassDeclarationUnknown:
            return unsupported +
                "a polymorphic argument whose declared type is not known";
        case GpuDeclineReason::ClassNonDataComponent:
            return unsupported +
                "a polymorphic argument with a non-data component";
        case GpuDeclineReason::ClassAllocatableComponent:
            return unsupported + "a polymorphic argument with an allocatable "
                "or pointer component the gpu backend cannot copy";
        case GpuDeclineReason::UnlimitedPolymorphicArgument:
            return unsupported + "an unlimited polymorphic argument";
        case GpuDeclineReason::PolymorphicArrayArgument:
            return unsupported + "an array of a polymorphic type";

        case GpuDeclineReason::ArrayElementTypeWidth:
            return unsupported + "an array of " + type_name +
                ", which has no gpu type of the same width";
        case GpuDeclineReason::ArrayElementNotNumeric:
            return unsupported + "an array whose elements are not numbers";
        case GpuDeclineReason::WorkspaceStructElementShape:
            return unsupported + "a workspace sized from a struct element "
                "whose shape may differ per thread";
        case GpuDeclineReason::LaunchVlaExtentNotRebuildable:
            if (!decline.name.empty()) {
                return unsupported + "the per-thread workspace for `" +
                    decline.name + "`, whose extent cannot be rebuilt on "
                    "the host";
            }
            return unsupported + "a variable length array whose extent "
                "cannot be rebuilt on the host";
        case GpuDeclineReason::KernelArgumentCountMismatch:
            return unsupported +
                "a kernel that takes a different number of arguments";
        case GpuDeclineReason::NestedAllocatableComponent:
            return unsupported + "an allocatable component `" + decline.name +
                "` reached through another component, which has no buffer "
                "of its own";
        case GpuDeclineReason::MissingArgument:
            return unsupported + "a missing argument";
        case GpuDeclineReason::ScalarTypeWidth:
            return unsupported + "a scalar of " + type_name +
                ", which has no gpu type of the same width";
        case GpuDeclineReason::ScalarNotNumeric:
            return unsupported +
                "a scalar that is not an integer, a real, or a logical";
        case GpuDeclineReason::ScalarKindMismatch:
            return unsupported +
                "a scalar whose kind differs from the kernel parameter";
    }
    return "";
}

const char* gpu_decline_class_name(GpuDeclineClass cls) {
    switch (cls) {
        case GpuDeclineClass::NotImplemented: return "not-implemented";
        case GpuDeclineClass::BackendCannot: return "backend-cannot";
    }
    return "unknown";
}

} // namespace LCompilers
