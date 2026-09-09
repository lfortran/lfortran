#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

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
        case GpuDeclineReason::NestedArraySection:
            return "a nested array section cannot be addressed on the gpu";
        case GpuDeclineReason::WorkspaceNotSizeableOnHost:
            return "workspace '" + decline.name +
                "' cannot be sized on the host";
        case GpuDeclineReason::VlaExtentNotRebuildableOnHost:
            return "a variable length array whose extent "
                "cannot be rebuilt on the host";

        case GpuDeclineReason::LocalTypeWidth:
            return "local '" + decline.name +
                "' has no gpu type of the same width";
        case GpuDeclineReason::SymbolTypeNotRepresentable:
            return "the type of '" + decline.name +
                "' is not representable on the gpu";
        case GpuDeclineReason::WideTypeNotOnDevice:
            return "the Metal backend does not support " + type_name +
                ", used by '" + decline.name + "'";

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

} // namespace LCompilers
