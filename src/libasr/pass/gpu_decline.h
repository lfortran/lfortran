#ifndef LIBASR_PASS_GPU_DECLINE_H
#define LIBASR_PASS_GPU_DECLINE_H

#include <string>

#include <libasr/asr.h>

namespace LCompilers {

/*
Why a parallel loop was not offloaded to the GPU, as a value rather than as a
sentence. The pass that makes the decision (gpu_offload.cpp) and the pass that
lays out the launch (device_launch_expand.cpp) both raise these; the wording
the user sees is built in one place, from the reason and what little it
quotes, so that the decision and its phrasing cannot drift apart.

*/

enum class GpuDeclineReason {
    None,

    // --- the shape of the loop itself ---
    ReductionClause,
    LoopWithoutIndex,
    IncompleteLoopHead,
    StridedLoop,

    // --- what the pass's own lowering cannot yet do to the body ---
    StructElementGather,
    UnsizedLocalArray,
    AliasTemporaryRuntimeSized,
    UngatherableStridedSection,
    DeviceFunctionInlining,
    NestedArraySection,
    WorkspaceNotSizeableOnHost,
    VlaExtentNotRebuildableOnHost,

    // --- what the device has no type for ---
    LocalTypeWidth,
    SymbolTypeNotRepresentable,
    WideTypeNotOnDevice,

    // --- what the device has no way to run ---
    // These, and every reason below them, name a thing rather than state a
    // fact, and so read as "the gpu backend does not support <thing>".
    StatementIo,
    StatementStop,

    // --- the layout of a kernel argument: derived types ---
    StructDeclarationUnknown,
    StructNonDataMember,
    StructPointerMember,
    StructAllocatableArrayMember,
    StructAllocatableScalarMember,
    StructAssumedShapeArrayMember,
    StructMemberTypeWidth,
    StructMemberNotNumeric,

    // --- the layout of a kernel argument: polymorphic arguments ---
    ClassComponentArrayRank,
    ClassComponentArrayExtents,
    ClassDeclarationUnknown,
    ClassNonDataComponent,
    ClassAllocatableComponent,
    UnlimitedPolymorphicArgument,
    PolymorphicArrayArgument,

    // --- the layout of a kernel argument: everything else ---
    ArrayElementTypeWidth,
    ArrayElementNotNumeric,
    WorkspaceStructElementShape,
    // The same limitation as VlaExtentNotRebuildableOnHost above, reached by
    // the launch layout rather than by the host pre-flight in gpu_offload.
    LaunchVlaExtentNotRebuildable,
    KernelArgumentCountMismatch,
    NestedAllocatableComponent,
    MissingArgument,
    ScalarTypeWidth,
    ScalarNotNumeric,
    ScalarKindMismatch,
};

// A decline, with the little the message quotes alongside it.
struct GpuDecline {
    GpuDeclineReason reason = GpuDeclineReason::None;
    // The name the message names: a local, a workspace, a component, or the
    // routine an unsupported statement was found in.
    std::string name;
    // The element type the decline is about, when the reason names one:
    // `a scalar of real(16), which has no gpu type of the same width`.
    ASR::ttype_t *type = nullptr;

    GpuDecline() = default;
    explicit GpuDecline(GpuDeclineReason reason_) : reason(reason_) {}
    GpuDecline(GpuDeclineReason reason_, const std::string &name_)
        : reason(reason_), name(name_) {}
    GpuDecline(GpuDeclineReason reason_, const std::string &name_,
            ASR::ttype_t *type_)
        : reason(reason_), name(name_), type(type_) {}

    bool declined() const { return reason != GpuDeclineReason::None; }
};

// The whole clause the diagnostic reads, lowercase and naming nothing
// internal. This is the only place the wording of a decline is written.
std::string gpu_decline_message(const GpuDecline &decline);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DECLINE_H
