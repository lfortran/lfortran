#ifndef LIBASR_PASS_GPU_DECLINE_H
#define LIBASR_PASS_GPU_DECLINE_H

#include <string>

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

/*
Why a parallel loop was not offloaded to the GPU, as a value rather than as a
sentence. The pass that makes the decision (gpu_offload.cpp) and the pass that
lays out the launch (device_launch_expand.cpp) both raise these; the wording
the user sees is built in one place, from the reason and what little it
quotes, so that the decision and its phrasing cannot drift apart.

Every reason is classified, and the classification is what the offload policy
is meant to act on: a `NotImplemented` decline is a gap in this compiler and
one day will not be raised at all, while a `BackendCannot` decline is a fact
about the device that no amount of work here would change.
*/

// The device dialect a loop is being offloaded to. The classification depends
// on it: `real(8)` data is something the Metal device has no type for at all,
// while the very same loop offloads onto CUDA.
enum class GpuDevice {
    None,
    Metal,
    Cuda,
};

// What a decline says about the compiler and about the device.
enum class GpuDeclineClass {
    // LFortran could lower this onto the selected device, but does not yet.
    // Every one of these is a gap to be closed.
    NotImplemented,
    // The selected device genuinely cannot express it, so no amount of work
    // in this pass would put this loop on this device.
    BackendCannot,
};

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
    // The element type the decline is about, when the reason is about a
    // type. The classification asks the selected device whether it has a
    // type of that width, so that the same reason can be a limit of the
    // device on one backend and a gap in this pass on another.
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

// Which device the pass options select, if any.
GpuDevice gpu_device_selected(const PassOptions &pass_options);

// The one backend capability the classification needs today: whether `device`
// has a scalar type of the same in-memory width as `t`. A buffer reaches the
// device as bytes sized from the host element type, so a device without a
// type of that width would stride through it wrongly.
//
// This is deliberately the smallest query that answers the question, and not
// a description of a backend. When backend capabilities grow a home of their
// own, this function is the seam that moves into it.
bool gpu_device_has_scalar_type(GpuDevice device, ASR::ttype_t *t);

// Whether this decline is a gap in LFortran or a limit of the device.
GpuDeclineClass gpu_decline_class(const GpuDecline &decline, GpuDevice device);

// The whole clause the diagnostic reads, lowercase and naming nothing
// internal. This is the only place the wording of a decline is written.
std::string gpu_decline_message(const GpuDecline &decline);

// A stable, greppable name for a class, for `--gpu-decline-stats`.
const char* gpu_decline_class_name(GpuDeclineClass cls);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DECLINE_H
