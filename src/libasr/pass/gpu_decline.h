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

// The device dialect a loop is being offloaded to. Nothing outside
// gpu_device_capabilities() below branches on this: it is the key the
// capability table is written against, not a thing to test for.
enum class GpuDevice {
    None,
    Metal,
    Cuda,
};

// What the selected device can do, in the terms the offload passes need to
// reason in. Those passes ask what the device is capable of and never which
// device it is, so that everything a dialect can and cannot do is written
// down once -- in gpu_device_capabilities() -- and adding a dialect does not
// mean finding every place that named the old ones.
struct GpuDeviceCapabilities {
    // The dialect these answers describe. Nothing outside this struct
    // branches on it.
    GpuDevice device = GpuDevice::None;

    // Whether a device was selected at all. When none was, the offload
    // passes have nothing to do.
    bool device_selected() const { return device != GpuDevice::None; }

    // The widest integer and real kind the device has a scalar type of, in
    // the kind numbers a Fortran program writes. A buffer reaches the device
    // as a block of bytes sized from the host element type, so a device
    // whose widest type is narrower than the host's would stride through
    // that buffer at the wrong size: it would read and write the wrong
    // elements, and nothing would say so. A loop touching data wider than
    // this stays on the host.
    int max_integer_kind = 8;
    int max_real_kind = 8;

    // Whether a kernel can write text out as it runs. A device that can is
    // only waiting on the lowering for a Fortran print or write to be
    // written here; a device that cannot has no way to run one at all, and
    // no amount of work here would give it one.
    bool device_printf = true;

    // Whether a kernel can bring the program to a halt from a thread. There
    // is no exit code to deliver either way -- a grid has no status to
    // return -- but stopping is the part of a Fortran `stop` a device can
    // honour, and a device that cannot stop can honour none of it.
    bool device_abort = true;

    // Whether a device function may declare a local array whose extent is
    // only known once the kernel runs. Where it may not, the pass splices
    // such a callee into the kernel body instead, so that the local becomes
    // a kernel-level one and the per-thread workspace machinery can bind it
    // to a slice of a device buffer.
    bool device_function_runtime_sized_locals = true;

    // Whether the device has a scalar type of the same in-memory width as
    // `t`. Every dialect shares a floor -- the widths in
    // gpu_scalar_width_supported() -- and the kinds above narrow it further.
    bool has_scalar_type(ASR::ttype_t *t) const;

    // Whether `t` is a width the shared floor has and this device has not.
    // This is the question "does this device narrow it", which is not the
    // same as "can this device take it": a type the shared floor already
    // turns down is nothing this device narrowed.
    bool narrows_scalar_type(ASR::ttype_t *t) const;

    // Whether this device narrows the shared floor at all. When it does not,
    // a sweep of every symbol reaching the kernel against this device's type
    // set would only ask again what the kernel-argument and kernel-local
    // checks already ask on every device.
    bool narrows_scalar_types() const;

    // Whether the pass splices device callees into the kernel body for this
    // device. It does so exactly when a device function may not declare a
    // run-time sized local. The splice is also what can leave a section of a
    // section in the body -- an address no device pointer can express -- so
    // the pass looks for that shape exactly when it splices.
    bool splices_device_functions() const {
        return !device_function_runtime_sized_locals;
    }
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
    DeviceFunctionImplementation,
    RecursiveDeviceFunction,
    FunctionResultAllocation,
    NestedArraySection,
    WorkspaceNotSizeableOnHost,

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
    // A per-thread workspace the launch layout cannot size, found when the
    // launch is expanded. The host pre-flight in gpu_offload asks the same
    // question of the loop, but of the loop as it stands before the passes
    // that create such a workspace have run.
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

// What that device is capable of. The one place a dialect's name decides
// anything.
GpuDeviceCapabilities gpu_device_capabilities(GpuDevice device);
GpuDeviceCapabilities gpu_device_capabilities(const PassOptions &pass_options);

// Whether this decline is a gap in LFortran or a limit of the device.
GpuDeclineClass gpu_decline_class(const GpuDecline &decline,
        const GpuDeviceCapabilities &caps);

// The whole clause the diagnostic reads, lowercase and naming nothing
// internal. This is the only place the wording of a decline is written.
std::string gpu_decline_message(const GpuDecline &decline);

// A stable, greppable name for a class, for `--gpu-decline-stats`.
const char* gpu_decline_class_name(GpuDeclineClass cls);

void report_gpu_decline(const PassOptions &options, const Location &where,
    const GpuDecline &decline, bool has_fallback = true);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DECLINE_H
