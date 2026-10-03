#ifndef LIBASR_PASS_GPU_UNSUPPORTED_CHECK_H
#define LIBASR_PASS_GPU_UNSUPPORTED_CHECK_H

#include <string>

#include <libasr/asr.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

/*
The unsupported-construct check of doc/src/gpu_offloading.md.

It decides, from a parallel loop as written and before any GPU lowering,
whether the loop uses a construct on the unsupported list for the selected
device:

* a real kind wider than the widest floating point type of the device
  (`real(8)` on Metal, `real(10)` and `real(16)` everywhere);
* an input/output statement, internal-file I/O included (every device);
* `error stop` or `stop`, on a device that cannot halt the program (Metal).

A loop that passes is committed to the device. A loop that fails is either an
error at the offending construct or, with `--gpu-allow-cpu-fallback`, assigned
to the host. The offloading pipeline does not revisit the decision.

What is examined: the loop, and every procedure it reaches transitively (their
bodies and the declared types of their dummy arguments, results and locals).
For types, every expression evaluated is looked at, with one refinement for
derived types: a component reference `s%m` is judged by the type of `m` alone,
since only that component reaches the device, while a derived-type value used
as a whole (passed, assigned, copied, or declared as device-side storage) is
judged by all of its components, recursively, since its whole layout reaches
the device.
*/

enum class GpuUnsupportedKind {
    None,
    RealWidth,
    InputOutput,
    ErrorStop,
    Stop,
};

struct GpuUnsupportedConstruct {
    GpuUnsupportedKind kind = GpuUnsupportedKind::None;
    // Where the construct is written.
    Location loc;
    // For RealWidth: the real type the device has no type for.
    ASR::ttype_t *type = nullptr;
    // For RealWidth: the variable or component the type belongs to, when the
    // construct is one; empty for any other expression.
    std::string name;
    // For RealWidth: the component of `name` that has the type, when a
    // derived-type value is used or declared as a whole.
    std::string component;
    // For RealWidth: whether `loc` is a declaration rather than a use.
    bool declaration = false;
    // The procedure the construct is in, when the loop reaches it through a
    // call; empty for the loop itself.
    std::string routine;

    bool found() const { return kind != GpuUnsupportedKind::None; }
};

// The first construct on the unsupported list of the device `caps` that the
// parallel loop `region` uses, or one whose `found()` is false.
GpuUnsupportedConstruct gpu_find_unsupported_construct(
    const ASR::OMPRegion_t &region, const GpuDeviceCapabilities &caps);

// "the Metal GPU does not support real(8)": what the construct is and which
// device lacks it.
std::string gpu_unsupported_construct_message(
    const GpuUnsupportedConstruct &construct,
    const GpuDeviceCapabilities &caps);

// The label attached to the construct itself.
std::string gpu_unsupported_construct_label(
    const GpuUnsupportedConstruct &construct);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_UNSUPPORTED_CHECK_H
