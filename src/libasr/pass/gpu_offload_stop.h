#ifndef LIBASR_PASS_GPU_OFFLOAD_STOP_H
#define LIBASR_PASS_GPU_OFFLOAD_STOP_H

#include <vector>

#include <libasr/asr.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

/*
How a `stop` or `error stop` that device code reaches is run.

A thread cannot end the program, so the statement is split between the two
sides of the launch:

* On the device, the thread records which statement it reached (its site, a
  number from 1) and, when the statement has an integer stop code, the value
  of that code, in a small status buffer the kernel takes as an argument, and
  returns. Other threads carry on; the program ends after the launch anyway.
* On the host, the status buffer is read once the host has waited for the
  launch, and if a site was recorded, that same statement is run on the host,
  with the recorded value as its stop code. So the program prints and exits
  exactly as the statement does on the CPU. The launch does not wait for the
  kernel, so the statement is attached to the launch and run at the first
  wait the `gpu_launch_sync` pass places after it; the buffer is cleared once,
  on entry to the procedure, and a thread records its stop only while no
  site is recorded, so the first launch that stopped is the one reported.

The buffer holds the site in its first element, and one more element per
site for the stop code of that site. A thread writes the code first and the
site after it, and only a thread that reached a site writes its code, so
whichever thread's site the host reads, the code beside it is one a thread
that reached that site wrote: no atomics are needed for the two to agree.
Which of several failing threads is reported is not specified, as the
iterations of a `do concurrent` have no order.

A procedure the kernel calls gets two hidden scalar dummies, the site and
the code, which it sets before returning; every call to such a procedure is
followed by a test of the site, which returns from the caller in turn, up to
the kernel, which records it in the buffer. A procedure is given them when it
holds a `stop` or calls one that does.

The stop code of a site has to be either an integer, recorded at run time,
or a character constant, which the host statement repeats. A character code
computed at run time is not supported.

Everything here is ASR: the backends see assignments, `return` statements
and an extra kernel argument, and no `stop` in device code.
*/
struct GpuDeviceStops {
    // The host array the launch passes as the status buffer, or nullptr
    // when the kernel reaches no `stop`, in which case nothing else here is
    // set either.
    ASR::symbol_t *host_status = nullptr;
    // Host statements that run the recorded statement once the host has
    // waited for the launch (GpuKernelLaunch.stop_replay).
    std::vector<ASR::stmt_t*> after_launch;
};

// Lowers every `stop` and `error stop` that `kernel` reaches as described
// above, adding the status buffer as the last parameter of `kernel`. The
// host array that has to be passed for it is declared in `host_scope`. A
// decline is returned for what cannot be lowered, before anything is
// changed.
GpuDecline gpu_lower_device_stops(Allocator &al, const Location &loc,
    ASR::Function_t &kernel, SymbolTable *host_scope, GpuDeviceStops &stops);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_STOP_H
