#ifndef LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H
#define LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <vector>

namespace LCompilers {

    void pass_device_launch_expand(Allocator &al, ASR::TranslationUnit_t &unit,
                                   const PassOptions &pass_options);

    // The selector an associate name is bound to, or nullptr when `name`
    // is not an associate name bound once.
    ASR::expr_t* gpu_associate_selector(ASR::symbol_t *name);

    // Whether the elements of the array argument `arg` may not be laid out
    // end to end: a dummy declared assumed-shape, or a pointer -- other
    // than an associate name bound to a whole array -- either of which may
    // be bound to a section of something larger.
    bool gpu_argument_may_be_strided(ASR::expr_t *arg);

    // Whether `arg` is a polymorphic value, which reaches the device as a
    // copy of the components of its declared type.
    bool gpu_argument_is_polymorphic_value(ASR::expr_t *arg);

    // How the host hands one buffer of a launch's layout to the device.
    enum class GpuBufferHandling {
        // The argument's own storage, which the runtime copies or keeps on
        // the device (lfortran_gpu_runtime.h).
        Storage,
        // A copy the host makes around the launch -- of a strided array,
        // of a component of each element of an array of derived type, or
        // of every array of a packed launch -- from the argument's data,
        // and back into it when the kernel writes it.
        HostCopy,
        // A copy of the components a polymorphic value holds itself, not
        // the data of its allocatable components.
        HostCopyShallow,
        // A buffer only the device holds.
        Scratch,
    };

    // How a launch with chunk dimensions (GpuKernelLaunch.chunk_dimensions)
    // that has more iterations than the chunk limit `limit` splits them:
    // appends to `out` the statements that set `pieces[p]` to the number of
    // values of the p-th dimension it splits a chunk covers, given the
    // `extents` of those dimensions, fastest first and each at least one,
    // and `largest` to the iterations of the largest chunk, the first one.
    // `fixed_iterations` is GpuKernelLaunch.chunk_fixed_iterations. Every
    // expression is an integer(8), and `pieces` and `largest` are variables.
    void gpu_chunk_pieces(Allocator &al, const Location &loc,
        Vec<ASR::stmt_t*> &out, const std::vector<ASR::expr_t*> &extents,
        int64_t fixed_iterations, int64_t limit,
        const std::vector<ASR::expr_t*> &pieces, ASR::expr_t *largest);

    GpuBufferHandling gpu_launch_buffer_handling(
        const ASR::GpuKernelLaunch_t &launch,
        const ASR::gpu_kernel_argument_t &entry);

} // namespace LCompilers

#endif // LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H
