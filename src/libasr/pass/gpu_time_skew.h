#ifndef LIBASR_PASS_GPU_TIME_SKEW_H
#define LIBASR_PASS_GPU_TIME_SKEW_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <map>
#include <set>

namespace LCompilers {

    struct GpuWindow;

    // Runs the parallel loops assigned to the device inside a sequential
    // (time) loop band by band, a few iterations of the time loop at a
    // time, so that a band stays in the device's cache across them, when
    // the arrays they touch do not fit in it. See gpu_time_skew.cpp.
    void pass_gpu_time_skew(Allocator &al, ASR::TranslationUnit_t &unit,
                            const PassOptions &pass_options);

    // The skewing, leaving the loops in `rarely_run` out of its cost model
    // and adding their windowed copies to it, and recording in `windows`
    // the domain of each loop it runs on a window (GpuWindows in
    // gpu_loop_utils.h).
    void gpu_time_skew(Allocator &al, ASR::TranslationUnit_t &unit,
                       const PassOptions &pass_options,
                       std::set<ASR::stmt_t*> &rarely_run,
                       std::map<ASR::stmt_t*, GpuWindow> &windows);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_TIME_SKEW_H
