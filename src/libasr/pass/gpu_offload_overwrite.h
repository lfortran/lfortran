#ifndef LIBASR_PASS_GPU_OFFLOAD_OVERWRITE_H
#define LIBASR_PASS_GPU_OFFLOAD_OVERWRITE_H

#include <vector>

#include <libasr/asr.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

// A host condition that is true when every iteration of `loop` taken
// together assigns each element of `array` before any iteration reads it,
// so that what the host holds in `array` does not have to reach the device;
// nullptr when that cannot be shown for every element. `starts` and `ends`
// are the limits of the loops of the nest, as the host evaluated them
// before the launch.
//
// It is shown only for an assignment `array(i, j, ...) = value`, with the
// loop variables as subscripts, one per loop, that every iteration the
// guards around it let through reaches, before anything else of the
// iteration names `array`. A guard is an IF whose test compares loop
// variables with what the iterations cannot change. The condition is that
// the loops, narrowed by the guards, run over exactly the bounds of `array`.
ASR::expr_t* gpu_overwrite_condition(Allocator &al, const Location &loc,
    const ParallelLoopNest &loop, const std::vector<ASR::expr_t*> &starts,
    const std::vector<ASR::expr_t*> &ends, ASR::expr_t *array);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_OVERWRITE_H
