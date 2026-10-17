#ifndef LIBASR_PASS_GPU_ARRAY_STATEMENTS_H
#define LIBASR_PASS_GPU_ARRAY_STATEMENTS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Runs on the device the whole-array assignments of a host loop that
    // launches kernels, when they work on the data those kernels use. See
    // gpu_array_statements.cpp.
    void pass_gpu_array_statements(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_ARRAY_STATEMENTS_H
