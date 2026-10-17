#ifndef LIBASR_PASS_GPU_DEVICE_HEAP_H
#define LIBASR_PASS_GPU_DEVICE_HEAP_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

// Allocates on the device heap every run-time sized array of device code
// that the host cannot size before the launch, gives every launch whose
// kernel allocates on the heap its arenas, and lowers every `stop` device
// code reaches (see gpu_device_heap.cpp and gpu_offload_stop.h).
void pass_gpu_device_heap(Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DEVICE_HEAP_H
