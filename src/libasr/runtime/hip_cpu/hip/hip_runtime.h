#ifndef LFORTRAN_HIP_CPU_RUNTIME_H
#define LFORTRAN_HIP_CPU_RUNTIME_H

// The header that the device code generated for HIP includes, in the CPU
// emulation of HIP (`--gpu=hip_cpu`). The emulated device code is host C++
// that cuda_cpu/cuda_cpu_device.h, which it includes next, makes compile:
// nothing of HIP is needed, so this header only has to exist.

#endif // LFORTRAN_HIP_CPU_RUNTIME_H
