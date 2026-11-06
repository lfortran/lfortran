#pragma once

#include <filesystem>
#include <set>
#include <string>
#include <vector>

#include "libasr/utils.h"

// The device compiler driver and the architecture option every device
// fragment, the GPU runtime and the link share: nvcc's -arch, the HIP
// driver's --offload-arch (`hip`), or nothing for the CPU emulation.
std::string cuda_device_compiler_command(
    const std::string &device_compiler,
    bool emulate_cpu,
    bool hip,
    const std::string &gpu_arch);

// The include directory (holding hip/hip_runtime_api.h) and the library
// directory (holding libamdhip64) of the ROCm installation whose HIP driver
// `device_compiler` (hipcc, by name or path) is. The host C compiler needs
// the headers to compile the GPU runtime, and the program needs the
// libraries at run time. False if the headers are not found.
bool hip_toolkit_dirs(const std::string &device_compiler,
    std::string &include_dir, std::string &lib_dir);

// The directory holding `cuda_runtime.h` of the CUDA toolkit whose
// `device_compiler` (nvcc, by name or path) is used, or "" if it is not found.
// The host C compiler needs it to compile the CUDA GPU runtime.
std::string cuda_toolkit_include_dir(const std::string &device_compiler);

bool resolve_linker_library(
    const std::string &library_name,
    const std::vector<std::string> &lib_dirs,
    LCompilers::Platform platform,
    bool static_only,
    std::filesystem::path &resolved_path,
    bool &is_static_archive);

bool append_cuda_source_from_library(
    const std::string &library_name,
    const std::vector<std::string> &lib_dirs,
    LCompilers::Platform platform,
    bool static_only,
    std::set<std::string> &seen_sidecars,
    std::vector<std::string> &cuda_sources);

bool append_cuda_source_from_input(
    const std::string &input,
    std::set<std::string> &seen_sidecars,
    std::vector<std::string> &cuda_sources);
