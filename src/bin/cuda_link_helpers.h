#pragma once

#include <filesystem>
#include <set>
#include <string>
#include <vector>

#include "libasr/utils.h"

std::string cuda_device_compiler_command(
    const std::string &device_compiler,
    bool emulate_cpu,
    const std::string &gpu_arch);

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
