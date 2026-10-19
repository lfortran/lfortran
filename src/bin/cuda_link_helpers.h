#pragma once

#include <filesystem>
#include <set>
#include <string>
#include <vector>

#include "libasr/utils.h"

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
