#ifndef LFORTRAN_UTILS_H
#define LFORTRAN_UTILS_H

#include <string>
#include <filesystem>
#include <libasr/utils.h>

namespace LFortran {

namespace fs = std::filesystem;

void get_executable_path(std::string &executable_path, int &dirname_length);
std::string get_runtime_library_dir();
std::string get_runtime_library_header_dir();
fs::path which(const std::string& exe, const std::string& override_path = "");
} // LFortran

#endif // LFORTRAN_UTILS_H
