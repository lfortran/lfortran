#ifndef LFORTRAN_UTILS_H
#define LFORTRAN_UTILS_H

#include <libasr/utils.h>

#include <string>

namespace LFortran {

void get_executable_path(std::string &executable_path, int &dirname_length);
std::string get_runtime_library_dir();
std::string get_runtime_library_header_dir();

}  // namespace LFortran

#endif  // LFORTRAN_UTILS_H
