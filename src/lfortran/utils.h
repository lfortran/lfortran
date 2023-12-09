#ifndef LFORTRAN_UTILS_H
#define LFORTRAN_UTILS_H

#include <string>
#include <libasr/utils.h>

namespace LCompilers::LFortran {

void get_executable_path(std::string &executable_path, int &dirname_length);
std::string get_runtime_library_dir();
std::string get_runtime_library_header_dir();
std::string get_runtime_library_c_header_dir();
int32_t get_exit_status(int32_t err);

} // LCompilers::LFortran

#endif // LFORTRAN_UTILS_H
