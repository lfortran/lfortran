#ifndef LFORTRAN_UTILS_H
#define LFORTRAN_UTILS_H

#include <string>
#include <libasr/utils.h>

namespace LCompilers::LFortran {

enum class ExecutionMode {
    LFortranDevelopment,
    LFortranInstalled,
    LFortranCtest,
};

void get_executable_path(std::string &executable_path, int &dirname_length);
void set_exec_path_and_mode(std::string &executable_path, int &dirname_length);
std::string get_runtime_library_dir();
std::string get_runtime_library_header_dir();
std::string get_runtime_library_c_header_dir();
std::string get_dwarf_scripts_dir();
int32_t get_exit_status(int32_t err);
std::string get_kokkos_includedir();
std::string get_kokkos_libdir();

} // LCompilers::LFortran

#endif // LFORTRAN_UTILS_H
