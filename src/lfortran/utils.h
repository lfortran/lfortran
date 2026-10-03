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
std::string get_c_include_dir();
std::string get_dwarf_scripts_dir();
int32_t get_exit_status(int32_t err);
std::string get_kokkos_includedir();
std::string get_kokkos_libdir();

// The name of a symbol the compiler declares for the user symbol `name` in
// the role `role`, e.g. `f~fpcast` for a call-site interface of `f`. `~`
// cannot appear in a Fortran name, so a generated symbol neither hides nor is
// hidden by a user symbol of its own scope or of any scope around it. Callers
// make it unique among the generated symbols of its scope with
// `get_unique_name`.
inline std::string generated_symbol_name(const std::string &name,
        const std::string &role) {
    return name + "~" + role;
}

// True if `name` is the name of a symbol the compiler declared (see
// generated_symbol_name), which is not shown to the user as a symbol.
inline bool is_generated_symbol_name(const std::string &name) {
    return name.find('~') != std::string::npos;
}

} // LCompilers::LFortran

#endif // LFORTRAN_UTILS_H
