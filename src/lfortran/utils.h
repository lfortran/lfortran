#ifndef LFORTRAN_UTILS_H
#define LFORTRAN_UTILS_H

#include <string>
#include <libasr/utils.h>

namespace LCompilers::LFortran {

void get_executable_path(std::string &executable_path, int &dirname_length);
std::string get_runtime_library_dir();
std::string get_runtime_library_header_dir();
std::string generate_visualize_html(std::string &astr_data_json);

} // LCompilers::LFortran

#endif // LFORTRAN_UTILS_H
