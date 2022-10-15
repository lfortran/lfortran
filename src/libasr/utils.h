#ifndef LIBASR_UTILS_H
#define LIBASR_UTILS_H

#include <string>
#include <filesystem>
#include <vector>
#include <libasr/containers.h>

namespace LFortran {

void get_executable_path(std::string &executable_path, int &dirname_length);
std::string get_runtime_library_dir();
std::string get_runtime_library_header_dir();

enum Platform {
    Linux,
    macOS_Intel,
    macOS_ARM,
    Windows,
    FreeBSD
};

Platform get_platform();

struct CompilerOptions {
    /*
      If it ever comes a day when libasr is split from lfortran, your life
      will be easier if you put mod files, library and runtime dirs on their
      own struct, PlatformOptions. They are used by both lfortran and libasr,
      yet passing CompilerOptions down to libasr is not the cleanest thing and
      PassOptions is managed by asr_to_lang functions and thus can't be declared
      on lfortran.cpp and be passed down like CompilerOptions.
     */
    std::filesystem::path mod_files_dir;
    std::vector<std::filesystem::path> include_dirs;
    std::filesystem::path rl_path;

    bool fixed_form = false;
    bool c_preprocessor = false;
    std::vector<std::string> c_preprocessor_defines;
    bool prescan = true;
    bool disable_main = false;
    bool symtab_only = false;
    bool show_stacktrace = false;
    bool use_colors = true;
    bool indent = false;
    bool tree = false;
    bool fast = false;
    bool openmp = false;
    bool generate_object_code = false;
    bool no_warnings = false;
    bool no_error_banner = false;
    std::string error_format = "human";
    bool new_parser = false;
    bool implicit_typing = false;
    bool implicit_interface = false;
    std::string target = "";
    Platform platform;

    CompilerOptions () : rl_path{ get_runtime_library_dir() }, platform{get_platform()} {};
};

bool read_file(const std::string &filename, std::string &text);
bool present(Vec<char*> &v, const char* name);
int initialize();

} // LFortran

namespace LCompilers {

    struct PassOptions {
        std::string run_fun; // for global_stmts pass
        bool always_run = false; // for unused_functions pass
        bool inline_external_symbol_calls = true; // for inline_function_calls pass
        int64_t unroll_factor = 32; // for loop_unroll pass
        bool fast = false; // is fast flag enabled.
    };

}

#endif // LIBASR_UTILS_H
