#ifndef LIBASR_UTILS_H
#define LIBASR_UTILS_H

#include <string>
#include <vector>
#include <filesystem>
#include <libasr/containers.h>

namespace LFortran {

enum Platform {
    Linux,
    macOS_Intel,
    macOS_ARM,
    Windows,
    FreeBSD
};

Platform get_platform();

struct CompilerOptions {
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
    bool rtlib = false;
    std::string target = "";
    Platform platform;

    CompilerOptions () : platform{get_platform()} {};
};

bool read_file(const std::string &filename, std::string &text);
bool present(Vec<char*> &v, const char* name);
int initialize();

} // LFortran

namespace LCompilers {

    struct PassOptions {
        std::filesystem::path mod_files_dir;
        std::vector<std::filesystem::path> include_dirs;

        std::string run_fun; // for global_stmts pass
        // TODO: Convert to std::filesystem::path (also change find_and_load_module())
        std::string runtime_library_dir;
        bool always_run = false; // for unused_functions pass
        bool inline_external_symbol_calls = true; // for inline_function_calls pass
        int64_t unroll_factor = 32; // for loop_unroll pass
        bool fast = false; // is fast flag enabled.
    };

}

#endif // LIBASR_UTILS_H
