#ifndef LIBASR_UTILS_H
#define LIBASR_UTILS_H

#include <string>
#include <filesystem>
#include <vector>
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
    std::filesystem::path mod_files_dir;
    std::vector<std::filesystem::path> include_dirs;
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
    std::string target = "";
    Platform platform;

    CompilerOptions () : platform{get_platform()} {};
};

// The reference returned should only ever be written by the cli arg parser.
// Absolutely do not use this as some kind of message broker.
// In the future it could be interesting to turn this into a singleton
// with setters and getters.
CompilerOptions& get_compiler_options();
bool read_file(const std::string &filename, std::string &text);
bool present(Vec<char*> &v, const char* name);
int initialize();

} // LFortran

#endif // LIBASR_UTILS_H
