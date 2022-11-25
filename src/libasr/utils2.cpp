#ifdef _WIN32
#define NOMINMAX
#include <windows.h>
#endif

#include <fstream>

#include <libasr/exception.h>
#include <libasr/utils.h>
#include <libasr/string_utils.h>

#ifdef HAVE_WHEREAMI
#include <bin/tpl/whereami/whereami.h>
#endif

namespace LCompilers {

bool read_file(const std::string &filename, std::string &text)
{
    std::ifstream ifs(filename.c_str(), std::ios::in | std::ios::binary
            | std::ios::ate);

    std::ifstream::pos_type filesize = ifs.tellg();
    if (filesize < 0) return false;

    ifs.seekg(0, std::ios::beg);

    std::vector<char> bytes(filesize);
    ifs.read(&bytes[0], filesize);

    text = std::string(&bytes[0], filesize);
    return true;
}

bool present(Vec<char*> &v, const char* name) {
    for (auto &a : v) {
        if (std::string(a) == std::string(name)) {
            return true;
        }
    }
    return false;
}

bool present(char** const v, size_t n, const std::string name) {
    for (size_t i = 0; i < n; i++) {
        if (std::string(v[i]) == name) {
            return true;
        }
    }
    return false;
}

Platform get_platform()
{
#if defined(_WIN32)
    return Platform::Windows;
#elif defined(__APPLE__)
#    ifdef __aarch64__
    return Platform::macOS_ARM;
#    else
    return Platform::macOS_Intel;
#    endif
#elif defined(__FreeBSD__)
    return Platform::FreeBSD;
#elif defined(__OpenBSD__)
    return Platform::OpenBSD;
#else
    return Platform::Linux;
#endif
}

// Platform-specific initialization
// On Windows, enable colors in terminal. On other systems, do nothing.
// Return value: 0 on success, negative number on failure.
int initialize()
{
#ifdef _WIN32
    HANDLE h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h_stdout == INVALID_HANDLE_VALUE)
        return -1;

    DWORD mode;
    if (! GetConsoleMode(h_stdout, &mode))
        return -2;

    mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    if (! SetConsoleMode(h_stdout, mode))
        return -3;

    return 0;
#else
    return 0;
#endif
}

void get_executable_path(std::string &executable_path, int &dirname_length)
{
#ifdef HAVE_WHEREAMI
    int length;

    length = wai_getExecutablePath(NULL, 0, &dirname_length);
    if (length > 0) {
        std::string path(length+1, '\0');
        wai_getExecutablePath(&path[0], length, &dirname_length);
        executable_path = path;
        if (executable_path[executable_path.size()-1] == '\0') {
            executable_path = executable_path.substr(0,executable_path.size()-1);
        }
    } else {
        throw LCompilersException("Cannot determine executable path.");
    }
#else
    executable_path = "src/bin/lpython.js";
    dirname_length = 7;
#endif
}

std::string get_runtime_library_dir() {
    char *env_p = std::getenv("LCOMPILERS_RUNTIME_LIBRARY_DIR");
    if (env_p) return env_p;

    std::string path;
    int dirname_length;
    get_executable_path(path, dirname_length);
    std::string dirname = path.substr(0,dirname_length);
    if (   endswith(dirname, "src/bin")
        || endswith(dirname, "src\\bin")
        || endswith(dirname, "SRC\\BIN")) {
        // Development version
        return dirname + "/../runtime";
    } else if (endswith(dirname, "src/lpython/tests") ||
               endswith(to_lower(dirname), "src\\lpython\\tests")) {
        // CTest Tests
        return dirname + "/../../runtime";
    } else {
        // Installed version
        return dirname + "/../share/lpython/lib";
    }
}

std::string get_runtime_library_header_dir()
{
    char *env_p = std::getenv("LCOMPILERS_RUNTIME_LIBRARY_HEADER_DIR");
    if (env_p) return env_p;

    return get_runtime_library_dir() + "/impure";
}

} // namespace LCompilers
