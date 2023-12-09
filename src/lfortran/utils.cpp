#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX
#endif // NOMINMAX
#include <windows.h>
#endif

#include <config.h>

#include <fstream>

#include <bin/tpl/whereami/whereami.h>

#include <libasr/exception.h>
#include <lfortran/utils.h>
#include <libasr/string_utils.h>

namespace LCompilers::LFortran {

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
    executable_path = "src/bin/lfortran.js";
    dirname_length = 7;
#endif
}

std::string get_runtime_library_dir()
{
#ifdef HAVE_BUILD_TO_WASM
    return "asset_dir";
#endif
    char *env_p = std::getenv("LFORTRAN_RUNTIME_LIBRARY_DIR");
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
    } else if (endswith(dirname, "src/lfortran/tests") ||
               endswith(to_lower(dirname), "src\\lfortran\\tests")) {
        // CTest Tests
        return dirname + "/../../runtime";
    } else {
        // Installed version
        return CMAKE_INSTALL_FULL_LIBDIR;
    }
}

std::string get_runtime_library_header_dir()
{
    char *env_p = std::getenv("LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR");
    if (env_p) return env_p;

    std::string install_p = CMAKE_INSTALL_FULL_INCLUDEDIR;
    return install_p + "/lfortran/impure";
}

std::string get_runtime_library_c_header_dir()
{
    char *env_p = std::getenv("LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR");
    if (env_p) return env_p;

    // The header file is in src/libasr/runtime for development, but in impure
    // in installed version
    std::string path;
    int dirname_length;
    get_executable_path(path, dirname_length);
    std::string dirname = path.substr(0,dirname_length);
    if (   endswith(dirname, "src/bin")
        || endswith(dirname, "src\\bin")
        || endswith(dirname, "SRC\\BIN")) {
        // Development version
        return dirname + "/../libasr/runtime";
    } else if (endswith(dirname, "src/lpython/tests") ||
               endswith(to_lower(dirname), "src\\lpython\\tests")) {
        // CTest Tests
        return dirname + "/../../libasr/runtime";
    } else {
        // Installed version
        return dirname + "/../share/lpython/lib/impure";
    }

    return path;
}

// Decodes the exit status code of the process (in Unix)
// See `WEXITSTATUS` for more information.
// https://stackoverflow.com/a/27117435/15913193
// https://linux.die.net/man/3/system
int32_t get_exit_status(int32_t err) {
    return (((err) >> 8) & 0x000000ff);
}

} // namespace LCompilers::LFortran
