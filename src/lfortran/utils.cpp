#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX
#endif // NOMINMAX
#include <windows.h>
#endif

#include <fstream>

#ifndef _WIN32
#include <unistd.h>
#endif

#include <bin/tpl/whereami/whereami.h>

#include <libasr/exception.h>
#include <lfortran/utils.h>
#include <libasr/string_utils.h>

namespace LFortran {

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

        constexpr inline const char* pathsep()
        {
#ifdef _WIN32
            return ";";
#else
            return ":";
#endif
        }

        fs::path which(const std::string& exe, const std::string& override_path)
        {
            // TODO maybe add a cache?
            auto env_path = std::getenv("PATH");
            if (env_path)
            {
                std::string path = env_path;
                auto parts = split(path, pathsep());
                for (auto& p : parts)
                {
                    if (!fs::exists(p) || !fs::is_directory(p))
                    {
                        continue;
                    }
                    for (const auto& entry : fs::directory_iterator(p))
                    {
                        if (entry.path().filename() == exe)
                        {
                            return entry.path();
                        }
                    }
                }
            }

#ifndef _WIN32
            if (override_path == "")
            {
                char* pathbuf;
                size_t n = confstr(_CS_PATH, NULL, (size_t) 0);
                pathbuf = (char*) malloc(n);
                if (pathbuf != NULL)
                {
                    confstr(_CS_PATH, pathbuf, n);
                    return which(exe, pathbuf);
                }
            }
#endif

            return "";  // empty path
        }



std::string get_runtime_library_dir()
{
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
        return dirname + "/../share/lfortran/lib";
    }
}

std::string get_runtime_library_header_dir()
{
    char *env_p = std::getenv("LFORTRAN_RUNTIME_LIBRARY_HEADER_DIR");
    if (env_p) return env_p;

    return get_runtime_library_dir() + "/impure";
}


}
