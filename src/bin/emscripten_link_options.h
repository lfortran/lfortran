#pragma once

#include <string>
#include <vector>

namespace LCompilers::CommandLineInterface {

inline std::string emcc_driver(const char *lfortran_emcc,
    const char *emsdk_path)
{
    if (lfortran_emcc != nullptr && lfortran_emcc[0] != '\0') {
        return lfortran_emcc;
    }
    if (emsdk_path != nullptr && emsdk_path[0] != '\0') {
        return std::string(emsdk_path) + "/upstream/emscripten/emcc";
    }
    return "emcc";
}

inline std::string emscripten_link_options(
    const std::vector<std::string> &settings)
{
    std::string options =
        " --target=wasm32-unknown-emscripten -sSTACK_SIZE=50mb -sINITIAL_MEMORY=256mb";
    // Keep user settings after the defaults, and each setting in one shell argument.
    for (const std::string &setting : settings) {
        std::string arg = "-s" + setting;
#ifdef _WIN32
        options += " \"";
        size_t backslashes = 0;
        for (char c : arg) {
            if (c == '\\') {
                ++backslashes;
                continue;
            }
            options.append(c == '"' ? 2 * backslashes + 1 : backslashes, '\\');
            backslashes = 0;
            options += c;
        }
        options.append(2 * backslashes, '\\');
        options += '"';
#else
        options += " '";
        for (char c : arg) {
            if (c == '\'') {
                options += "'\\''";
            } else {
                options += c;
            }
        }
        options += '\'';
#endif
    }
    return options;
}

}
