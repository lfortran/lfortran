#ifndef LFORTRAN_LLD_LINKER_H
#define LFORTRAN_LLD_LINKER_H

#include <string>
#include <vector>

#ifdef HAVE_LFORTRAN_LLD

// The built-in linker is LLD, linked into the lfortran binary: its ELF port
// on Linux and its Mach-O port on macOS. `triple` is the code-generation
// target, which must be the host's platform and architecture. An empty
// `requested_linker` selects it; `lld` selects it explicitly; `clang` and
// `gcc` keep the external driver.
bool lld_selected(const std::string &triple, const std::string &host_triple,
        const std::string &requested_linker);

// Link `object_files` into `outfile`. Returns 0 on success.
int link_with_lld(
        const std::vector<std::string> &object_files,
        const std::vector<std::string> &extra_objects,
        const std::string &outfile,
        const std::string &runtime_library_dir,
        const std::string &runtime_lib,
        bool static_executable, bool shared_executable, bool verbose,
        const std::vector<std::string> &lib_dirs,
        const std::vector<std::string> &libraries,
        const std::vector<std::string> &linker_flags,
        const std::vector<std::string> &extra_libs,
        const std::string &openmp_lib_dir, bool openmp,
        const std::string &triple);

#else

inline bool lld_selected(const std::string &/*triple*/,
        const std::string &/*host_triple*/,
        const std::string &/*requested_linker*/) {
    return false;
}

#endif

#endif
