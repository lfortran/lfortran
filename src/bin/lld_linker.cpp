#include <libasr/config.h>

#ifdef HAVE_LFORTRAN_LLD

#include "lld_linker.h"

#include <cstdlib>
#include <iostream>
#include <map>
#include <sstream>

#include <algorithm>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Config/llvm-config.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/raw_ostream.h>

// Included at global scope so LLD_HAS_DRIVER declares ::lld::elf::link outside
// any namespace. The signature of that entry point changed across LLVM
// versions; the calls below follow the headers of each version.
#include <lld/Common/Driver.h>
#if LLVM_VERSION_MAJOR >= 17
#ifdef __APPLE__
LLD_HAS_DRIVER(macho)
#else
LLD_HAS_DRIVER(elf)
#endif
#endif

#ifdef __APPLE__
#include <cstdio>
#include <sys/sysctl.h>
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#endif

namespace {

bool is_dir(const std::string &path) {
    return llvm::sys::fs::is_directory(path);
}

std::string arch_of(const std::string &triple) {
    if (triple.find("aarch64") != std::string::npos
            || triple.find("arm64") != std::string::npos) {
        return "aarch64";
    }
    if (triple.find("x86_64") != std::string::npos
            || triple.find("amd64") != std::string::npos) {
        return "x86_64";
    }
    if (triple.find("riscv64") != std::string::npos) {
        return "riscv64";
    }
    if (triple.find("ppc64le") != std::string::npos) {
        return "ppc64le";
    }
    return "";
}

#ifndef __APPLE__

bool file_exists(const std::string &path) {
    return llvm::sys::fs::exists(path);
}

void push_unique(std::vector<std::string> &paths, const std::string &path) {
    if (path.empty()) {
        return;
    }
    for (const std::string &existing : paths) {
        if (existing == path) {
            return;
        }
    }
    paths.push_back(path);
}

std::string debian_multiarch(const std::string &arch) {
    if (arch == "x86_64") {
        return "x86_64-linux-gnu";
    }
    if (arch == "aarch64") {
        return "aarch64-linux-gnu";
    }
    if (arch == "riscv64") {
        return "riscv64-linux-gnu";
    }
    if (arch == "ppc64le") {
        return "powerpc64le-linux-gnu";
    }
    return "";
}

std::vector<std::string> dynamic_linker_candidates(const std::string &arch) {
    if (arch == "x86_64") {
        return {"/lib64/ld-linux-x86-64.so.2",
            "/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2"};
    }
    if (arch == "aarch64") {
        return {"/lib/ld-linux-aarch64.so.1",
            "/lib/aarch64-linux-gnu/ld-linux-aarch64.so.1"};
    }
    if (arch == "riscv64") {
        return {"/lib/ld-linux-riscv64-lp64d.so.1"};
    }
    if (arch == "ppc64le") {
        return {"/lib64/ld64.so.2"};
    }
    return {};
}

// Numeric comparison of directory names such as "15.3.0". A name that is not
// a version sorts last.
std::vector<int> version_parts(const std::string &text) {
    std::vector<int> parts;
    int current = 0;
    bool any = false;
    for (char c : text) {
        if (c >= '0' && c <= '9') {
            current = current * 10 + (c - '0');
            any = true;
        } else if (c == '.' && any) {
            parts.push_back(current);
            current = 0;
            any = false;
        } else {
            return {};
        }
    }
    if (any) {
        parts.push_back(current);
    }
    return parts;
}

bool version_less(const std::vector<int> &left, const std::vector<int> &right) {
    size_t n = std::max(left.size(), right.size());
    for (size_t i = 0; i < n; i++) {
        int l = i < left.size() ? left[i] : 0;
        int r = i < right.size() ? right[i] : 0;
        if (l != r) {
            return l < r;
        }
    }
    return false;
}

std::string find_gcc_lib(const std::string &prefix, const std::string &arch) {
    std::string base = prefix + "/lib/gcc";
    std::error_code ec;
    if (!is_dir(base)) {
        return "";
    }
    std::string best;
    std::vector<int> best_version;
    bool best_matches_arch = false;
    for (llvm::sys::fs::directory_iterator triple_it(base, ec), end;
            !ec && triple_it != end; triple_it.increment(ec)) {
        if (!is_dir(triple_it->path())) {
            continue;
        }
        std::string triple = llvm::StringRef(triple_it->path()).str();
        triple = llvm::sys::path::filename(triple).str();
        bool matches_arch = !arch.empty() && triple.find(arch) != std::string::npos;
        std::error_code version_ec;
        for (llvm::sys::fs::directory_iterator version_it(triple_it->path(), version_ec),
                version_end;
                !version_ec && version_it != version_end;
                version_it.increment(version_ec)) {
            if (!is_dir(version_it->path())) {
                continue;
            }
            std::string dir = llvm::StringRef(version_it->path()).str();
            std::string version_name = llvm::sys::path::filename(dir).str();
            std::vector<int> version = version_parts(version_name);
            if (version.empty()) {
                continue;
            }
            if (!file_exists(dir + "/crtbeginS.o") && !file_exists(dir + "/crtbegin.o")) {
                continue;
            }
            bool better = best.empty()
                || (matches_arch && !best_matches_arch)
                || (matches_arch == best_matches_arch && version_less(best_version, version));
            if (better) {
                best = dir;
                best_version = version;
                best_matches_arch = matches_arch;
            }
        }
    }
    return best;
}

std::string find_crt_dir(const std::string &prefix, const std::string &gcc_triple,
        const std::string &arch) {
    std::vector<std::string> candidates;
    if (!gcc_triple.empty()) {
        std::string sysroot = prefix + "/" + gcc_triple + "/sysroot";
        candidates.push_back(sysroot + "/usr/lib64");
        candidates.push_back(sysroot + "/usr/lib");
        candidates.push_back(sysroot + "/lib64");
        candidates.push_back(sysroot + "/lib");
    }
    std::string multiarch = debian_multiarch(arch);
    if (!multiarch.empty()) {
        candidates.push_back(prefix + "/" + multiarch + "/sysroot/usr/lib");
        candidates.push_back(prefix + "/lib/" + multiarch);
        candidates.push_back("/usr/lib/" + multiarch);
        candidates.push_back("/lib/" + multiarch);
    }
    candidates.push_back(prefix + "/sysroot/usr/lib64");
    candidates.push_back(prefix + "/sysroot/usr/lib");
    candidates.push_back("/usr/lib64");
    candidates.push_back("/usr/lib");
    for (const std::string &dir : candidates) {
        if (file_exists(dir + "/crti.o")
                && (file_exists(dir + "/Scrt1.o") || file_exists(dir + "/crt1.o"))) {
            return dir;
        }
    }
    return "";
}

std::string find_dynamic_linker(const std::string &crt_dir, const std::string &arch) {
    for (const std::string &path : dynamic_linker_candidates(arch)) {
        if (file_exists(path)) {
            return path;
        }
    }
    // A Conda sysroot ships the loader next to its CRT objects. The host path
    // above is preferred so the binary runs outside that prefix.
    std::vector<std::string> names;
    if (arch == "x86_64") {
        names.push_back("ld-linux-x86-64.so.2");
    } else if (arch == "aarch64") {
        names.push_back("ld-linux-aarch64.so.1");
    } else if (arch == "riscv64") {
        names.push_back("ld-linux-riscv64-lp64d.so.1");
    } else if (arch == "ppc64le") {
        names.push_back("ld64.so.2");
    }
    std::vector<std::string> dirs = {crt_dir};
    if (!crt_dir.empty()) {
        dirs.push_back(crt_dir + "/../lib");
        dirs.push_back(crt_dir + "/../lib64");
        dirs.push_back(crt_dir + "/../../lib");
        dirs.push_back(crt_dir + "/../../lib64");
    }
    for (const std::string &dir : dirs) {
        for (const std::string &name : names) {
            std::string path = dir + "/" + name;
            if (file_exists(path)) {
                return path;
            }
        }
    }
    return "";
}

bool has_shared_library(const std::vector<std::string> &dirs, const std::string &name) {
    for (const std::string &dir : dirs) {
        if (file_exists(dir + "/lib" + name + ".so")) {
            return true;
        }
    }
    return false;
}

bool has_static_library(const std::vector<std::string> &dirs, const std::string &name) {
    for (const std::string &dir : dirs) {
        if (file_exists(dir + "/lib" + name + ".a")) {
            return true;
        }
    }
    return false;
}

struct ElfToolchain {
    std::string gcc_lib;
    std::string crt_dir;
    std::string sysroot;
    std::string dynamic_linker;
    std::vector<std::string> library_dirs;
};

ElfToolchain discover_elf_toolchain(const std::string &triple, std::string &error) {
    ElfToolchain toolchain;
    std::string arch = arch_of(triple);
    std::vector<std::string> prefixes;
    if (const char *conda = std::getenv("CONDA_PREFIX")) {
        push_unique(prefixes, conda);
    }
    if (const char *prefix = std::getenv("PREFIX")) {
        push_unique(prefixes, prefix);
    }
    push_unique(prefixes, "/usr/local");
    push_unique(prefixes, "/usr");

    for (const std::string &prefix : prefixes) {
        std::string gcc_lib = find_gcc_lib(prefix, arch);
        if (gcc_lib.empty()) {
            continue;
        }
        std::string gcc_triple = llvm::sys::path::filename(
            llvm::sys::path::parent_path(gcc_lib)).str();
        std::string crt_dir = find_crt_dir(prefix, gcc_triple, arch);
        if (crt_dir.empty()) {
            continue;
        }
        toolchain.gcc_lib = gcc_lib;
        toolchain.crt_dir = crt_dir;
        break;
    }
    if (toolchain.gcc_lib.empty() || toolchain.crt_dir.empty()) {
        error = "could not find the C runtime (crtbegin and Scrt1.o/crt1.o). "
            "Looked in CONDA_PREFIX and /usr for a GCC library directory and "
            "the matching startup objects";
        return toolchain;
    }

    // Conda's libc.so is a linker script whose absolute paths (/lib64/libc.so.6)
    // are meaningful only under the compiler sysroot. LLD applies the sysroot
    // to those paths when the script itself lives inside it.
    const std::string sysroot_mark = "/sysroot/";
    std::string::size_type sysroot_at = toolchain.crt_dir.find(sysroot_mark);
    if (sysroot_at != std::string::npos) {
        toolchain.sysroot = toolchain.crt_dir.substr(0, sysroot_at + sysroot_mark.size() - 1);
    }
    toolchain.dynamic_linker = find_dynamic_linker(toolchain.crt_dir, arch);
    push_unique(toolchain.library_dirs, toolchain.gcc_lib);
    push_unique(toolchain.library_dirs, toolchain.crt_dir);
    // glibc keeps libc.so.6 and libm.so.6 one directory above the CRT objects
    // on some sysroots (usr/lib64 vs lib64) and next to them on others.
    push_unique(toolchain.library_dirs, toolchain.crt_dir + "/../lib");
    push_unique(toolchain.library_dirs, toolchain.crt_dir + "/../lib64");
    push_unique(toolchain.library_dirs, toolchain.crt_dir + "/../../lib");
    push_unique(toolchain.library_dirs, toolchain.crt_dir + "/../../lib64");
    if (const char *conda = std::getenv("CONDA_PREFIX")) {
        push_unique(toolchain.library_dirs, std::string(conda) + "/lib");
    }
    std::string multiarch = debian_multiarch(arch);
    if (!multiarch.empty()) {
        push_unique(toolchain.library_dirs, "/usr/lib/" + multiarch);
        push_unique(toolchain.library_dirs, "/lib/" + multiarch);
    }
    push_unique(toolchain.library_dirs, "/usr/lib64");
    push_unique(toolchain.library_dirs, "/lib64");
    push_unique(toolchain.library_dirs, "/usr/lib");

    std::vector<std::string> existing;
    for (const std::string &dir : toolchain.library_dirs) {
        if (is_dir(dir)) {
            push_unique(existing, dir);
        }
    }
    toolchain.library_dirs.swap(existing);
    return toolchain;
}

#endif

struct ArgList {
    std::vector<std::string> store;

    void add(std::string argument) {
        store.push_back(std::move(argument));
    }

    std::vector<const char *> pointers() const {
        std::vector<const char *> out;
        out.reserve(store.size());
        for (const std::string &argument : store) {
            out.push_back(argument.c_str());
        }
        return out;
    }

    std::string command() const {
        std::string text;
        for (size_t i = 0; i < store.size(); i++) {
            if (i > 0) {
                text += " ";
            }
            text += store[i];
        }
        return text;
    }
};

// `-W` stores the text after `-W`. `-Wl,-rpath,/dir` is therefore `l,-rpath,/dir`.
// Other `-W` values are compiler warnings (`-Wno-unused`). Clang accepts them
// on a link line and ignores them. LLD would try to open them as input files.
void add_forwarded_linker_flags(ArgList &args, const std::vector<std::string> &flags) {
    for (const std::string &flag : flags) {
        if (flag.rfind("l,", 0) != 0) {
            continue;
        }
        std::stringstream stream(flag.substr(2));
        std::string part;
        while (std::getline(stream, part, ',')) {
            if (!part.empty()) {
                args.add(part);
            }
        }
    }
}

#ifndef __APPLE__

void add_lib_if_present(ArgList &args, const std::vector<std::string> &dirs,
        const std::string &name, bool static_link) {
    bool present = static_link
        ? has_static_library(dirs, name)
        : (has_shared_library(dirs, name) || has_static_library(dirs, name));
    if (present) {
        args.add("-l" + name);
    }
}

int invoke_elf_link(const std::vector<const char *> &arguments) {
#if LLVM_VERSION_MAJOR >= 17
    const ::lld::DriverDef drivers[] = {
        {::lld::Gnu, &::lld::elf::link},
    };
    ::lld::Result result = ::lld::lldMain(
        arguments, llvm::outs(), llvm::errs(), drivers);
    return result.retCode;
#elif LLVM_VERSION_MAJOR >= 14
    bool ok = ::lld::elf::link(arguments, llvm::outs(), llvm::errs(),
        /*exitEarly=*/false, /*disableOutput=*/false);
    return ok ? 0 : 1;
#elif LLVM_VERSION_MAJOR >= 10
    bool ok = ::lld::elf::link(arguments, /*canExitEarly=*/false,
        llvm::outs(), llvm::errs());
    return ok ? 0 : 1;
#else
    // LLVM 7–9 take one diagnostic stream.
    bool ok = ::lld::elf::link(arguments, /*canExitEarly=*/false,
        llvm::errs());
    return ok ? 0 : 1;
#endif
}

int link_elf(
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
        const std::string &triple) {
    std::string error;
    ElfToolchain toolchain = discover_elf_toolchain(triple, error);
    if (!error.empty()) {
        std::cerr << error << std::endl;
        return 10;
    }
    if (!static_executable && !shared_executable && toolchain.dynamic_linker.empty()) {
        std::cerr << "could not find the dynamic linker for " << triple << std::endl;
        return 10;
    }

    // Clang, the previous default driver, links a normal executable as
    // non-PIE. A PIE link rejects objects compiled without -fPIC, which is
    // how C files beside the Fortran are usually built. -Wl,-pie still
    // selects a position-independent executable.
    bool pie = false;
    if (!static_executable && !shared_executable) {
        for (const std::string &flag : linker_flags) {
            if (flag.rfind("l,", 0) != 0) {
                continue;
            }
            std::stringstream stream(flag.substr(2));
            std::string part;
            while (std::getline(stream, part, ',')) {
                if (part == "-pie") {
                    pie = true;
                } else if (part == "-no-pie") {
                    pie = false;
                }
            }
        }
        if (pie && !(file_exists(toolchain.crt_dir + "/Scrt1.o")
                && file_exists(toolchain.gcc_lib + "/crtbeginS.o")
                && file_exists(toolchain.gcc_lib + "/crtendS.o"))) {
            pie = false;
        }
    }

    std::string crt1 = toolchain.crt_dir + (pie ? "/Scrt1.o" : "/crt1.o");
    std::string crti = toolchain.crt_dir + "/crti.o";
    std::string crtn = toolchain.crt_dir + "/crtn.o";
    std::string crtbegin = toolchain.gcc_lib + (pie ? "/crtbeginS.o" : "/crtbegin.o");
    std::string crtend = toolchain.gcc_lib + (pie ? "/crtendS.o" : "/crtend.o");
    if (static_executable && file_exists(toolchain.gcc_lib + "/crtbeginT.o")) {
        crtbegin = toolchain.gcc_lib + "/crtbeginT.o";
    }
    if (shared_executable) {
        crtbegin = file_exists(toolchain.gcc_lib + "/crtbeginS.o")
            ? toolchain.gcc_lib + "/crtbeginS.o"
            : toolchain.gcc_lib + "/crtbegin.o";
        crtend = file_exists(toolchain.gcc_lib + "/crtendS.o")
            ? toolchain.gcc_lib + "/crtendS.o"
            : toolchain.gcc_lib + "/crtend.o";
    }
    if (!file_exists(crti) || !file_exists(crtn) || !file_exists(crtbegin)
            || !file_exists(crtend) || (!shared_executable && !file_exists(crt1))) {
        std::cerr << "could not find every C startup file under "
            << toolchain.crt_dir << " and " << toolchain.gcc_lib << std::endl;
        return 10;
    }

    ArgList args;
    args.add("ld.lld");
    if (static_executable) {
        args.add("-static");
    } else if (shared_executable) {
        args.add("-shared");
    } else if (pie) {
        args.add("-pie");
        args.add("-dynamic-linker");
        args.add(toolchain.dynamic_linker);
    } else {
        args.add("-dynamic-linker");
        args.add(toolchain.dynamic_linker);
        args.add("-no-pie");
    }
    args.add("--eh-frame-hdr");
    args.add("-z");
    args.add("relro");
    args.add("-z");
    args.add("now");
    // GNU ld allows undefined symbols in shared libraries. LLD reports them
    // for executables unless asked not to, which rejects libraries the system
    // driver accepts.
    args.add("--allow-shlib-undefined");
    if (!toolchain.sysroot.empty()) {
        args.add("--sysroot=" + toolchain.sysroot);
    }
    args.add("-o");
    args.add(outfile);

    if (!shared_executable) {
        args.add(crt1);
    }
    args.add(crti);
    args.add(crtbegin);
    for (const std::string &object : object_files) {
        args.add(object);
    }
    for (const std::string &object : extra_objects) {
        args.add(object);
    }

    for (const std::string &dir : lib_dirs) {
        args.add("-L" + dir);
    }
    for (const std::string &dir : toolchain.library_dirs) {
        args.add("-L" + dir);
    }
    args.add("-L" + runtime_library_dir);
    for (const std::string &library : libraries) {
        args.add("-l" + library);
    }
    if (!static_executable) {
        args.add("-rpath");
        args.add(runtime_library_dir);
        // The GCC driver in a Conda prefix records that prefix so libgcc_s
        // and libstdc++ resolve without LD_LIBRARY_PATH.
        if (const char *conda = std::getenv("CONDA_PREFIX")) {
            std::string conda_lib = std::string(conda) + "/lib";
            args.add("-rpath");
            args.add(conda_lib);
            args.add("-rpath-link");
            args.add(conda_lib);
        }
    }
    add_forwarded_linker_flags(args, linker_flags);
    args.add("-l" + runtime_lib);

    if (openmp && !openmp_lib_dir.empty()) {
        args.add("-L" + openmp_lib_dir);
        if (!static_executable) {
            args.add("-rpath");
            args.add(openmp_lib_dir);
        }
        args.add("-lomp");
    }
    for (const std::string &library : extra_libs) {
        args.add(library);
    }

    const std::vector<std::string> &dirs = toolchain.library_dirs;
    if (static_executable) {
        args.add("--start-group");
        add_lib_if_present(args, dirs, "m", true);
        add_lib_if_present(args, dirs, "pthread", true);
        add_lib_if_present(args, dirs, "dl", true);
        add_lib_if_present(args, dirs, "gcc", true);
        add_lib_if_present(args, dirs, "gcc_eh", true);
        add_lib_if_present(args, dirs, "c", true);
        args.add("--end-group");
    } else {
        add_lib_if_present(args, dirs, "m", false);
        add_lib_if_present(args, dirs, "pthread", false);
        add_lib_if_present(args, dirs, "dl", false);
        args.add("-lgcc");
        args.add("--as-needed");
        args.add("-lgcc_s");
        args.add("--no-as-needed");
        args.add("-lc");
        args.add("-lgcc");
        args.add("--as-needed");
        args.add("-lgcc_s");
        args.add("--no-as-needed");
    }
    args.add(crtend);
    args.add(crtn);

    if (verbose) {
        std::cout << args.command() << std::endl;
    }
    std::vector<const char *> pointers = args.pointers();
    int status = invoke_elf_link(pointers);
    if (status != 0) {
        std::cerr << "The built-in LLD linker failed." << std::endl;
        return 10;
    }
    return 0;
}

#else

// The first line `command` prints, or "" if it fails.
std::string first_output_line(const std::string &command) {
    FILE *pipe = popen(command.c_str(), "r");
    if (pipe == nullptr) {
        return "";
    }
    char buffer[4096];
    std::string line;
    if (fgets(buffer, sizeof(buffer), pipe) != nullptr) {
        line = buffer;
    }
    if (pclose(pipe) != 0) {
        return "";
    }
    while (!line.empty() && (line.back() == '\n' || line.back() == '\r')) {
        line.pop_back();
    }
    return line;
}

// The SDK that Apple's compiler driver would use: SDKROOT, then the one
// xcrun selects (which follows xcode-select), then the usual install paths.
std::string find_macos_sdk() {
    if (const char *sdkroot = std::getenv("SDKROOT")) {
        if (is_dir(sdkroot)) {
            return sdkroot;
        }
    }
    std::string sdk = first_output_line("xcrun --sdk macosx --show-sdk-path 2>/dev/null");
    if (!sdk.empty() && is_dir(sdk)) {
        return sdk;
    }
    for (const char *path : {
            "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
            "/Applications/Xcode.app/Contents/Developer/Platforms/"
                "MacOSX.platform/Developer/SDKs/MacOSX.sdk"}) {
        if (is_dir(path)) {
            return path;
        }
    }
    return "";
}

// The SDK version recorded in the executable, such as "26.2".
std::string macos_sdk_version(const std::string &sdk) {
    auto buffer = llvm::MemoryBuffer::getFile(sdk + "/SDKSettings.json");
    if (!buffer) {
        return "";
    }
    llvm::Expected<llvm::json::Value> value
        = llvm::json::parse((*buffer)->getBuffer());
    if (!value) {
        llvm::consumeError(value.takeError());
        return "";
    }
    if (const llvm::json::Object *object = value->getAsObject()) {
        if (auto version = object->getString("Version")) {
            return version->str();
        }
    }
    return "";
}

// MACOSX_DEPLOYMENT_TARGET, as for Apple's driver, or else the running
// macOS release. Objects and libraries built on this machine do not
// require a newer release than that.
std::string macos_deployment_target() {
    if (const char *target = std::getenv("MACOSX_DEPLOYMENT_TARGET")) {
        if (*target != '\0') {
            return target;
        }
    }
    char version[64];
    size_t size = sizeof(version);
    if (sysctlbyname("kern.osproductversion", version, &size, nullptr, 0) == 0) {
        return version;
    }
    return "11.0";
}

int invoke_macho_link(const std::vector<const char *> &arguments) {
#if LLVM_VERSION_MAJOR >= 17
    const ::lld::DriverDef drivers[] = {
        {::lld::Darwin, &::lld::macho::link},
    };
    ::lld::Result result = ::lld::lldMain(
        arguments, llvm::outs(), llvm::errs(), drivers);
    return result.retCode;
#else
    // CMake enables the Mach-O port from LLVM 15 on.
    bool ok = ::lld::macho::link(arguments, llvm::outs(), llvm::errs(),
        /*exitEarly=*/false, /*disableOutput=*/false);
    return ok ? 0 : 1;
#endif
}

int link_macho(
        const std::vector<std::string> &object_files,
        const std::vector<std::string> &extra_objects,
        const std::string &outfile,
        const std::string &runtime_library_dir,
        const std::string &runtime_lib,
        bool shared_executable, bool verbose,
        const std::vector<std::string> &lib_dirs,
        const std::vector<std::string> &libraries,
        const std::vector<std::string> &linker_flags,
        const std::vector<std::string> &extra_libs,
        const std::string &openmp_lib_dir, bool openmp,
        const std::string &triple) {
    std::string sdk = find_macos_sdk();
    if (sdk.empty()) {
        std::cerr << "could not find the macOS SDK. Set SDKROOT or install "
            "the Xcode Command Line Tools" << std::endl;
        return 10;
    }
    std::string min_version = macos_deployment_target();
    std::string sdk_version = macos_sdk_version(sdk);
    if (sdk_version.empty()) {
        sdk_version = min_version;
    }
    std::string arch = arch_of(triple);

    // The arguments Apple's clang passes to ld64. A macOS executable is
    // always dynamically linked against libSystem, so a static link (which
    // only selects the static Fortran runtime) is an ordinary link here.
    ArgList args;
    args.add("ld64.lld");
    args.add("-arch");
    args.add(arch == "aarch64" ? "arm64" : arch);
    args.add("-platform_version");
    args.add("macos");
    args.add(min_version);
    args.add(sdk_version);
    args.add("-syslibroot");
    args.add(sdk);
    if (shared_executable) {
        args.add("-dylib");
    }
    args.add("-o");
    args.add(outfile);
    for (const std::string &object : object_files) {
        args.add(object);
    }
    for (const std::string &object : extra_objects) {
        args.add(object);
    }
    for (const std::string &dir : lib_dirs) {
        args.add("-L" + dir);
    }
    args.add("-L" + runtime_library_dir);
    if (is_dir("/usr/local/lib")) {
        args.add("-L/usr/local/lib");
    }
    for (const std::string &library : libraries) {
        args.add("-l" + library);
    }
    args.add("-rpath");
    args.add(runtime_library_dir);
    add_forwarded_linker_flags(args, linker_flags);
    args.add("-l" + runtime_lib);
    args.add("-lm");
    if (openmp && !openmp_lib_dir.empty()) {
        args.add("-L" + openmp_lib_dir);
        args.add("-rpath");
        args.add(openmp_lib_dir);
        args.add("-lomp");
    }
    for (const std::string &library : extra_libs) {
        args.add(library);
    }
    args.add("-lSystem");

    if (verbose) {
        std::cout << args.command() << std::endl;
    }
    std::vector<const char *> pointers = args.pointers();
    int status = invoke_macho_link(pointers);
    if (status != 0) {
        std::cerr << "The built-in LLD linker failed." << std::endl;
        return 10;
    }
    return 0;
}

#endif

} // namespace

bool lld_selected(const std::string &triple, const std::string &host_triple,
        const std::string &requested_linker) {
    if (!requested_linker.empty() && requested_linker != "lld") {
        return false;
    }
#ifdef __APPLE__
    if (triple.find("darwin") == std::string::npos
            && triple.find("macos") == std::string::npos) {
        return false;
    }
#else
    if (triple.find("linux") == std::string::npos) {
        return false;
    }
#endif
    std::string arch = arch_of(triple);
    if (arch.empty() || arch != arch_of(host_triple)) {
        return false;
    }
    return true;
}

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
        const std::string &triple) {
#ifdef __APPLE__
    (void)static_executable;
    return link_macho(object_files, extra_objects, outfile,
        runtime_library_dir, runtime_lib, shared_executable, verbose,
        lib_dirs, libraries, linker_flags, extra_libs, openmp_lib_dir,
        openmp, triple);
#else
    return link_elf(object_files, extra_objects, outfile,
        runtime_library_dir, runtime_lib, static_executable,
        shared_executable, verbose, lib_dirs, libraries, linker_flags,
        extra_libs, openmp_lib_dir, openmp, triple);
#endif
}

#endif
