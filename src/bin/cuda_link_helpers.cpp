#include "cuda_link_helpers.h"

#include <filesystem>
#include <iostream>
#include <set>
#include <string>
#include <vector>

#include <libasr/string_utils.h>

#ifdef HAVE_LFORTRAN_LLVM
#include <llvm/Config/llvm-config.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/Binary.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Error.h>
#endif

std::string cuda_device_compiler_command(
    const std::string &device_compiler,
    bool emulate_cpu,
    const std::string &gpu_arch)
{
    if (emulate_cpu) {
        return device_compiler;
    }
    return device_compiler + " -arch="
        + (gpu_arch.empty() ? "native" : gpu_arch);
}

namespace {

bool append_cuda_sidecar(
    const std::filesystem::path &sidecar,
    std::set<std::string> &seen_sidecars,
    std::vector<std::string> &cuda_sources)
{
    if (!seen_sidecars.insert(sidecar.string()).second) {
        return true;
    }
    std::error_code ec;
    if (!std::filesystem::exists(sidecar, ec)) {
        if (ec) {
            std::cerr << "Failed to inspect CUDA sidecar '" << sidecar
                      << "': " << ec.message() << std::endl;
            return false;
        }
        return true;
    }
    if (!std::filesystem::is_regular_file(sidecar, ec)) {
        if (ec) {
            std::cerr << "Failed to inspect CUDA sidecar '" << sidecar
                      << "': " << ec.message() << std::endl;
        } else {
            std::cerr << "CUDA sidecar '" << sidecar
                      << "' is not a regular file" << std::endl;
        }
        return false;
    }

    std::string sidecar_source;
    if (!LCompilers::read_file(sidecar.string(), sidecar_source)) {
        std::cerr << "Failed to read CUDA sidecar '" << sidecar << "'"
                  << std::endl;
        return false;
    }
    cuda_sources.push_back(std::move(sidecar_source));
    return true;
}

#ifdef HAVE_LFORTRAN_LLVM
llvm::Expected<llvm::StringRef> cuda_section_name(
    const llvm::object::SectionRef &section)
{
#if LLVM_VERSION_MAJOR >= 10
    return section.getName();
#else
    llvm::StringRef name;
    if (auto error = section.getName(name)) {
        return llvm::errorCodeToError(error);
    }
    return name;
#endif
}

llvm::Expected<llvm::StringRef> cuda_section_contents(
    const llvm::object::SectionRef &section)
{
#if LLVM_VERSION_MAJOR >= 9
    return section.getContents();
#else
    llvm::StringRef contents;
    if (auto error = section.getContents(contents)) {
        return llvm::errorCodeToError(error);
    }
    return contents;
#endif
}

const char *cuda_source_section_name()
{
#ifdef HAVE_LFORTRAN_MACHO
    return "__lcgpu";
#else
    return ".lcompilers.gpu.cuda.source";
#endif
}

bool append_cuda_payload(
    const char *data,
    size_t size,
    std::vector<std::string> &cuda_sources)
{
    size_t pos = 0;
    while (pos < size) {
        size_t end = pos;
        while (end < size && data[end] != '\0') {
            end++;
        }
        if (end == size) {
            std::cerr << "Malformed CUDA metadata: missing NUL terminator"
                      << std::endl;
            return false;
        }
        if (end > pos) {
            cuda_sources.emplace_back(data + pos, end - pos);
        }
        pos = end + 1;
        while (pos < size && data[pos] == '\0') {
            pos++;
        }
    }
    return true;
}

bool append_cuda_section_from_object(
    llvm::object::ObjectFile &object,
    std::vector<std::string> &cuda_sources,
    bool &found_cuda_section)
{
    for (const auto &section : object.sections()) {
        auto section_name_or_err = cuda_section_name(section);
        if (!section_name_or_err) {
            std::cerr << "Failed to read CUDA section name: "
                      << llvm::toString(section_name_or_err.takeError())
                      << std::endl;
            return false;
        }
        if (*section_name_or_err != cuda_source_section_name()) {
            continue;
        }
        found_cuda_section = true;
        auto contents_or_err = cuda_section_contents(section);
        if (!contents_or_err) {
            std::cerr << "Failed to read CUDA section contents: "
                      << llvm::toString(contents_or_err.takeError())
                      << std::endl;
            return false;
        }
        if (!append_cuda_payload(contents_or_err->data(),
                contents_or_err->size(), cuda_sources)) {
            return false;
        }
    }
    return true;
}

bool append_cuda_source_from_input_impl(
    const std::string &input,
    std::set<std::string> &seen_sidecars,
    std::vector<std::string> &cuda_sources)
{
    auto binary_or_err = llvm::object::createBinary(input);
    if (!binary_or_err) {
        std::cerr << "Failed to open linker input '" << input << "': "
                  << llvm::toString(binary_or_err.takeError()) << std::endl;
        return false;
    }
    bool found_cuda_section = false;
    llvm::object::Binary *binary = binary_or_err->getBinary();
    if (auto *object = llvm::dyn_cast<llvm::object::ObjectFile>(binary)) {
        if (!append_cuda_section_from_object(*object, cuda_sources,
                found_cuda_section)) {
            return false;
        }
    } else if (auto *archive = llvm::dyn_cast<llvm::object::Archive>(binary)) {
        llvm::Error err = llvm::Error::success();
        for (auto child : archive->children(err)) {
            auto child_binary_or_err = child.getAsBinary();
            if (!child_binary_or_err) {
                std::cerr << "Failed to open archive member binary: "
                          << llvm::toString(child_binary_or_err.takeError())
                          << std::endl;
                return false;
            }
            if (auto *child_object = llvm::dyn_cast<llvm::object::ObjectFile>(
                    child_binary_or_err->get())) {
                bool child_found_cuda_section = false;
                if (!append_cuda_section_from_object(*child_object,
                        cuda_sources, child_found_cuda_section)) {
                    return false;
                }
                found_cuda_section = found_cuda_section || child_found_cuda_section;
            }
        }
        if (err) {
            std::cerr << "Failed to read archive member list: "
                      << llvm::toString(std::move(err)) << std::endl;
            return false;
        }
    }
    if (!found_cuda_section) {
        return append_cuda_sidecar(
            std::filesystem::path(input + ".cuda.cu"),
            seen_sidecars, cuda_sources);
    }
    return true;
}
#else
bool append_cuda_source_from_input_impl(
    const std::string &input,
    std::set<std::string> &seen_sidecars,
    std::vector<std::string> &cuda_sources)
{
    return append_cuda_sidecar(
        std::filesystem::path(input + ".cuda.cu"),
        seen_sidecars, cuda_sources);
}
#endif

} // namespace

bool append_cuda_source_from_input(
    const std::string &input,
    std::set<std::string> &seen_sidecars,
    std::vector<std::string> &cuda_sources)
{
    return append_cuda_source_from_input_impl(
        input, seen_sidecars, cuda_sources);
}

bool resolve_linker_library(
    const std::string &library_name,
    const std::vector<std::string> &lib_dirs,
    LCompilers::Platform platform,
    bool static_only,
    std::filesystem::path &resolved_path,
    bool &is_static_archive)
{
    std::vector<std::string> shared_suffixes;
    if (platform == LCompilers::Platform::macOS_Intel ||
            platform == LCompilers::Platform::macOS_ARM ||
            platform == LCompilers::Platform::macOS_PowerPC) {
        shared_suffixes = {".dylib", ".so", ".tbd"};
    } else if (platform == LCompilers::Platform::Windows) {
        shared_suffixes = {".dll"};
    } else {
        shared_suffixes = {".so"};
    }
    const std::vector<std::string> static_suffixes =
        (platform == LCompilers::Platform::Windows) ?
            std::vector<std::string>{".lib"} :
            std::vector<std::string>{".a"};
    const std::string library_prefix =
        (platform == LCompilers::Platform::Windows) ? "" : "lib";

    for (const auto &dir : lib_dirs) {
        const std::filesystem::path base_dir(dir);
        if (!static_only) {
            for (const auto &suffix : shared_suffixes) {
                const std::filesystem::path candidate =
                    base_dir / (library_prefix + library_name + suffix);
                std::error_code ec;
                if (std::filesystem::is_regular_file(candidate, ec) && !ec) {
                    resolved_path = candidate;
                    is_static_archive = false;
                    return true;
                }
            }
        }
        for (const auto &suffix : static_suffixes) {
            const std::filesystem::path candidate =
                base_dir / (library_prefix + library_name + suffix);
            std::error_code ec;
            if (std::filesystem::is_regular_file(candidate, ec) && !ec) {
                resolved_path = candidate;
                is_static_archive = true;
                return true;
            }
        }
    }
    return false;
}

bool append_cuda_source_from_library(
    const std::string &library_name,
    const std::vector<std::string> &lib_dirs,
    LCompilers::Platform platform,
    bool static_only,
    std::set<std::string> &seen_sidecars,
    std::vector<std::string> &cuda_sources)
{
    std::filesystem::path resolved_library;
    bool is_static_archive = false;
    if (!resolve_linker_library(library_name, lib_dirs, platform, static_only,
            resolved_library, is_static_archive)) {
        return true;
    }
    if (!is_static_archive) return true;
    return append_cuda_source_from_input(resolved_library.string(),
        seen_sidecars, cuda_sources);
}
