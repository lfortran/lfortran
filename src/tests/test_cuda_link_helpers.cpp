#include <tests/doctest.h>

#include "../bin/cuda_link_helpers.h"
#include "test_cuda_link_helpers_fixture.hpp"

#include <filesystem>
#include <fstream>
#include <set>
#include <string>
#include <vector>
#include <stdexcept>
#include <system_error>

namespace fs = std::filesystem;

namespace {

static void write_file(const fs::path &path, const std::string &text)
{
    std::ofstream out(path);
    if (!out.is_open()) {
        throw std::runtime_error("failed to open " + path.string());
    }
    out << text;
}

struct OwnedDirectory {
    fs::path path;

    explicit OwnedDirectory(const fs::path &root, const std::string &prefix)
    {
        fs::create_directories(root);
        for (int i = 0; i < 32; ++i) {
            fs::path candidate = root / (prefix + "_" + LCompilers::get_unique_ID());
            std::error_code ec;
            if (fs::create_directory(candidate, ec)) {
                path = candidate;
                return;
            }
        }
        throw std::runtime_error("failed to create unique directory under " + root.string());
    }

    ~OwnedDirectory()
    {
        std::error_code ec;
        fs::remove_all(path, ec);
    }
};

} // namespace

TEST_CASE("CUDA architecture selection configures only the native device compiler")
{
    CHECK(cuda_device_compiler_command("nvcc", false, false, "")
        == "nvcc -arch=native");
    CHECK(cuda_device_compiler_command("nvcc", false, false, "native")
        == "nvcc -arch=native");
    CHECK(cuda_device_compiler_command("nvcc", false, false, "sm_120")
        == "nvcc -arch=sm_120");
    CHECK(cuda_device_compiler_command("nvcc -ccbin=g++", false, false,
        "sm_90") == "nvcc -ccbin=g++ -arch=sm_90");
    CHECK(cuda_device_compiler_command("clang++", true, false, "")
        == "clang++");
    CHECK(cuda_device_compiler_command("clang++", true, false, "native")
        == "clang++");
    CHECK(cuda_device_compiler_command("clang++", true, false, "sm_120")
        == "clang++");
}

TEST_CASE("HIP architecture selection uses the offload architecture option")
{
    CHECK(cuda_device_compiler_command("hipcc", false, true, "")
        == "hipcc --offload-arch=native");
    CHECK(cuda_device_compiler_command("hipcc", false, true, "gfx942")
        == "hipcc --offload-arch=gfx942");
    CHECK(cuda_device_compiler_command("amdclang++ -x hip", false, true,
        "gfx90a") == "amdclang++ -x hip --offload-arch=gfx90a");
}

TEST_CASE("HIP headers and libraries are found next to the real HIP driver")
{
    OwnedDirectory workdir(fs::path(cuda_link_helpers_fixture::plain_archive).parent_path(),
        "hip_toolkit_dirs");
    fs::path rocm = workdir.path / "rocm-9.9.9";
    fs::create_directories(rocm / "bin");
    fs::create_directories(rocm / "include" / "hip");
    fs::create_directories(rocm / "lib");
    std::ofstream(rocm / "bin" / "hipcc") << "#!/bin/sh\n";
    std::ofstream(rocm / "include" / "hip" / "hip_runtime_api.h") << "\n";
    // A link to the driver elsewhere, as /usr/bin/hipcc is to the ROCm
    // installation it belongs to.
    fs::create_directories(workdir.path / "usr" / "bin");
    fs::create_symlink(rocm / "bin" / "hipcc",
        workdir.path / "usr" / "bin" / "hipcc");

    std::string include_dir, lib_dir;
    REQUIRE(hip_toolkit_dirs((workdir.path / "usr" / "bin" / "hipcc").string()
        + " -O2", include_dir, lib_dir));
    CHECK(fs::equivalent(include_dir, rocm / "include"));
    CHECK(fs::equivalent(lib_dir, rocm / "lib"));

    fs::remove(rocm / "include" / "hip" / "hip_runtime_api.h");
    CHECK_FALSE(hip_toolkit_dirs((rocm / "bin" / "hipcc").string(),
        include_dir, lib_dir));
    CHECK_FALSE(hip_toolkit_dirs((workdir.path / "missing").string(),
        include_dir, lib_dir));
}

TEST_CASE("CUDA toolkit headers are found next to the device compiler")
{
    OwnedDirectory workdir(fs::path(cuda_link_helpers_fixture::plain_archive).parent_path(),
        "cuda_link_helper_toolkit");

    const fs::path flat = workdir.path / "flat";
    fs::create_directories(flat / "bin");
    fs::create_directories(flat / "include");
    write_file(flat / "bin" / "nvcc", "");
    write_file(flat / "include" / "cuda_runtime.h", "");
    const std::string flat_nvcc = (flat / "bin" / "nvcc").string();
    CHECK(fs::equivalent(cuda_toolkit_include_dir(flat_nvcc),
        flat / "include"));
    CHECK(fs::equivalent(cuda_toolkit_include_dir(flat_nvcc + " -ccbin=g++"),
        flat / "include"));

    const fs::path targets = workdir.path / "targets_layout";
    const fs::path target_include = targets / "targets" / "x86_64-linux"
        / "include";
    fs::create_directories(targets / "bin");
    fs::create_directories(target_include);
    write_file(targets / "bin" / "nvcc", "");
    write_file(target_include / "cuda_runtime.h", "");
    CHECK(fs::equivalent(
        cuda_toolkit_include_dir((targets / "bin" / "nvcc").string()),
        target_include));

    const fs::path missing = workdir.path / "missing";
    fs::create_directories(missing / "bin");
    write_file(missing / "bin" / "nvcc", "");
    CHECK(cuda_toolkit_include_dir((missing / "bin" / "nvcc").string())
        == "");
    CHECK(cuda_toolkit_include_dir("") == "");
}

TEST_CASE("CUDA library resolution prefers shared libraries unless static is effective")
{
    OwnedDirectory workdir(fs::path(cuda_link_helpers_fixture::plain_archive).parent_path(),
        "cuda_link_helper_resolve");

    const fs::path so = workdir.path / "libformal.so";
    const fs::path archive = workdir.path / "libformal.a";
    write_file(so, "shared");
    write_file(archive, "archive");

    std::filesystem::path resolved;
    bool is_static_archive = false;

    REQUIRE(resolve_linker_library("formal", {workdir.path.string()},
        LCompilers::Platform::Linux, false, resolved, is_static_archive));
    CHECK(resolved == so);
    CHECK_FALSE(is_static_archive);

    REQUIRE(resolve_linker_library("formal", {workdir.path.string()},
        LCompilers::Platform::Linux, true, resolved, is_static_archive));
    CHECK(resolved == archive);
    CHECK(is_static_archive);
}

#ifdef HAVE_LFORTRAN_LLVM
TEST_CASE("Metadata-free archives ignore unrelated member sidecars") {
    OwnedDirectory workdir(fs::path(cuda_link_helpers_fixture::plain_archive).parent_path(),
        "cuda_link_helper_archive");

    const fs::path archive = workdir.path / "libplain.a";
    {
        std::error_code ec;
        fs::copy_file(cuda_link_helpers_fixture::plain_archive, archive, ec);
        if (ec) {
            throw std::runtime_error("failed to copy fixture archive to " + archive.string()
                + ": " + ec.message());
        }
    }

    const fs::path ignored_sidecar =
        workdir.path / (std::string(cuda_link_helpers_fixture::plain_member) + ".cuda.cu");
    write_file(ignored_sidecar, "#error unrelated member sidecar must be ignored\n");

    std::set<std::string> seen_sidecars;
    std::vector<std::string> cuda_sources;
    REQUIRE(append_cuda_source_from_library("plain", {workdir.path.string()},
        LCompilers::Platform::Linux, false, seen_sidecars, cuda_sources));
    CHECK(cuda_sources.empty());
}
#endif
