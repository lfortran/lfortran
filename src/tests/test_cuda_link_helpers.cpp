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
    CHECK(cuda_device_compiler_command("nvcc", false, "")
        == "nvcc -arch=native");
    CHECK(cuda_device_compiler_command("nvcc", false, "native")
        == "nvcc -arch=native");
    CHECK(cuda_device_compiler_command("nvcc", false, "sm_120")
        == "nvcc -arch=sm_120");
    CHECK(cuda_device_compiler_command("nvcc -ccbin=g++", false, "sm_90")
        == "nvcc -ccbin=g++ -arch=sm_90");
    CHECK(cuda_device_compiler_command("clang++", true, "") == "clang++");
    CHECK(cuda_device_compiler_command("clang++", true, "native") == "clang++");
    CHECK(cuda_device_compiler_command("clang++", true, "sm_120") == "clang++");
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
