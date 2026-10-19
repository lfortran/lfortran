#include <tests/doctest.h>

#include "../bin/cuda_link_helpers.h"

#include <filesystem>
#include <fstream>
#include <set>
#include <string>
#include <vector>
#include <stdexcept>

#include <cstdlib>

namespace fs = std::filesystem;

static void write_file(const fs::path &path, const std::string &text)
{
    std::ofstream out(path);
    if (!out.is_open()) {
        throw std::runtime_error("failed to open " + path.string());
    }
    out << text;
}

static void run_command(const std::string &cmd)
{
    int status = std::system(cmd.c_str());
    if (status != 0) {
        throw std::runtime_error("command failed: " + cmd);
    }
}

TEST_CASE("CUDA library resolution prefers shared libraries unless static is effective") {
    const fs::path workdir = "cuda_link_helper_resolve";
    fs::remove_all(workdir);
    fs::create_directories(workdir);

    const fs::path so = workdir / "libformal.so";
    const fs::path archive = workdir / "libformal.a";
    write_file(so, "shared");
    write_file(archive, "archive");

    std::filesystem::path resolved;
    bool is_static_archive = false;

    REQUIRE(resolve_linker_library("formal", {workdir.string()},
        LCompilers::Platform::Linux, false, resolved, is_static_archive));
    CHECK(resolved == so);
    CHECK_FALSE(is_static_archive);

    REQUIRE(resolve_linker_library("formal", {workdir.string()},
        LCompilers::Platform::Linux, true, resolved, is_static_archive));
    CHECK(resolved == archive);
    CHECK(is_static_archive);

    fs::remove_all(workdir);
}

TEST_CASE("Metadata-free archives ignore unrelated member sidecars") {
    const fs::path workdir = "cuda_link_helper_archive";
    fs::remove_all(workdir);
    fs::create_directories(workdir);

    const fs::path source = workdir / "plain.c";
    const fs::path object = workdir / "plain.o";
    const fs::path archive = workdir / "libplain.a";
    const fs::path ignored_sidecar = workdir / "plain.o.cuda.cu";
    write_file(source, "int plain(void) { return 7; }\n");
    run_command("cc -c " + source.string() + " -o " + object.string());
    run_command("ar rcs " + archive.string() + " " + object.string());
    write_file(ignored_sidecar, "#error unrelated member sidecar must be ignored\n");

    std::set<std::string> seen_sidecars;
    std::vector<std::string> cuda_sources;
    REQUIRE(append_cuda_source_from_library("plain", {workdir.string()},
        LCompilers::Platform::Linux, false, seen_sidecars, cuda_sources));
    CHECK(cuda_sources.empty());

    fs::remove_all(workdir);
}
