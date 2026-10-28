#include <tests/doctest.h>
#include <bin/lfortran_command_line_parser.h>
#include <bin/emscripten_link_options.h>

#include <cstdio>

using LCompilers::CommandLineInterface::LFortranCommandLineParser;
using LCompilers::CommandLineInterface::emscripten_link_options;

TEST_CASE("Emscripten settings accept attached and separated arguments")
{
    const char *argv[] = {"lfortran", "--target=wasm32-unknown-emscripten",
        "-sMODULARIZE", "-s", "EXPORT_ES6=1", "-sSTACK_SIZE=64mb",
        "-sEXPORTED_RUNTIME_METHODS=['ccall','cwrap']",
        "-sEXPORT_NAME=\"my module\"", "input.f90", "-o", "app.js"};
    LFortranCommandLineParser parser(sizeof(argv) / sizeof(argv[0]), argv);
    REQUIRE_NOTHROW(parser.parse());
    CHECK(parser.opts.arg_files == std::vector<std::string>{"input.f90"});
    CHECK(parser.opts.compiler_options.arg_o == "app.js");
    CHECK(parser.opts.emscripten_settings == std::vector<std::string>{
        "MODULARIZE", "EXPORT_ES6=1", "STACK_SIZE=64mb",
        "EXPORTED_RUNTIME_METHODS=['ccall','cwrap']", "EXPORT_NAME=\"my module\""});
}

TEST_CASE("Emscripten settings do not consume compile-only inputs")
{
    const char *argv[] = {"lfortran", "-c",
        "--target=wasm32-unknown-emscripten", "-s", "MODULARIZE", "input.f90"};
    LFortranCommandLineParser parser(sizeof(argv) / sizeof(argv[0]), argv);
    REQUIRE_NOTHROW(parser.parse());
    CHECK(parser.opts.arg_c);
    CHECK(parser.opts.arg_files == std::vector<std::string>{"input.f90"});
    CHECK(parser.opts.emscripten_settings == std::vector<std::string>{"MODULARIZE"});
}

TEST_CASE("Emscripten settings require an Emscripten LLVM target")
{
    const char *invalid[][4] = {
        {"lfortran", "-sMODULARIZE", "input.f90", "--static"},
        {"lfortran", "-sMODULARIZE", "--target=wasm32-wasi", "input.f90"},
        {"lfortran", "-sMODULARIZE", "--target=x86_64-pc-linux-gnu", "input.f90"},
        {"lfortran", "-sMODULARIZE", "--target=wasm32-unknown-emscripten", "--backend=c"},
        {"lfortran", "-sMODULARIZE", "--target=wasm32-unknown-emscripten", "--backend=wasm"}
    };
    for (const auto &argv : invalid) {
        LFortranCommandLineParser parser(4, argv);
        CHECK_THROWS_WITH_AS(parser.parse(),
            "error: -s settings require an LLVM-based backend and an emscripten target",
            LCompilers::LCompilersException);
    }
    const char *argv[] = {"lfortran", "--target=wasm32-unknown-emscripten",
        "--backend=mlir", "-sMODULARIZE"};
    LFortranCommandLineParser parser(4, argv);
    CHECK_NOTHROW(parser.parse());
}

TEST_CASE("Emscripten settings keep generic linker flags separate")
{
    const char *argv[] = {"lfortran", "--target=wasm32-unknown-emscripten",
        "-sMODULARIZE", "-Wl,--export=main", "-s", "EXPORT_ES6", "-W-sTEST",
        "input.f90"};
    LFortranCommandLineParser parser(sizeof(argv) / sizeof(argv[0]), argv);
    REQUIRE_NOTHROW(parser.parse());
    CHECK(parser.opts.linker_flags ==
        std::vector<std::string>{"l,--export=main", "-sTEST"});
    CHECK(parser.opts.emscripten_settings ==
        std::vector<std::string>{"MODULARIZE", "EXPORT_ES6"});
    const char *missing[] = {"lfortran", "--target=wasm32-unknown-emscripten", "-s"};
    LFortranCommandLineParser missing_parser(3, missing);
    CHECK_THROWS_AS(missing_parser.parse(), CLI::ArgumentMismatch);
}

TEST_CASE("Emscripten settings reach the link command in order without shell splitting")
{
    std::vector<const char *> argv = {"lfortran", "--target=wasm32-unknown-emscripten",
        "-sMODULARIZE", "-s", "EXPORT_ES6", "-sSTACK_SIZE=64mb",
        "-sEXPORTED_RUNTIME_METHODS=['ccall','cwrap']",
        "-sEXPORT_NAME=\"my module\"", "-sSTACK_SIZE=96mb",
        "-sTEST=$HOME;`echo wrong`\\", "input.f90"};
    SUBCASE("normal") {}
    SUBCASE("fast") { argv.push_back("--fast"); }
    SUBCASE("Fortran 2023") { argv.push_back("--std=f23"); }
    LFortranCommandLineParser parser(argv.size(), argv.data());
    REQUIRE_NOTHROW(parser.parse());
    const std::string options = emscripten_link_options(parser.opts.emscripten_settings);
#if !defined(_WIN32) && !defined(__EMSCRIPTEN__)
    // Exercise the same shell decoding as the driver's system() call, without an SDK.
    FILE *pipe = popen(("printf '%s\\n'" + options).c_str(), "r");
    REQUIRE(pipe != nullptr);
    std::string output;
    char buffer[256];
    while (fgets(buffer, sizeof(buffer), pipe)) {
        output += buffer;
    }
    CHECK(pclose(pipe) == 0);
    CHECK(output ==
        "--target=wasm32-unknown-emscripten\n-sSTACK_SIZE=50mb\n-sINITIAL_MEMORY=256mb\n"
        "-sMODULARIZE\n-sEXPORT_ES6\n-sSTACK_SIZE=64mb\n"
        "-sEXPORTED_RUNTIME_METHODS=['ccall','cwrap']\n"
        "-sEXPORT_NAME=\"my module\"\n-sSTACK_SIZE=96mb\n-sTEST=$HOME;`echo wrong`\\\n");
#elif defined(_WIN32)
    CHECK(options.find("\"-sEXPORT_NAME=\\\"my module\\\"\"") != std::string::npos);
    CHECK(options.find("\"-sEXPORTED_RUNTIME_METHODS=['ccall','cwrap']\"") != std::string::npos);
#else
    CHECK(options.find(" '-sEXPORT_NAME=\"my module\"'") != std::string::npos);
#endif
}
