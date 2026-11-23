#include <tests/doctest.h>
#include <bin/lfortran_command_line_parser.h>
#include <bin/emscripten_link_options.h>

#include <cstdio>

using LCompilers::CommandLineInterface::LFortranCommandLineParser;
using LCompilers::CommandLineInterface::emscripten_link_options;

TEST_CASE("--fast-safe keeps optimizations and runtime checks")
{
    const char *safe[] = {"lfortran", "--fast-safe", "input.f90"};
    LFortranCommandLineParser safe_parser(3, safe);
    REQUIRE_NOTHROW(safe_parser.parse());
    CHECK(safe_parser.opts.compiler_options.po.fast);
    CHECK(safe_parser.opts.compiler_options.po.fast_safe);
    CHECK(safe_parser.opts.compiler_options.po.bounds_checking);
    CHECK(!safe_parser.opts.compiler_options.po.assume_no_runtime_errors());

    const char *fast[] = {"lfortran", "--fast", "input.f90"};
    LFortranCommandLineParser fast_parser(3, fast);
    REQUIRE_NOTHROW(fast_parser.parse());
    CHECK(fast_parser.opts.compiler_options.po.fast);
    CHECK(!fast_parser.opts.compiler_options.po.fast_safe);
    CHECK(!fast_parser.opts.compiler_options.po.bounds_checking);
    CHECK(fast_parser.opts.compiler_options.po.assume_no_runtime_errors());

    const char *both[] = {"lfortran", "--fast", "--fast-safe", "input.f90"};
    LFortranCommandLineParser both_parser(4, both);
    CHECK_THROWS_WITH_AS(both_parser.parse(),
        "error: --fast and --fast-safe cannot be used together",
        LCompilers::LCompilersException);

    const char *fast_checks[] = {"lfortran", "--fast",
        "--array-bounds-checking", "input.f90"};
    LFortranCommandLineParser fast_checks_parser(4, fast_checks);
    REQUIRE_NOTHROW(fast_checks_parser.parse());
    CHECK(fast_checks_parser.opts.compiler_options.po.fast);
    CHECK(fast_checks_parser.opts.compiler_options.po.bounds_checking);
    CHECK(fast_checks_parser.opts.compiler_options.po.assume_no_runtime_errors());

    const char *safe_off[] = {"lfortran", "--fast-safe",
        "--no-array-bounds-checking", "input.f90"};
    LFortranCommandLineParser safe_off_parser(4, safe_off);
    REQUIRE_NOTHROW(safe_off_parser.parse());
    CHECK(safe_off_parser.opts.compiler_options.po.fast_safe);
    CHECK(!safe_off_parser.opts.compiler_options.po.bounds_checking);

    const char *conflict[] = {"lfortran", "--array-bounds-checking",
        "--no-array-bounds-checking", "input.f90"};
    LFortranCommandLineParser conflict_parser(4, conflict);
    CHECK_THROWS_WITH_AS(conflict_parser.parse(),
        "error: --array-bounds-checking and --no-array-bounds-checking"
        " cannot be used together",
        LCompilers::LCompilersException);
}

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
