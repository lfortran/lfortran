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

TEST_CASE("pthread accepts both the canonical flag and the compiler alias")
{
    for (const char *flag : {"--pthread", "-pthread"}) {
        const char *argv[] = {"lfortran", flag, "input.f90"};
        LFortranCommandLineParser parser(3, argv);
        REQUIRE_NOTHROW(parser.parse());
        CHECK(parser.opts.pthread);
        CHECK(parser.opts.arg_files == std::vector<std::string>{"input.f90"});

        std::vector<std::string> args = {"input.f90", flag};
        LFortranCommandLineParser vector_parser(args);
        REQUIRE_NOTHROW(vector_parser.parse());
        CHECK(vector_parser.opts.pthread);
        CHECK(vector_parser.opts.arg_files == std::vector<std::string>{"input.f90"});
    }
}

TEST_CASE("pthread-shaped filenames remain positional after --")
{
    const char *argv[] = {"lfortran", "fmt", "--", "-pthread"};
    LFortranCommandLineParser parser(4, argv);
    REQUIRE_NOTHROW(parser.parse());
    CHECK(parser.fmt->count() == 1);
    CHECK(parser.opts.arg_fmt_file == "-pthread");
    CHECK_FALSE(parser.opts.pthread);

    std::vector<std::string> args = {"-pthread", "--", "fmt"};
    LFortranCommandLineParser vector_parser(args);
    REQUIRE_NOTHROW(vector_parser.parse());
    CHECK(vector_parser.fmt->count() == 1);
    CHECK(vector_parser.opts.arg_fmt_file == "-pthread");
    CHECK_FALSE(vector_parser.opts.pthread);

    const char *with_flag[] = {"lfortran", "-pthread", "fmt", "--", "-pthread"};
    LFortranCommandLineParser with_flag_parser(5, with_flag);
    REQUIRE_NOTHROW(with_flag_parser.parse());
    CHECK(with_flag_parser.opts.pthread);
    CHECK(with_flag_parser.opts.arg_fmt_file == "-pthread");

    const char *compiler_argv[] = {"lfortran", "--", "-pthread"};
    LFortranCommandLineParser compiler_parser(3, compiler_argv);
    REQUIRE_NOTHROW(compiler_parser.parse());
    CHECK(compiler_parser.opts.arg_files == std::vector<std::string>{"-pthread"});
    CHECK_FALSE(compiler_parser.opts.pthread);
}

TEST_CASE("pthread-shaped option values are not compiler flags")
{
    const char *argv[] = {"lfortran", "-o", "-pthread", "input.f90"};
    LFortranCommandLineParser parser(4, argv);
    REQUIRE_NOTHROW(parser.parse());
    CHECK(parser.opts.compiler_options.arg_o == "-pthread");
    CHECK(parser.opts.arg_files == std::vector<std::string>{"input.f90"});
    CHECK_FALSE(parser.opts.pthread);

    std::vector<std::string> args = {"input.f90", "-pthread", "-o"};
    LFortranCommandLineParser vector_parser(args);
    REQUIRE_NOTHROW(vector_parser.parse());
    CHECK(vector_parser.opts.compiler_options.arg_o == "-pthread");
    CHECK(vector_parser.opts.arg_files == std::vector<std::string>{"input.f90"});
    CHECK_FALSE(vector_parser.opts.pthread);

    const char *clustered_argv[] = {"lfortran", "-vo", "-pthread", "input.f90"};
    LFortranCommandLineParser clustered_parser(4, clustered_argv);
    REQUIRE_NOTHROW(clustered_parser.parse());
    CHECK(clustered_parser.opts.arg_v);
    CHECK(clustered_parser.opts.compiler_options.arg_o == "-pthread");
    CHECK(clustered_parser.opts.arg_files == std::vector<std::string>{"input.f90"});
    CHECK_FALSE(clustered_parser.opts.pthread);

    const char *kernel_argv[] = {"lfortran", "kernel", "-f", "-pthread"};
    LFortranCommandLineParser kernel_parser(4, kernel_argv);
    REQUIRE_NOTHROW(kernel_parser.parse());
    CHECK(kernel_parser.opts.arg_kernel_f == "-pthread");
    CHECK_FALSE(kernel_parser.opts.pthread);

    const char *subcommand_value[] = {"lfortran", "-o", "fmt", "-pthread", "input.f90"};
    LFortranCommandLineParser subcommand_value_parser(5, subcommand_value);
    REQUIRE_NOTHROW(subcommand_value_parser.parse());
    CHECK(subcommand_value_parser.opts.compiler_options.arg_o == "fmt");
    CHECK(subcommand_value_parser.opts.pthread);
    CHECK(subcommand_value_parser.opts.arg_files == std::vector<std::string>{"input.f90"});
}
