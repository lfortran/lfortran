#define CLI11_HAS_FILESYSTEM 0
#include <bin/CLI11.hpp>

#include <libasr/utils.h>

namespace LCompilers::CommandLineInterface {

    auto init_compiler_options(
        CompilerOptions &compiler_options,
        CLI::App &app
    ) -> void;

    auto init_compiler_options(
        CompilerOptions &compiler_options,
        int argc,
        const char *const *argv
    ) -> int;

} // namespace LCompilers::CommandLineInterface
