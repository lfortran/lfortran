#include <string>
#include <vector>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#include <libasr/utils.h>

namespace LCompilers::CommandLineInterface {

    auto init_compiler_options(
        CompilerOptions &compiler_options,
        CLI::App &app
    ) -> void;

} // namespace LCompilers::CommandLineInterface
