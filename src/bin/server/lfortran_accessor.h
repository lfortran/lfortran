#pragma once

#include <string>
#include <vector>

#include <libasr/lsp_interface.h>
#include <libasr/utils.h>

namespace LCompilers::LLanguageServer {

  class LFortranAccessor {
  public:
    auto showErrors(
      const std::string &filename,
      const std::string &text,
      const CompilerOptions &compiler_options
    ) -> std::vector<LCompilers::error_highlight>;
  };

} // namespace LCompilers::LLanguageServer
