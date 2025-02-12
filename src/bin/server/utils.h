#pragma once

#include <libasr/asr.h>
#include <libasr/diagnostics.h>

#include <server/specification.h>

namespace LCompilers::LanguageServerProtocol {

  DiagnosticSeverity diagnostic_level_to_lsp_severity(diag::Level level);

  SymbolKind asr_symbol_type_to_lsp_symbol_kind(ASR::symbolType symbol_type);

} // namespace LCompilers::LanguageServerProtocol
