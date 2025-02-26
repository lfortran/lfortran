#pragma once

#include <libasr/asr.h>
#include <libasr/diagnostics.h>

#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {

    auto diagnostic_level_to_lsp_severity(diag::Level level) -> DiagnosticSeverity;

    auto asr_symbol_type_to_lsp_symbol_kind(ASR::symbolType symbol_type) -> SymbolKind;

} // namespace LCompilers::LanguageServerProtocol
