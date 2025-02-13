#include <bin/server/utils.h>

namespace LCompilers::LanguageServerProtocol {

    auto diagnostic_level_to_lsp_severity(diag::Level level) -> DiagnosticSeverity {
        switch (level) {
        case diag::Level::Error:
            return DiagnosticSeverity::ERROR;
        case diag::Level::Warning:
            return DiagnosticSeverity::WARNING;
        case diag::Level::Note:
            return DiagnosticSeverity::INFORMATION;
        case diag::Level::Help:
            return DiagnosticSeverity::HINT;
        default:
            return DiagnosticSeverity::WARNING;
        }
    }

    auto asr_symbol_type_to_lsp_symbol_kind(ASR::symbolType symbol_type) -> SymbolKind{
        switch (symbol_type) {
        case ASR::symbolType::Module:
            return SymbolKind::MODULE;
        case ASR::symbolType::Function:
            return SymbolKind::FUNCTION;
        case ASR::symbolType::GenericProcedure:
            return SymbolKind::FUNCTION;
        case ASR::symbolType::CustomOperator:
            return SymbolKind::OPERATOR;
        case ASR::symbolType::Struct:
            return SymbolKind::STRUCT;
        case ASR::symbolType::Enum:
            return SymbolKind::ENUM;
        case ASR::symbolType::Variable:
            return SymbolKind::VARIABLE;
        case ASR::symbolType::Class:
            return SymbolKind::CLASS;
        case ASR::symbolType::ClassProcedure:
            return SymbolKind::METHOD;
        case ASR::symbolType::Template:
            return SymbolKind::TYPE_PARAMETER;
        default:
            return SymbolKind::FUNCTION;
        }
    }

} // namespace LCompilers::LanguageServerProtocol
