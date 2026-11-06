#ifndef LFORTRAN_AST_TO_ASR_H
#define LFORTRAN_AST_TO_ASR_H

#include <map>
#include <string>

#include <lfortran/ast.h>
#include <libasr/asr.h>

namespace LCompilers::LFortran {

    // Interactive implicit types are input from the preceding cell and updated
    // only after successful semantic analysis of this cell.
    Result<ASR::TranslationUnit_t*> ast_to_asr(Allocator &al,
        AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics,
        SymbolTable *symbol_table,
        bool symtab_only,
        CompilerOptions &compiler_options, LCompilers::LocationManager &lm,
        std::map<std::string, ASR::ttype_t*> *interactive_implicit_types = nullptr);

} // namespace LCompilers::LFortran

#endif // LFORTRAN_AST_TO_ASR_H
