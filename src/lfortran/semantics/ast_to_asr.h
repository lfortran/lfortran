#ifndef LFORTRAN_AST_TO_ASR_H
#define LFORTRAN_AST_TO_ASR_H

#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LFortran {

    Result<ASR::TranslationUnit_t*> ast_to_asr(Allocator &al,
        AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics,
        SymbolTable *symbol_table,
        bool symtab_only,
        LCompilers::PassOptions &pass_options,
        CompilerOptions &compiler_options);

} // namespace LFortran

#endif // LFORTRAN_AST_TO_ASR_H
