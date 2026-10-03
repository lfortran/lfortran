#ifndef LIBASR_PASS_SCOPED_INLINING_H
#define LIBASR_PASS_SCOPED_INLINING_H

#include <libasr/asr.h>
#include <map>
#include <string>
#include <vector>

namespace LCompilers::PassUtils {

bool can_inline_in_block(const ASR::Function_t &function,
    const ASR::call_arg_t *args, size_t n_args);

std::string inline_local_name(SymbolTable *scope, const std::string &name);

ASR::stmt_t* inline_in_block(Allocator &al, const Location &loc,
    ASR::Function_t &function, SymbolTable *scope,
    std::map<ASR::symbol_t*, ASR::expr_t*> substitutions,
    ASR::expr_t *target, const std::vector<ASR::stmt_t*> &before,
    const std::vector<ASR::stmt_t*> &after);

} // namespace LCompilers::PassUtils

#endif
