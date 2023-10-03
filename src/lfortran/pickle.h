#ifndef LFORTRAN_PICKLE_H
#define LFORTRAN_PICKLE_H

#include <lfortran/parser/parser_stype.h>
#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <libasr/location.h>

namespace LCompilers::LFortran {

    // Pickle a token
    std::string pickle(int token, const YYSTYPE &yystype, bool colors=false);

    // Pickle an AST node
    std::string pickle(AST::ast_t &ast, bool colors=false, bool indent=false);
    std::string pickle(AST::TranslationUnit_t &ast, bool colors=false, bool indent=false);

    // Print the tree structure
    std::string pickle_tree(AST::ast_t &ast, bool colors=true);
    std::string pickle_tree(AST::TranslationUnit_t &ast, bool colors=true);

    // Print Json structure
    std::string pickle_json(AST::ast_t &ast, LocationManager &lm);
    std::string pickle_json(AST::TranslationUnit_t &ast, LocationManager &lm);

} // namespace LCompilers::LFortran

#endif // LFORTRAN_PICKLE_H
