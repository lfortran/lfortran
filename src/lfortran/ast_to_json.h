#ifndef LFORTRAN_AST_TO_JSON_H
#define LFORTRAN_AST_TO_JSON_H

#include <lfortran/ast.h>

namespace LCompilers::LFortran {

    std::string ast_to_json(LFortran::AST::ast_t &ast);

} // namespace LCompilers::LFortran

#endif // LFORTRAN_AST_TO_JSON_H
