#include <lfortran/semantics/ast_common_visitor.h>

// With LCOMPILERS_VIRTUAL_VISITORS the visitor that SymbolTableVisitor and
// BodyVisitor share is compiled here, once.

#if defined(LCOMPILERS_VIRTUAL_VISITORS)

namespace LCompilers::LFortran {

template class AST::BaseVisitor<CommonVisitorShared>;
template class CommonVisitorStatic<CommonVisitorShared>;
template class AST::BaseVisitorVirtual<CommonVisitorStatic<CommonVisitorShared>>;

CommonVisitorShared::ProcedureDeclaredLater
CommonVisitorShared::procedure_declared_later(
        const std::string &name) {
    return CommonVisitorStatic<CommonVisitorShared>::procedure_declared_later(
        name);
}

bool CommonVisitorShared::host_contains_procedure(SymbolTable *scope,
        const std::string &name) {
    return CommonVisitorStatic<CommonVisitorShared>::host_contains_procedure(
        scope, name);
}

} // namespace LCompilers::LFortran

#endif
