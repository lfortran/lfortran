#ifndef LFORTRAN_AST_KIND_H
#define LFORTRAN_AST_KIND_H

#include <lfortran/ast.h>

namespace LCompilers::LFortran::AST {

// The AST stores everything that can appear in the body of a program unit in a
// single `decl_stmt` list, in source order. `decl_stmt_kind()` tells the five
// sections of a specification part (F2018 R508) apart, which is what the
// parser's ordering check and the AST -> ASR visitors filter on.
enum class DeclStmtKind {
    Use,
    Import,
    Implicit,
    Declaration,
    Statement,
};

// The switch below has no `default:` on purpose: adding a constructor to
// `decl_stmt` in AST.asdl must produce a `-Wswitch` warning here rather than
// silently being treated as an executable statement.
static inline DeclStmtKind decl_stmt_kind(const decl_stmt_t &x) {
    switch (x.type) {
        case decl_stmtType::Use:
            return DeclStmtKind::Use;
        case decl_stmtType::Import:
            return DeclStmtKind::Import;
        case decl_stmtType::ImplicitNone:
        case decl_stmtType::Implicit:
            return DeclStmtKind::Implicit;
        case decl_stmtType::Declaration:
        case decl_stmtType::DeclarationPragma:
        case decl_stmtType::Interface:
        case decl_stmtType::DerivedType:
        case decl_stmtType::Template:
        case decl_stmtType::Enum:
        case decl_stmtType::Instantiate:
        case decl_stmtType::Requirement:
        case decl_stmtType::Require:
        case decl_stmtType::Union:
            return DeclStmtKind::Declaration;
        case decl_stmtType::Allocate:
        case decl_stmtType::Assign:
        case decl_stmtType::Assignment:
        case decl_stmtType::InferAssignment:
        case decl_stmtType::Associate:
        case decl_stmtType::Backspace:
        case decl_stmtType::Close:
        case decl_stmtType::Continue:
        case decl_stmtType::Cycle:
        case decl_stmtType::Deallocate:
        case decl_stmtType::Endfile:
        case decl_stmtType::Entry:
        case decl_stmtType::ErrorStop:
        case decl_stmtType::EventPost:
        case decl_stmtType::EventWait:
        case decl_stmtType::Exit:
        case decl_stmtType::Flush:
        case decl_stmtType::ForAllSingle:
        case decl_stmtType::Format:
        case decl_stmtType::DataStmt:
        case decl_stmtType::FormTeam:
        case decl_stmtType::GoTo:
        case decl_stmtType::Include:
        case decl_stmtType::Inquire:
        case decl_stmtType::Nullify:
        case decl_stmtType::Open:
        case decl_stmtType::Return:
        case decl_stmtType::Pragma:
        case decl_stmtType::Print:
        case decl_stmtType::Read:
        case decl_stmtType::Rewind:
        case decl_stmtType::Stop:
        case decl_stmtType::SubroutineCall:
        case decl_stmtType::SyncAll:
        case decl_stmtType::SyncImages:
        case decl_stmtType::SyncMemory:
        case decl_stmtType::SyncTeam:
        case decl_stmtType::Write:
        case decl_stmtType::AssociateBlock:
        case decl_stmtType::Block:
        case decl_stmtType::ChangeTeam:
        case decl_stmtType::Critical:
        case decl_stmtType::DoConcurrentLoop:
        case decl_stmtType::DoLoop:
        case decl_stmtType::ForAll:
        case decl_stmtType::If:
        case decl_stmtType::IfArithmetic:
        case decl_stmtType::Select:
        case decl_stmtType::SelectRank:
        case decl_stmtType::SelectType:
        case decl_stmtType::Where:
        case decl_stmtType::WhileLoop:
            return DeclStmtKind::Statement;
    }
    return DeclStmtKind::Statement;
}

static inline bool is_declaration(const decl_stmt_t &x) {
    return decl_stmt_kind(x) != DeclStmtKind::Statement;
}

static inline bool is_executable_stmt(const decl_stmt_t &x) {
    return decl_stmt_kind(x) == DeclStmtKind::Statement;
}

} // namespace LCompilers::LFortran::AST

#endif // LFORTRAN_AST_KIND_H
