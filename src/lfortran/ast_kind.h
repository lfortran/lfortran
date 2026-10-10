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
        case decl_stmtType::DeferredProcedure:
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

// The label of a statement, 0 if it has none. Every executable statement node
// stores its label in its own `m_label`, so it has to be read through the
// node's type. The switch has no `default:` for the same reason as above.
static inline int64_t stmt_label(const decl_stmt_t &x) {
#define LFORTRAN_STMT_LABEL(X) \
        case decl_stmtType::X: return ((const X##_t&)x).m_label;
    switch (x.type) {
        case decl_stmtType::Use:
        case decl_stmtType::Import:
        case decl_stmtType::ImplicitNone:
        case decl_stmtType::Implicit:
        case decl_stmtType::Declaration:
        case decl_stmtType::DeclarationPragma:
        case decl_stmtType::Interface:
        case decl_stmtType::DerivedType:
        case decl_stmtType::Template:
        case decl_stmtType::Enum:
        case decl_stmtType::Instantiate:
        case decl_stmtType::Requirement:
        case decl_stmtType::Require:
        case decl_stmtType::DeferredProcedure:
        case decl_stmtType::Union:
            return 0;
        LFORTRAN_STMT_LABEL(Allocate)
        LFORTRAN_STMT_LABEL(Assign)
        LFORTRAN_STMT_LABEL(Assignment)
        LFORTRAN_STMT_LABEL(InferAssignment)
        LFORTRAN_STMT_LABEL(Associate)
        LFORTRAN_STMT_LABEL(Backspace)
        LFORTRAN_STMT_LABEL(Close)
        LFORTRAN_STMT_LABEL(Continue)
        LFORTRAN_STMT_LABEL(Cycle)
        LFORTRAN_STMT_LABEL(Deallocate)
        LFORTRAN_STMT_LABEL(Endfile)
        LFORTRAN_STMT_LABEL(Entry)
        LFORTRAN_STMT_LABEL(ErrorStop)
        LFORTRAN_STMT_LABEL(EventPost)
        LFORTRAN_STMT_LABEL(EventWait)
        LFORTRAN_STMT_LABEL(Exit)
        LFORTRAN_STMT_LABEL(Flush)
        LFORTRAN_STMT_LABEL(ForAllSingle)
        LFORTRAN_STMT_LABEL(Format)
        LFORTRAN_STMT_LABEL(DataStmt)
        LFORTRAN_STMT_LABEL(FormTeam)
        LFORTRAN_STMT_LABEL(GoTo)
        LFORTRAN_STMT_LABEL(Include)
        LFORTRAN_STMT_LABEL(Inquire)
        LFORTRAN_STMT_LABEL(Nullify)
        LFORTRAN_STMT_LABEL(Open)
        LFORTRAN_STMT_LABEL(Return)
        LFORTRAN_STMT_LABEL(Pragma)
        LFORTRAN_STMT_LABEL(Print)
        LFORTRAN_STMT_LABEL(Read)
        LFORTRAN_STMT_LABEL(Rewind)
        LFORTRAN_STMT_LABEL(Stop)
        LFORTRAN_STMT_LABEL(SubroutineCall)
        LFORTRAN_STMT_LABEL(SyncAll)
        LFORTRAN_STMT_LABEL(SyncImages)
        LFORTRAN_STMT_LABEL(SyncMemory)
        LFORTRAN_STMT_LABEL(SyncTeam)
        LFORTRAN_STMT_LABEL(Write)
        LFORTRAN_STMT_LABEL(AssociateBlock)
        LFORTRAN_STMT_LABEL(Block)
        LFORTRAN_STMT_LABEL(ChangeTeam)
        LFORTRAN_STMT_LABEL(Critical)
        LFORTRAN_STMT_LABEL(DoConcurrentLoop)
        LFORTRAN_STMT_LABEL(DoLoop)
        LFORTRAN_STMT_LABEL(ForAll)
        LFORTRAN_STMT_LABEL(If)
        LFORTRAN_STMT_LABEL(IfArithmetic)
        LFORTRAN_STMT_LABEL(Select)
        LFORTRAN_STMT_LABEL(SelectRank)
        LFORTRAN_STMT_LABEL(SelectType)
        LFORTRAN_STMT_LABEL(Where)
        LFORTRAN_STMT_LABEL(WhileLoop)
    }
#undef LFORTRAN_STMT_LABEL
    return 0;
}

static inline bool is_declaration(const decl_stmt_t &x) {
    return decl_stmt_kind(x) != DeclStmtKind::Statement;
}

static inline bool is_executable_stmt(const decl_stmt_t &x) {
    return decl_stmt_kind(x) == DeclStmtKind::Statement;
}

static inline bool is_kind(const decl_stmt_t &x, DeclStmtKind k) {
    return decl_stmt_kind(x) == k;
}

static inline size_t count_kind(decl_stmt_t **items, size_t n_items,
        DeclStmtKind k) {
    size_t n = 0;
    for (size_t i=0; i < n_items; i++) {
        if (decl_stmt_kind(*items[i]) == k) n++;
    }
    return n;
}

} // namespace LCompilers::LFortran::AST

#endif // LFORTRAN_AST_KIND_H
