#include <libasr/asr_builder.h>
#include <libasr/pass/scoped_inlining.h>
#include <libasr/pass/symbol_expr_substitution.h>

namespace LCompilers::PassUtils {

namespace {

struct InlineScope {
    SymbolTable *symbols;
    ASR::stmt_t **body;
    size_t size;
};

bool entered_scope(ASR::stmt_t *statement, InlineScope &scope) {
    if (ASR::is_a<ASR::BlockCall_t>(*statement)) {
        auto *block = ASR::down_cast<ASR::Block_t>(
            ASR::down_cast<ASR::BlockCall_t>(statement)->m_m);
        scope = {block->m_symtab, block->m_body, block->n_body};
        return true;
    }
    if (ASR::is_a<ASR::AssociateBlockCall_t>(*statement)) {
        auto *block = ASR::down_cast<ASR::AssociateBlock_t>(
            ASR::down_cast<ASR::AssociateBlockCall_t>(statement)->m_m);
        scope = {block->m_symtab, block->m_body, block->n_body};
        return true;
    }
    return false;
}

bool collect_scopes(InlineScope scope, std::vector<InlineScope> &scopes) {
    scopes.push_back(scope);
    for (size_t i = 0; i < scope.size; i++) {
        InlineScope nested;
        if (entered_scope(scope.body[i], nested)) {
            if (!collect_scopes(nested, scopes)) return false;
        } else {
            struct NestedScope : ASR::BaseWalkVisitor<NestedScope> {
                bool found = false;
                void visit_BlockCall(const ASR::BlockCall_t &) { found = true; }
                void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &) {
                    found = true;
                }
            };
            NestedScope nested_scope;
            nested_scope.visit_stmt(*scope.body[i]);
            if (nested_scope.found) return false;
        }
    }
    return true;
}

bool clone_body(Allocator &al, InlineScope scope,
        ASRUtils::ExprStmtDuplicator &duplicator, Vec<ASR::stmt_t*> &body) {
    for (size_t i = 0; i < scope.size; i++) {
        InlineScope nested;
        if (entered_scope(scope.body[i], nested)) {
            if (!clone_body(al, nested, duplicator, body)) return false;
        } else if (!ASR::is_a<ASR::Return_t>(*scope.body[i])) {
            duplicator.success = true;
            ASR::stmt_t *copy = duplicator.duplicate_stmt(scope.body[i]);
            if (!duplicator.success || !copy) return false;
            body.push_back(al, copy);
        }
    }
    return true;
}

} // namespace

bool can_inline_in_block(const ASR::Function_t &function,
        const ASR::call_arg_t *args, size_t n_args) {
    const auto *type = ASRUtils::get_FunctionType(function);
    if (type->m_abi != ASR::abiType::Source ||
            type->m_deftype != ASR::deftypeType::Implementation ||
            function.n_args != n_args || function.n_body == 0) return false;
    for (size_t i = 0; i < n_args; i++) {
        if (!args[i].m_value ||
                !ASR::is_a<ASR::Var_t>(*function.m_args[i])) return false;
    }
    std::vector<InlineScope> scopes;
    if (!collect_scopes({function.m_symtab, function.m_body, function.n_body},
            scopes)) return false;
    std::set<SymbolTable*> flattened;
    for (const auto &scope : scopes) flattened.insert(scope.symbols);
    struct Returns : ASR::BaseWalkVisitor<Returns> {
        size_t count = 0;
        void visit_Return(const ASR::Return_t &) { count++; }
    };
    Returns returns;
    for (const auto &scope : scopes) {
        for (const auto &item : scope.symbols->get_scope()) {
            ASR::symbol_t *symbol = item.second;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*symbol)) continue;
            if (ASR::is_a<ASR::Block_t>(*symbol) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*symbol)) {
                if (!flattened.count(ASRUtils::symbol_symtab(symbol))) return false;
            } else if (!ASR::is_a<ASR::Variable_t>(*symbol) ||
                    ASR::down_cast<ASR::Variable_t>(symbol)->m_storage ==
                        ASR::storage_typeType::Save) {
                return false;
            }
        }
        for (size_t i = 0; i < scope.size; i++) {
            returns.visit_stmt(*scope.body[i]);
        }
    }
    return returns.count == 0 || (returns.count == 1 &&
        ASR::is_a<ASR::Return_t>(*function.m_body[function.n_body - 1]));
}

std::string inline_local_name(SymbolTable *scope, const std::string &name) {
    std::string candidate = scope->get_unique_name(name);
    for (size_t i = 1; scope->resolve_symbol(candidate); i++) {
        candidate = scope->get_unique_name(name + "_" + std::to_string(i));
    }
    return candidate;
}

ASR::stmt_t* inline_in_block(Allocator &al, const Location &loc,
        ASR::Function_t &function, SymbolTable *scope,
        std::map<ASR::symbol_t*, ASR::expr_t*> substitutions,
        ASR::expr_t *target, const std::vector<ASR::stmt_t*> &before,
        const std::vector<ASR::stmt_t*> &after) {
    std::vector<InlineScope> scopes;
    if (!collect_scopes({function.m_symtab, function.m_body, function.n_body},
            scopes)) return nullptr;
    for (const auto &source : scopes) {
        for (const auto &item : source.symbols->get_scope()) {
            if (substitutions.count(item.second)) continue;
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            auto *variable = ASR::down_cast<ASR::Variable_t>(item.second);
            ASRUtils::ExprStmtDuplicator duplicator(al);
            duplicator.success = true;
            ASR::ttype_t *type = duplicator.duplicate_ttype(variable->m_type);
            ASR::expr_t *initial = duplicator.duplicate_expr(
                variable->m_symbolic_value);
            ASR::expr_t *value = duplicator.duplicate_expr(variable->m_value);
            if (!duplicator.success) return nullptr;
            std::string name = inline_local_name(scope, variable->m_name);
            ASR::symbol_t *copy = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                    nullptr, 0, ASR::intentType::Local, initial, value,
                    variable->m_storage, type, variable->m_type_declaration,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Required, false,
                    variable->m_target_attr, variable->m_contiguous_attr));
            scope->add_symbol(name, copy);
            substitutions.emplace(item.second,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, copy)));
        }
    }
    AssociateVarResolverVisitor replace(al, substitutions);
    for (const auto &item : scope->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*item.second)) {
            replace.visit_Variable(*ASR::down_cast<ASR::Variable_t>(item.second));
        }
    }
    ASRUtils::ExprStmtDuplicator duplicator(al);
    Vec<ASR::stmt_t*> body;
    body.reserve(al, function.n_body + before.size() + after.size() + 1);
    for (ASR::stmt_t *statement : before) body.push_back(al, statement);
    size_t start = body.n;
    if (!clone_body(al, scopes.front(), duplicator, body)) return nullptr;
    for (size_t i = start; i < body.n; i++) replace.visit_stmt(*body[i]);
    for (ASR::stmt_t *statement : after) body.push_back(al, statement);
    if (function.m_return_var) {
        LCOMPILERS_ASSERT(target);
        auto *result = ASR::down_cast<ASR::Var_t>(function.m_return_var)->m_v;
        ASRUtils::ASRBuilder b(al, loc);
        body.push_back(al, b.Assignment(target, substitutions.at(result)));
    }
    std::string name = scope->parent->get_unique_name(
        "__inlined_" + std::string(function.m_name));
    ASR::symbol_t *block = ASR::down_cast<ASR::symbol_t>(
        ASR::make_Block_t(al, loc, scope, s2c(al, name), body.p, body.n));
    scope->parent->add_symbol(name, block);
    return ASRUtils::STMT(ASR::make_BlockCall_t(al, loc, -1, block));
}

} // namespace LCompilers::PassUtils
