#include <libasr/asr_utils.h>
#include <libasr/pass/entry_initialized_locals.h>
#include <libasr/pass/symbol_expr_substitution.h>

#include <vector>

namespace LCompilers {

// The locals initialized on entry of `scope`, of the procedures it contains
// and of its BLOCKs and ASSOCIATEs, with their initializers, and those scopes.
// Each local of the same name in the same place of `original`, of which
// `scope` is a copy, is given the same initializer.
static void collect_entry_initialized_locals(SymbolTable* scope,
        SymbolTable* original, std::map<ASR::symbol_t*, ASR::expr_t*>& locals,
        std::vector<SymbolTable*>& scopes) {
    scopes.push_back(scope);
    for (auto& item : scope->get_scope()) {
        ASR::symbol_t* sym = item.second;
        ASR::symbol_t* original_sym = original != nullptr
            ? original->get_symbol(item.first) : nullptr;
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
            if (ASRUtils::is_entry_initialized_local(*v)) {
                locals[sym] = v->m_symbolic_value;
                if (original_sym != nullptr && original_sym != sym) {
                    locals[original_sym] = v->m_symbolic_value;
                }
            }
        } else if (ASR::is_a<ASR::Function_t>(*sym)
                || ASR::is_a<ASR::Block_t>(*sym)
                || ASR::is_a<ASR::AssociateBlock_t>(*sym)) {
            SymbolTable* original_scope = original_sym != nullptr
                    && original_sym->type == sym->type
                ? ASRUtils::symbol_symtab(original_sym) : nullptr;
            collect_entry_initialized_locals(ASRUtils::symbol_symtab(sym),
                original_scope, locals, scopes);
        }
    }
}

void inline_entry_initialized_locals(Allocator &al, ASR::symbol_t* owner,
        ASR::symbol_t* original) {
    std::map<ASR::symbol_t*, ASR::expr_t*> locals;
    std::vector<SymbolTable*> scopes;
    collect_entry_initialized_locals(ASRUtils::symbol_symtab(owner),
        original != nullptr ? ASRUtils::symbol_symtab(original) : nullptr,
        locals, scopes);
    if (locals.empty()) {
        return;
    }
    // An initializer can refer to another such local (an extent to the
    // length it clamps), which each round replaces one level deeper.
    AssociateVarResolver resolver(al, locals);
    for (size_t round = 0; round < locals.size(); round++) {
        for (auto& item : locals) {
            resolver.current_expr = &item.second;
            resolver.replace_expr(item.second);
        }
    }
    AssociateVarResolverVisitor visitor(al, locals);
    visitor.visit_symbol(*owner);
    for (SymbolTable* scope : scopes) {
        std::vector<std::string> names;
        for (auto& item : scope->get_scope()) {
            if (locals.find(item.second) != locals.end()) {
                names.push_back(item.first);
            }
        }
        for (const std::string& name : names) {
            scope->erase_symbol(name);
        }
    }
    for (SymbolTable* scope : scopes) {
        for (auto& item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
                continue;
            }
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
            SetChar dependencies;
            dependencies.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, dependencies,
                v->m_type, v->m_symbolic_value, v->m_value, v->m_name);
            v->m_dependencies = dependencies.p;
            v->n_dependencies = dependencies.size();
        }
    }
}

void substitute_entry_initialized_locals(Allocator &al, ASR::Function_t &fn,
        std::map<ASR::symbol_t*, ASR::expr_t*> &substitutions) {
    std::map<ASR::symbol_t*, ASR::expr_t*> locals;
    std::vector<SymbolTable*> scopes;
    collect_entry_initialized_locals(fn.m_symtab, nullptr, locals, scopes);
    if (locals.empty()) {
        return;
    }
    std::map<ASR::symbol_t*, ASR::expr_t*> values = substitutions;
    for (auto& item : locals) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        values[item.first] = duplicator.duplicate_expr(item.second);
    }
    // As in inline_entry_initialized_locals, one level per round.
    AssociateVarResolver resolver(al, values);
    for (size_t round = 0; round <= locals.size(); round++) {
        for (auto& item : locals) {
            resolver.current_expr = &values[item.first];
            resolver.replace_expr(values[item.first]);
        }
    }
    for (auto& item : locals) {
        substitutions[item.first] = values[item.first];
    }
}

} // namespace LCompilers
