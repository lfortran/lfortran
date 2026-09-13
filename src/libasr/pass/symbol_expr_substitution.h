#ifndef LIBASR_PASS_SYMBOL_EXPR_SUBSTITUTION_H
#define LIBASR_PASS_SYMBOL_EXPR_SUBSTITUTION_H

#include <libasr/asr_utils.h>
#include <map>

namespace LCompilers {

class AssociateVarResolver : public ASR::BaseExprReplacer<AssociateVarResolver> {
public:
    Allocator &al;
    std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map;
    AssociateVarResolver(Allocator &al,
            std::map<ASR::symbol_t*, ASR::expr_t*> &map)
        : al(al), assoc_map(map) {}

    void replace_Var(ASR::Var_t *x) {
        auto it = assoc_map.find(x->m_v);
        if (it == assoc_map.end()) return;
        ASRUtils::ExprStmtDuplicator duplicator(al);
        duplicator.success = true;
        ASR::expr_t *copy = duplicator.duplicate_expr(it->second);
        LCOMPILERS_ASSERT(duplicator.success && copy);
        *current_expr = copy;
    }
};

class AssociateVarResolverVisitor :
        public ASR::CallReplacerOnExpressionsVisitor<AssociateVarResolverVisitor> {
public:
    AssociateVarResolver replacer;
    AssociateVarResolverVisitor(Allocator &al,
            std::map<ASR::symbol_t*, ASR::expr_t*> &map)
        : replacer(al, map) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

} // namespace LCompilers

#endif
