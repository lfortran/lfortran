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

    // An element of a name that stands for `ArrayPhysicalCast(a)` is the
    // same element of `a`: the cast changes how the array is represented,
    // not its bounds. Selecting through the cast would leave the element
    // hanging off an expression rather than the array itself, which no
    // consumer that follows the designator back to its variable recognises
    // -- the GPU kernel layout among them, which then cannot find the
    // buffers backing `it(k)%c` for `associate(it => s)`.
    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        bool substituted = ASR::is_a<ASR::Var_t>(*x->m_v)
            && assoc_map.count(ASR::down_cast<ASR::Var_t>(x->m_v)->m_v);
        ASR::BaseExprReplacer<AssociateVarResolver>::replace_ArrayItem(x);
        if (!substituted || !ASR::is_a<ASR::ArrayPhysicalCast_t>(*x->m_v)) {
            return;
        }
        ASR::expr_t *array =
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(x->m_v)->m_arg;
        if (ASR::is_a<ASR::Var_t>(*array)
                || ASR::is_a<ASR::StructInstanceMember_t>(*array)) {
            x->m_v = array;
        }
    }

    // A cast of a name converts from the representation the name has. The
    // expression substituted for it may have another one -- an inlined
    // explicit-shape dummy becomes a pointer to a descriptor -- so the cast
    // converts from that, or is dropped when the expression already has the
    // representation the cast gives.
    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t *x) {
        bool substituted = ASR::is_a<ASR::Var_t>(*x->m_arg)
            && assoc_map.count(ASR::down_cast<ASR::Var_t>(x->m_arg)->m_v);
        ASR::BaseExprReplacer<AssociateVarResolver>::replace_ArrayPhysicalCast(x);
        if (!substituted || !ASRUtils::is_array(ASRUtils::expr_type(x->m_arg))) {
            return;
        }
        ASR::array_physical_typeType from =
            ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
        if (from == x->m_old) return;
        if (from == x->m_new) {
            *current_expr = x->m_arg;
            return;
        }
        x->m_old = from;
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

    // A BLOCK of the statements is part of them: its body, and the
    // declarations of its locals (`real :: w(m)`), name the same symbols.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        visit_symbol(*x.m_m);
    }
};

} // namespace LCompilers

#endif
