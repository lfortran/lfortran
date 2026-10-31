#ifndef LIBASR_PASS_SYMBOL_EXPR_SUBSTITUTION_H
#define LIBASR_PASS_SYMBOL_EXPR_SUBSTITUTION_H

#include <libasr/asr_utils.h>

#include <algorithm>
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

    // An integer operation, or a maximum, whose operands the substitution
    // made constants is a constant too, so that, for example, the extent
    // max(0, n + 1) of an explicit-shape array is a constant again when a
    // constant is substituted for `n`.
    void replace_IntegerBinOp(ASR::IntegerBinOp_t *x) {
        ASR::BaseExprReplacer<AssociateVarResolver>::replace_IntegerBinOp(x);
        int64_t left = 0, right = 0;
        if (x->m_value != nullptr
                || !ASRUtils::extract_value(ASRUtils::expr_value(x->m_left), left)
                || !ASRUtils::extract_value(ASRUtils::expr_value(x->m_right), right)) {
            return;
        }
        int64_t value = 0;
        switch (x->m_op) {
            case ASR::binopType::Add: value = left + right; break;
            case ASR::binopType::Sub: value = left - right; break;
            case ASR::binopType::Mul: value = left * right; break;
            default: return;
        }
        x->m_value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
            x->base.base.loc, value, x->m_type));
    }

    // Substituting an assumed-shape dummy can expose a fixed component.
    // Its size is independent of the subscripts selecting its owner.
    void replace_ArraySize(ASR::ArraySize_t *x) {
        ASR::BaseExprReplacer<AssociateVarResolver>::replace_ArraySize(x);
        ASR::expr_t *array = x->m_v;
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*array)) {
            array = ASR::down_cast<ASR::ArrayPhysicalCast_t>(array)->m_arg;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(array);
        if (x->m_value || (!ASR::is_a<ASR::Var_t>(*array)
                && !ASR::is_a<ASR::StructInstanceMember_t>(*array))
                || ASRUtils::is_allocatable(type) || ASRUtils::is_pointer(type)
                || !ASRUtils::is_fixed_size_array(type)) {
            return;
        }
        int64_t size = 0;
        if (x->m_dim) {
            ASR::dimension_t *dims = nullptr;
            size_t rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
            int64_t dim = 0;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(x->m_dim), dim)
                    || dim < 1 || (size_t)dim > rank
                    || !ASRUtils::extract_value(
                        ASRUtils::expr_value(dims[dim - 1].m_length), size)) {
                return;
            }
        } else {
            size = ASRUtils::get_fixed_size_of_array(type);
        }
        *current_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
            x->base.base.loc, size, x->m_type));
    }

    void replace_IntrinsicElementalFunction(
            ASR::IntrinsicElementalFunction_t *x) {
        ASR::BaseExprReplacer<AssociateVarResolver>
            ::replace_IntrinsicElementalFunction(x);
        if (x->m_value != nullptr || !ASRUtils::is_intrinsic_max(*x)
                || !ASRUtils::is_integer(*x->m_type)) {
            return;
        }
        int64_t value = 0;
        for (size_t i = 0; i < x->n_args; i++) {
            int64_t arg = 0;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(x->m_args[i]),
                    arg)) {
                return;
            }
            value = i == 0 ? arg : std::max(value, arg);
        }
        *current_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
            x->base.base.loc, value, x->m_type));
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
