#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_designator.h>

namespace LCompilers {

GpuDesignatorBase gpu_designator_base(ASR::expr_t *e) {
    GpuDesignatorBase base;
    while (e) {
        switch (e->type) {
            case ASR::exprType::Var: {
                base.root = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                return base;
            }
            case ASR::exprType::StructInstanceMember: {
                ASR::StructInstanceMember_t *sm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(e);
                base.members.push_back(
                    ASRUtils::symbol_get_past_external(sm->m_m));
                e = sm->m_v;
                break;
            }
            case ASR::exprType::ArraySection: {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                break;
            }
            case ASR::exprType::ArrayItem: {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                break;
            }
            case ASR::exprType::ArrayPhysicalCast: {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                break;
            }
            default: {
                return GpuDesignatorBase();
            }
        }
    }
    return GpuDesignatorBase();
}

// Strict structural equality of two subscript expressions. Anything not
// understood here compares unequal, which makes the two designators
// differ and so errs towards materialising a temporary.
static bool gpu_same_subscript(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == b) return true;
    if (!a || !b || a->type != b->type) return false;
    switch (a->type) {
        case ASR::exprType::Var: {
            return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(a)->m_v)
                == ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(b)->m_v);
        }
        case ASR::exprType::IntegerConstant: {
            return ASR::down_cast<ASR::IntegerConstant_t>(a)->m_n
                == ASR::down_cast<ASR::IntegerConstant_t>(b)->m_n;
        }
        case ASR::exprType::IntegerUnaryMinus: {
            return gpu_same_subscript(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(a)->m_arg,
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(b)->m_arg);
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *x = ASR::down_cast<ASR::IntegerBinOp_t>(a);
            ASR::IntegerBinOp_t *y = ASR::down_cast<ASR::IntegerBinOp_t>(b);
            return x->m_op == y->m_op
                && gpu_same_subscript(x->m_left, y->m_left)
                && gpu_same_subscript(x->m_right, y->m_right);
        }
        // A whole-dimension `:` over an array whose extents are not known
        // until run time is lowered to `lbound(a,d):ubound(a,d):1`, so
        // two spellings of one section differ only by these nodes. Without
        // them `a(:,i) = a(:,i)` reads as an aliasing assignment and is
        // given a temporary it does not need.
        case ASR::exprType::ArrayBound: {
            ASR::ArrayBound_t *x = ASR::down_cast<ASR::ArrayBound_t>(a);
            ASR::ArrayBound_t *y = ASR::down_cast<ASR::ArrayBound_t>(b);
            return x->m_bound == y->m_bound
                && gpu_same_subscript(x->m_dim, y->m_dim)
                && gpu_same_designator(x->m_v, y->m_v);
        }
        case ASR::exprType::ArraySize: {
            ASR::ArraySize_t *x = ASR::down_cast<ASR::ArraySize_t>(a);
            ASR::ArraySize_t *y = ASR::down_cast<ASR::ArraySize_t>(b);
            if ((x->m_dim == nullptr) != (y->m_dim == nullptr)) {
                return false;
            }
            return (x->m_dim == nullptr
                    || gpu_same_subscript(x->m_dim, y->m_dim))
                && gpu_same_designator(x->m_v, y->m_v);
        }
        default: {
            return false;
        }
    }
}

bool gpu_same_designator(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == b) return true;
    if (!a || !b || a->type != b->type) return false;
    switch (a->type) {
        case ASR::exprType::Var: {
            return gpu_same_subscript(a, b);
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t *x = ASR::down_cast<ASR::ArraySection_t>(a);
            ASR::ArraySection_t *y = ASR::down_cast<ASR::ArraySection_t>(b);
            if (x->n_args != y->n_args) return false;
            if (!gpu_same_designator(x->m_v, y->m_v)) return false;
            for (size_t i = 0; i < x->n_args; i++) {
                if (!gpu_same_subscript(x->m_args[i].m_left,
                        y->m_args[i].m_left)
                    || !gpu_same_subscript(x->m_args[i].m_right,
                        y->m_args[i].m_right)
                    || !gpu_same_subscript(x->m_args[i].m_step,
                        y->m_args[i].m_step)) {
                    return false;
                }
            }
            return true;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t *x =
                ASR::down_cast<ASR::StructInstanceMember_t>(a);
            ASR::StructInstanceMember_t *y =
                ASR::down_cast<ASR::StructInstanceMember_t>(b);
            if (ASRUtils::symbol_get_past_external(x->m_m)
                    != ASRUtils::symbol_get_past_external(y->m_m)) {
                return false;
            }
            return gpu_same_designator(x->m_v, y->m_v);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t *x = ASR::down_cast<ASR::ArrayItem_t>(a);
            ASR::ArrayItem_t *y = ASR::down_cast<ASR::ArrayItem_t>(b);
            if (x->n_args != y->n_args) return false;
            if (!gpu_same_designator(x->m_v, y->m_v)) return false;
            for (size_t i = 0; i < x->n_args; i++) {
                if (!gpu_same_subscript(x->m_args[i].m_left,
                        y->m_args[i].m_left)
                    || !gpu_same_subscript(x->m_args[i].m_right,
                        y->m_args[i].m_right)
                    || !gpu_same_subscript(x->m_args[i].m_step,
                        y->m_args[i].m_step)) {
                    return false;
                }
            }
            return true;
        }
        default: {
            return false;
        }
    }
}

bool gpu_designator_within(ASR::expr_t *outer, ASR::expr_t *inner) {
    while (outer) {
        if (gpu_same_designator(outer, inner)) return true;
        switch (outer->type) {
            case ASR::exprType::StructInstanceMember: {
                outer = ASR::down_cast<ASR::StructInstanceMember_t>(
                    outer)->m_v;
                break;
            }
            case ASR::exprType::ArraySection: {
                outer = ASR::down_cast<ASR::ArraySection_t>(outer)->m_v;
                break;
            }
            case ASR::exprType::ArrayItem: {
                outer = ASR::down_cast<ASR::ArrayItem_t>(outer)->m_v;
                break;
            }
            case ASR::exprType::ArrayPhysicalCast: {
                outer = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    outer)->m_arg;
                break;
            }
            default: {
                return false;
            }
        }
    }
    return false;
}

} // namespace LCompilers
