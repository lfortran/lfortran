#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/full_section_target.h>

namespace LCompilers {

/*
Full-section targets.

`x(:) = v`, `x(:, :) = v`, ... with every subscript a full-range, unit-stride
section designates every element of the array `x` in array element order, so
it is the intrinsic assignment `x = v` of an allocated, conforming `x`
(F2018 10.2.1.3): a section is never reallocated. This pass rewrites the
target to the whole array, with `realloc_lhs` false, so that every pass below
sees exactly what it sees for `x = v` without reallocation and decides the
same: the same overlap analysis, a function result computed into `x`, no
pointer to the section. A subscript is full when it is `:` (the semantics
spell it `lbound(x, i):ubound(x, i)`), or a constant range equal to the
constant bounds of the dimension, with no stride or a stride of 1.

`x` is a variable or a component, of an object or of an array element
(`p(i)%d(:)`), of an intrinsic or nonpolymorphic derived type. The
lower bounds of a section are 1, but those of the target do not matter: an
intrinsic assignment pairs the elements in array element order. Not
rewritten:

* a defined assignment, whose subroutine is called with the section;
* a polymorphic `x`, which a whole-array assignment reallocates to the
  dynamic type of `v`;
* a character `x` of deferred length, whose whole-array assignment
  reallocates it to the length of `v`;
* an allocatable component of an `intent(out)` dummy or of a function
  result, whose whole-array assignment array_op reallocates in any case;
* an allocatable `x` whose value is a reference to a function with an
  allocatable array result, which subroutine_from_function moves into a
  whole array in any case, reallocating it to the shape and lower bounds
  of the result, or a RESHAPE, whose result is given to a whole
  allocatable array with its shape;
* an assumed-rank or assumed-size `x`, a coarray;
* with reallocation on assignment, an allocatable `x`.

A run-time check in Debug that `x` is allocated and conforms to `v` stays,
as for the whole array.
*/

namespace {

// Whether `x` and `y` are the same designator, made only of variables,
// components, and elements with constant or variable subscripts, so that
// both refer to the same object whenever either is evaluated.
bool same_designator(ASR::expr_t *x, ASR::expr_t *y) {
    if (x->type != y->type) return false;
    switch (x->type) {
        case ASR::exprType::Var: {
            return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(x)->m_v) ==
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(y)->m_v);
        }
        case ASR::exprType::IntegerConstant: {
            return ASR::down_cast<ASR::IntegerConstant_t>(x)->m_n ==
                ASR::down_cast<ASR::IntegerConstant_t>(y)->m_n;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t *a =
                ASR::down_cast<ASR::StructInstanceMember_t>(x);
            ASR::StructInstanceMember_t *b =
                ASR::down_cast<ASR::StructInstanceMember_t>(y);
            return ASRUtils::symbol_get_past_external(a->m_m) ==
                    ASRUtils::symbol_get_past_external(b->m_m) &&
                same_designator(a->m_v, b->m_v);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(x);
            ASR::ArrayItem_t *b = ASR::down_cast<ASR::ArrayItem_t>(y);
            if (a->n_args != b->n_args || !same_designator(a->m_v, b->m_v)) {
                return false;
            }
            for (size_t i = 0; i < a->n_args; i++) {
                ASR::array_index_t &ia = a->m_args[i];
                ASR::array_index_t &ib = b->m_args[i];
                if (ia.m_left || ia.m_step || ib.m_left || ib.m_step ||
                        ia.m_right == nullptr || ib.m_right == nullptr ||
                        !same_designator(ia.m_right, ib.m_right)) {
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

// The value of a constant integer expression, if it is one.
bool constant_integer(ASR::expr_t *x, int64_t &value) {
    if (x == nullptr) return false;
    x = ASRUtils::expr_value(x) ? ASRUtils::expr_value(x) : x;
    if (!ASR::is_a<ASR::IntegerConstant_t>(*x)) return false;
    value = ASR::down_cast<ASR::IntegerConstant_t>(x)->m_n;
    return true;
}

// Whether `bound` is the lower or upper (`upper`) bound of dimension `dim`
// (from 1) of the array `base`: `lbound(base, dim)`, `ubound(base, dim)`, or
// a constant equal to the constant bound of that dimension.
bool is_bound_of(ASR::expr_t *bound, ASR::expr_t *base, ASR::dimension_t &d,
        size_t dim, bool upper) {
    if (bound == nullptr) return true;
    if (ASR::is_a<ASR::ArrayBound_t>(*bound)) {
        ASR::ArrayBound_t *b = ASR::down_cast<ASR::ArrayBound_t>(bound);
        int64_t b_dim;
        if (b->m_bound == (upper ? ASR::arrayboundType::UBound
                    : ASR::arrayboundType::LBound) &&
                constant_integer(b->m_dim, b_dim) &&
                b_dim == static_cast<int64_t>(dim) &&
                same_designator(b->m_v, base)) {
            return true;
        }
    }
    int64_t value, start, length;
    if (!constant_integer(bound, value) ||
            !constant_integer(d.m_start, start) ||
            !constant_integer(d.m_length, length)) {
        return false;
    }
    return value == (upper ? start + length - 1 : start);
}

// Whether the section `x` designates every element of its array, in array
// element order.
bool is_full_section(ASR::ArraySection_t *x) {
    ASR::ttype_t *type = ASRUtils::expr_type(x->m_v);
    ASR::dimension_t *dims = nullptr;
    size_t rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
    if (rank == 0 || rank != x->n_args) return false;
    for (size_t i = 0; i < rank; i++) {
        ASR::array_index_t &index = x->m_args[i];
        int64_t step = 1;
        if ((index.m_step && (!constant_integer(index.m_step, step) ||
                    step != 1)) ||
                (index.m_left == nullptr && index.m_step == nullptr) ||
                !is_bound_of(index.m_left, x->m_v, dims[i], i + 1, false) ||
                !is_bound_of(index.m_right, x->m_v, dims[i], i + 1, true)) {
            return false;
        }
    }
    return true;
}

// The variable a designator is part of.
ASR::Variable_t *base_variable(ASR::expr_t *x) {
    while (true) {
        if (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
            x = ASR::down_cast<ASR::StructInstanceMember_t>(x)->m_v;
        } else if (ASR::is_a<ASR::ArrayItem_t>(*x)) {
            x = ASR::down_cast<ASR::ArrayItem_t>(x)->m_v;
        } else {
            break;
        }
    }
    if (!ASR::is_a<ASR::Var_t>(*x)) return nullptr;
    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(x)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    return ASR::down_cast<ASR::Variable_t>(sym);
}

// Whether the array `x` is assigned as a whole like its full section: see
// the comment at the top.
bool is_whole_array_like_section(ASR::expr_t *x) {
    ASR::Variable_t *variable = base_variable(x);
    if (variable == nullptr) return false;
    if (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
        ASR::StructInstanceMember_t *member =
            ASR::down_cast<ASR::StructInstanceMember_t>(x);
        if (ASRUtils::is_array(ASRUtils::expr_type(member->m_v))) {
            return false;
        }
        if (ASRUtils::is_allocatable(ASRUtils::expr_type(x)) &&
                (variable->m_intent == ASR::intentType::Out ||
                 variable->m_intent == ASR::intentType::ReturnVar)) {
            return false;
        }
    } else if (!ASR::is_a<ASR::Var_t>(*x)) {
        return false;
    }
    ASR::ttype_t *type = ASRUtils::expr_type(x);
    ASR::ttype_t *element = ASRUtils::extract_type(type);
    if (!ASRUtils::is_array(type) || ASRUtils::is_assumed_rank_array(type) ||
            ASRUtils::is_assumed_size_array(type) ||
            ASRUtils::is_class_type(element) ||
            ASR::is_a<ASR::CPtr_t>(*element) ||
            ASR::is_a<ASR::FunctionType_t>(*element) ||
            ASRUtils::is_coarray(ASRUtils::symbol_get_past_external(
                &variable->base))) {
        return false;
    }
    if (ASR::is_a<ASR::String_t>(*element) &&
            ASR::down_cast<ASR::String_t>(element)->m_len_kind ==
                ASR::string_length_kindType::DeferredLength) {
        return false;
    }
    return true;
}

// Whether `value` is a reference to a function with an allocatable array
// result, which a whole-array assignment moves into its target, or a
// RESHAPE, whose result it gives to the target with its shape.
bool is_reallocating_value(ASR::expr_t *value) {
    value = ASRUtils::get_past_array_physical_cast(value);
    if (ASR::is_a<ASR::ArrayReshape_t>(*value)) return true;
    if (!ASR::is_a<ASR::FunctionCall_t>(*value)) return false;
    ASR::FunctionCall_t *call = ASR::down_cast<ASR::FunctionCall_t>(value);
    return !ASRUtils::is_elemental(call->m_name) &&
        ASRUtils::is_allocatable(ASRUtils::expr_type(value));
}

class FullSectionTarget: public ASR::BaseWalkVisitor<FullSectionTarget> {
public:
    explicit FullSectionTarget(bool realloc_lhs_): realloc_lhs(realloc_lhs_) {}

    void visit_Assignment(const ASR::Assignment_t &x) {
        ASR::Assignment_t &xx = const_cast<ASR::Assignment_t&>(x);
        if (xx.m_overloaded || xx.m_move_allocation ||
                !ASR::is_a<ASR::ArraySection_t>(*xx.m_target)) {
            return;
        }
        ASR::ArraySection_t *section =
            ASR::down_cast<ASR::ArraySection_t>(xx.m_target);
        if (!is_whole_array_like_section(section->m_v) ||
                !is_full_section(section) ||
                (ASRUtils::is_allocatable(ASRUtils::expr_type(section->m_v)) &&
                 (realloc_lhs || is_reallocating_value(xx.m_value)))) {
            return;
        }
        xx.m_target = section->m_v;
        xx.m_realloc_lhs = false;
    }

private:
    // With reallocation on assignment, the passes below reallocate an
    // allocatable whole-array target whatever the assignment says.
    bool realloc_lhs;
};

} // namespace

void pass_full_section_target(Allocator &/*al*/, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    FullSectionTarget v(pass_options.realloc_lhs_arrays);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
