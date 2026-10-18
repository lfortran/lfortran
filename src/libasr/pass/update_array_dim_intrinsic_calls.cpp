#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/update_array_dim_intrinsic_calls.h>


namespace LCompilers {

/*

The following class implements replacer methods for replacing
calls to intrinsic functions (which provide dimensional information
of arrays like size) with the actual result. This happens only for arrays
which are input arguments to a procedure, and the dimensional information
in their type is not empty. See the example below,

subrotuine copy(x, y, n)
    integer, intent(in) :: n
    integer, intent(in) :: x(n)
    integer, intent(in) :: y(n)
    do i = 1, size(x)
        do j  = 1, size(y)
            print *, x(i) + y(j)
        end do
    end do
end subroutine

converted to,

subrotuine copy(x, y, n)
    integer, intent(in) :: n
    integer, intent(in) :: x(n)
    integer, intent(in) :: y(n)
    do i = 1, n
        do j  = 1, n
            print *, x(i) + y(j)
        end do
    end do
end subroutine

*/
class ReplaceArrayDimIntrinsicCalls: public ASR::BaseExprReplacer<ReplaceArrayDimIntrinsicCalls> {

    private:

    Allocator& al;

    public:

    // Device code keeps the declared extents (see explicit_shape_extent).
    bool zero_extent_bounds = true;

    ReplaceArrayDimIntrinsicCalls(Allocator& al_) : al(al_)
    {}

    // A bound or length is copied out of the type of the array for each use,
    // so that a later pass rewriting the expression in the body (e.g.
    // inlining a call in max(0, f(n))) leaves the type unchanged.
    ASR::expr_t* copy_of(ASR::expr_t* expr) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        return duplicator.duplicate_expr(expr);
    }

    void replace_ArraySize(ASR::ArraySize_t* x) {
        ASR::expr_t* x_m_v = x->m_v;
        if ( ASR::is_a<ASR::Cast_t>(*x_m_v) ) {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(x_m_v);
            if( ASR::is_a<ASR::Var_t>(*cast->m_arg) ) {
                x_m_v = cast->m_arg;
            }
        }
        if( !ASR::is_a<ASR::Var_t>(*x_m_v) ||
            (x->m_dim != nullptr && !ASRUtils::is_value_constant(x->m_dim)) ) {
            return ;
        }

        ASR::Variable_t* v = ASRUtils::EXPR2VAR(x_m_v);
        ASR::ttype_t* array_type = ASRUtils::expr_type(x_m_v);
        ASR::dimension_t* dims = nullptr;
        int n = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(array_type, dims));
        bool is_argument = v->m_intent == ASRUtils::intent_in || v->m_intent == ASRUtils::intent_out || v->m_intent == ASRUtils::intent_inout;
        if( !(n > 0 && is_argument &&
              !ASRUtils::is_dimension_empty(dims, n)) ) {
            return ;
        }
        if( x->m_dim != nullptr ) {
            int64_t dim = -1;
            ASRUtils::extract_value(x->m_dim, dim);
            *current_expr = copy_of(dims[dim - 1].m_length);
            return ;
        }

        ASR::expr_t* array_size = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                    al, x->base.base.loc, 1, x->m_type));
        for( int i = 0; i < n; i++ ) {
            ASR::expr_t* dim_length = ASRUtils::EXPR(ASR::make_Cast_t(
                al, x->base.base.loc, copy_of(dims[i].m_length), ASR::cast_kindType::IntegerToInteger, x->m_type, nullptr, nullptr));

            array_size = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, x->base.base.loc,
                            array_size, ASR::binopType::Mul, dim_length, x->m_type,
                            nullptr));
        }
        *current_expr = array_size;
    }

    void replace_ArrayBound(ASR::ArrayBound_t* x) {
        if( !ASR::is_a<ASR::Var_t>(*x->m_v) ||
            !ASRUtils::is_value_constant(x->m_dim) ) {
            return ;
        }

        ASR::Variable_t* v = ASRUtils::EXPR2VAR(x->m_v);
        ASR::ttype_t* array_type = ASRUtils::expr_type(x->m_v);
        ASR::dimension_t* dims = nullptr;
        int n = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(array_type, dims));
        bool is_argument = ASRUtils::is_arg_dummy(v->m_intent);
        if( !(n > 0 && is_argument &&
              !ASRUtils::is_dimension_empty(dims, n)) ) {
            return ;
        }
        int64_t dim = -1;
        ASRUtils::extract_value(x->m_dim, dim);
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::expr_t* length = copy_of(dims[dim - 1].m_length);
        ASR::expr_t* start = copy_of(dims[dim - 1].m_start);
        length = b.t2t(length, ASRUtils::expr_type(length), x->m_type);
        start = b.t2t(start, ASRUtils::expr_type(start), x->m_type);
        ASR::expr_t* bound = nullptr;
        if( x->m_bound == ASR::arrayboundType::LBound ) {
            bound = start;
        } else {
            ASR::expr_t* ub = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
                                x->base.base.loc, length,
                                ASR::binopType::Add, start,
                                x->m_type, nullptr));
            ASR::expr_t* const_1 = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                                        x->base.base.loc, 1, x->m_type));
            bound = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, x->base.base.loc,
                                ub, ASR::binopType::Sub, const_1, x->m_type, nullptr));
        }
        // A dimension of zero extent has the bounds 1 and 0 (Fortran 2018,
        // 16.9.109 and 16.9.196). The extent of a dimension that starts at 1
        // is never negative, so only other starts need the test.
        int64_t length_value = 0, start_value = 0;
        bool constant_length = ASRUtils::extract_value(
            ASRUtils::expr_value(dims[dim - 1].m_length), length_value);
        bool unit_start = ASRUtils::extract_value(
            ASRUtils::expr_value(dims[dim - 1].m_start), start_value)
            && start_value == 1;
        if( zero_extent_bounds && !unit_start
                && !(constant_length && length_value > 0) ) {
            ASR::expr_t* empty_bound = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                al, x->base.base.loc,
                x->m_bound == ASR::arrayboundType::LBound ? 1 : 0, x->m_type));
            if( constant_length ) {
                bound = empty_bound;
            } else {
                bound = ASRUtils::EXPR(ASR::make_IfExp_t(al, x->base.base.loc,
                    b.Gt(copy_of(length), ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, x->base.base.loc, 0, x->m_type))),
                    bound, empty_bound, x->m_type, nullptr));
            }
        }
        *current_expr = bound;
    }

};

/*
This class implements call_replacer method which calls the
ReplaceArrayDimIntrinsicCalls.replace_expr method on all the expressions
present in the ASR.
*/
class ArrayDimIntrinsicCallsVisitor : public ASR::CallReplacerOnExpressionsVisitor<ArrayDimIntrinsicCallsVisitor>
{
    private:

        ReplaceArrayDimIntrinsicCalls replacer;

    public:

        ArrayDimIntrinsicCallsVisitor(Allocator& al_) : replacer(al_) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }

        void visit_Function(const ASR::Function_t& x) {
            bool zero_extent_bounds = replacer.zero_extent_bounds;
            replacer.zero_extent_bounds = !ASRUtils::is_device_only_function(x);
            ASR::CallReplacerOnExpressionsVisitor<ArrayDimIntrinsicCallsVisitor>::visit_Function(x);
            replacer.zero_extent_bounds = zero_extent_bounds;
        }

};

void pass_update_array_dim_intrinsic_calls(Allocator &al, ASR::TranslationUnit_t &unit,
                                           const LCompilers::PassOptions& /*pass_options*/) {
    ArrayDimIntrinsicCallsVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
