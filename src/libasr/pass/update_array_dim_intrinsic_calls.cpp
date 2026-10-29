#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/intrinsic_function_registry.h>
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

    // Device code keeps the declared extents (see entry_initialized_locals).
    bool zero_extent_bounds = true;

    ReplaceArrayDimIntrinsicCalls(Allocator& al_) : al(al_)
    {}

    // The size of an array section follows from its subscripts, so it is
    // computed here and the backend only reads the sizes of the arrays it
    // stores. A `dim` known only at run time selects among the extents
    // with `merge(extent 1, merge(extent 2, ..., dim == 2), dim == 1)`.
    ASR::expr_t* section_size(ASR::expr_t* section, ASR::expr_t* dim,
            ASR::ttype_t* type, const Location& loc) {
        int64_t dim_value = -1;
        if( dim == nullptr || ASRUtils::extract_value(
                ASRUtils::expr_value(dim), dim_value) ) {
            return ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(al, loc,
                section, dim, type, nullptr, false));
        }
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t* dim_type = ASRUtils::expr_type(dim);
        size_t rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::expr_type(section));
        auto extent = [&](size_t i) {
            return ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(al, loc,
                section, b.i_t(i, dim_type), type, nullptr, false));
        };
        ASR::expr_t* size = extent(rank);
        for( size_t i = rank - 1; i >= 1; i-- ) {
            Vec<ASR::expr_t*> merge_args; merge_args.reserve(al, 3);
            merge_args.push_back(al, extent(i));
            merge_args.push_back(al, size);
            merge_args.push_back(al, b.Eq(dim, b.i_t(i, dim_type)));
            diag::Diagnostics diag;
            ASR::asr_t* merged = ASRUtils::Merge::create_Merge(al, loc,
                merge_args, diag);
            // Two integer extents of `type` and a logical test cannot be
            // rejected by Merge.
            LCOMPILERS_ASSERT(merged != nullptr && !diag.has_error());
            size = ASRUtils::EXPR(merged);
        }
        return size;
    }

    // A bound or length is copied out of the type of the array for each use,
    // so that a later pass rewriting the expression in the body (e.g.
    // inlining a call in max(0, f(n))) leaves the type unchanged.
    ASR::expr_t* copy_of(ASR::expr_t* expr) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        return duplicator.duplicate_expr(expr);
    }

    void replace_ArraySize(ASR::ArraySize_t* x) {
        ASR::expr_t* size_v = ASRUtils::get_expr_size_expr(x->m_v);
        if( x->m_value == nullptr && size_v != nullptr &&
                ASR::is_a<ASR::ArraySection_t>(*size_v) ) {
            *current_expr = section_size(size_v, x->m_dim, x->m_type,
                x->base.base.loc);
            return ;
        }
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
            // The extent is declared with a kind of its own, as for `a(n)`
            // with an integer(8) `n`, and SIZE returns its own kind
            ASR::expr_t* length = copy_of(dims[dim - 1].m_length);
            if( ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(length)) !=
                    ASRUtils::extract_kind_from_ttype_t(x->m_type) ) {
                length = ASRUtils::EXPR(ASRUtils::make_Cast_t_value(al,
                    x->base.base.loc, length, ASR::cast_kindType::IntegerToInteger,
                    x->m_type));
            }
            *current_expr = length;
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

    ASR::expr_t* declared_bound(ASR::ArrayBound_t* x,
            ASR::dimension_t* dims, int64_t dim) {
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::expr_t* length = dims[dim - 1].m_length
            ? copy_of(dims[dim - 1].m_length)
            : ASRUtils::EXPR(ASR::make_ArraySize_t(al, x->base.base.loc,
                copy_of(x->m_v), b.i32(dim), x->m_type, nullptr));
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
            ASRUtils::expr_value(length), length_value);
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
        return bound;
    }

    void replace_ArrayBound(ASR::ArrayBound_t* x) {
        if( !ASR::is_a<ASR::Var_t>(*x->m_v) || !x->m_dim ) {
            return ;
        }
        int64_t dim = -1;
        bool constant_dim = ASRUtils::extract_value(
            ASRUtils::expr_value(x->m_dim), dim);
        // A runtime selector can occur in more than one test. Leave other
        // expressions for a lowering that can materialize their value.
        if( !constant_dim && !PassUtils::is_reevaluable_integer_expr(x->m_dim) ) {
            return ;
        }
        ASR::Variable_t* v = ASRUtils::EXPR2VAR(x->m_v);
        ASR::dimension_t* dims = nullptr;
        int n = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(x->m_v), dims));
        if( !(n > 0 && ASRUtils::is_arg_dummy(v->m_intent)) ) {
            return ;
        }
        if (ASRUtils::is_dimension_empty(dims, n)) {
            if (ASRUtils::is_allocatable_or_pointer(v->m_type) ||
                    ASRUtils::extract_physical_type(v->m_type) !=
                        ASR::array_physical_typeType::DescriptorArray) {
                return;
            }
            for (int i = 0; i < n; i++) {
                if (!dims[i].m_start) return;
            }
        }
        if( constant_dim ) {
            *current_expr = declared_bound(x, dims, dim);
            return ;
        }
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::expr_t* bound = declared_bound(x, dims, n);
        for( int i = n - 1; i >= 1; i-- ) {
            bound = ASRUtils::EXPR(ASR::make_IfExp_t(al, x->base.base.loc,
                b.Eq(copy_of(x->m_dim), b.i_t(i, ASRUtils::expr_type(x->m_dim))),
                declared_bound(x, dims, i), bound, x->m_type, nullptr));
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
