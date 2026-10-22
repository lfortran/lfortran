#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_array_op.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/intrinsic_subroutines.h>

#include <libasr/asr_builder.h>

#include <vector>

namespace LCompilers {

bool is_parameter_designator(ASR::expr_t* expr) {
    while (expr != nullptr) {
        if (ASR::is_a<ASR::Var_t>(*expr)) {
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(expr)->m_v);
            return ASR::is_a<ASR::Variable_t>(*sym) &&
                ASR::down_cast<ASR::Variable_t>(sym)->m_storage ==
                    ASR::storage_typeType::Parameter;
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
            expr = ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v;
        } else if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v;
        } else if (ASR::is_a<ASR::ArraySection_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArraySection_t>(expr)->m_v;
        } else {
            return false;
        }
    }
    return false;
}

class ArrayVarAddressReplacer: public ASR::BaseExprReplacer<ArrayVarAddressReplacer> {

    public:

    Allocator& al;
    Vec<ASR::expr_t**>& vars;

    ArrayVarAddressReplacer(Allocator& al_, Vec<ASR::expr_t**>& vars_):
        al(al_), vars(vars_) {
        call_replacer_on_value = false;
    }

    void replace_ArraySize(ASR::ArraySize_t* /*x*/) {

    }

    void replace_ArrayBound(ASR::ArrayBound_t* /*x*/) {

    }

    void replace_ArrayRank(ASR::ArrayRank_t* /*x*/) {

    }

    void replace_StringLen(ASR::StringLen_t* /*x*/) {
        // LEN of a character array is a scalar inquiry, like SIZE; its
        // array argument is not an elemental operand of the expression.
    }

    void replace_Var(ASR::Var_t* x) {
        if( ASRUtils::is_array(ASRUtils::symbol_type(x->m_v)) ) {
            vars.push_back(al, current_expr);
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t* x) {
        if( !ASRUtils::is_array(x->m_type) ) {
            return ;
        }
        if( ASRUtils::is_array(ASRUtils::symbol_type(x->m_m)) ) {
            vars.push_back(al, current_expr);
        } else {
            ASR::BaseExprReplacer<ArrayVarAddressReplacer>::replace_StructInstanceMember(x);
        }
    }

    void replace_ArrayItem(ASR::ArrayItem_t* x) {
        // `w%u(2)` with `w` an array is an array although its subscripts are
        // scalar: they select one element of the component out of every
        // element of the base, so the shape comes from the base. Descend
        // past the component to the base array, so that the loop indexes
        // the array the shape actually comes from.
        if( !ASRUtils::is_array(x->m_type) ||
            ASRUtils::struct_base_lending_shape(x) == nullptr ) {
            return ;
        }
        ASR::BaseExprReplacer<ArrayVarAddressReplacer>::replace_StructInstanceMember(
            ASR::down_cast<ASR::StructInstanceMember_t>(x->m_v));
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        if( !ASRUtils::is_elemental(x->m_name) ) {
            return ;
        }

        ASR::BaseExprReplacer<ArrayVarAddressReplacer>::replace_FunctionCall(x);
    }

    void replace_IntrinsicArrayFunction(ASR::IntrinsicArrayFunction_t* /*x*/) {
        // Do not descend into intrinsic array functions (reductions like
        // MaxVal, MinVal, Sum, etc.) because they operate on whole arrays
        // and their array arguments must not be replaced with element accesses.
    }

};

class ArrayVarAddressCollector: public ASR::CallReplacerOnExpressionsVisitor<ArrayVarAddressCollector> {

    private:

    ArrayVarAddressReplacer replacer;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    ArrayVarAddressCollector(Allocator& al_, Vec<ASR::expr_t**>& vars_):
        replacer(al_, vars_) {
        visit_expr_after_replacement = false;
    }

    void visit_Allocate(const ASR::Allocate_t& /*x*/) {
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t& /*x*/) {
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t& /*x*/) {
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        if( !ASRUtils::is_elemental(x.m_name) ) {
            return ;
        }
    }

    void visit_Associate(const ASR::Associate_t& /*x*/) {
    }

};

class FixTypeVisitor: public ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor> {
    public:

    FixTypeVisitor(Allocator& al_) {
        (void)al_;      // Explicitly mark the parameter as unused
    }

    void visit_Cast(const ASR::Cast_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_Cast(x);
        ASR::Cast_t& xx = const_cast<ASR::Cast_t&>(x);
        if( !ASRUtils::is_array(ASRUtils::expr_type(x.m_arg)) &&
             ASRUtils::is_array(x.m_type) ) {
            xx.m_type = ASRUtils::type_get_past_array(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_IntrinsicElementalFunction(x);
        ASR::IntrinsicElementalFunction_t& xx = const_cast<ASR::IntrinsicElementalFunction_t&>(x);
        if (ASRUtils::is_array(xx.m_type)) {
            xx.m_type = ASRUtils::extract_type(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_FunctionCall(x);
        if( !ASRUtils::is_elemental(x.m_name) ) {
            return ;
        }
        ASR::FunctionCall_t& xx = const_cast<ASR::FunctionCall_t&>(x);
        if (ASRUtils::is_array(xx.m_type)) {
            xx.m_type = ASRUtils::extract_type(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    template <typename T>
    void visit_ArrayOp(const T& x) {
        T& xx = const_cast<T&>(x);
        if( !ASRUtils::is_array(ASRUtils::expr_type(xx.m_left)) &&
            !ASRUtils::is_array(ASRUtils::expr_type(xx.m_right)) &&
            ASRUtils::is_array(xx.m_type) ) {
            xx.m_type = ASRUtils::extract_type(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_IntegerBinOp(x);
        visit_ArrayOp(x);
    }

    void visit_UnsignedIntegerBinOp(const ASR::UnsignedIntegerBinOp_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_UnsignedIntegerBinOp(x);
        visit_ArrayOp(x);
    }

    void visit_RealBinOp(const ASR::RealBinOp_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_RealBinOp(x);
        visit_ArrayOp(x);
    }

    void visit_ComplexBinOp(const ASR::ComplexBinOp_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_ComplexBinOp(x);
        visit_ArrayOp(x);
    }

    void visit_RealCompare(const ASR::RealCompare_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_RealCompare(x);
        visit_ArrayOp(x);
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_IntegerCompare(x);
        visit_ArrayOp(x);
    }

    void visit_ComplexCompare(const ASR::ComplexCompare_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_ComplexCompare(x);
        visit_ArrayOp(x);
    }

    void visit_StringCompare(const ASR::StringCompare_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_StringCompare(x);
        visit_ArrayOp(x);
    }

    void visit_StringConcat(const ASR::StringConcat_t& x){
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_StringConcat(x);
        visit_ArrayOp(x);
    }

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_LogicalBinOp(x);
        visit_ArrayOp(x);
    }

    void visit_BitCast(const ASR::BitCast_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_BitCast(x);
        ASR::BitCast_t& xx = const_cast<ASR::BitCast_t&>(x);
        if (!ASRUtils::is_array(ASRUtils::expr_type(x.m_mold)) &&
             ASRUtils::is_array(x.m_type)) {
            xx.m_type = ASRUtils::type_get_past_array(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    void visit_ComplexRe(const ASR::ComplexRe_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_ComplexRe(x);
        ASR::ComplexRe_t& xx = const_cast<ASR::ComplexRe_t&>(x);
        if (!ASRUtils::is_array(ASRUtils::expr_type(x.m_arg)) &&
             ASRUtils::is_array(x.m_type)) {
            xx.m_type = ASRUtils::type_get_past_array(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    void visit_ComplexIm(const ASR::ComplexIm_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_ComplexIm(x);
        ASR::ComplexIm_t& xx = const_cast<ASR::ComplexIm_t&>(x);
        if (!ASRUtils::is_array(ASRUtils::expr_type(x.m_arg)) &&
             ASRUtils::is_array(x.m_type)) {
            xx.m_type = ASRUtils::type_get_past_array(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_StructInstanceMember(x);
        if( !ASRUtils::is_array(x.m_type) ) {
            return ;
        }
        if( !ASRUtils::is_array(ASRUtils::expr_type(x.m_v)) &&
            !ASRUtils::is_array(ASRUtils::symbol_type(x.m_m)) ) {
            ASR::StructInstanceMember_t& xx = const_cast<ASR::StructInstanceMember_t&>(x);
            xx.m_type = ASRUtils::extract_type(x.m_type);
        }
    }

    void visit_ArrayItem(const ASR::ArrayItem_t& x) {
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_ArrayItem(x);
        // The shape of `w%u(2)` is the shape of its base `w`. Once the base
        // has been indexed by the loop this element selection is a scalar
        // again, so it must not keep claiming the shape it inherited.
        if( !ASRUtils::is_array(x.m_type) ||
            ASRUtils::is_array_indexed_with_array_indices(x.m_args, x.n_args) ||
            ASRUtils::struct_base_lending_shape(
                const_cast<ASR::ArrayItem_t*>(&x)) != nullptr ) {
            return ;
        }
        ASR::ArrayItem_t& xx = const_cast<ASR::ArrayItem_t&>(x);
        xx.m_type = ASRUtils::extract_type(x.m_type);
    }

    void visit_RealUnaryMinus(const ASR::RealUnaryMinus_t& x){
        if( !ASRUtils::is_array(x.m_type) ) {
            return ;
        }
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_RealUnaryMinus(x);
        ASR::RealUnaryMinus_t& xx = const_cast<ASR::RealUnaryMinus_t&>(x);
        xx.m_type = ASRUtils::extract_type(x.m_type);
    }

    void visit_IntegerUnaryMinus(const ASR::IntegerUnaryMinus_t& x){
        if( !ASRUtils::is_array(x.m_type) ) {
            return ;
        }
        ASR::CallReplacerOnExpressionsVisitor<FixTypeVisitor>::visit_IntegerUnaryMinus(x);
        ASR::IntegerUnaryMinus_t& xx = const_cast<ASR::IntegerUnaryMinus_t&>(x);
        xx.m_type = ASRUtils::extract_type(x.m_type);
    }
};

class CleanupDegenerateArraySection: public ASR::BaseExprReplacer<CleanupDegenerateArraySection> {
    public:
    Allocator& al;
    CleanupDegenerateArraySection(Allocator& al_): al(al_) {}

    void replace_ArraySection(ASR::ArraySection_t* x) {
        ASR::BaseExprReplacer<CleanupDegenerateArraySection>::replace_ArraySection(x);
        if (!ASRUtils::is_array(ASRUtils::expr_type(x->m_v))) {
            *current_expr = x->m_v;
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t* x) {
        ASR::BaseExprReplacer<CleanupDegenerateArraySection>::replace_StructInstanceMember(x);
        if (ASRUtils::is_array(x->m_type) &&
            !ASRUtils::is_array(ASRUtils::expr_type(x->m_v)) &&
            !ASRUtils::is_array(ASRUtils::symbol_type(x->m_m))) {
            x->m_type = ASRUtils::extract_type(x->m_type);
        }
    }
};

class ReplaceArrayOp: public ASR::BaseExprReplacer<ReplaceArrayOp> {

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>& pass_result;

    public:

    SymbolTable* current_scope;
    ASR::expr_t* result_expr;
    bool& remove_original_stmt;

    ReplaceArrayOp(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_,
                   bool& remove_original_stmt_):
        al(al_), pass_result(pass_result_),
        current_scope(nullptr), result_expr(nullptr),
        remove_original_stmt(remove_original_stmt_) {}

    #define remove_original_stmt_if_size_0(type) if( ASRUtils::get_fixed_size_of_array(type) == 0 ) { \
            remove_original_stmt = true; \
            return ; \
        } \

    void replace_ArrayConstant(ASR::ArrayConstant_t* x) {
        remove_original_stmt_if_size_0(x->m_type)
        if (result_expr == nullptr) {
            return;
        }
        pass_result.reserve(al, x->m_n_data);
        const Location& loc = x->base.base.loc;
        ASR::ttype_t* result_type = ASRUtils::expr_type(result_expr);
        ASR::ttype_t* result_element_type = ASRUtils::extract_type(result_type);

        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(x->m_type, m_dims);

        int64_t size = ASRUtils::get_fixed_size_of_array(x->m_type);
        int64_t repeat = 1;
        std::vector<std::vector<int64_t>> rank_indexes(n_dims, std::vector<int64_t>(size));
        for (size_t i = 0; i < n_dims; i++) {
            int64_t length = 0, start = 1, ubound;
            ASRUtils::extract_value(m_dims[i].m_length, length);
            ASRUtils::extract_value(m_dims[i].m_start, start);
            ubound = length + start - 1;
            int64_t c = 0;
            while (c != size) {
                for (int64_t j = start; j <= ubound; j++) {
                    for (int64_t k = 1; k <= repeat; k++) {
                        rank_indexes[i][c++] = j;
                    }
                }
            }
            repeat *= length;
        }

        for (int64_t i = 0; i < size; i++) {
            ASR::expr_t* x_i = ASRUtils::fetch_ArrayConstant_value(al, x, i);
            Vec<ASR::array_index_t> array_index_args;
            array_index_args.reserve(al, n_dims);
            for (size_t j = 0; j < n_dims; j++) {
                ASR::array_index_t array_index_arg;
                array_index_arg.loc = loc;
                array_index_arg.m_left = nullptr;
                array_index_arg.m_right = make_ConstantWithKind(
                    make_IntegerConstant_t, make_Integer_t, rank_indexes[j][i], 4, loc);
                array_index_arg.m_step = nullptr;
                array_index_args.push_back(al, array_index_arg);
            }
            ASR::expr_t* y_i = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                result_expr, array_index_args.p, array_index_args.size(),
                result_element_type, ASR::arraystorageType::ColMajor, nullptr));
            pass_result.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, y_i, x_i, nullptr, false, false)));
        }
    }

    void replace_IntrinsicElementalFunction(ASR::IntrinsicElementalFunction_t* x) {
        for (size_t i=0; i<x->n_args; i++) {
            current_expr = &(x->m_args[i]);
            replace_expr(*current_expr);
        }
        if (result_expr == nullptr) {
            return;
        }
    }

    bool are_all_elements_scalars(ASR::expr_t** args, size_t n) {
        for( size_t i = 0; i < n; i++ ) {
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*args[i])) {
                return false;
            }
            if( ASRUtils::is_array(ASRUtils::expr_type(args[i])) ) {
                return false;
            }
        }
        return true;
    }

    void replace_ArrayConstructor(ASR::ArrayConstructor_t* x) {
        // TODO: Remove this because the ArrayConstructor node should
        // be replaced with its value already (if present) in array_struct_temporary pass.
        if( x->m_value == nullptr ) {
            if( !are_all_elements_scalars(x->m_args, x->n_args) ) {
                PassUtils::ReplacerUtils::replace_ArrayConstructor_(
                    al, x, result_expr, &pass_result, current_scope);
                return ;
            }

            if( !ASRUtils::is_fixed_size_array(x->m_type) ) {
                PassUtils::ReplacerUtils::replace_ArrayConstructor_(
                    al, x, result_expr, &pass_result, current_scope);
                return ;
            }
        }

        ASR::ttype_t* arr_type = nullptr;
        ASR::ArrayConstant_t* arr_value = nullptr;
        if( x->m_value ) {
            arr_value = ASR::down_cast<ASR::ArrayConstant_t>(x->m_value);
            arr_type = arr_value->m_type;
        } else {
            arr_type = x->m_type;
        }

        remove_original_stmt_if_size_0(arr_type)

        if (result_expr == nullptr) {
            return;
        }
        pass_result.reserve(al, x->n_args);
        const Location& loc = x->base.base.loc;

        ASR::ttype_t* result_type = ASRUtils::expr_type(result_expr);
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::ttype_t* result_element_type = ASRUtils::extract_type(result_type);
        result_element_type = duplicator.duplicate_ttype(result_element_type);

        FixTypeVisitor fix_type_visitor(al);
        fix_type_visitor.current_scope = current_scope;
        fix_type_visitor.visit_ttype(*result_element_type);

        ASRUtils::ASRBuilder builder(al, loc);
        // The elements are stored from the lower bound of the variable
        ASR::dimension_t* m_dims = nullptr;
        ASRUtils::extract_dimensions_from_ttype(result_type, m_dims);
        ASR::expr_t* start = m_dims[0].m_start;
        if (start == nullptr) {
            start = PassUtils::get_bound(result_expr, 1, "lbound", al);
        }

        for( int64_t i = 0; i < ASRUtils::get_fixed_size_of_array(arr_type); i++ ) {
            ASR::expr_t* x_i = nullptr;
            if( x->m_value ) {
                x_i = ASRUtils::fetch_ArrayConstant_value(al, arr_value, i);
            } else {
                x_i = x->m_args[i];
            }
            LCOMPILERS_ASSERT(!ASRUtils::is_array(ASRUtils::expr_type(x_i)));
            Vec<ASR::array_index_t> array_index_args;
            array_index_args.reserve(al, 1);
            ASR::array_index_t array_index_arg;
            array_index_arg.loc = loc;
            array_index_arg.m_left = nullptr;
            array_index_arg.m_right = builder.Add(start, make_ConstantWithKind(
                make_IntegerConstant_t, make_Integer_t, i,
                ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(start)), loc));
            array_index_arg.m_step = nullptr;
            array_index_args.push_back(al, array_index_arg);
            ASR::expr_t* y_i = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                result_expr, array_index_args.p, array_index_args.size(),
                result_element_type, ASR::arraystorageType::ColMajor, nullptr));
            pass_result.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, y_i, x_i, nullptr, false, false)));
        }
    }

};

ASR::expr_t* at(Vec<ASR::expr_t*>& vec, int64_t index) {
    index = index + vec.size();
    if( index < 0 ) {
        return nullptr;
    }
    return vec[index];
}

// Whether an expression has a section of an array among its operands.
class ArraySectionFinder: public ASR::BaseWalkVisitor<ArraySectionFinder> {
    public:

    bool found = false;

    void visit_ArraySection(const ASR::ArraySection_t& /*x*/) {
        found = true;
    }
};

// Collect the array expressions which should be of the same size
class CollectComponentsFromElementalExpr: public ASR::BaseWalkVisitor<CollectComponentsFromElementalExpr> {
private:
        Allocator& al;
        Vec<ASR::expr_t*>& vars;
public:

        CollectComponentsFromElementalExpr(Allocator& al_, Vec<ASR::expr_t*>& vars_) :
        al(al_), vars(vars_) {}

        void push_expr_if_array(ASR::expr_t *v) {
            if (ASRUtils::is_array(ASRUtils::expr_type(v))) {
                vars.push_back(al, v);
            }
        }

        // Don't go inside these
        void visit_ttype(const ASR::ttype_t &) {}
        void visit_ArraySection(const ASR::ArraySection_t&) {}

        void visit_ArrayItem(const ASR::ArrayItem_t& x) {
            // An ordinary element selection is a scalar and has no shape to
            // contribute. `w%u(2)` with `w` an array is different: it reads
            // one element of the component out of every element of the base,
            // so it is shaped like that base. Contribute the base, which is
            // the expression that carries the shape.
            ASR::expr_t* base = ASRUtils::struct_base_lending_shape(
                const_cast<ASR::ArrayItem_t*>(&x));
            if (base != nullptr) {
                visit_expr(*base);
            }
        }
        void visit_ArraySize(const ASR::ArraySize_t&) {}
        void visit_ArrayReshape(const ASR::ArrayReshape_t&) {}
        void visit_ArrayBound(const ASR::ArrayBound_t&) {}
        void visit_StringLen(const ASR::StringLen_t&) {}

        void visit_Var(const ASR::Var_t& x) {
            ASR::Var_t *xx = const_cast<ASR::Var_t*>(&x);
            push_expr_if_array((ASR::expr_t *)xx);
        }

        void visit_ArrayPhysicalCast(const ASR::ArrayPhysicalCast_t& x) {
            ASR::ArrayPhysicalCast_t *xx = const_cast<ASR::ArrayPhysicalCast_t*>(&x);
            push_expr_if_array((ASR::expr_t *)xx);
        }

        void visit_StructInstanceMember(const ASR::StructInstanceMember_t& x) {
            ASR::StructInstanceMember_t *xx = const_cast<ASR::StructInstanceMember_t*>(&x);
            push_expr_if_array((ASR::expr_t *)xx);
        }

        void visit_BitCast(const ASR::BitCast_t& x) {
            ASR::BitCast_t *xx = const_cast<ASR::BitCast_t*>(&x);
            push_expr_if_array((ASR::expr_t *)xx);
        }

        void visit_ArrayConstant(const ASR::ArrayConstant_t& x) {
            ASR::ArrayConstant_t *xx = const_cast<ASR::ArrayConstant_t*>(&x);
            push_expr_if_array((ASR::expr_t *)xx);
        }

        // Only go inside the FunctionCall if the Function is elemental
        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            if (ASRUtils::is_elemental(x.m_name)) {
                ASR::BaseWalkVisitor<CollectComponentsFromElementalExpr>::visit_FunctionCall(x);
            }
        }
};

// Replaces BitCast (transfer) nodes that return arrays with temporary variables
class BitCastArrayReplacer: public ASR::BaseExprReplacer<BitCastArrayReplacer> {
    public:

    Allocator& al;
    SymbolTable* current_scope;
    Vec<ASR::stmt_t*> bc_stmts;

    BitCastArrayReplacer(Allocator& al_):
        al(al_), current_scope(nullptr) {
        bc_stmts.n = 0;
    }

    void replace_BitCast(ASR::BitCast_t* x) {
        if( !ASRUtils::is_array(x->m_type) ) {
            return ;
        }
        // Create a temporary variable with the BitCast's result type
        const Location& loc = x->base.base.loc;
        std::string tmp_name = current_scope->get_unique_name(
            "__libasr_bitcast_tmp_");
        ASR::symbol_t* tmp_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(
                al, loc, current_scope, s2c(al, tmp_name), nullptr, 0,
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, x->m_type, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        current_scope->add_symbol(tmp_name, tmp_sym);
        ASR::expr_t* tmp_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, tmp_sym));

        // Create assignment: tmp = BitCast(...)
        ASR::expr_t* bitcast_expr = ASRUtils::EXPR((ASR::asr_t*)x);
        bc_stmts.push_back(al, ASRUtils::STMT(
            ASRUtils::make_Assignment_t_util(
                al, loc, tmp_var, bitcast_expr, nullptr, false, false)));

        // Replace the BitCast in the expression with the temporary
        *current_expr = tmp_var;
    }
};

class BitCastArrayVisitor: public ASR::CallReplacerOnExpressionsVisitor<BitCastArrayVisitor> {
    private:

    BitCastArrayReplacer replacer;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    BitCastArrayVisitor(Allocator& al_):
        replacer(al_) {}

    void set_scope(SymbolTable* scope) {
        replacer.current_scope = scope;
    }

    Vec<ASR::stmt_t*>& get_bc_stmts() {
        return replacer.bc_stmts;
    }

    void init_bc_stmts() {
        replacer.bc_stmts.n = 0;
        replacer.bc_stmts.reserve(replacer.al, 1);
    }
};

// Whether `components` is not empty and every component has the rank of
// `target`, so that the shape check of an assignment compares each of them
// with the target in every dimension.
static bool have_same_rank(ASR::expr_t* target, const Vec<ASR::expr_t*>& components) {
    int rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(target));
    if (components.size() == 0) {
        return false;
    }
    for (size_t i = 0; i < components.size(); i++) {
        if (ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(components[i])) != rank) {
            return false;
        }
    }
    return true;
}

// The extents of a section whose subscript triplets have compile-time
// start, end and stride, such as `a(:, i)` of an array `a` with constant
// bounds.
static bool get_compile_time_section_extents(ASR::ArraySection_t* section,
        std::vector<int64_t>& extents) {
    extents.clear();
    for (size_t i = 0; i < section->n_args; i++) {
        ASR::array_index_t& index = section->m_args[i];
        if (index.m_left == nullptr && index.m_step == nullptr) {
            continue;
        }
        int64_t start = 0, end = 0, step = 0;
        if (index.m_left == nullptr || index.m_right == nullptr ||
                index.m_step == nullptr ||
                !ASRUtils::extract_value(ASRUtils::expr_value(index.m_left), start) ||
                !ASRUtils::extract_value(ASRUtils::expr_value(index.m_right), end) ||
                !ASRUtils::extract_value(ASRUtils::expr_value(index.m_step), step) ||
                step == 0) {
            return false;
        }
        extents.push_back(std::max<int64_t>((end - start + step) / step, 0));
    }
    return true;
}

static bool get_compile_time_extents(ASR::expr_t* expr, std::vector<int64_t>& extents) {
    ASR::dimension_t* dims = nullptr;
    size_t rank = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(expr), dims);
    extents.clear();
    for (size_t i = 0; i < rank; i++) {
        int64_t extent = 0;
        if (dims[i].m_length == nullptr ||
                !ASRUtils::extract_value(dims[i].m_length, extent)) {
            if (ASR::is_a<ASR::ArraySection_t>(*expr)) {
                return get_compile_time_section_extents(
                    ASR::down_cast<ASR::ArraySection_t>(expr), extents);
            }
            return false;
        }
        extents.push_back(extent);
    }
    return true;
}

// Whether the extents of `target` and of all `components` are known at
// compile time and equal, so that a runtime shape check cannot fail.
static bool have_same_compile_time_shape(ASR::expr_t* target,
        const Vec<ASR::expr_t*>& components) {
    std::vector<int64_t> target_extents, extents;
    if (!get_compile_time_extents(target, target_extents)) {
        return false;
    }
    for (size_t i = 0; i < components.size(); i++) {
        if (!get_compile_time_extents(components[i], extents) ||
                extents != target_extents) {
            return false;
        }
    }
    return true;
}

class ArrayOpVisitor: public ASR::CallReplacerOnExpressionsVisitor<ArrayOpVisitor> {
    private:

    Allocator& al;
    ReplaceArrayOp replacer;
    Vec<ASR::stmt_t*> pass_result;
    Vec<ASR::stmt_t*>* parent_body;
    bool realloc_lhs;
    bool bounds_checking;
    bool remove_original_stmt;
    const LCompilers::PassOptions& pass_options;
    inline static std::set<const ASR::Assignment_t*> debug_inserted;

    public:

    int get_index_kind() const {
        return pass_options.descriptor_index_64 ? 8 : 4;
    }

    ASR::ttype_t* get_index_type(const Location& loc) {
        return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, get_index_kind()));
    }

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
    }

    ArrayOpVisitor(Allocator& al_, const LCompilers::PassOptions& pass_options_):
        al(al_), replacer(al, pass_result, remove_original_stmt),
        parent_body(nullptr), realloc_lhs(pass_options_.realloc_lhs_arrays),
        bounds_checking(pass_options_.bounds_checking),
        remove_original_stmt(false), pass_options(pass_options_) {
        pass_result.n = 0;
        pass_result.reserve(al, 0);
    }

    void visit_Template(const ASR::Template_t& /*x*/) {
        // Scalarize concrete specializations, not deferred-type definitions.
    }

    void visit_Variable(const ASR::Variable_t& /*x*/) {
        // Do nothing
    }

    void visit_FileWrite(const ASR::FileWrite_t& x) {
        /* 
        Handle FileWrite with character-array arguments, where x and a are arrays:
            write(x,"format") (a)
        Expand as following in ASR:
            do i=1,n:
                write(x(i),"format") (a(i))
            end do
        Also handles array sections on RHS, eg:
            write(x,"format") (a(k:m))
        And do-loops in write statements, eg:
            write(x,"format") (a(i),i=k,m)
        TODO: Generalize for N-Dimensional arrays, currently handles only 1-D access
        */
        if (!x.m_unit || !ASRUtils::is_character(*ASRUtils::expr_type(x.m_unit)) ||
            !ASRUtils::is_array(ASRUtils::expr_type(x.m_unit))) return;
        if (x.n_values == 0 || !ASR::is_a<ASR::StringFormat_t>(*x.m_values[0])) return;

        ASR::StringFormat_t* fmt = ASR::down_cast<ASR::StringFormat_t>(x.m_values[0]);
        if (fmt->n_args == 0) return;
        ASR::expr_t* val_arg = fmt->m_args[0];
        bool is_do_loop = ASR::is_a<ASR::ImpliedDoLoop_t>(*val_arg);

        if (!is_do_loop && !ASRUtils::is_array(ASRUtils::expr_type(val_arg))) return;

        // For formatted writes with plain array values whose format contains
        // a slash (/), skip the per-element loop transformation. The slash
        // means "advance to next record" (next array element in internal
        // write). Splitting into per-element writes breaks this because each
        // call only has one value, so the slash is never reached. The LLVM
        // codegen handles this correctly via is_string_array_unit and
        // _lfortran_string_write's newline-based record splitting.
        if (x.m_is_formatted && !is_do_loop && fmt->m_fmt &&
            ASR::is_a<ASR::StringConstant_t>(*fmt->m_fmt)) {
            ASR::StringConstant_t* fmt_str =
                ASR::down_cast<ASR::StringConstant_t>(fmt->m_fmt);
            if (strchr(fmt_str->m_s, '/') != nullptr) return;
        }

        const Location& loc = x.base.base.loc;
        int ikind = get_index_kind();
        ASR::ttype_t* int_type = get_index_type(loc);

        // Extract unit base and bounds
        ASR::expr_t* unit = x.m_unit;
        ASR::expr_t* lb_lhs = PassUtils::get_bound(x.m_unit, 1, "lbound", al, ikind);
        ASR::expr_t* ub_lhs = PassUtils::get_bound(x.m_unit, 1, "ubound", al, ikind);

        ASR::expr_t* lb_rhs = nullptr;
        if (is_do_loop){
            lb_rhs = ASR::down_cast<ASR::ImpliedDoLoop_t>(val_arg)->m_start;
        } else {
            lb_rhs = PassUtils::get_bound(val_arg, 1, "lbound", al, ikind);
        }
        // Loop variable: offset = 0 .. ub_lhs - lb_lhs
        Vec<ASR::expr_t*> idx_vars;
        idx_vars.reserve(al, 1);
        PassUtils::create_idx_vars(idx_vars, 1, loc, al, current_scope, "_fw", ikind);
        ASR::expr_t* offset = idx_vars[0];
        ASR::expr_t* zero = make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 0, ikind, loc);
        ASR::expr_t* loop_end = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
            al, loc, ub_lhs, ASR::binopType::Sub, lb_lhs, int_type, nullptr));

        // unit(lb_lhs + offset)
        ASR::expr_t* unit_i = PassUtils::create_array_ref(unit,
            ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, lb_lhs, ASR::binopType::Add, offset, int_type, nullptr)),
            al, current_scope);

        // Replace inline do-loop logic (since they are handled outside)
        ASR::expr_t* rhs_idx = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
            al, loc, lb_rhs, ASR::binopType::Add, offset, int_type, nullptr));
        ASR::expr_t* val_i = nullptr;  
        if (!is_do_loop) {
            val_i = PassUtils::create_array_ref(val_arg, rhs_idx, al, current_scope);
        } else { 
            ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(val_arg);
            ASRUtils::ExprStmtDuplicator dup(al);
            ASR::expr_t* body_copy = dup.duplicate_expr(idl->m_values[0]);
            struct ReplaceIDLVar : public ASR::BaseExprReplacer<ReplaceIDLVar> {
                ASR::symbol_t* idl_sym;
                ASR::expr_t* replacement;
                ReplaceIDLVar(ASR::symbol_t* s, ASR::expr_t* r) : idl_sym(s), replacement(r) {}
                void replace_Var(ASR::Var_t* v) {
                    if (v->m_v == idl_sym) *current_expr = replacement;
                }
            };
            ReplaceIDLVar replacer(ASR::down_cast<ASR::Var_t>(idl->m_var)->m_v, rhs_idx);
            replacer.current_expr = &body_copy;
            replacer.replace_expr(body_copy);
            val_i = body_copy;
        }

        // Rewrite StringFormat with scalar val_i, then inner FileWrite
        Vec<ASR::expr_t*> fmt_args; fmt_args.reserve(al, 1);
        fmt_args.push_back(al, val_i);
        ASR::expr_t* fmt_i = ASRUtils::EXPR(ASRUtils::make_StringFormat_t_util(
            al, loc, fmt->m_fmt, fmt_args.p, 1, fmt->m_kind,
            fmt->m_dtio_bindings, fmt->n_dtio_bindings, fmt->m_dtio_unit,
            fmt->m_type, fmt->m_value));
        Vec<ASR::expr_t*> inner_vals; inner_vals.reserve(al, 1);
        inner_vals.push_back(al, fmt_i);
        ASR::stmt_t* inner_write = ASRUtils::STMT(ASR::make_FileWrite_t(
            al, loc, x.m_label, unit_i,
            x.m_iomsg, x.m_iostat, x.m_id,
            inner_vals.p, 1,
            x.m_separator, x.m_end, x.m_overloaded,
            x.m_is_formatted, x.m_nml, x.m_rec, x.m_pos, x.m_asynchronous, x.m_decimal));

        // Wrap Scalar FileWrites in DoLoop
        Vec<ASR::stmt_t*> loop_body; loop_body.reserve(al, 1);
        loop_body.push_back(al, inner_write);
        ASR::do_loop_head_t head;
        head.m_v = offset; head.m_start = zero; head.m_end = loop_end;
        head.m_increment = nullptr; head.loc = loc;
        pass_result.push_back(al, ASRUtils::STMT(ASR::make_DoLoop_t(
            al, loc, nullptr, head, loop_body.p, 1, nullptr, 0)));
        remove_original_stmt = true;
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        bool remove_original_stmt_copy = remove_original_stmt;
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        if( parent_body ) {
            for (size_t j=0; j < pass_result.size(); j++) {
                parent_body->push_back(al, pass_result[j]);
            }
        }
        for (size_t i = 0; i < n_body; i++) {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
            remove_original_stmt = false;
            Vec<ASR::stmt_t*>* parent_body_copy = parent_body;
            parent_body = &body;
            visit_stmt(*m_body[i]);
            parent_body = parent_body_copy;
            if( pass_result.size() > 0 ) {
                for (size_t j=0; j < pass_result.size(); j++) {
                    body.push_back(al, pass_result[j]);
                }
            } else {
                if( !remove_original_stmt ) {
                    body.push_back(al, m_body[i]);
                    remove_original_stmt = false;
                }
            }
        }
        m_body = body.p;
        n_body = body.size();
        pass_result.n = 0;
        remove_original_stmt = remove_original_stmt_copy;
    }

    bool call_replace_on_expr(ASR::exprType expr_type) {
        switch( expr_type ) {
            case ASR::exprType::ArrayConstant:
            case ASR::exprType::ArrayConstructor: {
                return true;
            }
            default: {
                return false;
            }
        }
    }

    void increment_index_variables(std::unordered_map<size_t, Vec<ASR::expr_t*>>& var2indices,
                                   size_t var_with_maxrank, int64_t loop_depth,
                                   Vec<ASR::stmt_t*>& do_loop_body, const Location& loc,
                                   std::unordered_map<ASR::expr_t*, ASR::expr_t*>* index2step = nullptr) {
        ASR::expr_t* default_step = make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, get_index_kind(), loc);
        for( size_t i = 0; i < var2indices.size(); i++ ) {
            if( i == var_with_maxrank ) {
                continue;
            }
            // Skip variables with lower rank than the current loop depth
            if( loop_depth >= static_cast<int64_t>(var2indices[i].n) ) {
                continue;
            }
            ASR::expr_t* index_var = var2indices[i].p[loop_depth];
            if( index_var == nullptr ) {
                continue;
            }
            ASR::expr_t* step = default_step;
            if( index2step != nullptr ) {
                auto it = index2step->find(index_var);
                if( it != index2step->end() ) {
                    step = it->second;
                }
            }
            ASR::expr_t* plus_one = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, index_var,
                ASR::binopType::Add, step, ASRUtils::expr_type(index_var), nullptr));
            ASR::stmt_t* increment = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                al, loc, index_var, plus_one, nullptr, false, false));
            do_loop_body.push_back(al, increment);
        }
    }

    void set_index_variables(std::unordered_map<size_t, Vec<ASR::expr_t*>>& var2indices,
                             Vec<ASR::expr_t*>& vars_expr, size_t var_with_maxrank,
                             int64_t loop_depth, Vec<ASR::stmt_t*>& dest_vec, const Location& loc) {
        for( size_t i = 0; i < var2indices.size(); i++ ) {
            if( i == var_with_maxrank ) {
                continue;
            }
            // Skip variables with lower rank than the current loop depth
            if( loop_depth >= static_cast<int64_t>(var2indices[i].n) ) {
                continue;
            }
            ASR::expr_t* index_var = var2indices[i].p[loop_depth];
            if( index_var == nullptr ) {
                continue;
            }
            ASR::expr_t* lbound = nullptr;
            ASR::expr_t* ubound = nullptr;
            ASR::expr_t* step = nullptr;
            get_loop_bounds(vars_expr[i], loop_depth, lbound, ubound, step);
            ASR::stmt_t* set_index_var = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                al, loc, index_var, lbound, nullptr, false, false));
            dest_vec.push_back(al, set_index_var);
        }
    }

    ASR::expr_t* to_index_kind(ASR::expr_t* expr) {
        if( ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(expr)) ==
                get_index_kind() ) {
            return expr;
        }
        ASRUtils::ASRBuilder builder(al, expr->base.loc);
        return builder.i2i_t(expr, get_index_type(expr->base.loc));
    }

    static bool is_scalar_subscript(const ASR::array_index_t& index) {
        return index.m_left == nullptr && index.m_step == nullptr;
    }

    // The bounds and step that dimension `dim` (0-based) of the array `var`
    // is traversed with: those of its subscript triplet for a section, which
    // is indexed through its base array, and its own bounds otherwise.
    // `step` is nullptr for a unit step.
    void get_loop_bounds(ASR::expr_t* var, size_t dim, ASR::expr_t*& start,
            ASR::expr_t*& end, ASR::expr_t*& step) {
        step = nullptr;
        if( !ASR::is_a<ASR::ArraySection_t>(*var) ) {
            start = PassUtils::get_bound(var, dim + 1, "lbound", al, get_index_kind());
            end = PassUtils::get_bound(var, dim + 1, "ubound", al, get_index_kind());
            return ;
        }
        ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(var);
        size_t arg = 0;
        for( size_t rank_dim = 0; arg < section->n_args; arg++ ) {
            if( !is_scalar_subscript(section->m_args[arg]) ) {
                if( rank_dim == dim ) {
                    break;
                }
                rank_dim++;
            }
        }
        LCOMPILERS_ASSERT(arg < section->n_args);
        ASR::array_index_t& triplet = section->m_args[arg];
        ASRUtils::ExprStmtDuplicator duplicator(al);
        start = triplet.m_left ? to_index_kind(duplicator.duplicate_expr(triplet.m_left))
            : PassUtils::get_bound(section->m_v, arg + 1, "lbound", al, get_index_kind());
        end = triplet.m_right ? to_index_kind(duplicator.duplicate_expr(triplet.m_right))
            : PassUtils::get_bound(section->m_v, arg + 1, "ubound", al, get_index_kind());
        int64_t step_value = 0;
        if( triplet.m_step != nullptr && !(ASRUtils::extract_value(
                ASRUtils::expr_value(triplet.m_step), step_value) && step_value == 1) ) {
            step = to_index_kind(duplicator.duplicate_expr(triplet.m_step));
        }
    }

    // Whether `subscript` of dimension `dim` (0-based) of the array `v` is
    // known not to lie beyond its bound `bound` of that dimension: below the
    // lower bound for `LBound`, above the upper one for `UBound`.
    static bool is_subscript_within_bound(ASR::expr_t* v, size_t dim,
            ASR::expr_t* subscript, ASR::arrayboundType bound) {
        if( ASR::is_a<ASR::ArrayBound_t>(*subscript) ) {
            ASR::ArrayBound_t* array_bound = ASR::down_cast<ASR::ArrayBound_t>(subscript);
            int64_t bound_dim = 0;
            if( array_bound->m_bound == bound && array_bound->m_dim != nullptr &&
                ASR::is_a<ASR::Var_t>(*array_bound->m_v) && ASR::is_a<ASR::Var_t>(*v) &&
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(array_bound->m_v)->m_v) ==
                ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(v)->m_v) &&
                ASRUtils::extract_value(ASRUtils::expr_value(array_bound->m_dim), bound_dim) &&
                bound_dim == static_cast<int64_t>(dim) + 1 ) {
                return true;
            }
        }
        ASR::dimension_t* dims = nullptr;
        size_t rank = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(v), dims);
        int64_t value = 0, start = 0, length = 0;
        if( dim >= rank || dims[dim].m_start == nullptr || dims[dim].m_length == nullptr ||
            !ASRUtils::extract_value(ASRUtils::expr_value(subscript), value) ||
            !ASRUtils::extract_value(ASRUtils::expr_value(dims[dim].m_start), start) ||
            !ASRUtils::extract_value(ASRUtils::expr_value(dims[dim].m_length), length) ) {
            return false;
        }
        return bound == ASR::arrayboundType::LBound ? value >= start
            : value <= start + length - 1;
    }

    // Whether evaluating `subscript` once more has no effect: a constant, a
    // variable or a bound of an array variable.
    static bool can_reevaluate_subscript(ASR::expr_t* subscript) {
        return ASRUtils::is_value_constant(ASRUtils::expr_value(subscript)) ||
            ASR::is_a<ASR::Var_t>(*subscript) ||
            (ASR::is_a<ASR::ArrayBound_t>(*subscript) && ASR::is_a<ASR::Var_t>(
                *ASR::down_cast<ASR::ArrayBound_t>(subscript)->m_v));
    }

    // Whether every element of the base array that the loops over the
    // triplets of the section target `section` reference is within its
    // bounds. The subscripts that the loop over a triplet runs through lie
    // between its start and its end, so it suffices that its end is known
    // to be within the bounds and that the subscripts of the first element
    // referenced, its starts and the scalar subscripts, are. With bounds
    // checking, those not known at compile time are checked once, before
    // the loops, by referencing that element if the section is not empty
    // and its elements are of intrinsic numeric or logical type.
    bool is_section_traversal_in_bounds(ASR::ArraySection_t* section,
            const Location& loc) {
        ASRUtils::ASRBuilder b(al, loc);
        bool check_first_element = false;
        ASR::expr_t* is_not_empty = nullptr;
        for( size_t i = 0; i < section->n_args; i++ ) {
            ASR::array_index_t& index = section->m_args[i];
            if( is_scalar_subscript(index) ) {
                if( index.m_right == nullptr ) {
                    return false;
                }
                check_first_element = check_first_element ||
                    !is_subscript_within_bound(section->m_v, i, index.m_right,
                        ASR::arrayboundType::LBound) ||
                    !is_subscript_within_bound(section->m_v, i, index.m_right,
                        ASR::arrayboundType::UBound);
                continue;
            }
            int64_t step = 0;
            if( index.m_left == nullptr || index.m_right == nullptr ||
                index.m_step == nullptr ||
                !ASRUtils::extract_value(ASRUtils::expr_value(index.m_step), step) ||
                step == 0 ) {
                return false;
            }
            ASR::arrayboundType start_bound = step > 0 ?
                ASR::arrayboundType::LBound : ASR::arrayboundType::UBound;
            ASR::arrayboundType end_bound = step > 0 ?
                ASR::arrayboundType::UBound : ASR::arrayboundType::LBound;
            if( !is_subscript_within_bound(section->m_v, i, index.m_right, end_bound) ) {
                return false;
            }
            check_first_element = check_first_element ||
                !is_subscript_within_bound(section->m_v, i, index.m_left, start_bound);
            int64_t start = 0, end = 0;
            if( ASRUtils::extract_value(ASRUtils::expr_value(index.m_left), start) &&
                ASRUtils::extract_value(ASRUtils::expr_value(index.m_right), end) ) {
                if( step > 0 ? start > end : start < end ) {
                    // The section is empty, no element is referenced
                    return true;
                }
                continue;
            }
            ASRUtils::ExprStmtDuplicator duplicator(al);
            ASR::expr_t* start_expr = duplicator.duplicate_expr(index.m_left);
            ASR::expr_t* end_expr = duplicator.duplicate_expr(index.m_right);
            ASR::expr_t* is_dim_not_empty = step > 0 ? b.LtE(start_expr, end_expr)
                : b.GtE(start_expr, end_expr);
            is_not_empty = is_not_empty ? b.And(is_not_empty, is_dim_not_empty)
                : is_dim_not_empty;
        }
        if( !check_first_element ) {
            return true;
        }
        ASR::ttype_t* element_type = ASRUtils::extract_type(section->m_type);
        if( !bounds_checking || !(ASR::is_a<ASR::Integer_t>(*element_type) ||
                ASR::is_a<ASR::UnsignedInteger_t>(*element_type) ||
                ASR::is_a<ASR::Real_t>(*element_type) ||
                ASR::is_a<ASR::Complex_t>(*element_type) ||
                ASR::is_a<ASR::Logical_t>(*element_type)) ) {
            return false;
        }
        for( size_t i = 0; i < section->n_args; i++ ) {
            ASR::array_index_t& index = section->m_args[i];
            if( !can_reevaluate_subscript(index.m_right) ||
                (index.m_left != nullptr && !can_reevaluate_subscript(index.m_left)) ) {
                return false;
            }
        }
        ASRUtils::ExprStmtDuplicator duplicator(al);
        Vec<ASR::array_index_t> args;
        args.reserve(al, section->n_args);
        for( size_t i = 0; i < section->n_args; i++ ) {
            ASR::array_index_t arg;
            arg.loc = loc;
            arg.m_left = nullptr;
            arg.m_step = nullptr;
            arg.m_right = duplicator.duplicate_expr(is_scalar_subscript(section->m_args[i]) ?
                section->m_args[i].m_right : section->m_args[i].m_left);
            args.push_back(al, arg);
        }
        ASR::expr_t* first_element = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al,
            loc, section->m_v, args.p, args.size(), element_type,
            ASR::arraystorageType::ColMajor, nullptr, false));
        std::string name = current_scope->get_unique_name("__libasr_section_first_element_");
        ASR::symbol_t* temp = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
            al, loc, current_scope, s2c(al, name), nullptr, 0, ASR::intentType::Local,
            nullptr, nullptr, ASR::storage_typeType::Default,
            ASRUtils::duplicate_type(al, element_type), nullptr, ASR::abiType::Source,
            ASR::accessType::Public, ASR::presenceType::Required, false));
        current_scope->add_symbol(name, temp);
        ASR::stmt_t* check = b.Assignment(ASRUtils::EXPR(ASR::make_Var_t(al, loc, temp)),
            first_element);
        if( is_not_empty != nullptr ) {
            check = b.If(is_not_empty, {check}, {});
        }
        pass_result.push_back(al, check);
        return true;
    }

    // The element of the base array of `section` that the loop indices
    // `indices` select, one index for each subscript triplet.
    ASR::expr_t* index_section_through_base(ASR::ArraySection_t* section,
            Vec<ASR::expr_t*>& indices, const Location& loc, bool in_bounds) {
        Vec<ASR::array_index_t> args;
        args.reserve(al, section->n_args);
        size_t k = 0;
        for( size_t i = 0; i < section->n_args; i++ ) {
            ASR::array_index_t arg;
            arg.loc = loc;
            arg.m_left = nullptr;
            arg.m_step = nullptr;
            arg.m_right = is_scalar_subscript(section->m_args[i]) ?
                section->m_args[i].m_right : indices[k++];
            args.push_back(al, arg);
        }
        return ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
            section->m_v, args.p, args.size(),
            ASRUtils::extract_type(section->m_type),
            ASR::arraystorageType::ColMajor, nullptr, in_bounds));
    }

    enum IndexType {
        ScalarIndex, ArrayIndex
    };

    void set_index_variables(std::unordered_map<size_t, Vec<ASR::expr_t*>>& var2indices,
                             std::unordered_map<ASR::expr_t*, std::pair<ASR::expr_t*, IndexType>>& index2var,
                             size_t var_with_maxrank, int64_t loop_depth,
                             Vec<ASR::stmt_t*>& dest_vec, const Location& loc) {
        for( size_t i = 0; i < var2indices.size(); i++ ) {
            if( i == var_with_maxrank ) {
                continue;
            }
            // Skip variables with lower rank than the current loop depth
            if( loop_depth >= static_cast<int64_t>(var2indices[i].n) ) {
                continue;
            }
            ASR::expr_t* index_var = var2indices[i].p[loop_depth];
            if( index_var == nullptr ) {
                continue;
            }
            size_t bound_dim = loop_depth + 1;
            if( index2var[index_var].second == IndexType::ArrayIndex ) {
                bound_dim = 1;
            }
            ASR::expr_t* lbound;
            if( !ASRUtils::is_array(ASRUtils::expr_type(index2var[index_var].first)) ){
                lbound = index2var[index_var].first;
            } else {
                lbound = PassUtils::get_bound(
                    index2var[index_var].first, bound_dim, "lbound", al, get_index_kind());
            }
            ASR::stmt_t* set_index_var = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                al, loc, index_var, lbound, nullptr, false, false));
            dest_vec.push_back(al, set_index_var);
        }
    }

    inline void fill_array_indices_in_vars_expr(
        ASR::expr_t* expr, bool is_expr_array,
        Vec<ASR::expr_t*>& vars_expr,
        size_t& offset_for_array_indices) {
        if( is_expr_array ) {
            ASR::array_index_t* m_args = nullptr; size_t n_args = 0;
            ASRUtils::extract_indices(expr, m_args, n_args);
            for( size_t i = 0; i < n_args; i++ ) {
                if( m_args[i].m_left == nullptr &&
                    m_args[i].m_right != nullptr &&
                    m_args[i].m_step == nullptr ) {
                    if( ASRUtils::is_array(ASRUtils::expr_type(
                            m_args[i].m_right)) ) {
                        vars_expr.push_back(al, m_args[i].m_right);
                    }
                }
            }
            offset_for_array_indices++;
        }
    }

    inline void create_array_item_array_indexed_expr(
            ASR::expr_t* expr, ASR::expr_t** expr_address,
            bool is_expr_array, int var2indices_key,
            size_t var_rank, const Location& loc,
            std::unordered_map<size_t, Vec<ASR::expr_t*>>& var2indices,
            size_t& j) {
        if( is_expr_array ) {
            ASR::array_index_t* m_args = nullptr; size_t n_args = 0;
            Vec<ASR::array_index_t> array_item_args;
            array_item_args.reserve(al, n_args);
            ASRUtils::extract_indices(expr, m_args, n_args);
            ASRUtils::ExprStmtDuplicator expr_duplicator(al);
            Vec<ASR::expr_t*> new_indices; new_indices.reserve(al, n_args);
            int k = 0;
            for( size_t i = 0; i < (n_args == 0 ? var_rank : n_args); i++ ) {
                if( m_args && m_args[i].m_left == nullptr &&
                    m_args[i].m_right != nullptr &&
                    m_args[i].m_step == nullptr ) {
                    if( ASRUtils::is_array(ASRUtils::expr_type(
                            m_args[i].m_right)) ) {
                        ASR::array_index_t array_index;
                        array_index.loc = loc;
                        array_index.m_left = nullptr;
                        Vec<ASR::array_index_t> indices1; indices1.reserve(al, 1);
                        ASR::array_index_t index1; index1.loc = loc; index1.m_left = nullptr;
                        index1.m_right = var2indices[j][0]; index1.m_step = nullptr;
                        new_indices.push_back(al, index1.m_right);
                        indices1.push_back(al, index1);
                        ASR::ttype_t* nested_index_type = ASRUtils::extract_type(
                            ASRUtils::expr_type(m_args[i].m_right));
                        array_index.m_right = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                            m_args[i].m_right, indices1.p, 1, nested_index_type,
                            ASR::arraystorageType::ColMajor, nullptr));
                        array_index.m_step = nullptr;
                        array_item_args.push_back(al, array_index);
                        j++;
                        k++;
                    } else {
                        array_item_args.push_back(al, m_args[i]);
                    }
                } else {
                    ASR::array_index_t index1; index1.loc = loc; index1.m_left = nullptr;
                    index1.m_right = var2indices[var2indices_key][k]; index1.m_step = nullptr;
                    array_item_args.push_back(al, index1);
                    new_indices.push_back(al, var2indices[var2indices_key][k]);
                    k++;
                }
            }
            var2indices[var2indices_key] = new_indices;
            ASR::ttype_t* expr_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(expr));
            *expr_address = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(
                al, loc, ASRUtils::extract_array_variable(expr), array_item_args.p,
                array_item_args.size(), expr_type, ASR::arraystorageType::ColMajor, nullptr));
        }
    }

    template <typename T>
    void generate_loop_for_array_indexed_with_array_indices(const T& x,
        ASR::expr_t** target_address, ASR::expr_t** value_address,
        const Location& loc) {
        ASR::expr_t* target = *target_address;
        ASR::expr_t* value = *value_address;
        size_t var_rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(target));
        Vec<ASR::expr_t*> vars_expr; vars_expr.reserve(al, 2);
        bool is_target_array = ASRUtils::is_array(ASRUtils::expr_type(target));
        bool is_value_array = ASRUtils::is_array(ASRUtils::expr_type(value));
        Vec<ASR::array_index_t> array_indices_args; array_indices_args.reserve(al, 1);
        Vec<ASR::array_index_t> rhs_array_indices_args; rhs_array_indices_args.reserve(al, 1);
        int n_array_indices_args = -1;
        int temp_n = -1;
        size_t do_loop_depth = 0;
        if( is_target_array ) {
            vars_expr.push_back(al, ASRUtils::extract_array_variable(target));
            ASRUtils::extract_array_indices(target, al, array_indices_args, n_array_indices_args);
        }
        if( is_value_array ) {
            vars_expr.push_back(al, ASRUtils::extract_array_variable(value));
            ASRUtils::extract_array_indices(value, al, rhs_array_indices_args, temp_n);
        }

        size_t offset_for_array_indices = 0;

        fill_array_indices_in_vars_expr(
            target, is_target_array,
            vars_expr, offset_for_array_indices);
        fill_array_indices_in_vars_expr(
            value, is_value_array,
            vars_expr, offset_for_array_indices);

        // Common code for target and value
        std::unordered_map<size_t, Vec<ASR::expr_t*>> var2indices;
        std::unordered_map<ASR::expr_t*, std::pair<ASR::expr_t*, IndexType>> index2var;
        std::unordered_map<ASR::expr_t*, ASR::expr_t*> index2step;
        ASR::ttype_t* index_type = get_index_type(loc);
        for( size_t i = 0; i < vars_expr.size(); i++ ) {
            Vec<ASR::expr_t*> indices;
            indices.reserve(al, var_rank);
            for( size_t j = 0; j < (i >= offset_for_array_indices ? 1 : var_rank); j++ ) {
                std::string index_var_name = current_scope->get_unique_name(
                    "__libasr_index_" + std::to_string(j) + "_");
                ASR::symbol_t* index = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
                    al, loc, current_scope, s2c(al, index_var_name), nullptr, 0, ASR::intentType::Local,
                    nullptr, nullptr, ASR::storage_typeType::Default, index_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required, false));
                current_scope->add_symbol(index_var_name, index);
                ASR::expr_t* index_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, index));
                if ((i == offset_for_array_indices - 1) && is_value_array && j < rhs_array_indices_args.size() &&
                        rhs_array_indices_args[j].m_left != nullptr) {
                    index2var[index_expr] = std::make_pair(rhs_array_indices_args[j].m_left, IndexType::ScalarIndex);
                    if (rhs_array_indices_args[j].m_step != nullptr) {
                        index2step[index_expr] = rhs_array_indices_args[j].m_step;
                    }
                } else {
                    index2var[index_expr] = std::make_pair(vars_expr[i],
                        i >= offset_for_array_indices ? IndexType::ArrayIndex : IndexType::ScalarIndex);
                }
                indices.push_back(al, index_expr);
            }
            var2indices[i] = indices;
        }

        size_t j = offset_for_array_indices;

        create_array_item_array_indexed_expr(
            target, target_address, is_target_array, 0,
            var_rank, loc, var2indices, j);
        create_array_item_array_indexed_expr(
            value, value_address, is_value_array, 1,
            var_rank, loc, var2indices, j);

        size_t vars_expr_size = vars_expr.size();
        for( size_t i = offset_for_array_indices; i < vars_expr_size; i++ ) {
            var2indices.erase(i);
        }
        vars_expr.n = offset_for_array_indices;

        size_t var_with_maxrank = 0;

        ASR::do_loop_head_t do_loop_head;
        do_loop_head.loc = loc;
        do_loop_head.m_v = var2indices[var_with_maxrank].p[0];
        size_t bound_dim = do_loop_depth + 1;
        if( index2var[do_loop_head.m_v].second == IndexType::ArrayIndex ) {
            bound_dim = 1;
        }
        int64_t array_indices_args_i = 0;
        if( n_array_indices_args > -1 && array_indices_args[array_indices_args_i].m_right != nullptr &&
                array_indices_args[array_indices_args_i].m_left != nullptr &&
                array_indices_args[array_indices_args_i].m_step != nullptr) {
            do_loop_head.m_start = array_indices_args[array_indices_args_i].m_left;
            do_loop_head.m_end = array_indices_args[array_indices_args_i].m_right;
            do_loop_head.m_increment = array_indices_args[array_indices_args_i].m_step;
        } else {
            do_loop_head.m_start = PassUtils::get_bound(
                index2var[do_loop_head.m_v].first, bound_dim, "lbound", al, get_index_kind());
            do_loop_head.m_end = PassUtils::get_bound(
                index2var[do_loop_head.m_v].first, bound_dim, "ubound", al, get_index_kind());
            do_loop_head.m_increment = nullptr;
        }
        Vec<ASR::stmt_t*> parent_do_loop_body; parent_do_loop_body.reserve(al, 1);
        Vec<ASR::stmt_t*> do_loop_body; do_loop_body.reserve(al, 1);
        set_index_variables(var2indices, index2var, var_with_maxrank,
                            0, parent_do_loop_body, loc);
        do_loop_body.push_back(al, const_cast<ASR::stmt_t*>(&(x.base)));
        increment_index_variables(var2indices, var_with_maxrank, 0,
                                  do_loop_body, loc, &index2step);
        ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
            do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
        do_loop_depth += 1;
        array_indices_args_i += 1;
        parent_do_loop_body.push_back(al, do_loop);
        do_loop_body.from_pointer_n_copy(al, parent_do_loop_body.p, parent_do_loop_body.size());
        parent_do_loop_body.reserve(al, 1);

        for( int64_t i = 1; i < static_cast<int64_t>(var_rank); i++ ) {
            set_index_variables(var2indices, index2var, var_with_maxrank,
                                i, parent_do_loop_body, loc);
            increment_index_variables(var2indices, var_with_maxrank, i,
                                      do_loop_body, loc, &index2step);
            ASR::do_loop_head_t do_loop_head;
            do_loop_head.loc = loc;
            do_loop_head.m_v = var2indices[var_with_maxrank].p[i];
            bound_dim = do_loop_depth + 1;
            if( index2var[do_loop_head.m_v].second == IndexType::ArrayIndex ) {
                bound_dim = 1;
            }
            if( n_array_indices_args > -1 && array_indices_args[array_indices_args_i].m_right != nullptr &&
                    array_indices_args[array_indices_args_i].m_left != nullptr &&
                    array_indices_args[array_indices_args_i].m_step != nullptr) {
                do_loop_head.m_start = array_indices_args[array_indices_args_i].m_left;
                do_loop_head.m_end = array_indices_args[array_indices_args_i].m_right;
                do_loop_head.m_increment = array_indices_args[array_indices_args_i].m_step;
            } else {
                do_loop_head.m_start = PassUtils::get_bound(
                    index2var[do_loop_head.m_v].first, bound_dim, "lbound", al, get_index_kind());
                do_loop_head.m_end = PassUtils::get_bound(
                    index2var[do_loop_head.m_v].first, bound_dim, "ubound", al, get_index_kind());
                do_loop_head.m_increment = nullptr;
            }
            ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
            do_loop_depth++;
            array_indices_args_i += 1;
            parent_do_loop_body.push_back(al, do_loop);
            do_loop_body.from_pointer_n_copy(al, parent_do_loop_body.p, parent_do_loop_body.size());
            parent_do_loop_body.reserve(al, 1);
        }

        for( size_t i = 0; i < do_loop_body.size(); i++ ) {
            pass_result.push_back(al, do_loop_body[i]);
        }
    }

    template <typename T>
    void generate_loop(const T& x, Vec<ASR::expr_t**>& vars,
                       Vec<ASR::expr_t**>& fix_types_args,
                       const Location& loc,
                       const std::vector<ASR::expr_t*>* scalar_targets = nullptr,
                       const std::set<ASR::expr_t*>* conforming_operands = nullptr) {
        Vec<size_t> var_ranks;
        Vec<ASR::expr_t*> vars_expr;
        var_ranks.reserve(al, vars.size()); vars_expr.reserve(al, vars.size());
        for( size_t i = 0; i < vars.size(); i++ ) {
            ASR::expr_t* expr = *vars[i];
            ASR::ttype_t* type = ASRUtils::expr_type(expr);
            var_ranks.push_back(al, ASRUtils::extract_n_dims_from_ttype(type));
            vars_expr.push_back(al, expr);
        }

        std::unordered_map<size_t, Vec<ASR::expr_t*>> var2indices;
        ASR::ttype_t* index_type = get_index_type(loc);
        for( size_t i = 0; i < vars.size(); i++ ) {
            Vec<ASR::expr_t*> indices;
            indices.reserve(al, var_ranks[i]);
            for( size_t j = 0; j < var_ranks[i]; j++ ) {
                std::string index_var_name = current_scope->get_unique_name(
                    "__libasr_index_" + std::to_string(j) + "_");
                ASR::symbol_t* index = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
                    al, loc, current_scope, s2c(al, index_var_name), nullptr, 0, ASR::intentType::Local,
                    nullptr, nullptr, ASR::storage_typeType::Default, index_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required, false));
                current_scope->add_symbol(index_var_name, index);
                ASR::expr_t* index_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, index));
                indices.push_back(al, index_expr);
            }
            var2indices[i] = indices;
        }

        size_t var_with_maxrank = 0;
        for( size_t i = 0; i < var_ranks.size(); i++ ) {
            if( var_ranks[i] > var_ranks[var_with_maxrank] ) {
                var_with_maxrank = i;
            }
        }
        // The loops run over the bounds of the operand with the maximum
        // rank, so its elements are always in bounds. The index of every
        // other operand starts at its lower bound and advances with the
        // loops, so its elements are in bounds if its shape is known to
        // conform with that operand.
        auto is_conforming = [&](size_t i) {
            return conforming_operands != nullptr &&
                var_ranks[i] == var_ranks[var_with_maxrank] &&
                conforming_operands->count(*vars[i]) > 0 &&
                conforming_operands->count(*vars[var_with_maxrank]) > 0;
        };
        std::vector<bool> in_bounds(vars.size(), false);
        for( size_t i = 0; i < vars.size(); i++ ) {
            ASR::expr_t* var = *vars[i];
            in_bounds[i] = (ASR::is_a<ASR::Var_t>(*var) ||
                    ASR::is_a<ASR::StructInstanceMember_t>(*var) ||
                    ASR::is_a<ASR::ArrayConstant_t>(*var)) &&
                (i == var_with_maxrank || is_conforming(i));
        }

        // The index of a section, which runs over the elements of its base
        // array the section selects, advances by the stride of the section.
        std::unordered_map<ASR::expr_t*, ASR::expr_t*> index2step;
        for( size_t i = 0; i < vars.size(); i++ ) {
            if (var_ranks[i] == 0) continue;
            if (ASR::is_a<ASR::ArraySection_t>(*vars_expr[i])) {
                for( size_t j = 0; j < var_ranks[i]; j++ ) {
                    ASR::expr_t *start = nullptr, *end = nullptr, *step = nullptr;
                    get_loop_bounds(vars_expr[i], j, start, end, step);
                    if (step != nullptr) {
                        index2step[var2indices[i][j]] = step;
                    }
                }
                ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(
                    vars_expr[i]);
                // The loops run over the triplets of the section only if it
                // drives them
                bool section_in_bounds = i == var_with_maxrank &&
                    is_section_traversal_in_bounds(section, loc);
                *vars[i] = index_section_through_base(section, var2indices[i], loc,
                    section_in_bounds);
                continue;
            }
            Vec<ASR::array_index_t> indices;
            indices.reserve(al, var_ranks[i]);
            for( size_t j = 0; j < var_ranks[i]; j++ ) {
                ASR::array_index_t array_index;
                array_index.loc = loc;
                array_index.m_left = nullptr;
                array_index.m_right = get_singleton_safe_index(*vars[i],
                    var2indices[i][j], j + 1, loc, scalar_targets);
                array_index.m_step = nullptr;
                if( array_index.m_right != var2indices[i][j] ) {
                    in_bounds[i] = false;
                }
                indices.push_back(al, array_index);
            }
            ASR::ttype_t* var_i_type = ASRUtils::extract_type(
                ASRUtils::expr_type(*vars[i]));
            *vars[i] = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc, *vars[i], indices.p,
                indices.size(), var_i_type, ASR::arraystorageType::ColMajor, nullptr,
                in_bounds[i]));
        }

        ASRUtils::RemoveArrayProcessingNodeVisitor array_broadcast_visitor(al);
        for( size_t i = 0; i < fix_types_args.size(); i++ ) {
            array_broadcast_visitor.current_expr = fix_types_args[i];
            array_broadcast_visitor.call_replacer();
        }

        CleanupDegenerateArraySection cleanup(al);
        for( size_t i = 0; i < fix_types_args.size(); i++ ) {
            cleanup.current_expr = fix_types_args[i];
            cleanup.replace_expr(*fix_types_args[i]);
        }

        FixTypeVisitor fix_types(al);
        fix_types.current_scope = current_scope;
        for( size_t i = 0; i < fix_types_args.size(); i++ ) {
            fix_types.visit_expr(*(*fix_types_args[i]));
        }

        ASR::do_loop_head_t do_loop_head;
        do_loop_head.loc = loc;
        do_loop_head.m_v = var2indices[var_with_maxrank].p[0];
        get_loop_bounds(vars_expr[var_with_maxrank], 0, do_loop_head.m_start,
            do_loop_head.m_end, do_loop_head.m_increment);
        Vec<ASR::stmt_t*> parent_do_loop_body; parent_do_loop_body.reserve(al, 1);
        Vec<ASR::stmt_t*> do_loop_body; do_loop_body.reserve(al, 1);
        set_index_variables(var2indices, vars_expr, var_with_maxrank,
                            0, parent_do_loop_body, loc);
        do_loop_body.push_back(al, const_cast<ASR::stmt_t*>(&(x.base)));
        increment_index_variables(var2indices, var_with_maxrank, 0,
                                  do_loop_body, loc, &index2step);
        ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
            do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
        parent_do_loop_body.push_back(al, do_loop);
        do_loop_body.from_pointer_n_copy(al, parent_do_loop_body.p, parent_do_loop_body.size());
        parent_do_loop_body.reserve(al, 1);

        for( int64_t i = 1; i < static_cast<int64_t>(var_ranks[var_with_maxrank]); i++ ) {
            set_index_variables(var2indices, vars_expr, var_with_maxrank,
                                i, parent_do_loop_body, loc);
            increment_index_variables(var2indices, var_with_maxrank, i,
                                      do_loop_body, loc, &index2step);
            ASR::do_loop_head_t do_loop_head;
            do_loop_head.loc = loc;
            do_loop_head.m_v = var2indices[var_with_maxrank].p[i];
            get_loop_bounds(vars_expr[var_with_maxrank], i, do_loop_head.m_start,
                do_loop_head.m_end, do_loop_head.m_increment);
            ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
            parent_do_loop_body.push_back(al, do_loop);
            do_loop_body.from_pointer_n_copy(al, parent_do_loop_body.p, parent_do_loop_body.size());
            parent_do_loop_body.reserve(al, 1);
        }

        for( size_t i = 0; i < do_loop_body.size(); i++ ) {
            pass_result.push_back(al, do_loop_body[i]);
        }
    }

    ASR::expr_t* compute_indexed_dim_size(const ASR::array_index_t& idx,
            const Location& loc, ASRUtils::ASRBuilder& builder,
            ASR::ttype_t* idx_type) {
        if (idx.m_left == nullptr && idx.m_step == nullptr &&
                idx.m_right != nullptr &&
                ASRUtils::is_array(ASRUtils::expr_type(idx.m_right))) {
            ASR::expr_t* dim_one = builder.i_t(1, idx_type);
            return ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
                idx.m_right, dim_one, idx_type, nullptr));
        }
        if (idx.m_left != nullptr && idx.m_right != nullptr) {
            ASRUtils::ExprStmtDuplicator d(al);
            ASR::expr_t* lo = d.duplicate_expr(idx.m_left);
            ASR::expr_t* hi = d.duplicate_expr(idx.m_right);
            ASR::expr_t* step = idx.m_step
                ? d.duplicate_expr(idx.m_step)
                : builder.i_t(1, idx_type);
            ASR::expr_t* diff = builder.Sub(hi, lo);
            ASR::expr_t* num = builder.Add(builder.Div(diff, step),
                builder.i_t(1, idx_type));
            return num;
        }
        return nullptr;
    }

    bool insert_realloc_for_indexed_value(ASR::expr_t* target,
            ASR::expr_t* value, const Location& loc) {
        ASR::array_index_t* args = nullptr;
        size_t n_args = 0;
        if (ASR::is_a<ASR::ArrayItem_t>(*value)) {
            ASR::ArrayItem_t* ai = ASR::down_cast<ASR::ArrayItem_t>(value);
            args = ai->m_args; n_args = ai->n_args;
        } else if (ASR::is_a<ASR::ArraySection_t>(*value)) {
            ASR::ArraySection_t* as = ASR::down_cast<ASR::ArraySection_t>(value);
            args = as->m_args; n_args = as->n_args;
        } else {
            return false;
        }
        ASRUtils::ASRBuilder builder(al, loc);
        ASR::ttype_t* idx_type = get_index_type(loc);
        Vec<ASR::dimension_t> realloc_dims;
        realloc_dims.reserve(al, n_args);
        for (size_t i = 0; i < n_args; i++) {
            const ASR::array_index_t& idx = args[i];
            bool is_scalar_index = (idx.m_left == nullptr &&
                idx.m_step == nullptr && idx.m_right != nullptr &&
                !ASRUtils::is_array(ASRUtils::expr_type(idx.m_right)));
            if (is_scalar_index) continue;
            ASR::expr_t* dim_size = compute_indexed_dim_size(idx, loc,
                builder, idx_type);
            if (dim_size == nullptr) return false;
            ASR::dimension_t d;
            d.loc = loc;
            d.m_start = builder.i_t(1, idx_type);
            d.m_length = dim_size;
            realloc_dims.push_back(al, d);
        }
        size_t target_rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::expr_type(target));
        if (realloc_dims.size() != target_rank) return false;

        ASR::expr_t* realloc_str_len = nullptr;
        ASR::ttype_t* target_type = ASRUtils::expr_type(target);
        if (ASRUtils::is_character(*target_type)) {
            ASR::String_t* target_str = ASR::down_cast<ASR::String_t>(
                ASRUtils::extract_type(target_type));
            ASRUtils::ExprStmtDuplicator d(al);
            if (target_str->m_len_kind != ASR::string_length_kindType::DeferredLength) {
                int64_t target_len {};
                if (target_str->m_len == nullptr) {
                    // An assumed-length target retains its runtime element length.
                    realloc_str_len = ASRUtils::EXPR(ASR::make_StringLen_t(
                        al, loc, d.duplicate_expr(target), int32, nullptr));
                } else if (ASRUtils::is_value_constant(target_str->m_len, target_len)) {
                    realloc_str_len = builder.i32(target_len);
                } else {
                    realloc_str_len = d.duplicate_expr(target_str->m_len);
                }
            } else {
                ASR::expr_t* len_value = nullptr;
                int64_t len {};
                ASR::String_t* val_str = ASR::down_cast<ASR::String_t>(
                    ASRUtils::extract_type(ASRUtils::expr_type(value)));
                if (val_str->m_len != nullptr &&
                        ASRUtils::is_value_constant(val_str->m_len, len)) {
                    len_value = builder.i32(len);
                }
                ASR::expr_t* len_src = nullptr;
                if (ASR::is_a<ASR::ArrayItem_t>(*value)) {
                    len_src = ASR::down_cast<ASR::ArrayItem_t>(value)->m_v;
                } else if (ASR::is_a<ASR::ArraySection_t>(*value)) {
                    len_src = ASR::down_cast<ASR::ArraySection_t>(value)->m_v;
                }
                if (len_src == nullptr) {
                    return false;
                }
                realloc_str_len = ASRUtils::EXPR(ASR::make_StringLen_t(
                    al, loc, d.duplicate_expr(len_src), int32, len_value));
            }
        }

        ASR::alloc_arg_t aa;
        aa.loc = loc;
        aa.m_a = target;
        aa.m_dims = realloc_dims.p;
        aa.n_dims = realloc_dims.size();
        aa.m_len_expr = realloc_str_len;
        aa.m_type = nullptr;
        aa.m_sym_subclass = nullptr;
        aa.m_codims = nullptr;
        aa.n_codims = 0;
        reallocate_target(aa, target, value, loc);
        return true;
    }

    void insert_realloc_for_target(ASR::expr_t* target, ASR::expr_t* value, Vec<ASR::expr_t**>& vars, bool per_assign_realloc = false) {
        ASR::ttype_t* target_type = ASRUtils::expr_type(target);
        ASR::expr_t* underlying_target = ASRUtils::get_past_array_physical_cast(target);
        ASR::ttype_t* underlying_target_type = ASRUtils::expr_type(underlying_target);
        ASR::expr_t* underlying_value = ASRUtils::get_past_array_physical_cast(value);
        bool array_copy = ASR::is_a<ASR::Var_t>(*underlying_value) && ASR::is_a<ASR::Var_t>(*underlying_target);
        if (!realloc_lhs && !per_assign_realloc) {
            return;
        }
        bool is_bitcast = ASR::is_a<ASR::BitCast_t>(*underlying_value);
        if( (!ASRUtils::is_allocatable(underlying_target_type) || vars.size() == 1) &&
            !(array_copy && ASRUtils::is_allocatable(underlying_target_type)) &&
            !(is_bitcast && ASRUtils::is_allocatable(underlying_target_type)) ) {
            return ;
        }

        ASRUtils::ExprStmtDuplicator d(al);
        ASR::expr_t* size_expr = ASRUtils::get_expr_size_expr(value);
        if (size_expr == nullptr) {
            return;
        }
        ASR::expr_t* realloc_var = d.duplicate_expr(size_expr);

        Location loc; loc.first = 1, loc.last = 1;
        ASRUtils::ASRBuilder builder(al, loc);
        ASR::ttype_t* idx_type = get_index_type(loc);
        int idx_kind = get_index_kind();
        // When assigning back from function result, the LHS
        // allocatable variable must have lower bounds reset to 1, 
        // It should not inherit the bounds set within the function from target. 
        // Note: Instead of string matching, a more robust approach can be using 
        // m_move_allocation flags, but it requires parsing entire ASR to 
        // extract those flags, which might increase time for this rare edge case.
        // So, using string matching patterns for now.
        bool from_function_result = false;
        if (ASR::is_a<ASR::Var_t>(*value)) {
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(value)->m_v);
            if (ASR::is_a<ASR::Variable_t>(*sym)) {
                // Temporary allocatable array for return type 
                // Created inside pass: subroutine_from_function
                std::string src_name = std::string(ASR::down_cast<ASR::Variable_t>(sym)->m_name);
                from_function_result = src_name.find("__libasr_created__function_call_") == 0;
            }
        }
        // F2018 10.2.1.3 allocates the variable with LBOUND(expr), which is
        // the operand's lower bound only for a whole array (F2018 16.9.109).
        // Any other array expression has lower bounds of 1.
        bool value_is_whole_array = ASR::is_a<ASR::Var_t>(*underlying_value) ||
            (ASR::is_a<ASR::StructInstanceMember_t>(*underlying_value) &&
             !ASRUtils::is_array(ASRUtils::expr_type(
                ASR::down_cast<ASR::StructInstanceMember_t>(underlying_value)->m_v)));
        Vec<ASR::dimension_t> realloc_dims;
        size_t target_rank = ASRUtils::extract_n_dims_from_ttype(target_type);
        realloc_dims.reserve(al, target_rank);
        for( size_t i = 0; i < target_rank; i++ ) {
            ASR::dimension_t realloc_dim;
            realloc_dim.loc = loc;
            if (from_function_result || !value_is_whole_array) {
                // Reset lower bound to 1, for function return assignment
                // and for array expressions
                realloc_dim.m_start = make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, idx_kind, loc);
            } else {
                // For other cases (For example, variable copy a = b) preserve source lbounds
                realloc_dim.m_start = PassUtils::get_bound(realloc_var, i + 1, "lbound", al, idx_kind);
            }
            ASR::expr_t* dim_expr = make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, i + 1, idx_kind, loc);
            realloc_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                al, loc, realloc_var, dim_expr, idx_type, nullptr));
            realloc_dims.push_back(al, realloc_dim);
        }
        ASR::expr_t* realloc_str_len {};
        if(ASRUtils::is_character(*ASRUtils::expr_type(value))){
            bool use_target_len = false;
            if (ASRUtils::is_character(*target_type)) {
                ASR::String_t* target_str = ASR::down_cast<ASR::String_t>(
                    ASRUtils::extract_type(target_type));
                if (target_str->m_len_kind != ASR::string_length_kindType::DeferredLength) {
                    int64_t target_len {};
                    if (target_str->m_len == nullptr) {
                        // An assumed-length target retains its runtime element length.
                        realloc_str_len = ASRUtils::EXPR(ASR::make_StringLen_t(
                            al, loc, d.duplicate_expr(underlying_target), int32, nullptr));
                    } else if (ASRUtils::is_value_constant(target_str->m_len, target_len)) {
                        realloc_str_len = builder.i32(target_len);
                    } else {
                        ASRUtils::ExprStmtDuplicator d2(al);
                        realloc_str_len = d2.duplicate_expr(target_str->m_len);
                    }
                    use_target_len = true;
                }
            }
            if (!use_target_len) {
                ASR::String_t* value_str = ASRUtils::get_string_type(
                    ASRUtils::expr_type(value));
                if (value_str->m_len != nullptr) {
                    realloc_str_len = d.duplicate_expr(value_str->m_len);
                } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*value) &&
                    ASR::down_cast<ASR::IntrinsicElementalFunction_t>(value)->m_intrinsic_id ==
                        static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::StringConcat)) {
                    realloc_str_len = ASRUtils::StringConcat::get_safe_string_len(
                        al, loc, value, ASRUtils::expr_type(value), builder);
                } else {
                    ASR::expr_t* len_value{}; // Compile-Time Length
                    int64_t len {};
                    if(ASRUtils::is_value_constant(ASR::down_cast<ASR::String_t>(
                        ASRUtils::extract_type(ASRUtils::expr_type(realloc_var)))->m_len), len) {
                        len_value = builder.i32(len);
                    }
                    realloc_str_len = ASRUtils::EXPR(ASR::make_StringLen_t(
                        al, loc, realloc_var, int32, len_value));
                }
            }
        }

        ASR::alloc_arg_t alloc_arg;
        alloc_arg.loc = loc;
        alloc_arg.m_a = underlying_target;
        alloc_arg.m_dims = realloc_dims.p;
        alloc_arg.n_dims = realloc_dims.size();
        alloc_arg.m_len_expr = realloc_str_len;
        alloc_arg.m_type = nullptr;
        alloc_arg.m_sym_subclass = nullptr;
        alloc_arg.m_codims = nullptr;
        alloc_arg.n_codims = 0;
        reallocate_target(alloc_arg, underlying_target, value, loc);
    }

    // Reallocates the target of `alloc_arg` for the assignment of `value`.
    // A polymorphic target takes the dynamic type of the value: a value
    // that is not polymorphic gives it statically, and a polymorphic one
    // gives it at run time, as the MOLD= of an ALLOCATE of the target.
    void reallocate_target(ASR::alloc_arg_t& alloc_arg, ASR::expr_t* target,
            ASR::expr_t* value, const Location& loc) {
        ASR::expr_t* mold = get_polymorphic_mold(target, value);
        set_dynamic_type_of_polymorphic_target(alloc_arg, target, value, mold);
        deallocate_polymorphic_target_of_other_shape_or_type(alloc_arg, mold,
            loc);
        Vec<ASR::alloc_arg_t> alloc_args;
        alloc_args.reserve(al, 1);
        alloc_args.push_back(al, alloc_arg);
        if (mold == nullptr) {
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_ReAlloc_t(
                al, loc, alloc_args.p, alloc_args.size(),
                make_shape_differs(alloc_arg, loc))));
            return;
        }
        ASRUtils::ASRBuilder b(al, loc);
        ASR::stmt_t* allocate = ASRUtils::STMT(ASR::make_Allocate_t(al, loc,
            alloc_args.p, alloc_args.size(), nullptr, nullptr, mold));
        pass_result.push_back(al, b.If(b.Not(make_allocated(alloc_arg.m_a,
            loc)), {allocate}, {}));
    }

    // Whether the allocated object of `alloc_arg` has a shape other than
    // the one `alloc_arg` gives it (F2018 10.2.1.3), also when both have the
    // same number of elements, e.g. `a = transpose(a)`. It reads the
    // descriptor, so it is evaluated only while the object is allocated.
    // An object of rank 0 never differs.
    ASR::expr_t* make_shape_differs(const ASR::alloc_arg_t& alloc_arg,
            const Location& loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASRUtils::ExprStmtDuplicator d(al);
        ASR::ttype_t* idx_type = get_index_type(loc);
        ASR::expr_t* shape_differs = nullptr;
        for (size_t i = 0; i < alloc_arg.n_dims; i++) {
            ASR::expr_t* length = d.duplicate_expr(alloc_arg.m_dims[i].m_length);
            if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(length))
                    != get_index_kind()) {
                length = b.i2i_t(length, idx_type);
            }
            ASR::expr_t* dim_differs = b.NotEq(b.ArraySize(
                d.duplicate_expr(alloc_arg.m_a), b.i_t(i + 1, idx_type),
                idx_type), length);
            shape_differs = shape_differs == nullptr ? dim_differs
                : b.Or(shape_differs, dim_differs);
        }
        if (shape_differs == nullptr) {
            shape_differs = b.bool_t(false, ASRUtils::TYPE(
                ASR::make_Logical_t(al, loc, 4)));
        }
        return shape_differs;
    }

    // The variable whose dynamic type a polymorphic array `value` has, for
    // a polymorphic `target`, or nullptr when the value is not polymorphic
    // or that variable is not known. A section of an array has the dynamic
    // type of the array.
    ASR::expr_t* get_polymorphic_mold(ASR::expr_t* target, ASR::expr_t* value) {
        if (!ASRUtils::is_class_type(ASRUtils::extract_type(
                    ASRUtils::expr_type(target))) ||
                !ASRUtils::is_class_type(ASRUtils::extract_type(
                    ASRUtils::expr_type(value)))) {
            return nullptr;
        }
        ASR::expr_t* mold = ASRUtils::get_past_array_physical_cast(value);
        if (ASR::is_a<ASR::ArraySection_t>(*mold)) {
            mold = ASRUtils::get_past_array_physical_cast(
                ASR::down_cast<ASR::ArraySection_t>(mold)->m_v);
        } else if (ASR::is_a<ASR::ArrayItem_t>(*mold)) {
            mold = ASRUtils::get_past_array_physical_cast(
                ASR::down_cast<ASR::ArrayItem_t>(mold)->m_v);
        }
        ASR::ttype_t* mold_type = ASRUtils::expr_type(mold);
        if (!(ASR::is_a<ASR::Var_t>(*mold) ||
                ASR::is_a<ASR::StructInstanceMember_t>(*mold)) ||
                !ASRUtils::is_array(mold_type) ||
                ASRUtils::extract_physical_type(mold_type) !=
                    ASR::array_physical_typeType::DescriptorArray) {
            return nullptr;
        }
        ASRUtils::ExprStmtDuplicator d(al);
        return d.duplicate_expr(mold);
    }

    ASR::expr_t* make_allocated(ASR::expr_t* target, const Location& loc) {
        Vec<ASR::expr_t*> allocated_args;
        allocated_args.reserve(al, 1);
        allocated_args.push_back(al, target);
        return ASRUtils::EXPR(ASR::make_IntrinsicImpureFunction_t(al, loc,
            static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated),
            allocated_args.p, allocated_args.size(), 0,
            ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
    }

    // F2018 10.2.1.3: an allocatable polymorphic variable that is
    // (re)allocated by an intrinsic assignment takes the dynamic type of
    // the expression. When the expression is not polymorphic, that type is
    // its declared type (an intrinsic type only for an unlimited
    // polymorphic variable), so allocate the target with it. Otherwise it
    // is the dynamic type of `mold`, and the declared type of the mold
    // marks the allocation as one with MOLD=.
    void set_dynamic_type_of_polymorphic_target(ASR::alloc_arg_t& alloc_arg,
            ASR::expr_t* target, ASR::expr_t* value, ASR::expr_t* mold) {
        if (mold != nullptr) {
            alloc_arg.m_type = ASRUtils::duplicate_type(al,
                ASRUtils::extract_type(ASRUtils::expr_type(mold)));
            return;
        }
        ASR::ttype_t* target_el_type = ASRUtils::extract_type(
            ASRUtils::expr_type(target));
        ASR::ttype_t* value_el_type = ASRUtils::extract_type(
            ASRUtils::expr_type(value));
        if (!ASRUtils::is_class_type(target_el_type) ||
                ASRUtils::is_class_type(value_el_type)) {
            return;
        }
        if (!ASR::is_a<ASR::StructType_t>(*value_el_type)) {
            if (ASRUtils::is_unlimited_polymorphic_type(target_el_type)) {
                alloc_arg.m_type = ASRUtils::duplicate_type(al, value_el_type);
            }
            return;
        }
        ASR::symbol_t* value_struct_sym =
            ASRUtils::get_struct_sym_from_struct_expr(value);
        if (value_struct_sym == nullptr) {
            return;
        }
        alloc_arg.m_type = ASRUtils::make_StructType_t_util(al,
            alloc_arg.loc, value_struct_sym, true);
        alloc_arg.m_sym_subclass = value_struct_sym;
    }

    // F2018 10.2.1.3: an allocated polymorphic variable whose shape or
    // dynamic type differs from the expression's is deallocated before the
    // ReAlloc of `alloc_arg`, which then allocates it with the dynamic type
    // set by set_dynamic_type_of_polymorphic_target, or the ALLOCATE with
    // `mold`, which then allocates it with the dynamic type of the mold.
    // The ReAlloc itself only compares the shape. The value is read after
    // the deallocation: a function result is already in a temporary here,
    // and a value that can refer to the variable is evaluated into one by
    // array_struct_temporary.
    void deallocate_polymorphic_target_of_other_shape_or_type(
            const ASR::alloc_arg_t& alloc_arg, ASR::expr_t* mold,
            const Location& loc) {
        if (alloc_arg.m_type == nullptr) {
            return;
        }
        ASR::expr_t* target = alloc_arg.m_a;
        ASRUtils::ASRBuilder b(al, loc);
        ASRUtils::ExprStmtDuplicator d(al);
        ASR::ttype_t* logical_type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, loc, 4));
        auto make_deallocate = [&]() {
            Vec<ASR::expr_t*> vars;
            vars.reserve(al, 1);
            vars.push_back(al, target);
            return ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
                al, loc, vars.p, vars.size()));
        };
        ASR::expr_t* shape_differs = alloc_arg.n_dims > 0
            ? make_shape_differs(alloc_arg, loc) : nullptr;
        // The guard naming the type of the expression keeps a variable of
        // that type. A character expression has no guard, as its length is
        // part of its dynamic type but not of a guard. A variable of the
        // dynamic type of a mold is kept as well.
        ASR::type_stmt_t* same_type_guard = nullptr;
        if (mold == nullptr && alloc_arg.m_sym_subclass != nullptr) {
            same_type_guard = ASR::down_cast<ASR::type_stmt_t>(
                ASR::make_TypeStmtName_t(al, loc,
                    alloc_arg.m_sym_subclass, nullptr, 0));
        } else if (mold == nullptr &&
                !ASRUtils::is_character(*alloc_arg.m_type)) {
            same_type_guard = ASR::down_cast<ASR::type_stmt_t>(
                ASR::make_TypeStmtType_t(al, loc,
                    ASRUtils::duplicate_type(al, alloc_arg.m_type), nullptr, 0));
        }
        ASR::stmt_t* deallocate_if_other_type = make_deallocate();
        if (mold != nullptr) {
            Vec<ASR::expr_t*> same_type_as_args;
            same_type_as_args.reserve(al, 2);
            same_type_as_args.push_back(al, target);
            same_type_as_args.push_back(al, d.duplicate_expr(mold));
            ASR::expr_t* same_type = ASRUtils::EXPR(
                ASR::make_IntrinsicElementalFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicElementalFunctions::SameTypeAs),
                    same_type_as_args.p, same_type_as_args.size(), 0,
                    logical_type, nullptr));
            deallocate_if_other_type = b.If(b.Not(same_type),
                {deallocate_if_other_type}, {});
        } else if (same_type_guard != nullptr) {
            Vec<ASR::type_stmt_t*> type_guards;
            type_guards.reserve(al, 1);
            type_guards.push_back(al, same_type_guard);
            Vec<ASR::stmt_t*> other_type_body;
            other_type_body.reserve(al, 1);
            other_type_body.push_back(al, deallocate_if_other_type);
            deallocate_if_other_type = ASRUtils::STMT(ASR::make_SelectType_t(
                al, loc, target, nullptr, type_guards.p, type_guards.size(),
                other_type_body.p, other_type_body.size()));
        }
        ASR::expr_t* is_allocated = make_allocated(target, loc);
        ASR::stmt_t* deallocate_if_differs = deallocate_if_other_type;
        if (shape_differs != nullptr) {
            deallocate_if_differs = b.If(shape_differs, {make_deallocate()},
                {deallocate_if_other_type});
        }
        pass_result.push_back(al, b.If(is_allocated, {deallocate_if_differs},
            {}));
    }

    ASR::Variable_t* get_base_variable(ASR::expr_t* expr) {
        ASR::expr_t* cur = expr;
        while (cur) {
            if (ASR::is_a<ASR::Var_t>(*cur)) {
                ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(cur)->m_v);
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    return ASR::down_cast<ASR::Variable_t>(sym);
                }
                return nullptr;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*cur)) {
                cur = ASR::down_cast<ASR::StructInstanceMember_t>(cur)->m_v;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*cur)) {
                cur = ASR::down_cast<ASR::ArrayItem_t>(cur)->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*cur)) {
                cur = ASR::down_cast<ASR::ArraySection_t>(cur)->m_v;
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*cur)) {
                cur = ASR::down_cast<ASR::ArrayPhysicalCast_t>(cur)->m_arg;
            } else if (ASR::is_a<ASR::Cast_t>(*cur)) {
                cur = ASR::down_cast<ASR::Cast_t>(cur)->m_arg;
            } else {
                return nullptr;
            }
        }
        return nullptr;
    }

    // `a op b` of integer(4) sizes, folded when both are constants
    ASR::expr_t* size_binop(ASR::expr_t* a, ASR::binopType op, ASR::expr_t* b,
            const Location& loc) {
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        int64_t a_value = 0, b_value = 0;
        if (ASRUtils::expr_value(a) && ASRUtils::expr_value(b) &&
                ASRUtils::extract_value(ASRUtils::expr_value(a), a_value) &&
                ASRUtils::extract_value(ASRUtils::expr_value(b), b_value)) {
            int64_t result = op == ASR::binopType::Add ? a_value + b_value : a_value * b_value;
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, result, int_type));
        }
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, a, op, b, int_type, nullptr));
    }

    // The integer(4) number of elements of a constructor with elements
    // `args`, or nullptr if it is only known once they are evaluated
    ASR::expr_t* get_constructor_size(ASR::expr_t** args, size_t n_args,
            const Location& loc) {
        ASRUtils::ASRBuilder builder(al, loc);
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        auto to_int = [&](ASR::expr_t* e) -> ASR::expr_t* {
            if (e == nullptr) return nullptr;
            e = duplicator.duplicate_expr(e);
            if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e)) != 4) {
                e = builder.i2i_t(e, int_type);
            }
            return e;
        };
        ASR::expr_t* size = builder.i32(0);
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t* arg = args[i];
            ASR::expr_t* arg_size = nullptr;
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*arg)) {
                ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(arg);
                ASR::expr_t* values_size = get_constructor_size(
                    idl->m_values, idl->n_values, loc);
                if (values_size == nullptr || !ASR::is_a<ASR::Var_t>(*idl->m_var) ||
                        ASRUtils::expr_references_symbol(values_size,
                            ASR::down_cast<ASR::Var_t>(idl->m_var)->m_v)) {
                    return nullptr;
                }
                ASR::expr_t* trips = ASRUtils::compute_length_from_start_end_step(al,
                    to_int(idl->m_start), to_int(idl->m_end), to_int(idl->m_increment));
                if (ASRUtils::expr_value(trips) == nullptr) {
                    // An empty loop has no elements rather than a negative number
                    trips = builder.Max(trips, builder.i32(0));
                    ASR::IfExp_t* max = ASR::down_cast<ASR::IfExp_t>(trips);
                    max->m_body = duplicator.duplicate_expr(max->m_body);
                }
                arg_size = size_binop(values_size, ASR::binopType::Mul, trips, loc);
            } else if (ASR::is_a<ASR::ArrayConstructor_t>(*arg)) {
                ASR::ArrayConstructor_t* constructor = ASR::down_cast<ASR::ArrayConstructor_t>(arg);
                arg_size = get_constructor_size(constructor->m_args, constructor->n_args, loc);
                if (arg_size == nullptr) {
                    return nullptr;
                }
            } else if (!ASRUtils::is_array(ASRUtils::expr_type(arg))) {
                arg_size = builder.i32(1);
            } else if (ASR::is_a<ASR::Var_t>(*arg) ||
                    ASR::is_a<ASR::StructInstanceMember_t>(*arg)) {
                arg_size = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
                    duplicator.duplicate_expr(arg), nullptr, int_type, nullptr));
            } else {
                return nullptr;
            }
            size = size_binop(size, ASR::binopType::Add, arg_size, loc);
        }
        return size;
    }

    // A copy of constructor `x` whose type gives its size, where a shape
    // check reads it from, or nullptr if the size is not known in advance
    ASR::expr_t* get_constructor_with_size(ASR::ArrayConstructor_t* x) {
        const Location& loc = x->base.base.loc;
        ASR::expr_t* size = get_constructor_size(x->m_args, x->n_args, loc);
        if (size == nullptr) {
            return nullptr;
        }
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::ArrayConstructor_t* constructor = ASR::down_cast<ASR::ArrayConstructor_t>(
            duplicator.duplicate_expr(&x->base));
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
        dim.m_length = size;
        dims.push_back(al, dim);
        constructor->m_type = ASRUtils::make_Array_t_util(al, loc,
            ASRUtils::type_get_past_array(constructor->m_type), dims.p, dims.size());
        return &constructor->base;
    }

    bool should_auto_realloc_component_assignment(ASR::expr_t* target) {
        if (!ASR::is_a<ASR::StructInstanceMember_t>(*target)) {
            return false;
        }
        ASR::ttype_t* target_type = ASRUtils::expr_type(target);
        if (!ASRUtils::is_array(target_type) || !ASRUtils::is_allocatable(target_type)) {
            return false;
        }
        ASR::Variable_t* base_var = get_base_variable(target);
        if (!base_var) {
            return false;
        }
        return base_var->m_intent == ASR::intentType::Out ||
            base_var->m_intent == ASR::intentType::ReturnVar;
    }

    void collect_array_if_scalar_targets(ASR::stmt_t** body, size_t n_body,
            std::vector<ASR::expr_t*>& scalar_targets) {
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t* stmt = body[i];
            if (ASR::is_a<ASR::Assignment_t>(*stmt)) {
                ASR::Assignment_t& assignment = *ASR::down_cast<ASR::Assignment_t>(stmt);
                ASR::expr_t* value = ASRUtils::get_past_array_broadcast(assignment.m_value);
                if (assignment.m_move_allocation ||
                    !ASRUtils::is_array(ASRUtils::expr_type(assignment.m_target)) ||
                    ASRUtils::is_array(ASRUtils::expr_type(value))) {
                    continue;
                }

                if (std::find(scalar_targets.begin(), scalar_targets.end(),
                        assignment.m_target) != scalar_targets.end()) {
                    continue;
                }
                scalar_targets.push_back(assignment.m_target);
            } else if (ASR::is_a<ASR::If_t>(*stmt)) {
                ASR::If_t& if_stmt = *ASR::down_cast<ASR::If_t>(stmt);
                collect_array_if_scalar_targets(if_stmt.m_body,
                    if_stmt.n_body, scalar_targets);
                collect_array_if_scalar_targets(if_stmt.m_orelse,
                    if_stmt.n_orelse, scalar_targets);
            }
        }
    }

    ASR::expr_t* get_singleton_safe_index(ASR::expr_t* expr, ASR::expr_t* index_expr,
            size_t dim, const Location& loc,
            const std::vector<ASR::expr_t*>* scalar_targets) {
        if (!scalar_targets ||
                std::find(scalar_targets->begin(), scalar_targets->end(), expr) ==
                    scalar_targets->end()) {
            return index_expr;
        }

        ASRUtils::ASRBuilder builder(al, loc);
        ASR::ttype_t* idx_type = get_index_type(loc);
        int idx_kind = get_index_kind();
        ASR::expr_t* dim_expr = make_ConstantWithKind(make_IntegerConstant_t,
            make_Integer_t, dim, idx_kind, loc);
        ASR::expr_t* one = make_ConstantWithKind(make_IntegerConstant_t,
            make_Integer_t, 1, idx_kind, loc);
        ASR::expr_t* target_extent = ASRUtils::EXPR(ASR::make_ArraySize_t(
            al, loc, expr, dim_expr, idx_type, nullptr));
        ASR::expr_t* is_singleton_extent = builder.Eq(target_extent, one);
        ASR::expr_t* lower_bound = PassUtils::get_bound(expr, dim,
            "lbound", al, idx_kind);
        return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc, is_singleton_extent,
            lower_bound, index_expr, idx_type, nullptr));
    }

    void generate_assumed_rank_source_loop(ASR::Allocate_t& xx, size_t arg_i,
            const Location& loc) {
        size_t n_dims = xx.m_args[arg_i].n_dims;
        ASR::expr_t* target_var = xx.m_args[arg_i].m_a;
        ASR::ttype_t* idx_type = get_index_type(loc);
        int idx_kind = get_index_kind();

        ASR::ttype_t* base_type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(target_var));
        ASR::ttype_t* descriptor_array_type =
            ASRUtils::create_array_type_with_empty_dims(al, n_dims,
                ASRUtils::extract_type(base_type));
        ASR::expr_t* casted_target = ASRUtils::EXPR(
            ASRUtils::make_ArrayPhysicalCast_t_util(al, loc, target_var,
                ASR::array_physical_typeType::AssumedRankArray,
                ASR::array_physical_typeType::DescriptorArray,
                descriptor_array_type, nullptr));

        Vec<ASR::expr_t*> index_exprs;
        index_exprs.reserve(al, n_dims);
        for (size_t j = 0; j < n_dims; j++) {
            std::string idx_name = current_scope->get_unique_name(
                "__libasr_index_" + std::to_string(j) + "_");
            ASR::symbol_t* idx_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, current_scope,
                    s2c(al, idx_name), nullptr, 0, ASR::intentType::Local,
                    nullptr, nullptr, ASR::storage_typeType::Default, idx_type,
                    nullptr, ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            current_scope->add_symbol(idx_name, idx_sym);
            index_exprs.push_back(al, ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, idx_sym)));
        }

        Vec<ASR::array_index_t> array_indices;
        array_indices.reserve(al, n_dims);
        for (size_t j = 0; j < n_dims; j++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = index_exprs[j];
            ai.m_step = nullptr;
            array_indices.push_back(al, ai);
        }
        ASR::ttype_t* elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(target_var));
        ASR::expr_t* indexed_target = ASRUtils::EXPR(
            ASRUtils::make_ArrayItem_t_util(al, loc, casted_target,
                array_indices.p, array_indices.size(), elem_type,
                ASR::arraystorageType::ColMajor, nullptr));

        ASRUtils::ExprStmtDuplicator dup(al);
        ASR::expr_t* source_copy = dup.duplicate_expr(xx.m_source);

        // The elements are new: they are defined by the copy, not finalized.
        ASR::stmt_t* curr_stmt = ASRUtils::STMT(
            ASRUtils::make_Assignment_t_util(al, loc, indexed_target,
                source_copy, nullptr, realloc_lhs, false, true));

        for (size_t k = 0; k < n_dims; k++) {
            size_t dim = n_dims - 1 - k;
            ASRUtils::ExprStmtDuplicator bound_dup(al);
            ASR::expr_t* lb_arr = bound_dup.duplicate_expr(casted_target);
            ASR::expr_t* ub_arr = bound_dup.duplicate_expr(casted_target);
            ASR::do_loop_head_t loop_head;
            loop_head.loc = loc;
            loop_head.m_v = index_exprs[dim];
            loop_head.m_start = PassUtils::get_bound(lb_arr,
                dim + 1, "lbound", al, idx_kind);
            loop_head.m_end = PassUtils::get_bound(ub_arr,
                dim + 1, "ubound", al, idx_kind);
            loop_head.m_increment = nullptr;
            Vec<ASR::stmt_t*> body;
            body.reserve(al, 1);
            body.push_back(al, curr_stmt);
            curr_stmt = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                loop_head, body.p, body.size(), nullptr, 0));
        }
        pass_result.push_back(al, curr_stmt);
    }

    void visit_Allocate(const ASR::Allocate_t& x) {
        ASR::Allocate_t& xx = const_cast<ASR::Allocate_t&>(x);
        if (xx.m_source) {
            size_t allocate_pos = pass_result.size();
            pass_result.push_back(al, &(xx.base));
            bool generated_loop = false;
            ASR::expr_t* mold = nullptr;
            // Pushing assignment statements to source
            for (size_t i = 0; i < x.n_args ; i++) {
                if( !ASRUtils::is_array(
                        ASRUtils::expr_type(x.m_args[i].m_a)) ) {
                    continue;
                }
                // Skip mold-based allocations: m_type being set indicates
                // the allocate uses mold= (type/shape only, no data copy).
                if (x.m_args[i].m_type != nullptr) {
                    continue;
                }
                // Assumed-rank target: generate_loop can't read the rank from
                // the variable's static type (which has 0 dims). The runtime
                // rank is given by the allocate dims, so build the loop nest
                // explicitly using m_args[i].n_dims.
                ASR::ttype_t* tgt_t = ASRUtils::type_get_past_allocatable_pointer(
                    ASRUtils::expr_type(x.m_args[i].m_a));
                if (ASRUtils::is_assumed_rank_array(tgt_t) &&
                        x.m_args[i].n_dims > 0) {
                    generate_assumed_rank_source_loop(xx, i, x.base.base.loc);
                    generated_loop = true;
                    continue;
                }
                ASRUtils::ExprStmtDuplicator duplicator(al);
                ASR::expr_t* source_copy = duplicator.duplicate_expr(xx.m_source);
                // The source is copied into the new elements, which are
                // defined by the copy and not finalized (F2018 7.5.6.3).
                ASR::stmt_t* assign = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, xx.m_args[i].m_a->base.loc, xx.m_args[i].m_a, source_copy, nullptr, realloc_lhs, false, true));
                ASR::Assignment_t* assignment_t = ASR::down_cast<ASR::Assignment_t>(assign);
                Vec<ASR::expr_t**> fix_type_args;
                fix_type_args.reserve(al, 2);
                fix_type_args.push_back(al, const_cast<ASR::expr_t**>(&(assignment_t->m_target)));
                fix_type_args.push_back(al, const_cast<ASR::expr_t**>(&(assignment_t->m_value)));

                Vec<ASR::expr_t**> vars1;
                vars1.reserve(al, 1);
                ArrayVarAddressCollector var_collector_target(al, vars1);
                var_collector_target.current_expr = const_cast<ASR::expr_t**>(&(assignment_t->m_target));
                var_collector_target.call_replacer();
                ArrayVarAddressCollector var_collector_value(al, vars1);
                var_collector_value.current_expr = const_cast<ASR::expr_t**>(&(assignment_t->m_value));
                var_collector_value.call_replacer();

                // TODO: Remove the following. Instead directly handle
                // allocate with source in the backend.
                if( xx.m_args[i].n_dims == 0 ) {
                    Vec<ASR::expr_t**> vars2;
                    vars2.reserve(al, 1);
                    ArrayVarAddressCollector var_collector2_target(al, vars2);
                    var_collector2_target.current_expr = const_cast<ASR::expr_t**>(&(assignment_t->m_target));
                    var_collector2_target.call_replacer();
                    ArrayVarAddressCollector var_collector2_value(al, vars2);
                    var_collector2_value.current_expr = const_cast<ASR::expr_t**>(&(assignment_t->m_value));
                    var_collector2_value.call_replacer();
                    insert_realloc_for_target(
                        xx.m_args[i].m_a, xx.m_source, vars2);
                }
                generate_loop(*assignment_t, vars1, fix_type_args, x.base.base.loc);
                generated_loop = true;
                // A polymorphic array takes the dynamic type of a
                // polymorphic source, which the loops do not copy, so it
                // is allocated with that source as its mold.
                ASR::expr_t* arg_mold = get_polymorphic_mold(
                    xx.m_args[i].m_a, xx.m_source);
                if (arg_mold != nullptr) {
                    mold = arg_mold;
                    xx.m_args[i].m_type = ASRUtils::duplicate_type(al,
                        ASRUtils::extract_type(ASRUtils::expr_type(mold)));
                }
            }
            // Clear source when data copy loops were generated, since
            // the loops handle the copy. Keeping array-typed source
            // expressions (e.g. real(a)) would cause backend errors. Only
            // the mold of a polymorphic array is kept.
            if (generated_loop) {
                xx.m_source = mold;
            }
            if (mold != nullptr) {
                split_polymorphic_allocate(xx, allocate_pos);
            }
        }
    }

    // Moves the objects allocated with a mold out of `x`, which is at
    // `allocate_pos` in pass_result, into their own ALLOCATE with the mold
    // as SOURCE=, placed right after `x`, when `x` also allocates other
    // objects. A later run of this pass generates the copy loops of the
    // other objects again and clears SOURCE=, which would leave the
    // objects allocated with a mold without it.
    void split_polymorphic_allocate(ASR::Allocate_t& x, size_t allocate_pos) {
        Vec<ASR::alloc_arg_t> mold_args, other_args;
        mold_args.reserve(al, x.n_args);
        other_args.reserve(al, x.n_args);
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_type != nullptr) {
                mold_args.push_back(al, x.m_args[i]);
            } else {
                other_args.push_back(al, x.m_args[i]);
            }
        }
        if (other_args.size() == 0) {
            return;
        }
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::stmt_t* mold_allocate = ASRUtils::STMT(ASR::make_Allocate_t(al,
            x.base.base.loc, mold_args.p, mold_args.size(),
            duplicator.duplicate_expr(x.m_stat),
            duplicator.duplicate_expr(x.m_errmsg), x.m_source));
        x.m_args = other_args.p;
        x.n_args = other_args.size();
        x.m_source = nullptr;
        pass_result.push_back(al, mold_allocate);
        for (size_t j = pass_result.size() - 1; j > allocate_pos + 1; j--) {
            pass_result.p[j] = pass_result.p[j - 1];
        }
        pass_result.p[allocate_pos + 1] = mold_allocate;
        if (x.m_stat != nullptr) {
            // The mold ALLOCATE and the copy loops run only when `x`
            // succeeded, so that its failure stays in STAT= (F2018 9.7.4).
            ASRUtils::ASRBuilder b(al, x.base.base.loc);
            ASR::expr_t* stat = duplicator.duplicate_expr(x.m_stat);
            std::vector<ASR::stmt_t*> body(pass_result.p + allocate_pos + 1,
                pass_result.p + pass_result.size());
            pass_result.n = allocate_pos + 1;
            pass_result.push_back(al, b.If(b.Eq(stat,
                b.i_t(0, ASRUtils::expr_type(stat))), body, {}));
        }
    }

    // Don't visit inside DebugCheckArrayBounds, it may contain ArrayConstant and result_expr will be nullptr
    void visit_DebugCheckArrayBounds(const ASR::DebugCheckArrayBounds_t& x) {
        (void)x;
    }

    bool is_looping_necessary_for_bitcast(ASR::expr_t* value) {
        if (ASR::is_a<ASR::BitCast_t>(*value)) {
            ASR::BitCast_t* bit_cast = ASR::down_cast<ASR::BitCast_t>(value);
            return !ASRUtils::is_string_only(ASRUtils::expr_type(bit_cast->m_source));
        } else {
            return false;
        }
    }

    // Size in bytes of one element of `elem_type`, or -1 when it is not
    // known at compile time.
    int64_t get_element_byte_size(ASR::ttype_t* elem_type) {
        if( ASR::is_a<ASR::String_t>(*elem_type) ) {
            int64_t len = -1;
            if( !ASRUtils::extract_value(
                    ASR::down_cast<ASR::String_t>(elem_type)->m_len, len) ) {
                return -1;
            }
            return len;
        }
        int64_t nbytes = ASRUtils::get_type_byte_size(elem_type);
        return nbytes > 0 ? nbytes : -1;
    }

    // `transfer()` with CHARACTER on the array side cannot be scalarised into
    // an element-wise loop. The bytes of result element `i` start at byte
    // `i * storage_size(mold)/8` of the source, an offset that is not
    // expressible as an element index of the source, so an element-wise loop
    // would bit-cast every element from byte 0 of the source (and, for a
    // CHARACTER result, hand the backend a whole-array source it cannot
    // lower at all).
    //
    // Lower such an assignment into a loop over the result elements that
    // slices a CHARACTER buffer holding the source bytes, one slice of
    // `elem_bytes` characters per element. Returns false when the assignment
    // is not of a shape handled here, and the caller then falls back to the
    // generic lowering.
    bool lower_character_bitcast(const ASR::Assignment_t& x,
            ASR::BitCast_t* bc, const Location& loc) {
        ASR::expr_t* target = ASRUtils::get_past_array_physical_cast(
            const_cast<ASR::expr_t*>(x.m_target));
        if( !ASR::is_a<ASR::Var_t>(*target) &&
            !ASR::is_a<ASR::StructInstanceMember_t>(*target) ) {
            return false;
        }
        ASR::ttype_t* target_type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(target));
        if( !ASRUtils::is_array(target_type) ||
            ASRUtils::extract_n_dims_from_ttype(target_type) != 1 ) {
            return false;
        }
        ASR::ttype_t* source_type = ASRUtils::expr_type(bc->m_source);
        bool source_is_string = ASRUtils::is_string_only(source_type);
        ASR::ttype_t* elem_type = ASRUtils::extract_type(target_type);
        bool target_is_string = ASR::is_a<ASR::String_t>(*elem_type);
        if( !source_is_string && !target_is_string ) {
            return false;
        }
        // Only default character kind can be sliced byte by byte.
        if( (source_is_string &&
             ASRUtils::extract_kind_from_ttype_t(source_type) != 1) ||
            (target_is_string &&
             ASRUtils::extract_kind_from_ttype_t(elem_type) != 1) ) {
            return false;
        }
        int64_t elem_bytes = get_element_byte_size(elem_type);
        if( elem_bytes <= 0 ) {
            return false;
        }

        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t* buffer = nullptr;
        if( source_is_string ) {
            buffer = bc->m_source;
        } else {
            // Materialise the source bytes into a CHARACTER buffer first;
            // `buffer = transfer(source, buffer)` with a scalar CHARACTER
            // result is already a plain byte copy in the backend.
            int64_t n_elems = -1;
            if( bc->m_size && ASRUtils::expr_value(bc->m_size) ) {
                ASRUtils::extract_value(ASRUtils::expr_value(bc->m_size), n_elems);
            }
            if( n_elems <= 0 ) {
                n_elems = ASRUtils::get_fixed_size_of_array(bc->m_type);
            }
            if( n_elems <= 0 ) {
                n_elems = ASRUtils::get_fixed_size_of_array(target_type);
            }
            if( n_elems <= 0 ) {
                return false;
            }
            ASR::ttype_t* buffer_type = b.String(b.i32(n_elems * elem_bytes),
                ASR::string_length_kindType::ExpressionLength,
                ASR::string_physical_typeType::DescriptorString, 1);
            std::string buffer_name = current_scope->get_unique_name(
                "__libasr_bitcast_buf_");
            ASR::symbol_t* buffer_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, current_scope, s2c(al, buffer_name), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default, buffer_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Required, false));
            current_scope->add_symbol(buffer_name, buffer_sym);
            buffer = ASRUtils::EXPR(ASR::make_Var_t(al, loc, buffer_sym));
            ASR::expr_t* buffer_value = ASRUtils::EXPR(ASR::make_BitCast_t(
                al, loc, bc->m_source, buffer, nullptr, buffer_type, nullptr));
            pass_result.push_back(al, ASRUtils::STMT(
                ASRUtils::make_Assignment_t_util(al, loc, buffer, buffer_value,
                    nullptr, false, false)));
        }

        int index_kind = get_index_kind();
        ASR::ttype_t* index_type = get_index_type(loc);
        std::string index_name = current_scope->get_unique_name(
            "__libasr_index_0_");
        ASR::symbol_t* index_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(
                al, loc, current_scope, s2c(al, index_name), nullptr, 0,
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, index_type, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        current_scope->add_symbol(index_name, index_sym);
        ASR::expr_t* index = ASRUtils::EXPR(ASR::make_Var_t(al, loc, index_sym));

        ASRUtils::ExprStmtDuplicator duplicator(al);
        // start = (index - lbound(target, 1)) * elem_bytes + 1
        ASR::expr_t* offset = b.Mul(
            b.Sub(index, PassUtils::get_bound(
                    duplicator.duplicate_expr(target), 1, "lbound", al, index_kind)),
            b.i_t(elem_bytes, index_type));
        ASR::expr_t* start = b.Add(offset, b.i_t(1, index_type));
        ASR::expr_t* end = b.Add(duplicator.duplicate_expr(start),
            b.i_t(elem_bytes - 1, index_type));
        ASR::ttype_t* section_type = b.String(b.i32(elem_bytes),
            ASR::string_length_kindType::ExpressionLength,
            ASR::string_physical_typeType::DescriptorString, 1);
        ASR::expr_t* section = ASRUtils::EXPR(ASR::make_StringSection_t(
            al, loc, buffer, start, end, b.i_t(1, index_type),
            section_type, nullptr));

        Vec<ASR::array_index_t> indices;
        indices.reserve(al, 1);
        ASR::array_index_t array_index;
        array_index.loc = loc;
        array_index.m_left = nullptr;
        array_index.m_right = index;
        array_index.m_step = nullptr;
        indices.push_back(al, array_index);
        ASR::expr_t* target_item = ASRUtils::EXPR(
            ASRUtils::make_ArrayItem_t_util(al, loc,
                duplicator.duplicate_expr(target), indices.p, indices.size(),
                elem_type, ASR::arraystorageType::ColMajor, nullptr));

        ASR::expr_t* value = section;
        if( !target_is_string ) {
            value = ASRUtils::EXPR(ASR::make_BitCast_t(al, loc, section,
                duplicator.duplicate_expr(target_item), nullptr, elem_type,
                nullptr));
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        ASR::stmt_t* assign = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
            al, loc, target_item, value, nullptr, false, false));
        // The standard leaves the trailing part of the result undefined when
        // the source is shorter than the result. Skip those elements instead
        // of slicing past the end of the buffer.
        int64_t buffer_len = -1;
        ASR::String_t* buffer_str_type = ASR::down_cast<ASR::String_t>(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(buffer)));
        if( buffer_str_type->m_len ) {
            ASRUtils::extract_value(
                ASRUtils::expr_value(buffer_str_type->m_len), buffer_len);
        }
        int64_t n_target_elems = ASRUtils::get_fixed_size_of_array(target_type);
        if( buffer_len < 0 || n_target_elems < 0 ||
            n_target_elems * elem_bytes > buffer_len ) {
            Vec<ASR::stmt_t*> if_body;
            if_body.reserve(al, 1);
            if_body.push_back(al, assign);
            ASR::expr_t* buffer_len_expr = ASRUtils::EXPR(ASR::make_StringLen_t(
                al, loc, duplicator.duplicate_expr(buffer), index_type, nullptr));
            assign = ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr,
                b.LtE(duplicator.duplicate_expr(end), buffer_len_expr),
                if_body.p, if_body.size(), nullptr, 0));
        }
        body.push_back(al, assign);

        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = index;
        head.m_start = PassUtils::get_bound(
            duplicator.duplicate_expr(target), 1, "lbound", al, index_kind);
        head.m_end = PassUtils::get_bound(
            duplicator.duplicate_expr(target), 1, "ubound", al, index_kind);
        head.m_increment = nullptr;
        pass_result.push_back(al, ASRUtils::STMT(ASR::make_DoLoop_t(
            al, loc, nullptr, head, body.p, body.size(), nullptr, 0)));
        return true;
    }


    void visit_Assignment(const ASR::Assignment_t& x) {
        // A non-elemental defined assignment is entirely carried out by the
        // overloaded subroutine call, which takes both sides as actual
        // arguments. There is no element-wise assignment to expand and no
        // shape conformance to check. (An elemental one is expanded below.)
        if (x.m_overloaded != nullptr &&
                ASR::is_a<ASR::SubroutineCall_t>(*x.m_overloaded) &&
                !ASRUtils::is_elemental(ASR::down_cast<ASR::SubroutineCall_t>(
                    x.m_overloaded)->m_name)) {
            return ;
        }
        if (ASRUtils::is_simd_array(x.m_target)) {
            if( !(ASRUtils::is_allocatable(x.m_value) ||
                  ASRUtils::is_pointer(ASRUtils::expr_type(x.m_value))) ) {
                return ;
            }
        }
        ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);
        const std::vector<ASR::exprType>& skip_exprs = {
            ASR::exprType::IntrinsicArrayFunction,
            ASR::exprType::ArrayReshape,
        };
        if ( ASR::is_a<ASR::IntrinsicArrayFunction_t>(*xx.m_value) ) {
            // We need to do this because, we may have an assignment
            // in which IntrinsicArrayFunction is evaluated already and
            // value is an ArrayConstant, thus we need to unroll it.
            ASR::IntrinsicArrayFunction_t* iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(xx.m_value);
            if ( iaf->m_value != nullptr ) {
                xx.m_value = iaf->m_value;
            }
        }
        if ( ASR::is_a<ASR::ArrayReshape_t>(*xx.m_value) ) {
            ASR::ArrayReshape_t* ar = ASR::down_cast<ASR::ArrayReshape_t>(xx.m_value);
            if ( ar->m_value != nullptr ) {
                xx.m_value = ar->m_value;
            }
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*xx.m_value)) {
            ASR::StructInstanceMember_t* member =
                ASR::down_cast<ASR::StructInstanceMember_t>(xx.m_value);
            if (member->m_value != nullptr &&
                    is_parameter_designator(member->m_v) &&
                    ASRUtils::is_array(ASRUtils::expr_type(member->m_value)) &&
                    ASRUtils::is_value_constant(member->m_value)) {
                xx.m_value = member->m_value;
            }
        }
        if( !ASRUtils::is_array(ASRUtils::expr_type(xx.m_target)) ||
            std::find(skip_exprs.begin(), skip_exprs.end(), xx.m_value->type) != skip_exprs.end() ||
            (ASRUtils::is_simd_array(xx.m_target) && ASRUtils::is_simd_array(xx.m_value)) ) {
            return ;
        }
        bool is_target_assumed_rank = (ASR::is_a<ASR::ArrayPhysicalCast_t>(*xx.m_target) && 
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(xx.m_target)->m_old == ASR::array_physical_typeType::AssumedRankArray) 
            || ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(xx.m_target));
        bool is_value_assumed_rank = (ASR::is_a<ASR::ArrayPhysicalCast_t>(*xx.m_value) && 
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(xx.m_value)->m_old == ASR::array_physical_typeType::AssumedRankArray)
            || ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(xx.m_value));
        xx.m_value = ASRUtils::get_past_array_broadcast(xx.m_value);
        const Location loc = x.base.base.loc;

        #define is_array_indexed_with_array_indices_check(expr) \
            (ASR::is_a<ASR::ArraySection_t>(*expr) || ( \
            ASR::is_a<ASR::ArrayItem_t>(*expr) && \
            ASRUtils::is_array_indexed_with_array_indices( \
                ASR::down_cast<ASR::ArrayItem_t>(expr))))
        // A section target with scalar and triplet subscripts only, such as
        // `a(:, i)`, is assigned element by element through its base array
        // in the general loop below, once every section in the value has
        // been replaced by a temporary.
        bool is_target_section_indexed_through_base = false;
        if( ASR::is_a<ASR::ArraySection_t>(*xx.m_target) &&
            !ASRUtils::is_array_indexed_with_array_indices(
                ASR::down_cast<ASR::ArraySection_t>(xx.m_target)) &&
            !ASR::is_a<ASR::BitCast_t>(*xx.m_value) &&
            !is_array_indexed_with_array_indices_check(xx.m_value) &&
            !call_replace_on_expr(xx.m_value->type) &&
            !ASR::is_a<ASR::ImpliedDoLoop_t>(*xx.m_value) ) {
            ArraySectionFinder value_section_finder;
            value_section_finder.visit_expr(*xx.m_value);
            is_target_section_indexed_through_base = !value_section_finder.found;
        }
        if( ( is_array_indexed_with_array_indices_check(xx.m_value) ) ||
            ( is_array_indexed_with_array_indices_check(xx.m_target) &&
              !is_target_section_indexed_through_base ) ) {
            // For an allocatable LHS that needs (re)allocation, emit
            // an explicit ReAlloc using the value's per-dimension sizes
            // (computed from the vector subscripts / section bounds)
            // before generating the element-wise loop, so the loop can
            // index into a buffer of the correct size.
            if ((realloc_lhs ||
                 xx.m_realloc_lhs ||
                 should_auto_realloc_component_assignment(xx.m_target)) &&
                ASR::is_a<ASR::Var_t>(*xx.m_target) &&
                ASRUtils::is_allocatable(ASRUtils::expr_type(xx.m_target)) &&
                ASRUtils::is_array(ASRUtils::expr_type(xx.m_value)) &&
                is_array_indexed_with_array_indices_check(xx.m_value)) {
                insert_realloc_for_indexed_value(xx.m_target, xx.m_value, loc);
            }
            generate_loop_for_array_indexed_with_array_indices(
                x, &(xx.m_target), &(xx.m_value), loc);
            return ;
        }

        if( call_replace_on_expr(xx.m_value->type) ||
            ASR::is_a<ASR::ImpliedDoLoop_t>(*xx.m_value) ) {
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*xx.m_value)) {
                ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(xx.m_value);
                Vec<ASR::expr_t*> args;
                args.reserve(al, 1);
                args.push_back(al, xx.m_value);
                xx.m_value = ASRUtils::EXPR(ASR::make_ArrayConstructor_t(
                    al, idl->base.base.loc, args.p, args.size(),
                    idl->m_type, nullptr, ASR::arraystorageType::ColMajor, nullptr));
            }
            // The constructor is stored into the variable element by
            // element, so the variable is checked first to be allocated and
            // to have the shape of the constructor, as for a whole array,
            // unless it is reallocated to the constructor
            ASR::expr_t* checked_constructor = nullptr;
            if (bounds_checking && !xx.m_move_allocation &&
                    ASR::is_a<ASR::ArrayConstructor_t>(*xx.m_value) &&
                    !(ASRUtils::is_allocatable(ASRUtils::expr_type(xx.m_target)) &&
                      (realloc_lhs || xx.m_realloc_lhs ||
                       should_auto_realloc_component_assignment(xx.m_target)))) {
                checked_constructor = get_constructor_with_size(
                    ASR::down_cast<ASR::ArrayConstructor_t>(xx.m_value));
            }
            replacer.result_expr = xx.m_target;
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&xx.m_value);
            this->call_replacer();
            current_expr = current_expr_copy;
            replacer.result_expr = nullptr;
            if (checked_constructor) {
                ASRUtils::ExprStmtDuplicator expr_duplicator(al);
                Vec<ASR::stmt_t*> stmts;
                stmts.reserve(al, pass_result.size() + 1);
                Vec<ASR::expr_t*> components;
                components.reserve(al, 1);
                components.push_back(al, checked_constructor);
                stmts.push_back(al, PassUtils::make_DebugCheckArrayBounds(
                    al, x.base.base.loc, expr_duplicator.duplicate_expr(xx.m_target),
                    components, false));
                if (pass_result.size() == 0 && !remove_original_stmt) {
                    stmts.push_back(al, const_cast<ASR::stmt_t*>(&x.base));
                }
                for (size_t i = 0; i < pass_result.size(); i++) {
                    stmts.push_back(al, pass_result[i]);
                }
                pass_result.from_pointer_n_copy(al, stmts.p, stmts.size());
            }
            return ;
        }

        // Pre-evaluate any BitCast (transfer) nodes with array results
        // that are nested inside the value expression. BitCast is a
        // whole-array operation and must not be expanded element-wise.
        // We materialize them into temporaries before the loop so that
        // the loop can index into the temporary instead.
        // Skip when the value IS a BitCast directly — that case is
        // handled specially below.
        if (!ASR::is_a<ASR::BitCast_t>(*xx.m_value)) {
            BitCastArrayVisitor bc_visitor(al);
            bc_visitor.set_scope(current_scope);
            bc_visitor.init_bc_stmts();
            bc_visitor.current_expr = const_cast<ASR::expr_t**>(&(xx.m_value));
            bc_visitor.call_replacer();
            // Lower each `tmp = BitCast(...)` assignment through visit_stmt
            // so it goes through the full array_op lowering (element-wise
            // loop expansion for char BitCast, memcpy for mismatched sizes, etc.)
            Vec<ASR::stmt_t*>& bc_stmts = bc_visitor.get_bc_stmts();
            for (size_t i = 0; i < bc_stmts.size(); i++) {
                Vec<ASR::stmt_t*> saved_pass_result;
                saved_pass_result.from_pointer_n_copy(al,
                    pass_result.p, pass_result.size());
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                visit_stmt(*bc_stmts[i]);
                if (pass_result.size() > 0) {
                    for (size_t j = 0; j < pass_result.size(); j++) {
                        saved_pass_result.push_back(al, pass_result[j]);
                    }
                } else {
                    saved_pass_result.push_back(al, bc_stmts[i]);
                }
                pass_result.from_pointer_n_copy(al,
                    saved_pass_result.p, saved_pass_result.size());
            }
        }

        Vec<ASR::expr_t**> vars;
        vars.reserve(al, 1);
        ArrayVarAddressCollector var_collector_target(al, vars);
        var_collector_target.current_expr = const_cast<ASR::expr_t**>(&(xx.m_target));
        if (is_target_section_indexed_through_base) {
            vars.push_back(al, const_cast<ASR::expr_t**>(&(xx.m_target)));
        } else if (!is_target_assumed_rank) {
            var_collector_target.call_replacer();
        } else {
            vars.push_back(al, const_cast<ASR::expr_t**>(&(xx.m_target)));
        }
        ArrayVarAddressCollector var_collector_value(al, vars);
        var_collector_value.current_expr = const_cast<ASR::expr_t**>(&(xx.m_value));
        if (!is_value_assumed_rank) {
            var_collector_value.call_replacer();
        } else {
            vars.push_back(al, const_cast<ASR::expr_t**>(&(xx.m_value)));
        }

        // Collect array variables from overloaded SubroutineCall arguments
        // so they are replaced with indexed ArrayItem nodes in generate_loop
        if (xx.m_overloaded != nullptr &&
                ASR::is_a<ASR::SubroutineCall_t>(*xx.m_overloaded)) {
            ASR::SubroutineCall_t* sc = ASR::down_cast<ASR::SubroutineCall_t>(
                xx.m_overloaded);
            if (ASRUtils::is_elemental(sc->m_name)) {
                for (size_t i = 0; i < sc->n_args; i++) {
                    if (sc->m_args[i].m_value != nullptr &&
                            ASRUtils::is_array(ASRUtils::expr_type(
                                sc->m_args[i].m_value))) {
                        vars.push_back(al, &(sc->m_args[i].m_value));
                    }
                }
            }
        }

        if (ASRUtils::is_array(ASRUtils::expr_type(xx.m_value))) {
            bool per_assign_realloc = xx.m_realloc_lhs ||
                should_auto_realloc_component_assignment(xx.m_target);
            insert_realloc_for_target(xx.m_target, xx.m_value, vars, per_assign_realloc);
        }

        if (vars.size() == 1 && !is_looping_necessary_for_bitcast(xx.m_value) && 
            ASRUtils::is_array(ASRUtils::expr_type(ASRUtils::get_past_array_broadcast(xx.m_value)))
        ) {
            if (pass_result.size() > 0) {
                pass_result.push_back(al, const_cast<ASR::stmt_t*>(&(x.base)));
            }
            return ;
        }

        // Operands whose shape is known to conform with the target before
        // the loop runs, either by the shape check below or at compile time
        std::set<ASR::expr_t*> conforming_operands;
        if (bounds_checking && 
            ASRUtils::is_array(ASRUtils::expr_type(x.m_target)) &&
            ASRUtils::is_array(ASRUtils::expr_type(x.m_value)) &&
            ASRUtils::get_expr_size_expr(x.m_target) != nullptr) {
            Vec<ASR::expr_t*> components;
            components.reserve(al, 1);
            CollectComponentsFromElementalExpr collect_components(al, components);
            collect_components.visit_expr(*x.m_value);
            bool conforms_at_compile_time = false;
            if (have_same_rank(x.m_target, components)) {
                conforming_operands.insert(x.m_target);
                for (size_t i = 0; i < components.size(); i++) {
                    conforming_operands.insert(components[i]);
                    conforming_operands.insert(
                        ASRUtils::get_past_array_physical_cast(components[i]));
                }
                conforms_at_compile_time = have_same_compile_time_shape(
                    x.m_target, components);
            }

            ASRUtils::ExprStmtDuplicator expr_duplicator(al);
            ASR::expr_t* d_target = expr_duplicator.duplicate_expr(x.m_target);
            Vec<ASR::expr_t*> vars;
            vars.reserve(al, components.size());
            for (size_t i = 0; i < components.size(); i++) {
                ASR::expr_t* component = components[i];
                // A component of a named constant is checked through its
                // constant value, which is how this pass reads it
                if (ASR::is_a<ASR::StructInstanceMember_t>(*component)) {
                    ASR::StructInstanceMember_t* member =
                        ASR::down_cast<ASR::StructInstanceMember_t>(component);
                    if (member->m_value != nullptr &&
                            is_parameter_designator(member->m_v) &&
                            ASRUtils::is_array(ASRUtils::expr_type(member->m_value)) &&
                            ASRUtils::is_value_constant(member->m_value)) {
                        component = member->m_value;
                    }
                }
                vars.push_back(al, expr_duplicator.duplicate_expr(component));
            }

            if (!conforms_at_compile_time &&
                    debug_inserted.find(&x) == debug_inserted.end()) {
                pass_result.push_back(al, PassUtils::make_DebugCheckArrayBounds(al,
                    x.base.base.loc, d_target, vars, x.m_move_allocation));
                if (!x.m_move_allocation) {
                    debug_inserted.insert(&x);
                }
            }
        }

        // Don't generate a loop for a move assignment
        // The assignment should be handled in the backend
        if (x.m_move_allocation) {
            if (ASRUtils::is_allocatable(ASRUtils::expr_type(x.m_target)) && vars.size() != 1) {
                // ReAlloc was inserted for an allocatable target in previous part,
                // but the move assignment overwrites target.data
                // with value.data — leaking the buffer ReAlloc just allocated.
                // Free it first to avoid this memory leak. 
                // Note: This fix is temporary. This fix should be in backend, where 
                // while assigning to any allocatable variable target, backend should 
                // deallocate target first to free any heap allocation (malloc) used
                // to store in the target variable
                Vec<ASR::expr_t*> dealloc_vars;
                dealloc_vars.reserve(al, 1); 
                dealloc_vars.push_back(al, const_cast<ASR::expr_t*>(x.m_target));
                pass_result.push_back(al, ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(al, loc,
                    dealloc_vars.p, dealloc_vars.size())));
            } 
            ASR::stmt_t* stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, x.m_target, x.m_value, x.m_overloaded, x.m_realloc_lhs, x.m_move_allocation, x.m_new_target, x.m_new_allocation));
            pass_result.push_back(al, stmt);
            debug_inserted.insert(ASR::down_cast<ASR::Assignment_t>(stmt));
            return;
        }

        if (ASR::is_a<ASR::BitCast_t>(*xx.m_value)) {
            ASR::BitCast_t* bc = ASR::down_cast<ASR::BitCast_t>(xx.m_value);
            ASR::ttype_t* src_type = ASRUtils::expr_type(bc->m_source);

            if (lower_character_bitcast(x, bc, loc)) {
                return;
            }

            if (bc->m_size != nullptr &&
                ASRUtils::is_array(src_type) &&
                !ASRUtils::is_string_only(src_type)) {
                ASR::stmt_t* stmt = ASRUtils::STMT(
                    ASRUtils::make_Assignment_t_util(al, loc,
                        x.m_target, x.m_value, x.m_overloaded,
                        x.m_realloc_lhs, x.m_move_allocation, x.m_new_target, x.m_new_allocation));
                pass_result.push_back(al, stmt);
                return;
            }

            // Don't generate an element-wise loop for transfer() (BitCast) when
            // source and result are arrays with different element byte sizes.
            // The element counts differ so the loop indices would go out
            // of bounds on the source array. Leave the whole-array BitCast for
            // the LLVM backend to lower as a memcpy.
            if (ASRUtils::is_array(src_type) && ASRUtils::is_array(bc->m_type)) {
                int64_t src_size = ASRUtils::get_type_byte_size(
                    ASRUtils::extract_type(src_type));
                int64_t res_size = ASRUtils::get_type_byte_size(
                    ASRUtils::extract_type(bc->m_type));
                if (src_size != res_size) {
                    pass_result.push_back(al,
                        const_cast<ASR::stmt_t*>(&(x.base)));
                    return;
                }
            }
        }

        Vec<ASR::expr_t**> fix_type_args;
        fix_type_args.reserve(al, 2);
        fix_type_args.push_back(al, const_cast<ASR::expr_t**>(&(xx.m_target)));
        fix_type_args.push_back(al, const_cast<ASR::expr_t**>(&(xx.m_value)));
        generate_loop(x, vars, fix_type_args, loc, nullptr, &conforming_operands);
    }

    void visit_IntrinsicImpureSubroutine(const ASR::IntrinsicImpureSubroutine_t &x) {

        if( (int64_t)ASRUtils::IntrinsicImpureSubroutines::Mvbits != x.m_sub_intrinsic_id ){
            return; // If want to check more functions, Use an array + loop
        } 

        Vec<ASR::expr_t**> vars;
        vars.reserve(al, 1);
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i] && ASRUtils::is_array(ASRUtils::expr_type(x.m_args[i]))) {
                vars.push_back(al, &(x.m_args[i]));
            }
        }

        if (vars.size() == 0) {
            return;
        }

        Vec<ASR::expr_t**> fix_type_args;
        fix_type_args.reserve(al, 1);

        generate_loop(x, vars, fix_type_args, x.base.base.loc);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        if( !ASRUtils::is_elemental(x.m_name) ) {
            return ;
        }
        const Location loc = x.base.base.loc;

        Vec<ASR::expr_t**> vars;
        vars.reserve(al, 1);
        for( size_t i = 0; i < x.n_args; i++ ) {
            if( x.m_args[i].m_value != nullptr &&
                ASRUtils::is_array(ASRUtils::expr_type(x.m_args[i].m_value)) ) {
                vars.push_back(al, &(x.m_args[i].m_value));
            }
        }

        // If x.m_dt is an array, and the subroutine is elemental, we need to replace the m_dt with an array item
        // and call the elemental function on each element of the x.m_dt array not on the whole array
        if (x.m_dt && ASRUtils::is_array(ASRUtils::expr_type(x.m_dt))) {
            ASR::SubroutineCall_t& xx = const_cast<ASR::SubroutineCall_t&>(x);
            vars.push_back(al, &(xx.m_dt));
        }

        if( vars.size() == 0 ) {
            return ;
        }

        Vec<ASR::expr_t**> fix_type_args;
        fix_type_args.reserve(al, 1);

        generate_loop(x, vars, fix_type_args, loc);
    }

    void visit_If(const ASR::If_t& x) {
        if( !ASRUtils::is_array(ASRUtils::expr_type(x.m_test)) ) {
            ASR::CallReplacerOnExpressionsVisitor<ArrayOpVisitor>::visit_If(x);
            return ;
        }

        const Location loc = x.base.base.loc;

        Vec<ASR::expr_t**> vars;
        vars.reserve(al, 1);
        ArrayVarAddressCollector array_var_adress_collector_target(al, vars);
        array_var_adress_collector_target.visit_If(x);

        if( vars.size() == 0 ) {
            return ;
        }

        std::vector<ASR::expr_t*> scalar_targets;
        collect_array_if_scalar_targets(x.m_body, x.n_body, scalar_targets);
        collect_array_if_scalar_targets(x.m_orelse, x.n_orelse, scalar_targets);

        Vec<ASR::expr_t**> fix_type_args;
        fix_type_args.reserve(al, 1);

        generate_loop(x, vars, fix_type_args, loc, &scalar_targets);

        ASRUtils::RemoveArrayProcessingNodeVisitor remove_array_processing_node_visitor(al);
        remove_array_processing_node_visitor.visit_If(x);

        FixTypeVisitor fix_type_visitor(al);
        fix_type_visitor.current_scope = current_scope;
        fix_type_visitor.visit_If(x);
    }

};

void pass_replace_array_op(Allocator &al, ASR::TranslationUnit_t &unit,
                           const LCompilers::PassOptions& pass_options) {
    ArrayOpVisitor v(al, pass_options);
    v.call_replacer_on_value = false;
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
