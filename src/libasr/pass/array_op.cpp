#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_array_op.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <libasr/asr_builder.h>

#include <vector>

namespace LCompilers {

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

    void replace_ArrayItem(ASR::ArrayItem_t* /*x*/) {
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
        if( !ASRUtils::is_array(ASRUtils::expr_type(x.m_args[0])) ) {
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
        if( (x.m_dt && !ASRUtils::is_array(ASRUtils::expr_type(x.m_dt))) ||
            !ASRUtils::is_array(ASRUtils::expr_type(x.m_args[0].m_value)) ) {
            xx.m_type = ASRUtils::extract_type(xx.m_type);
            xx.m_value = nullptr;
        }
    }

    template <typename T>
    void visit_ArrayOp(const T& x) {
        T& xx = const_cast<T&>(x);
        if( !ASRUtils::is_array(ASRUtils::expr_type(xx.m_left)) &&
            !ASRUtils::is_array(ASRUtils::expr_type(xx.m_right)) ) {
            xx.m_type = ASRUtils::extract_type(xx.m_type);
            xx.m_value = nullptr;
        }
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
        int repeat = 1;
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
        ASR::dimension_t* m_dims = nullptr;
        ASRUtils::extract_dimensions_from_ttype(arr_type, m_dims);

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
            array_index_arg.m_right = builder.Add(m_dims[0].m_start, make_ConstantWithKind(
                make_IntegerConstant_t, make_Integer_t, i, 4, loc));
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
        void visit_ArrayItem(const ASR::ArrayItem_t&) {}
        void visit_ArraySize(const ASR::ArraySize_t&) {}
        void visit_ArrayReshape(const ASR::ArrayReshape_t&) {}
        void visit_ArrayBound(const ASR::ArrayBound_t&) {}

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
        Also handles arraysections on LHS and RHS, eg:
            write(x(i:j),"format") (a(k:m))
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

        const Location& loc = x.base.base.loc;
        int ikind = get_index_kind();
        ASR::ttype_t* int_type = get_index_type(loc);

        // Extract unit base and bounds
        ASR::expr_t* unit = x.m_unit;
        ASR::expr_t* lb_lhs = PassUtils::get_bound(x.m_unit, 1, "lbound", al, ikind);
        ASR::expr_t* ub_lhs = PassUtils::get_bound(x.m_unit, 1, "ubound", al, ikind);
        // For array-sections, use bounds passed as args
        if (ASR::is_a<ASR::ArraySection_t>(*x.m_unit)) {
            ASR::ArraySection_t* sec = ASR::down_cast<ASR::ArraySection_t>(x.m_unit);
            unit = sec->m_v;
            if (sec->m_args[0].m_left){
                lb_lhs = sec->m_args[0].m_left;
            }
            if (sec->m_args[0].m_right) {
                ub_lhs = sec->m_args[0].m_right;
            }
        }
        
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
            al, loc, fmt->m_fmt, fmt_args.p, 1, fmt->m_kind, fmt->m_type, fmt->m_value));
        Vec<ASR::expr_t*> inner_vals; inner_vals.reserve(al, 1);
        inner_vals.push_back(al, fmt_i);
        ASR::stmt_t* inner_write = ASRUtils::STMT(ASR::make_FileWrite_t(
            al, loc, x.m_label, unit_i,
            x.m_iomsg, x.m_iostat, x.m_id,
            inner_vals.p, 1,
            x.m_separator, x.m_end, x.m_overloaded,
            x.m_is_formatted, x.m_nml, x.m_rec));

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
                                   Vec<ASR::stmt_t*>& do_loop_body, const Location& loc) {
        ASR::expr_t* step = make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, get_index_kind(), loc);
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
            ASR::expr_t* lbound = PassUtils::get_bound(vars_expr[i],
                loop_depth + 1, "lbound", al, get_index_kind());
            ASR::stmt_t* set_index_var = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                al, loc, index_var, lbound, nullptr, false, false));
            dest_vec.push_back(al, set_index_var);
        }
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
            size_t& j, ASR::ttype_t* int32_type) {
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
                        array_index.m_right = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                            m_args[i].m_right, indices1.p, 1, int32_type,
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
            var_rank, loc, var2indices, j, index_type);
        create_array_item_array_indexed_expr(
            value, value_address, is_value_array, 1,
            var_rank, loc, var2indices, j, index_type);

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
                                  do_loop_body, loc);
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
                                      do_loop_body, loc);
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
                       const Location& loc) {
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

        for( size_t i = 0; i < vars.size(); i++ ) {
            Vec<ASR::array_index_t> indices;
            indices.reserve(al, var_ranks[i]);
            for( size_t j = 0; j < var_ranks[i]; j++ ) {
                ASR::array_index_t array_index;
                array_index.loc = loc;
                array_index.m_left = nullptr;
                array_index.m_right = var2indices[i][j];
                array_index.m_step = nullptr;
                indices.push_back(al, array_index);
            }
            ASR::ttype_t* var_i_type = ASRUtils::extract_type(
                ASRUtils::expr_type(*vars[i]));
            *vars[i] = ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc, *vars[i], indices.p,
                indices.size(), var_i_type, ASR::arraystorageType::ColMajor, nullptr));
        }

        ASRUtils::RemoveArrayProcessingNodeVisitor array_broadcast_visitor(al);
        for( size_t i = 0; i < fix_types_args.size(); i++ ) {
            array_broadcast_visitor.current_expr = fix_types_args[i];
            array_broadcast_visitor.call_replacer();
        }

        FixTypeVisitor fix_types(al);
        fix_types.current_scope = current_scope;
        for( size_t i = 0; i < fix_types_args.size(); i++ ) {
            fix_types.visit_expr(*(*fix_types_args[i]));
        }

        size_t var_with_maxrank = 0;
        for( size_t i = 0; i < var_ranks.size(); i++ ) {
            if( var_ranks[i] > var_ranks[var_with_maxrank] ) {
                var_with_maxrank = i;
            }
        }

        ASR::do_loop_head_t do_loop_head;
        do_loop_head.loc = loc;
        do_loop_head.m_v = var2indices[var_with_maxrank].p[0];
        do_loop_head.m_start = PassUtils::get_bound(vars_expr[var_with_maxrank],
            1, "lbound", al, get_index_kind());
        do_loop_head.m_end = PassUtils::get_bound(vars_expr[var_with_maxrank],
            1, "ubound", al, get_index_kind());
        do_loop_head.m_increment = nullptr;
        Vec<ASR::stmt_t*> parent_do_loop_body; parent_do_loop_body.reserve(al, 1);
        Vec<ASR::stmt_t*> do_loop_body; do_loop_body.reserve(al, 1);
        set_index_variables(var2indices, vars_expr, var_with_maxrank,
                            0, parent_do_loop_body, loc);
        do_loop_body.push_back(al, const_cast<ASR::stmt_t*>(&(x.base)));
        increment_index_variables(var2indices, var_with_maxrank, 0,
                                  do_loop_body, loc);
        ASR::stmt_t* do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
            do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
        parent_do_loop_body.push_back(al, do_loop);
        do_loop_body.from_pointer_n_copy(al, parent_do_loop_body.p, parent_do_loop_body.size());
        parent_do_loop_body.reserve(al, 1);

        for( int64_t i = 1; i < static_cast<int64_t>(var_ranks[var_with_maxrank]); i++ ) {
            set_index_variables(var2indices, vars_expr, var_with_maxrank,
                                i, parent_do_loop_body, loc);
            increment_index_variables(var2indices, var_with_maxrank, i,
                                      do_loop_body, loc);
            ASR::do_loop_head_t do_loop_head;
            do_loop_head.loc = loc;
            do_loop_head.m_v = var2indices[var_with_maxrank].p[i];
            do_loop_head.m_start = PassUtils::get_bound(
                vars_expr[var_with_maxrank], i + 1, "lbound", al, get_index_kind());
            do_loop_head.m_end = PassUtils::get_bound(
                vars_expr[var_with_maxrank], i + 1, "ubound", al, get_index_kind());
            do_loop_head.m_increment = nullptr;
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

    void insert_realloc_for_target(ASR::expr_t* target, ASR::expr_t* value, Vec<ASR::expr_t**>& vars, bool per_assign_realloc = false) {
        ASR::ttype_t* target_type = ASRUtils::expr_type(target);
        bool array_copy = ASR::is_a<ASR::Var_t>(*value) && ASR::is_a<ASR::Var_t>(*target);
        if (!realloc_lhs && !per_assign_realloc) {
            return;
        }
        if( (!ASRUtils::is_allocatable(target_type) || vars.size() == 1) &&
            !(array_copy && ASRUtils::is_allocatable(target_type)) ) {
            return ;
        }

        ASRUtils::ExprStmtDuplicator d(al);
        ASR::expr_t* realloc_var = d.duplicate_expr(ASRUtils::get_expr_size_expr(value));

        Location loc; loc.first = 1, loc.last = 1;
        ASRUtils::ASRBuilder builder(al, loc);
        ASR::ttype_t* idx_type = get_index_type(loc);
        int idx_kind = get_index_kind();
        Vec<ASR::dimension_t> realloc_dims;
        size_t target_rank = ASRUtils::extract_n_dims_from_ttype(target_type);
        realloc_dims.reserve(al, target_rank);
        for( size_t i = 0; i < target_rank; i++ ) {
            ASR::dimension_t realloc_dim;
            realloc_dim.loc = loc;
            realloc_dim.m_start = PassUtils::get_bound(realloc_var, i + 1, "lbound", al, idx_kind);
            ASR::expr_t* dim_expr = make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, i + 1, idx_kind, loc);
            realloc_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                al, loc, realloc_var, dim_expr, idx_type, nullptr));
            realloc_dims.push_back(al, realloc_dim);
        }
        ASR::expr_t* realloc_str_len {};
        if(ASRUtils::is_character(*ASRUtils::expr_type(realloc_var))){
            if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*value) &&
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

        Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
        ASR::alloc_arg_t alloc_arg;
        alloc_arg.loc = loc;
        alloc_arg.m_a = target;
        alloc_arg.m_dims = realloc_dims.p;
        alloc_arg.n_dims = realloc_dims.size();
        alloc_arg.m_len_expr = realloc_str_len;
        alloc_arg.m_type = nullptr;
        alloc_arg.m_sym_subclass = nullptr;
        alloc_args.push_back(al, alloc_arg);

        pass_result.push_back(al, ASRUtils::STMT(ASR::make_ReAlloc_t(
                al, loc, alloc_args.p, alloc_args.size())));
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

    void visit_Allocate(const ASR::Allocate_t& x) {
        ASR::Allocate_t& xx = const_cast<ASR::Allocate_t&>(x);
        if (xx.m_source) {
            pass_result.push_back(al, &(xx.base));
            bool generated_loop = false;
            // Pushing assignment statements to source
            for (size_t i = 0; i < x.n_args ; i++) {
                if( !ASRUtils::is_array(
                        ASRUtils::expr_type(x.m_args[i].m_a)) ) {
                    continue;
                }
                ASRUtils::ExprStmtDuplicator duplicator(al);
                ASR::expr_t* source_copy = duplicator.duplicate_expr(xx.m_source);
                ASR::stmt_t* assign = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, xx.m_args[i].m_a->base.loc, xx.m_args[i].m_a, source_copy, nullptr, realloc_lhs, false));
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
            }
            // Clear source when data copy loops were generated, since
            // the loops handle the copy. Keeping array-typed source
            // expressions (e.g. real(a)) would cause backend errors.
            if (generated_loop) {
                xx.m_source = nullptr;
            }
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


    void visit_Assignment(const ASR::Assignment_t& x) {
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
            ASR::is_a<ASR::ArraySection_t>(*expr) || ( \
            ASR::is_a<ASR::ArrayItem_t>(*expr) && \
            ASRUtils::is_array_indexed_with_array_indices( \
                ASR::down_cast<ASR::ArrayItem_t>(expr)))
        if( ( is_array_indexed_with_array_indices_check(xx.m_value) ) ||
            ( is_array_indexed_with_array_indices_check(xx.m_target) ) ) {
            generate_loop_for_array_indexed_with_array_indices(
                x, &(xx.m_target), &(xx.m_value), loc);
            return ;
        }

        if( call_replace_on_expr(xx.m_value->type) ) {
            replacer.result_expr = xx.m_target;
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&xx.m_value);
            this->call_replacer();
            current_expr = current_expr_copy;
            replacer.result_expr = nullptr;
            return ;
        }

        Vec<ASR::expr_t**> vars;
        vars.reserve(al, 1);
        ArrayVarAddressCollector var_collector_target(al, vars);
        var_collector_target.current_expr = const_cast<ASR::expr_t**>(&(xx.m_target));
        if (!is_target_assumed_rank) {
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

        if (vars.size() == 1 && !is_looping_necessary_for_bitcast(xx.m_value) && 
            ASRUtils::is_array(ASRUtils::expr_type(ASRUtils::get_past_array_broadcast(xx.m_value)))
        ) {
            return ;
        }

        if ((bounds_checking || !pass_options.realloc_lhs_arrays_silent) &&
            ASRUtils::is_array(ASRUtils::expr_type(x.m_target)) &&
            ASRUtils::is_array(ASRUtils::expr_type(x.m_value))) {
            ASRUtils::ExprStmtDuplicator expr_duplicator(al);
            ASR::expr_t* d_target = expr_duplicator.duplicate_expr(x.m_target);
            ASR::expr_t* d_value = expr_duplicator.duplicate_expr(x.m_value);

            Vec<ASR::expr_t*> vars;
            vars.reserve(al, 1);

            CollectComponentsFromElementalExpr cv(al, vars);
            cv.visit_expr(*d_value);

            if (debug_inserted.find(&x) == debug_inserted.end()) {
                pass_result.push_back(al, ASRUtils::STMT(ASR::make_DebugCheckArrayBounds_t(al, x.base.base.loc, d_target, vars.p, vars.n, x.m_move_allocation)));
                if (!x.m_move_allocation) {
                    debug_inserted.insert(&x);
                }
            }
        }

        if (ASRUtils::is_array(ASRUtils::expr_type(xx.m_value))) {
            bool per_assign_realloc = xx.m_realloc_lhs ||
                should_auto_realloc_component_assignment(xx.m_target);
            insert_realloc_for_target(xx.m_target, xx.m_value, vars, per_assign_realloc);
        }
        // Don't generate a loop for a move assignment
        // The assignment should be handled in the backend
        if (x.m_move_allocation) {
            ASR::stmt_t* stmt = ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc, x.m_target, x.m_value, x.m_overloaded, x.m_realloc_lhs, x.m_move_allocation));
            pass_result.push_back(al, stmt);
            debug_inserted.insert(ASR::down_cast<ASR::Assignment_t>(stmt));
            return;
        }

        Vec<ASR::expr_t**> fix_type_args;
        fix_type_args.reserve(al, 2);
        fix_type_args.push_back(al, const_cast<ASR::expr_t**>(&(xx.m_target)));
        fix_type_args.push_back(al, const_cast<ASR::expr_t**>(&(xx.m_value)));
        generate_loop(x, vars, fix_type_args, loc);
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

        Vec<ASR::expr_t**> fix_type_args;
        fix_type_args.reserve(al, 1);

        generate_loop(x, vars, fix_type_args, loc);

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
