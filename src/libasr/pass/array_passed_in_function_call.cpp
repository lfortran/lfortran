#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_array_passed_in_function_call.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_subroutine_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pickle.h>

#include <set>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

/*
This pass collector that the BinOp only Var nodes and nothing else.
*/
class ArrayVarCollector: public ASR::BaseWalkVisitor<ArrayVarCollector> {
    private:

    Allocator& al;
    Vec<ASR::expr_t*>& vars;

    public:

    ArrayVarCollector(Allocator& al_, Vec<ASR::expr_t*>& vars_): al(al_), vars(vars_) {}

    void visit_Var(const ASR::Var_t& x) {
        if( ASRUtils::is_array(ASRUtils::symbol_type(x.m_v)) ) {
            vars.push_back(al, const_cast<ASR::expr_t*>(&(x.base)));
        }
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t& x) {
        if( ASRUtils::is_array(ASRUtils::symbol_type(x.m_m)) ) {
            vars.push_back(al, const_cast<ASR::expr_t*>(&(x.base)));
        }
    }

    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t& /*x*/) {

    }

    void visit_ArraySize(const ASR::ArraySize_t& /*x*/) {

    }

};

void transform_stmts_impl(Allocator& al, ASR::stmt_t**& m_body, size_t& n_body,
    Vec<ASR::stmt_t*>*& current_body, Vec<ASR::stmt_t*>*& body_after_curr_stmt,
    std::function<void(const ASR::stmt_t&)> visit_stmt) {
    Vec<ASR::stmt_t*>* current_body_copy = current_body;
    Vec<ASR::stmt_t*> current_body_vec; current_body_vec.reserve(al, 1);
    current_body_vec.reserve(al, n_body);
    current_body = &current_body_vec;
    for (size_t i = 0; i < n_body; i++) {
        Vec<ASR::stmt_t*>* body_after_curr_stmt_copy = body_after_curr_stmt;
        Vec<ASR::stmt_t*> body_after_curr_stmt_vec; body_after_curr_stmt_vec.reserve(al, 1);
        body_after_curr_stmt = &body_after_curr_stmt_vec;
        visit_stmt(*m_body[i]);
        current_body->push_back(al, m_body[i]);
        for (size_t j = 0; j < body_after_curr_stmt_vec.size(); j++) { 
            current_body->push_back(al, body_after_curr_stmt_vec[j]);
        }
        body_after_curr_stmt = body_after_curr_stmt_copy;
    }
    m_body = current_body_vec.p; n_body = current_body_vec.size();
    current_body = current_body_copy;
}

/*
    This pass is responsible to convert non-contiguous ( DescriptorArray, arrays with stride != 1  )
    arrays passed to functions by casting to contiguous ( PointerArray ) arrays.

    For example:

    subroutine matprod(y)
        real(8), intent(inout) :: y(:, :)
        call istril(y)
    end subroutine 

    gets converted to:

    subroutine matprod(y)
        real(8), intent(inout) :: y(:, :)
        real(8), pointer :: y_tmp(:, :)
        if (.not. is_contiguous(y))
            allocate(y_tmp(size(y, 1), size(y, 2)))
            y_tmp = y
        else
            y_tmp => y
        end if
        call istril(y_tmp)
        if (.not. is_contiguous(y)) ! only if intent is inout, out
            y = y_tmp
            deallocate(y_tmp)
        end if
    end subroutine
*/
/*
Records the rank of the assumed-rank selector in each `rank (N)` block of a
select rank statement without an associate name: in the block the selector
keeps its assumed-rank type, and this rank is the only record of it.
*/
class SelectRankBlockCollector: public ASR::BaseWalkVisitor<SelectRankBlockCollector> {
    public:

    // The block scope, mapped to the selector and its rank in the block.
    std::map<SymbolTable*, std::pair<ASR::symbol_t*, int64_t>> blocks;

    void visit_SelectRank(const ASR::SelectRank_t& x) {
        ASR::expr_t* selector = ASRUtils::get_past_array_physical_cast(x.m_selector);
        if ( ASR::is_a<ASR::Var_t>(*selector) &&
             ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(selector)) ) {
            ASR::symbol_t* selector_sym = ASR::down_cast<ASR::Var_t>(selector)->m_v;
            for ( size_t i = 0; i < x.n_body; i++ ) {
                if ( !ASR::is_a<ASR::RankExpr_t>(*x.m_body[i]) ) {
                    continue;
                }
                ASR::RankExpr_t* rank_expr = ASR::down_cast<ASR::RankExpr_t>(x.m_body[i]);
                int64_t rank = -1;
                if ( !ASRUtils::extract_value(ASRUtils::expr_value(rank_expr->m_rank), rank) ) {
                    continue;
                }
                for ( size_t j = 0; j < rank_expr->n_body; j++ ) {
                    if ( ASR::is_a<ASR::BlockCall_t>(*rank_expr->m_body[j]) ) {
                        ASR::symbol_t* block = ASR::down_cast<ASR::BlockCall_t>(
                            rank_expr->m_body[j])->m_m;
                        if ( ASR::is_a<ASR::Block_t>(*block) ) {
                            blocks[ASR::down_cast<ASR::Block_t>(block)->m_symtab] =
                                {selector_sym, rank};
                        }
                    }
                }
            }
        }
        ASR::BaseWalkVisitor<SelectRankBlockCollector>::visit_SelectRank(x);
    }
};

class CallVisitor : public ASR::CallReplacerOnExpressionsVisitor<CallVisitor>
{
public:

    Allocator &al;
    int is_current_body_set = 0;
    Vec<ASR::stmt_t*>* current_body;
    Vec<ASR::stmt_t*>* body_after_curr_stmt;
    const LCompilers::PassOptions& pass_options;

    // Variables that were associated (by pass_array_by_data) with an
    // ArraySection that is passed without copy-in/copy-out:
    // - a section of an UnboundedPointerArray (assumed-size) array whose
    //   bounds reference its undefined UBound: the size of the copy cannot
    //   be determined;
    // - a simply contiguous section of a FixedSizeArray, such as b(2:4) or
    //   a(:, j): it is known to be contiguous at compile time, so no
    //   run-time contiguity check is needed.
    std::set<ASR::symbol_t*> vars_from_uncopied_sections;

    // See SelectRankBlockCollector.
    std::map<SymbolTable*, std::pair<ASR::symbol_t*, int64_t>> select_rank_blocks;

    // The rank of the assumed-rank variable `sym` in the innermost enclosing
    // `rank (N)` block of a select rank statement on it, or -1 if unknown.
    int64_t get_select_rank(ASR::symbol_t* sym) {
        for ( SymbolTable* scope = current_scope; scope; scope = scope->parent ) {
            auto it = select_rank_blocks.find(scope);
            if ( it != select_rank_blocks.end() && it->second.first == sym ) {
                return it->second.second;
            }
        }
        return -1;
    }

    CallVisitor(Allocator &al_, const LCompilers::PassOptions& pass_options_) : al(al_), pass_options(pass_options_) {}

    int get_index_kind() const {
        return pass_options.descriptor_index_64 ? 8 : 4;
    }

    ASR::ttype_t* get_index_type(const Location& loc) {
        return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, get_index_kind()));
    }

    ASR::expr_t* get_index_one(const Location& loc) {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, get_index_type(loc)));
    }

    ASR::expr_t* get_index_constant(const Location& loc, int64_t value) {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, value, get_index_type(loc)));
    }

    bool is_descriptor_array_casted_to_pointer_to_data( ASR::expr_t* expr ) {
        if ( ASRUtils::is_array(ASRUtils::expr_type(expr) ) &&
             ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr) ) {
            ASR::ArrayPhysicalCast_t* cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
            if ( !((cast->m_new == ASR::array_physical_typeType::PointerArray ||
                     cast->m_new == ASR::array_physical_typeType::UnboundedPointerArray) &&
                    (cast->m_old == ASR::array_physical_typeType::DescriptorArray ||
                     cast->m_old == ASR::array_physical_typeType::AssumedRankArray)) ) {
                return false;
            }
            return is_descriptor_array_to_copy(cast->m_arg, cast->m_old);
        }
        return false;
    }

    // Whether the array `inner`, a descriptor of physical type `old`, may be
    // discontiguous and can be copied to a contiguous temporary.
    bool is_descriptor_array_to_copy( ASR::expr_t* inner,
            ASR::array_physical_typeType old ) {
        // An assumed-rank array needs its rank, from select rank, to be
        // copied.
        if ( old == ASR::array_physical_typeType::AssumedRankArray &&
             !(ASR::is_a<ASR::Var_t>(*inner) &&
               get_select_rank(ASR::down_cast<ASR::Var_t>(inner)->m_v) >= 1) ) {
            return false;
        }
        // Contiguous arrays can pass their data without a copy-in temporary.
        ASR::ttype_t* inner_type = ASRUtils::expr_type(inner);
        if ( (ASRUtils::is_allocatable(inner_type) ||
                ASRUtils::is_contiguous_array_designator(inner)) &&
             !ASRUtils::is_class_type(ASRUtils::extract_type(inner_type)) ) {
            return false;
        }
        // Skip copy-in/copy-out when the inner expression is an ArraySection
        // whose source array is UnboundedPointerArray (assumed-size) and has
        // an undefined upper bound on the assumed-size dimension.
        if ( ASR::is_a<ASR::ArraySection_t>(*inner) ) {
            ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(inner);
            ASR::ttype_t* source_type = ASRUtils::expr_type(section->m_v);
            if ( ASRUtils::is_array(source_type) &&
                 ASRUtils::extract_physical_type(source_type) ==
                     ASR::array_physical_typeType::UnboundedPointerArray &&
                 has_undefined_assumed_size_bound(section) ) {
                return false;
            }
        }
        // Also skip when the inner Var was created by an earlier pass
        // (pass_array_by_data) from an ArraySection of an assumed-size
        // array or from a contiguous section of a fixed-size array.
        if ( ASR::is_a<ASR::Var_t>(*inner) ) {
            ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(inner)->m_v;
            if ( vars_from_uncopied_sections.count(sym) ) {
                return false;
            }
        }
        return true;
    }

    // Whether `expr`, the actual argument of a CONTIGUOUS assumed-shape or
    // assumed-rank dummy, is a descriptor that may be discontiguous, so it is
    // passed through a contiguous temporary (F2018 15.5.2.4), like an actual
    // argument of an explicit-shape or assumed-size dummy.
    bool is_discontiguous_actual_of_contiguous_dummy( ASR::expr_t* expr ) {
        ASR::expr_t* inner = expr;
        ASR::array_physical_typeType old = ASR::array_physical_typeType::DescriptorArray;
        if ( ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr) ) {
            ASR::ArrayPhysicalCast_t* cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
            if ( !(cast->m_old == ASR::array_physical_typeType::DescriptorArray ||
                   cast->m_old == ASR::array_physical_typeType::AssumedRankArray) ||
                 !(cast->m_new == ASR::array_physical_typeType::DescriptorArray ||
                   cast->m_new == ASR::array_physical_typeType::AssumedRankArray) ) {
                return false;
            }
            inner = cast->m_arg;
            old = cast->m_old;
        }
        ASR::ttype_t* inner_type = ASRUtils::expr_type(inner);
        if ( !ASRUtils::is_array(inner_type) ||
             ASRUtils::extract_physical_type(inner_type) != old ||
             ASRUtils::is_class_type(ASRUtils::extract_type(inner_type)) ||
             ASRUtils::is_simply_contiguous(inner) ) {
            return false;
        }
        // An absent optional dummy forwarded as the actual has no descriptor
        // to check or copy. Only an optional dummy of a bind(c) procedure is
        // still optional here: transform_optional_argument_functions makes
        // the others required, passing a placeholder array when absent.
        ASR::Variable_t* var = ASRUtils::rightmost_part_variable(inner);
        if ( var && var->m_presence == ASR::presenceType::Optional ) {
            return false;
        }
        return is_descriptor_array_to_copy(inner, old);
    }

    void transform_stmts(ASR::stmt_t**& m_body, size_t& n_body) {
        is_current_body_set++;
        transform_stmts_impl(al, m_body, n_body, current_body, body_after_curr_stmt,
            [this](const ASR::stmt_t& stmt) { visit_stmt(stmt); });
        is_current_body_set--;
    }

    /*
        The copy-out and cleanup of the temporaries for a call go to
        body_after_curr_stmt, after the statement. For a statement that
        holds other statements, or jumps, that is too late when the call is
        in its test or loop bounds: the block would see the old values of
        the actual, and EXIT, CYCLE, RETURN or a jump would skip the
        copy-out. So the expression is evaluated into a temporary first:

        if (f(a(::2)) > 0) then   ->   copy-in; tmp = f(t) > 0; copy-out
                                       if (tmp) then

        do while (f(a(::2)) > 0)  ->   do while (.true.)
                                           copy-in; tmp = f(t) > 0; copy-out
                                           if (.not. tmp) exit

        do i = 1, f(a(::2))       ->   copy-in; tmp = f(t); copy-out
                                       do i = 1, tmp
    */

    // Visits `expr`, collecting the statements to put after its evaluation
    // in `after`, and the ones before it in `before` if not null (otherwise
    // in current_body, before the statement).
    void visit_expr_with_copy_out(ASR::expr_t* expr, Vec<ASR::stmt_t*>& after,
            Vec<ASR::stmt_t*>* before=nullptr) {
        Vec<ASR::stmt_t*>* current_body_copy = current_body;
        Vec<ASR::stmt_t*>* body_after_curr_stmt_copy = body_after_curr_stmt;
        if ( before ) {
            current_body = before;
        }
        body_after_curr_stmt = &after;
        visit_expr(*expr);
        current_body = current_body_copy;
        body_after_curr_stmt = body_after_curr_stmt_copy;
    }

    // Appends `tmp = expr` to `body`, for a new scalar temporary `tmp`,
    // and returns `tmp`.
    ASR::expr_t* assign_to_temporary(ASR::expr_t* expr, Vec<ASR::stmt_t*>& body,
            const std::string& name_hint) {
        ASRUtils::ASRBuilder b(al, expr->base.loc);
        ASR::ttype_t* type = ASRUtils::duplicate_type(al,
            ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(expr)));
        ASR::expr_t* tmp = b.Variable(current_scope,
            current_scope->get_unique_name(name_hint), type, ASR::intentType::Local);
        body.push_back(al, b.Assignment(tmp, expr));
        return tmp;
    }

    void append_stmts(Vec<ASR::stmt_t*>& body, Vec<ASR::stmt_t*>& stmts) {
        for ( size_t i = 0; i < stmts.size(); i++ ) {
            body.push_back(al, stmts[i]);
        }
    }

    void visit_If(const ASR::If_t& x) {
        ASR::If_t& xx = const_cast<ASR::If_t&>(x);
        if ( is_current_body_set == 0 ) {
            ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_If(x);
            return;
        }
        Vec<ASR::stmt_t*> after; after.reserve(al, 1);
        visit_expr_with_copy_out(xx.m_test, after);
        if ( after.size() > 0 ) {
            xx.m_test = assign_to_temporary(xx.m_test, *current_body, "__lcompilers_if_test");
            append_stmts(*current_body, after);
        }
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
    }

    void visit_IfArithmetic(const ASR::IfArithmetic_t& x) {
        ASR::IfArithmetic_t& xx = const_cast<ASR::IfArithmetic_t&>(x);
        if ( is_current_body_set == 0 ) {
            ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_IfArithmetic(x);
            return;
        }
        Vec<ASR::stmt_t*> after; after.reserve(al, 1);
        visit_expr_with_copy_out(xx.m_test, after);
        if ( after.size() > 0 ) {
            xx.m_test = assign_to_temporary(xx.m_test, *current_body, "__lcompilers_if_test");
            append_stmts(*current_body, after);
        }
    }

    void visit_WhileLoop(const ASR::WhileLoop_t& x) {
        ASR::WhileLoop_t& xx = const_cast<ASR::WhileLoop_t&>(x);
        // A loop with an else block (while_else) cannot leave with EXIT.
        if ( is_current_body_set == 0 || x.n_orelse > 0 ) {
            ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_WhileLoop(x);
            return;
        }
        // The copy-in, too, is done at each evaluation of the test.
        Vec<ASR::stmt_t*> body; body.reserve(al, x.n_body + 4);
        Vec<ASR::stmt_t*> after; after.reserve(al, 1);
        visit_expr_with_copy_out(xx.m_test, after, &body);
        transform_stmts(xx.m_body, xx.n_body);
        if ( body.size() == 0 && after.size() == 0 ) {
            return;
        }
        ASRUtils::ASRBuilder b(al, x.base.base.loc);
        ASR::expr_t* tmp = assign_to_temporary(xx.m_test, body, "__lcompilers_while_test");
        append_stmts(body, after);
        body.push_back(al, b.If(b.Not(tmp), {
            ASRUtils::STMT(ASR::make_Exit_t(al, x.base.base.loc, x.m_name))
        }, {}));
        for ( size_t i = 0; i < xx.n_body; i++ ) {
            body.push_back(al, xx.m_body[i]);
        }
        xx.m_test = b.bool_t(true, ASRUtils::expr_type(tmp));
        xx.m_body = body.p;
        xx.n_body = body.size();
    }

    // Evaluates the bounds of the loops `head` into temporaries before the
    // copy-out of their calls, if any.
    void visit_do_loop_heads(ASR::do_loop_head_t* head, size_t n_head) {
        Vec<ASR::stmt_t*> after; after.reserve(al, 1);
        Vec<ASR::stmt_t*>* body_after_curr_stmt_copy = body_after_curr_stmt;
        body_after_curr_stmt = &after;
        for ( size_t i = 0; i < n_head; i++ ) {
            visit_do_loop_head(head[i]);
        }
        body_after_curr_stmt = body_after_curr_stmt_copy;
        if ( after.size() == 0 ) {
            return;
        }
        for ( size_t i = 0; i < n_head; i++ ) {
            for ( ASR::expr_t** bound: {&head[i].m_start, &head[i].m_end,
                                        &head[i].m_increment} ) {
                if ( *bound && !ASRUtils::expr_value(*bound) ) {
                    *bound = assign_to_temporary(*bound, *current_body,
                        "__lcompilers_loop_bound");
                }
            }
        }
        append_stmts(*current_body, after);
    }

    void visit_DoLoop(const ASR::DoLoop_t& x) {
        ASR::DoLoop_t& xx = const_cast<ASR::DoLoop_t&>(x);
        if ( is_current_body_set == 0 ) {
            ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_DoLoop(x);
            return;
        }
        visit_do_loop_heads(&xx.m_head, 1);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
    }

    template <typename T>
    ASR::expr_t* get_first_array_function_args(T* func) {
        int64_t first_array_arg_idx = -1;
        ASR::expr_t* first_array_arg = nullptr;
        for (int64_t i = 0; i < (int64_t)func->n_args; i++) {
            ASR::ttype_t* func_arg_type;
            if constexpr (std::is_same_v<T, ASR::FunctionCall_t>) {
                func_arg_type = ASRUtils::expr_type(func->m_args[i].m_value);
            } else {
                func_arg_type = ASRUtils::expr_type(func->m_args[i]);
            }
            if (ASRUtils::is_array(func_arg_type)) {
                first_array_arg_idx = i;
                break;
            }
        }
        LCOMPILERS_ASSERT(first_array_arg_idx != -1)
        if constexpr (std::is_same_v<T, ASR::FunctionCall_t>) {
            first_array_arg = func->m_args[first_array_arg_idx].m_value;
        } else {
            first_array_arg = func->m_args[first_array_arg_idx];
        }
        return first_array_arg;
    }


    /*
        sets allocation size of an elemental function, which can be
        either an intrinsic elemental function or a user-defined
    */
    template <typename T>
    void set_allocation_size_elemental_function(
        Allocator& al, const Location& loc,
        T* elemental_function,
        Vec<ASR::dimension_t>& allocate_dims
    ) {
        ASR::ttype_t* index_type = get_index_type(loc);
        ASR::expr_t* index_one = get_index_one(loc);
        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(elemental_function->m_type);
        allocate_dims.reserve(al, n_dims);
        ASR::expr_t* first_array_arg = get_first_array_function_args(elemental_function);
        for( size_t i = 0; i < n_dims; i++ ) {
            ASR::dimension_t allocate_dim;
            allocate_dim.loc = loc;
            allocate_dim.m_start = index_one;
            ASR::expr_t* size_i_1 = ASRUtils::EXPR(ASR::make_ArraySize_t(
                al, loc, ASRUtils::get_past_array_physical_cast(first_array_arg),
                get_index_constant(loc, i + 1),
                index_type, nullptr));
            allocate_dim.m_length = size_i_1;
            allocate_dims.push_back(al, allocate_dim);
        }
    }

    ASR::expr_t* create_temporary_variable_for_array(Allocator& al,
        ASR::expr_t* value, SymbolTable* scope, std::string name_hint,
        bool is_pointer_required=false) {
        ASR::ttype_t* value_type = ASRUtils::expr_type(value);
        LCOMPILERS_ASSERT(ASRUtils::is_array(value_type));
    
        /* Figure out the type of the temporary array variable */
        ASR::dimension_t* value_m_dims = nullptr;
        size_t value_n_dims = ASRUtils::extract_dimensions_from_ttype(value_type, value_m_dims);
    
        if (ASR::is_a<ASR::IntegerCompare_t>(*value)) {
            ASR::IntegerCompare_t* integer_compare = ASR::down_cast<ASR::IntegerCompare_t>(value);
            ASR::ttype_t* logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, value->base.loc, 4));
    
            ASR::ttype_t* left_type = ASRUtils::expr_type(integer_compare->m_left);
            ASR::ttype_t* right_type = ASRUtils::expr_type(integer_compare->m_right);
    
            if (ASR::is_a<ASR::Array_t>(*left_type)) {
                ASR::Array_t* left_array_type = ASR::down_cast<ASR::Array_t>(left_type);
                ASR::dimension_t* left_m_dims = nullptr;
                size_t left_n_dims = ASRUtils::extract_dimensions_from_ttype(left_type, left_m_dims);
                value_m_dims = left_m_dims;
                value_n_dims = left_n_dims;
    
                if (left_array_type->m_physical_type == ASR::array_physical_typeType::FixedSizeArray) {
                    ASR::ttype_t* logical_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, value->base.loc, logical_type, left_m_dims, left_n_dims, ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));
                    value_type = logical_array_type;
                } else {
                    ASR::ttype_t* logical_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, value->base.loc, logical_type, left_m_dims, left_n_dims, ASR::array_physical_typeType::PointerArray, ASR::memory_spaceType::Global));
                    value_type = logical_array_type;
                }
            } else if (ASR::is_a<ASR::Array_t>(*right_type)) {
                ASR::Array_t* right_array_type = ASR::down_cast<ASR::Array_t>(right_type);
                ASR::dimension_t* right_m_dims = nullptr;
                size_t right_n_dims = ASRUtils::extract_dimensions_from_ttype(right_type, right_m_dims);
                value_m_dims = right_m_dims;
                value_n_dims = right_n_dims;
    
                if (right_array_type->m_physical_type == ASR::array_physical_typeType::FixedSizeArray) {
                    ASR::ttype_t* logical_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, value->base.loc, logical_type, right_m_dims, right_n_dims, ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));
                    value_type = logical_array_type;
                } else {
                    ASR::ttype_t* logical_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, value->base.loc, logical_type, right_m_dims, right_n_dims, ASR::array_physical_typeType::PointerArray, ASR::memory_spaceType::Global));
                    value_type = logical_array_type;
                }
            }
        }
        // dimensions can be different for an ArrayConstructor e.g. [1, a], where `a` is an
        // ArrayConstructor like [5, 2, 1]
        if (ASR::is_a<ASR::ArrayConstructor_t>(*value) &&
               !PassUtils::is_args_contains_allocatable(value)) {
            ASR::ArrayConstructor_t* arr_constructor = ASR::down_cast<ASR::ArrayConstructor_t>(value);
            ASR::expr_t* constructor_size = ASRUtils::get_ArrayConstructor_size(al, arr_constructor);
            if( constructor_size != nullptr ) {
                value_m_dims->m_length = constructor_size;
            }
        }

        /*
            Handle character type with assumed length OR functioncall (to avoid duplicate calls).
            `character(*), intent(in) :: inp_char_arr(:)` --TempType--> `character(len(inp_char_arr)), intent(in) :: tmp(:)`
        */
        if(ASRUtils::is_character(*value_type)){
            if(ASRUtils::get_string_type(value_type)->m_len_kind == ASR::AssumedLength || 
                (ASRUtils::get_string_type(value_type)->m_len &&
                    ASR::is_a<ASR::FunctionCall_t>(*ASRUtils::get_string_type(value_type)->m_len)
                )
            ){
                ASRUtils::ASRBuilder b(al, value->base.loc);
                value_type = b.String(b.StringLen(value), ASR::ExpressionLength);             
            }
        }
        bool is_fixed_sized_array = ASRUtils::is_fixed_size_array(value_type);
        bool is_size_only_dependent_on_arguments = ASRUtils::is_dimension_dependent_only_on_arguments(
            value_m_dims, value_n_dims);
        bool is_allocatable = ASRUtils::is_allocatable(value_type);
        ASR::ttype_t* var_type = nullptr;
        if( (is_fixed_sized_array || is_size_only_dependent_on_arguments || is_allocatable) &&
            !is_pointer_required ) {
            var_type = value_type;
        } else {
            var_type = ASRUtils::create_array_type_with_empty_dims(al, value_n_dims, value_type);
            var_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, var_type->base.loc, var_type));
        }
    
        std::string var_name = PassUtils::temporary_name(scope, name_hint);
        ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
            al, value->base.loc, scope, s2c(al, var_name), nullptr, 0, ASR::intentType::Local,
            nullptr, nullptr, ASR::storage_typeType::Default, var_type, ASRUtils::get_struct_sym_from_struct_expr(value), ASR::abiType::Source,
            ASR::accessType::Public, ASR::presenceType::Required, false));
        scope->add_symbol(var_name, temporary_variable);
    
        return ASRUtils::EXPR(ASR::make_Var_t(al, temporary_variable->base.loc, temporary_variable));
    }

    bool set_allocation_size(
        Allocator& al, ASR::expr_t* value,
        Vec<ASR::dimension_t>& allocate_dims,
        size_t target_n_dims,
        ASR::expr_t* &len_expr /*allocate(character(len(x)) :: y(size(x)))*/
    ) {
        if ( !ASRUtils::is_array(ASRUtils::expr_type(value)) ) {
            return false;
        }

        const Location& loc = value->base.loc;
        ASR::ttype_t* index_type = get_index_type(loc);
        ASR::expr_t* index_one = get_index_one(loc);
        
        if( ASRUtils::is_fixed_size_array(ASRUtils::expr_type(value)) ) {
            ASR::dimension_t* m_dims = nullptr;
            size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(value), m_dims);
            allocate_dims.reserve(al, n_dims);
            for( size_t i = 0; i < n_dims; i++ ) {
                ASR::dimension_t allocate_dim;
                allocate_dim.loc = value->base.loc;
                allocate_dim.m_start = index_one;
                allocate_dim.m_length = m_dims[i].m_length;
                allocate_dims.push_back(al, allocate_dim);
            }
            return true;
        }
        if(ASRUtils::is_character(*ASRUtils::expr_type(value))){
            bool is_const_len = ASRUtils::is_value_constant(ASRUtils::get_string_type(value)->m_len);
            len_expr = ASRUtils::EXPR(ASR::make_StringLen_t(al,
                value->base.loc, value,
                index_type,
                is_const_len ? ASRUtils::get_string_type(value)->m_len : nullptr));
        }
        switch( value->type ) {
            case ASR::exprType::FunctionCall: {
                ASR::FunctionCall_t* function_call = ASR::down_cast<ASR::FunctionCall_t>(value);
                ASR::ttype_t* type = function_call->m_type;
                if( ASRUtils::is_allocatable(type) ) {
                    return false;
                }
                if (ASRUtils::is_elemental(function_call->m_name)) {
                    set_allocation_size_elemental_function(al, loc, function_call, allocate_dims);
                    break;
                }
                ASRUtils::ExprStmtDuplicator duplicator(al);
                ASR::dimension_t* dims = nullptr;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, dims);
                allocate_dims.reserve(al, n_dims);
                for( size_t i = 0; i < n_dims; i++ ) {
                    ASR::dimension_t dim = dims[i];
                    ASR::dimension_t dim_copy;
                    dim_copy.loc = dim.loc;
                    dim_copy.m_start = !dim.m_start ? nullptr : duplicator.duplicate_expr(dim.m_start);
                    dim_copy.m_length = !dim.m_length ? nullptr : duplicator.duplicate_expr(dim.m_length);
                    LCOMPILERS_ASSERT(dim_copy.m_start);
                    LCOMPILERS_ASSERT(dim_copy.m_length);
                    allocate_dims.push_back(al, dim_copy);
                }
                break ;
            }
            case ASR::exprType::IntegerBinOp:
            case ASR::exprType::RealBinOp:
            case ASR::exprType::ComplexBinOp:
            case ASR::exprType::LogicalBinOp:
            case ASR::exprType::UnsignedIntegerBinOp:
            case ASR::exprType::IntegerCompare:
            case ASR::exprType::RealCompare:
            case ASR::exprType::ComplexCompare:
            case ASR::exprType::LogicalCompare:
            case ASR::exprType::UnsignedIntegerCompare:
            case ASR::exprType::StringCompare:
            case ASR::exprType::IntegerUnaryMinus:
            case ASR::exprType::RealUnaryMinus:
            case ASR::exprType::ComplexUnaryMinus: {
                /*
                    Collect all the variables from these expressions,
                    then take the size of one of the arrays having
                    maximum dimensions for now. For now LFortran will
                    assume that broadcasting is doable for arrays with lesser
                    dimensions and the array having maximum dimensions
                    has compatible size of each dimension with other arrays.
                */
    
                Vec<ASR::expr_t*> array_vars; array_vars.reserve(al, 1);
                ArrayVarCollector array_var_collector(al, array_vars);
                array_var_collector.visit_expr(*value);
                Vec<ASR::expr_t*> arrays_with_maximum_rank;
                arrays_with_maximum_rank.reserve(al, 1);
                LCOMPILERS_ASSERT(target_n_dims > 0);
                for( size_t i = 0; i < array_vars.size(); i++ ) {
                    if( (size_t) ASRUtils::extract_n_dims_from_ttype(
                            ASRUtils::expr_type(array_vars[i])) == target_n_dims ) {
                        arrays_with_maximum_rank.push_back(al, array_vars[i]);
                    }
                }
    
                LCOMPILERS_ASSERT(arrays_with_maximum_rank.size() > 0);
                ASR::expr_t* selected_array = arrays_with_maximum_rank[0];
                allocate_dims.reserve(al, target_n_dims);
                for( size_t i = 0; i < target_n_dims; i++ ) {
                    ASR::dimension_t allocate_dim;
                    Location loc_inner; loc_inner.first = 1, loc_inner.last = 1;
                    allocate_dim.loc = loc_inner;
                    // Assume 1 for Fortran.
                    allocate_dim.m_start = get_index_one(loc_inner);
                    ASR::expr_t* dim = get_index_constant(loc_inner, i + 1);
                    allocate_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                        al, loc_inner, ASRUtils::get_past_array_physical_cast(selected_array),
                        dim, index_type, nullptr));
                    allocate_dims.push_back(al, allocate_dim);
                }
                break;
            }
            case ASR::exprType::LogicalNot: {
                ASR::LogicalNot_t* logical_not = ASR::down_cast<ASR::LogicalNot_t>(value);
                if ( ASRUtils::is_array(ASRUtils::expr_type(logical_not->m_arg)) ) {
                    size_t rank = ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::expr_type(logical_not->m_arg));
                    ASR::expr_t* selected_array = logical_not->m_arg;
                    allocate_dims.reserve(al, rank);
                    for( size_t i = 0; i < rank; i++ ) {
                        ASR::dimension_t allocate_dim;
                        allocate_dim.loc = loc;
                        // Assume 1 for Fortran.
                        allocate_dim.m_start = index_one;
                        ASR::expr_t* dim = get_index_constant(loc, i + 1);
                        allocate_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                            al, loc, ASRUtils::get_past_array_physical_cast(selected_array),
                            dim, index_type, nullptr));
                        allocate_dims.push_back(al, allocate_dim);
                    }
                }
                break;
            }
            case ASR::exprType::Cast: {
                ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(value);
                if ( ASRUtils::is_array(ASRUtils::expr_type(cast->m_arg)) ) {
                    size_t rank = ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::expr_type(cast->m_arg));
                    ASR::expr_t* selected_array = cast->m_arg;
                    allocate_dims.reserve(al, rank);
                    for( size_t i = 0; i < rank; i++ ) {
                        ASR::dimension_t allocate_dim;
                        allocate_dim.loc = loc;
                        // Assume 1 for Fortran.
                        allocate_dim.m_start = index_one;
                        ASR::expr_t* dim = get_index_constant(loc, i + 1);
                        allocate_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                            al, loc, ASRUtils::get_past_array_physical_cast(selected_array),
                            dim, index_type, nullptr));
                        allocate_dims.push_back(al, allocate_dim);
                    }
                }
                break;
            }
            case ASR::exprType::ComplexRe:
            case ASR::exprType::ComplexIm: {
                ASR::expr_t* complex_arg = ASR::is_a<ASR::ComplexRe_t>(*value) ?
                    ASR::down_cast<ASR::ComplexRe_t>(value)->m_arg :
                    ASR::down_cast<ASR::ComplexIm_t>(value)->m_arg;
                if ( ASRUtils::is_array(ASRUtils::expr_type(complex_arg)) ) {
                    size_t rank = ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::expr_type(complex_arg));
                    allocate_dims.reserve(al, rank);
                    for( size_t i = 0; i < rank; i++ ) {
                        ASR::dimension_t allocate_dim;
                        allocate_dim.loc = loc;
                        // Assume 1 for Fortran.
                        allocate_dim.m_start = index_one;
                        ASR::expr_t* dim = get_index_constant(loc, i + 1);
                        allocate_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                            al, loc, ASRUtils::get_past_array_physical_cast(complex_arg),
                            dim, index_type, nullptr));
                        allocate_dims.push_back(al, allocate_dim);
                    }
                }
                break;
            }
            case ASR::exprType::ArraySection: {
                ASR::ArraySection_t* array_section_t = ASR::down_cast<ASR::ArraySection_t>(value);
                allocate_dims.reserve(al, array_section_t->n_args);
                for( size_t i = 0; i < array_section_t->n_args; i++ ) {
                    ASR::expr_t* start = array_section_t->m_args[i].m_left;
                    ASR::expr_t* end = array_section_t->m_args[i].m_right;
                    ASR::expr_t* step = array_section_t->m_args[i].m_step;
                    ASR::dimension_t allocate_dim;
                    allocate_dim.loc = loc;
                    allocate_dim.m_start = index_one;
                    if( start == nullptr && step == nullptr && end != nullptr ) {
                        if( ASRUtils::is_array(ASRUtils::expr_type(end)) ) {
                            allocate_dim.m_length = ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(
                                al, loc, end, nullptr, index_type, nullptr, false));
                            allocate_dims.push_back(al, allocate_dim);
                        }
                    } else {
                        bool is_any_kind_8 = false;
                        ASR::expr_t * int_one = index_one;
                        if( ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(end)) == 8 ||
                            ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(start)) == 8 ||
                            ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(step)) == 8 ) {
                                is_any_kind_8 = true;
                        }
                        if( is_any_kind_8 || pass_options.descriptor_index_64 ) {
                            int_one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                al, loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8))));
                        }
                        ASR::expr_t* end_minus_start = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                            end, ASR::binopType::Sub, start, ASRUtils::expr_type(end), nullptr));
                        ASR::expr_t* by_step = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                            end_minus_start, ASR::binopType::Div, step, ASRUtils::expr_type(end_minus_start),
                            nullptr));
                        ASR::expr_t* length = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                            by_step, ASR::binopType::Add, int_one, ASRUtils::expr_type(by_step), nullptr));
                        allocate_dim.m_length = length;
                        allocate_dims.push_back(al, allocate_dim);
                    }
                }
                break;
            }
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t* array_item_t = ASR::down_cast<ASR::ArrayItem_t>(value);
                allocate_dims.reserve(al, array_item_t->n_args);
                for( size_t i = 0; i < array_item_t->n_args; i++ ) {
                    ASR::expr_t* start = array_item_t->m_args[i].m_left;
                    ASR::expr_t* end = array_item_t->m_args[i].m_right;
                    ASR::expr_t* step = array_item_t->m_args[i].m_step;
                    if( !(start == nullptr && step == nullptr && end != nullptr) ) {
                        continue ;
                    }
                    if( !ASRUtils::is_array(ASRUtils::expr_type(end)) ) {
                        continue ;
                    }
                    ASR::dimension_t allocate_dim;
                    allocate_dim.loc = loc;
                    allocate_dim.m_start = index_one;
                    allocate_dim.m_length = ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(
                        al, loc, end, nullptr, index_type, nullptr, false));
                    allocate_dims.push_back(al, allocate_dim);
                }
                break;
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                ASR::IntrinsicElementalFunction_t* intrinsic_elemental_function =
                    ASR::down_cast<ASR::IntrinsicElementalFunction_t>(value);
                set_allocation_size_elemental_function(al, loc, intrinsic_elemental_function,
                            allocate_dims);
                break;
            }
            case ASR::exprType::IntrinsicArrayFunction: {
                ASR::IntrinsicArrayFunction_t* intrinsic_array_function =
                    ASR::down_cast<ASR::IntrinsicArrayFunction_t>(value);
                switch (intrinsic_array_function->m_arr_intrinsic_id) {
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::All):
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Any):
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Count):
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Parity):
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Sum):
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::MaxVal):
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::MinVal): {
                        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
                            intrinsic_array_function->m_type);
                        allocate_dims.reserve(al, n_dims);
                        for( size_t i = 0; i < n_dims; i++ ) {
                            ASR::dimension_t allocate_dim;
                            allocate_dim.loc = loc;
                            allocate_dim.m_start = index_one;
                            ASR::expr_t* size_i_1 = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                al, loc, ASRUtils::get_past_array_physical_cast(intrinsic_array_function->m_args[0]),
                                get_index_constant(loc, i + 1),
                                index_type, nullptr));
                            ASR::expr_t* size_i_2 = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                al, loc, ASRUtils::get_past_array_physical_cast(intrinsic_array_function->m_args[0]),
                                get_index_constant(loc, i + 2),
                                index_type, nullptr));
                            Vec<ASR::expr_t*> merge_i_args; merge_i_args.reserve(al, 3);
                            merge_i_args.push_back(al, size_i_1); merge_i_args.push_back(al, size_i_2);
                            merge_i_args.push_back(al, ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                                get_index_constant(loc, i + 1), ASR::cmpopType::Lt,
                                    intrinsic_array_function->m_args[1],
                                    ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr)));
                            ASR::expr_t* merge_i = ASRUtils::EXPR(ASRUtils::make_IntrinsicElementalFunction_t_util(
                                al, loc, static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Merge),
                                merge_i_args.p, merge_i_args.size(), 0, index_type, nullptr));
                            allocate_dim.m_length = merge_i;
                            allocate_dims.push_back(al, allocate_dim);
                        }
                        break;
                    }
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Pack): {
                        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
                            intrinsic_array_function->m_type);
                        allocate_dims.reserve(al, n_dims);
                        for ( size_t i = 0; i < n_dims; i++ ) {
                            ASR::dimension_t allocate_dim;
                            allocate_dim.loc = loc;
                            allocate_dim.m_start = index_one;
                            ASR::expr_t* size_i_1 = nullptr;
                            if (intrinsic_array_function->n_args == 3) {
                                size_i_1 = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                    al, loc, ASRUtils::get_past_array_physical_cast(intrinsic_array_function->m_args[2]),
                                    get_index_constant(loc, i + 1),
                                    index_type, nullptr));
                            } else {
                                Vec<ASR::expr_t*> count_i_args; count_i_args.reserve(al, 1);
                                count_i_args.push_back(al, intrinsic_array_function->m_args[1]);
                                size_i_1 = ASRUtils::EXPR(ASRUtils::make_IntrinsicArrayFunction_t_util(
                                    al, loc, static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Count),
                                    count_i_args.p, count_i_args.size(), 0, index_type, nullptr));
                            }
                            allocate_dim.m_length = size_i_1;
                            allocate_dims.push_back(al, allocate_dim);
                        }
                        break;
                    }
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Shape): {
                        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
                            intrinsic_array_function->m_type);
                        allocate_dims.reserve(al, n_dims);
                        for( size_t i = 0; i < n_dims; i++ ) {
                            ASR::dimension_t allocate_dim;
                            allocate_dim.loc = loc;
                            allocate_dim.m_start = index_one;
                            allocate_dim.m_length = index_one;
                            allocate_dims.push_back(al, allocate_dim);
                        }
                        break;
                    }
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Transpose): {
                        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(intrinsic_array_function->m_type);
                        LCOMPILERS_ASSERT(n_dims == 2);
                        allocate_dims.reserve(al, n_dims);
                        // Transpose swaps the dimensions
                        for (size_t i = 0; i < n_dims; i++) {
                            ASR::dimension_t allocate_dim;
                            allocate_dim.loc = loc;
                            allocate_dim.m_start = index_one;
                            ASR::expr_t* size_i = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                al, loc, intrinsic_array_function->m_args[0],
                                get_index_constant(loc, n_dims - i),
                                index_type, nullptr));

                            allocate_dim.m_length = size_i;
                            allocate_dims.push_back(al, allocate_dim);
                        }
                        break;
                    }
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Cshift): {
                        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(intrinsic_array_function->m_type);
                        allocate_dims.reserve(al, n_dims);
                        for (size_t i = 0; i < n_dims; i++) {
                            ASR::dimension_t allocate_dim;
                            allocate_dim.loc = loc;
                            allocate_dim.m_start = index_one;

                            ASR::expr_t* size_i = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                al, loc, intrinsic_array_function->m_args[0],
                                get_index_constant(loc, i + 1),
                                index_type, nullptr));

                            allocate_dim.m_length = size_i;
                            allocate_dims.push_back(al, allocate_dim);
                        }
                        break;
                    }
                    case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Spread): {
                        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(intrinsic_array_function->m_type);
                        ASR::expr_t* dim_arg = intrinsic_array_function->m_args[1];
                        allocate_dims.reserve(al, n_dims);
                        if (dim_arg && (ASRUtils::expr_value(dim_arg) != nullptr)) {
                            // Compile time value of `dim`
                            ASRUtils::ASRBuilder b(al, intrinsic_array_function->base.base.loc);
                            ASR::IntegerConstant_t* dim_val = ASR::down_cast<ASR::IntegerConstant_t>(dim_arg);
                            size_t dim_index = dim_val->m_n;
                            ASR::expr_t* ncopies = intrinsic_array_function->m_args[2];
                            int ncopies_inserted = 0;
                            for (size_t i = 0; i < n_dims; i++) {
                                ASR::dimension_t allocate_dim;
                                allocate_dim.loc = loc;
                                allocate_dim.m_start = index_one;
                                if ( i == dim_index - 1 ) {
                                    allocate_dim.m_length = ncopies;
                                    ncopies_inserted = 1;
                                } else {
                                    allocate_dim.m_length = b.ArraySize(intrinsic_array_function->m_args[0],
                                            get_index_constant(loc, i + 1 - ncopies_inserted), index_type);
                                }
                                allocate_dims.push_back(al, allocate_dim);
                            }
                        } else {
                            // Here `dim` is runtime so can't decide where to insert ncopies
                            // Just copy original dimensions
                            ASR::dimension_t* dims;
                            ASRUtils::extract_dimensions_from_ttype(intrinsic_array_function->m_type, dims);
                            for (size_t i = 0; i < n_dims; i++) {
                                allocate_dims.push_back(al, dims[i]);
                            }
                        }
                        break;
                    }
    
                    default: {
                        LCOMPILERS_ASSERT_MSG(false, "ASR::IntrinsicArrayFunctions::" +
                            ASRUtils::get_array_intrinsic_name(intrinsic_array_function->m_arr_intrinsic_id)
                            + " not handled yet in set_allocation_size");
                    }
                }
                break;
            }
            case ASR::exprType::StructInstanceMember: {
                ASR::StructInstanceMember_t* struct_instance_member_t =
                    ASR::down_cast<ASR::StructInstanceMember_t>(value);
                size_t n_dims = ASRUtils::extract_n_dims_from_ttype(struct_instance_member_t->m_type);
                allocate_dims.reserve(al, n_dims);
                if( ASRUtils::is_array(ASRUtils::expr_type(struct_instance_member_t->m_v)) ) {
                    value = struct_instance_member_t->m_v;
                }
                ASRUtils::ExprStmtDuplicator expr_duplicator(al);
                for( size_t i = 0; i < n_dims; i++ ) {
                    ASR::dimension_t allocate_dim;
                    allocate_dim.loc = loc;
                    allocate_dim.m_start = index_one;
                    allocate_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                        al, loc, expr_duplicator.duplicate_expr(
                            ASRUtils::get_past_array_physical_cast(value)),
                            get_index_constant(loc, i + 1),
                        index_type, nullptr));
                    allocate_dims.push_back(al, allocate_dim);
                }
                break;
            }
            case ASR::exprType::ArrayReshape: {
                ASR::ArrayReshape_t* array_reshape_t = ASR::down_cast<ASR::ArrayReshape_t>(value);
                size_t n_dims = ASRUtils::get_fixed_size_of_array(
                    ASRUtils::expr_type(array_reshape_t->m_shape));
                allocate_dims.reserve(al, n_dims);
                ASRUtils::ASRBuilder b(al, array_reshape_t->base.base.loc);
                for( size_t i = 0; i < n_dims; i++ ) {
                    ASR::dimension_t allocate_dim;
                    allocate_dim.loc = loc;
                    allocate_dim.m_start = index_one;
                    allocate_dim.m_length = b.ArrayItem_01(array_reshape_t->m_shape, {get_index_constant(loc, i + 1)});
                    allocate_dims.push_back(al, allocate_dim);
                }
                break;
            }
            case ASR::exprType::ArrayConstructor: {
                allocate_dims.reserve(al, 1);
                ASR::dimension_t allocate_dim;
                allocate_dim.loc = loc;
                allocate_dim.m_start = index_one;
                // The statements that count its elements, if it needs
                // any, go before the allocation.
                ASRUtils::ArraySizeStatements size_stmts{current_body, current_scope};
                allocate_dim.m_length = ASRUtils::get_ArrayConstructor_size(al,
                    ASR::down_cast<ASR::ArrayConstructor_t>(value), &size_stmts);
                allocate_dims.push_back(al, allocate_dim);
                break;
            }
            case ASR::exprType::ArrayConstant: {
                allocate_dims.reserve(al, 1);
                ASR::dimension_t allocate_dim;
                allocate_dim.loc = loc;
                allocate_dim.m_start = index_one;
                allocate_dim.m_length = ASRUtils::get_ArrayConstant_size(al,
                    ASR::down_cast<ASR::ArrayConstant_t>(value));
                allocate_dims.push_back(al, allocate_dim);
                break;
            }
            case ASR::exprType::Var: {
                ASRUtils::ASRBuilder b(al, value->base.loc);
                if ( ASRUtils::is_array(ASRUtils::expr_type(value))) {
                    ASR::dimension_t* m_dims;
                    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
                        ASRUtils::expr_type(value), m_dims);
                    // An assumed-rank array has no static rank: use the
                    // rank of the temporary.
                    if ( ASRUtils::extract_physical_type(ASRUtils::expr_type(value)) ==
                            ASR::array_physical_typeType::AssumedRankArray ) {
                        n_dims = target_n_dims;
                    }
                    allocate_dims.reserve(al, n_dims);
                    for( size_t i = 0; i < n_dims; i++ ) {
                        ASR::dimension_t allocate_dim;
                        allocate_dim.loc = loc;
                        // The copy loop uses the same indices in both arrays.
                        // Keep the source bounds, not a 1:ubound allocation.
                        allocate_dim.m_start = b.ArrayLBound(value, i+1, get_index_kind());
                        allocate_dim.m_length = b.ArraySize(value,
                            get_index_constant(loc, i + 1), index_type);
                        allocate_dims.push_back(al, allocate_dim);
                    }
                } else {
                    return false;
                }
                break;
            }
            default: {
                LCOMPILERS_ASSERT_MSG(false, "ASR::exprType::" + std::to_string(value->type)
                    + " not handled yet in set_allocation_size");
            }
        }
        return true;
    }


    ASR::stmt_t* create_do_loop(Allocator &al, const Location &loc, std::vector<ASR::expr_t*> do_loop_variables, ASR::expr_t* left_arr, ASR::expr_t* right_arr, int curr_idx, int integer_kind) {
        ASRUtils::ASRBuilder b(al, loc);

        if (curr_idx == 1) {
            std::vector<ASR::expr_t*> vars;
            for (size_t i = 0; i < do_loop_variables.size(); i++) {
                vars.push_back(do_loop_variables[i]);
            }
            return b.DoLoop(do_loop_variables[curr_idx - 1],
                PassUtils::get_bound(left_arr, curr_idx, "lbound", al, integer_kind),
                PassUtils::get_bound(left_arr, curr_idx, "ubound", al, integer_kind), {
                b.Assignment(b.ArrayItem_01(left_arr, vars), b.ArrayItem_01(right_arr, vars))
            }, nullptr);
        }
        return b.DoLoop(do_loop_variables[curr_idx - 1],
            PassUtils::get_bound(left_arr, curr_idx, "lbound", al, integer_kind),
            PassUtils::get_bound(left_arr, curr_idx, "ubound", al, integer_kind), {
            create_do_loop(al, loc, do_loop_variables, left_arr, right_arr, curr_idx - 1, integer_kind)
        }, nullptr);
    }

    void traverse_call_args(Vec<ASR::call_arg_t>& x_m_args_vec, ASR::call_arg_t* x_m_args,
        size_t x_n_args, const std::string& name_hint, std::vector<bool> is_arg_intent_out = {}, bool is_func_bind_c = false,
        std::vector<bool> is_arg_contiguous_dummy = {}) {
        /* For other frontends, we might need to traverse the arguments
           in reverse order. */
        for( size_t i = 0; i < x_n_args; i++ ) {
            ASR::expr_t* arg_expr = x_m_args[i].m_value;
            if ( arg_expr &&
                 ((!is_func_bind_c && is_descriptor_array_casted_to_pointer_to_data(arg_expr)) ||
                  (i < is_arg_contiguous_dummy.size() && is_arg_contiguous_dummy[i] &&
                   is_discontiguous_actual_of_contiguous_dummy(arg_expr))) &&
                 !ASR::is_a<ASR::FunctionParam_t>(*ASRUtils::get_past_array_physical_cast(arg_expr))
                 && !ASRUtils::is_stringToArray_cast(ASRUtils::get_past_array_physical_cast(arg_expr))) {
                // The cast of the actual argument to the physical type of the
                // dummy, if any. An array descriptor (a pointer, for example)
                // passed to a descriptor dummy is not cast.
                ASR::ArrayPhysicalCast_t* array_physical_cast = ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_expr) ?
                    ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_expr) : nullptr;
                ASR::expr_t* arg_expr_past_cast = ASRUtils::get_past_array_physical_cast(arg_expr);
                const Location& loc = arg_expr->base.loc;
                // A descriptor view of the array with a static rank: an
                // assumed-rank array is viewed with its rank in the
                // enclosing `rank (N)` block.
                ASR::expr_t* array_view = arg_expr_past_cast;
                if ( array_physical_cast &&
                     array_physical_cast->m_old == ASR::array_physical_typeType::AssumedRankArray ) {
                    int64_t rank = get_select_rank(
                        ASR::down_cast<ASR::Var_t>(arg_expr_past_cast)->m_v);
                    ASR::ttype_t* view_type = ASRUtils::create_array_type_with_empty_dims(al,
                        rank, ASRUtils::extract_type(ASRUtils::expr_type(arg_expr_past_cast)));
                    array_view = ASRUtils::EXPR(ASR::make_ArrayPhysicalCast_t(al, loc,
                        arg_expr_past_cast, ASR::array_physical_typeType::AssumedRankArray,
                        ASR::array_physical_typeType::DescriptorArray, view_type, nullptr));
                }
                ASR::expr_t* array_var_temporary = create_temporary_variable_for_array(
                    al, array_view, current_scope, name_hint, true);
                ASR::call_arg_t array_var_temporary_arg;
                array_var_temporary_arg.loc = loc;
                const bool unhandled_case = ASRUtils::is_unlimited_polymorphic_type(ASRUtils::expr_type(arg_expr)) || 
                        (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_expr) 
                        && ASRUtils::is_unlimited_polymorphic_type(ASRUtils::expr_type(
                            ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_expr)->m_arg))); // TODO : remove -- Look `class_95.f90`  
                if( ASRUtils::is_pointer(ASRUtils::expr_type(array_var_temporary)) && !unhandled_case ) {
                    array_var_temporary_arg.m_value = array_physical_cast ?
                        ASRUtils::EXPR(ASR::make_ArrayPhysicalCast_t(al, loc,
                            array_var_temporary, ASR::array_physical_typeType::DescriptorArray, array_physical_cast->m_new,
                            array_physical_cast->m_type, nullptr)) :
                        array_var_temporary;
                    x_m_args_vec.push_back(al, array_var_temporary_arg);
                    // This should be always true as we pass `true` to `create_temporary_variable_for_array`
                    ASRUtils::ASRBuilder b(al, arg_expr_past_cast->base.loc);
                    Vec<ASR::dimension_t> allocate_dims; allocate_dims.reserve(al, 1);
                    ASR::expr_t* len_expr{}; // Character length to allocate with
                    size_t target_n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(array_var_temporary));
                    if( !set_allocation_size(al, arg_expr_past_cast, allocate_dims, target_n_dims, len_expr) ) {
                        current_body->push_back(al, ASRUtils::STMT(ASR::make_Associate_t(
                            al, loc, array_var_temporary, array_view, ASR::pointer_bounds_formType::NoPointerBounds)));
                        return;
                    }
                    LCOMPILERS_ASSERT(target_n_dims == allocate_dims.size());
                    Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
                    ASR::alloc_arg_t alloc_arg;
                    alloc_arg.loc = arg_expr_past_cast->base.loc;
                    alloc_arg.m_a = array_var_temporary;
                    alloc_arg.m_dims = allocate_dims.p;
                    alloc_arg.n_dims = allocate_dims.size();
                    alloc_arg.m_len_expr = len_expr;
                    alloc_arg.m_type = nullptr;
                    alloc_arg.m_sym_subclass = nullptr;
                    alloc_arg.m_codims = nullptr;
                    alloc_arg.n_codims = 0;
                    alloc_args.push_back(al, alloc_arg);
                
                    Vec<ASR::expr_t*> dealloc_args; dealloc_args.reserve(al, 1);
                    dealloc_args.push_back(al, array_var_temporary);
                    ASR::expr_t* is_contiguous = ASRUtils::EXPR(ASR::make_ArrayIsContiguous_t(al, loc,
                        arg_expr_past_cast, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
                    ASR::expr_t* not_is_contiguous = ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc, is_contiguous,
                        ASRUtils::expr_type(is_contiguous), nullptr));
                    ASR::dimension_t* array_dims = nullptr;
                    int array_rank = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(array_var_temporary), array_dims));
                    std::vector<ASR::expr_t*> do_loop_variables;
                    #define declare(var_name, type, intent)                                     \
                    b.Variable(fn_symtab, var_name, type, ASR::intentType::intent)
                    for (int i = 0; i < array_rank; i++) {
                        std::string var_name = current_scope->get_unique_name("__lcompilers_i_" + std::to_string(i));
                        ASR::ttype_t* loop_var_type = pass_options.descriptor_index_64
                            ? ASRUtils::expr_type(b.i64(0))
                            : ASRUtils::expr_type(b.i32(0));
                        do_loop_variables.push_back(b.Variable(current_scope, var_name, loop_var_type, ASR::intentType::Local));
                    }
                    int integer_kind = pass_options.descriptor_index_64 ? 8 : 4;
                    current_body->push_back(al,
                        b.If(not_is_contiguous, {
                            ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(al,
                                array_var_temporary->base.loc, dealloc_args.p, dealloc_args.size(), nullptr, nullptr)),
                            ASRUtils::STMT(ASR::make_Allocate_t(al,
                                array_var_temporary->base.loc, alloc_args.p, alloc_args.size(),
                                nullptr, nullptr, nullptr)),
                                create_do_loop(al, loc, do_loop_variables, array_var_temporary, array_view, array_rank, integer_kind)
                        }, {
                            ASRUtils::STMT(ASR::make_Associate_t(
                                al, loc, array_var_temporary, array_view, ASR::pointer_bounds_formType::NoPointerBounds))
                        })
                    );
                    if ( is_arg_intent_out.size() > 0 && is_arg_intent_out[i] ) {
                        body_after_curr_stmt->push_back(al, b.If(not_is_contiguous, {
                            create_do_loop(al, loc, do_loop_variables, array_view, array_var_temporary, array_rank, integer_kind)
                        }, {}));
                    }
                    // Nullify the pointer after the call if it was associated (contiguous case).
                    // This prevents double-free on the next loop iteration: without nullification,
                    // the ExplicitDeallocate at the start of the loop would try to free memory
                    // that belongs to the source array (was aliased via Associate).
                    // In the non-contiguous case, deallocate the heap-allocated temporary
                    // that was used for copy-in (and copy-out).
                    body_after_curr_stmt->push_back(al, b.If(is_contiguous, {
                        ASRUtils::STMT(ASR::make_Nullify_t(al, loc, dealloc_args.p, dealloc_args.size()))
                    }, {
                        ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(al, loc, dealloc_args.p, dealloc_args.size(), nullptr, nullptr))
                    }));
                } else {
                    x_m_args_vec.push_back(al, x_m_args[i]);
                }
            } else {
                x_m_args_vec.push_back(al, x_m_args[i]);
            }
        }
    }

    template <typename T>
    void add_class_to_struct_casts(Vec<ASR::call_arg_t>& call_args, const T& x) {
        ASR::symbol_t* call_sym = ASRUtils::symbol_get_past_external(x.m_name);
        ASR::Function_t* func = nullptr;
        if (ASR::is_a<ASR::Function_t>(*call_sym)) {
            func = ASR::down_cast<ASR::Function_t>(call_sym);
        } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(*call_sym)) {
            ASR::StructMethodDeclaration_t* method = ASR::down_cast<ASR::StructMethodDeclaration_t>(call_sym);
            func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(method->m_proc));
        } else {
            return;
        }

        for (size_t i = 0; i < call_args.size(); i++) {
            if (call_args.p[i].m_value == nullptr || i >= func->n_args) {
                continue;
            }
            ASR::expr_t* actual_arg = call_args.p[i].m_value;
            ASR::expr_t* formal_arg = func->m_args[i];
            ASR::ttype_t* formal_arg_full_type = ASRUtils::expr_type(formal_arg);
            if (ASRUtils::is_allocatable(formal_arg_full_type) ||
                ASRUtils::is_pointer(formal_arg_full_type)) {
                continue;
            }
            if (!ASR::is_a<ASR::ArrayItem_t>(*actual_arg)) {
                continue;
            }
            ASR::ttype_t* actual_type = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(actual_arg));
            ASR::ttype_t* formal_type = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(formal_arg));
            if (!actual_type || !formal_type ||
                !ASR::is_a<ASR::StructType_t>(*actual_type) ||
                !ASR::is_a<ASR::StructType_t>(*formal_type) ||
                !ASRUtils::is_class_type(actual_type) ||
                ASRUtils::is_class_type(formal_type)) {
                continue;
            }

            ASR::symbol_t* actual_struct_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(actual_arg));
            ASR::symbol_t* formal_struct_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(formal_arg));
            if (!actual_struct_sym || !formal_struct_sym ||
                !ASR::is_a<ASR::Struct_t>(*actual_struct_sym) ||
                !ASR::is_a<ASR::Struct_t>(*formal_struct_sym)) {
                continue;
            }

            if (actual_struct_sym != formal_struct_sym) {
                continue;
            }

            call_args.p[i].m_value = ASRUtils::EXPR(ASR::make_Cast_t(al, actual_arg->base.loc,
                actual_arg, ASR::cast_kindType::ClassToStruct,
                ASRUtils::duplicate_type(al, ASRUtils::expr_type(formal_arg)), nullptr, nullptr));
        }
    }

    template <typename T>
    void visit_Call(const T& x, const std::string& name_hint) {
        Vec<ASR::call_arg_t> x_m_args; x_m_args.reserve(al, x.n_args);
        ASR::symbol_t* callee = ASRUtils::symbol_get_past_external(x.m_name);
        bool is_procedure_variable = ASR::is_a<ASR::Variable_t>(*callee) &&
            ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(
                ASR::down_cast<ASR::Variable_t>(callee)->m_type));
        if ( ASR::is_a<ASR::Function_t>(*callee) || is_procedure_variable ) {
            // The dummies come from the interface the call goes through: the
            // procedure itself, or the interface of a procedure variable (for
            // example the call-site interface of an implicit-interface call).
            // Without an interface every argument may be modified.
            ASR::Function_t* func = ASRUtils::get_function(callee);
            bool is_func_bind_c = ASRUtils::get_FunctionType(callee)->m_abi == ASR::abiType::BindC;
            std::vector<bool> is_arg_intent_out(x.n_args, func == nullptr);
            // CONTIGUOUS assumed-shape and assumed-rank dummies, which take
            // an array descriptor of contiguous data.
            std::vector<bool> is_arg_contiguous_dummy(x.n_args, false);
            for (size_t i = 0; func && i < func->n_args && i < x.n_args; i++ ) {
                ASR::Variable_t* var = ASRUtils::expr_to_variable_or_null(func->m_args[i]);
                is_arg_intent_out[i] = var && (
                    var->m_intent == ASR::intentType::Out ||
                    var->m_intent == ASR::intentType::InOut ||
                    var->m_intent == ASR::intentType::Unspecified);
                is_arg_contiguous_dummy[i] = var && var->m_contiguous_attr &&
                    ASRUtils::is_array(var->m_type) &&
                    !ASRUtils::is_allocatable(var->m_type) &&
                    !ASRUtils::is_pointer(var->m_type) &&
                    (ASRUtils::extract_physical_type(var->m_type) ==
                        ASR::array_physical_typeType::DescriptorArray ||
                     ASRUtils::extract_physical_type(var->m_type) ==
                        ASR::array_physical_typeType::AssumedRankArray);
            }
            traverse_call_args(x_m_args, x.m_args, x.n_args,
                name_hint + ASRUtils::symbol_name(x.m_name), is_arg_intent_out, is_func_bind_c,
                is_arg_contiguous_dummy);
        } else {
            traverse_call_args(x_m_args, x.m_args, x.n_args,
                name_hint + ASRUtils::symbol_name(x.m_name));
        }
        add_class_to_struct_casts(x_m_args, x);

        T& xx = const_cast<T&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        visit_Call(x, "_subroutine_call_");
        ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        if (is_current_body_set != 0) {
            visit_Call(x, "_function_call_");
        }
        ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_FunctionCall(x);
    }

    void visit_Function(const ASR::Function_t& x) {
        vars_from_uncopied_sections.clear();
        ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_Function(x);
    }

    // Track Associate statements created by pass_array_by_data where the
    // source is an ArraySection that is passed without copy-in/copy-out
    // (see vars_from_uncopied_sections). Every other section, such as
    // a(1, :) of a fixed-size array, may be non-contiguous, so it gets
    // copy-in/copy-out. An array element passed for sequence association
    // (with --legacy-array-sections) is not a section here: semantics
    // passes it as the address of the element.
    void visit_Associate(const ASR::Associate_t& x) {
        if ( ASR::is_a<ASR::ArraySection_t>(*x.m_value) &&
             ASR::is_a<ASR::Var_t>(*x.m_target) ) {
            ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(x.m_value);
            ASR::ttype_t* source_type = ASRUtils::expr_type(section->m_v);
            if ( ASRUtils::is_array(source_type) ) {
                // is_simply_contiguous reads ASR rewritten by earlier passes
                // here, where a true result still holds (see its comment).
                // The base of a FixedSizeArray section is the array itself:
                // the pointers that passes put in place of a section, such
                // as the section pointers of array_struct_temporary, are
                // DescriptorArray.
                ASR::array_physical_typeType phys = ASRUtils::extract_physical_type(source_type);
                if ( (phys == ASR::array_physical_typeType::UnboundedPointerArray &&
                        has_undefined_assumed_size_bound(section)) ||
                     (phys == ASR::array_physical_typeType::FixedSizeArray &&
                        ASRUtils::is_simply_contiguous(x.m_value)) ) {
                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
                    vars_from_uncopied_sections.insert(sym);
                }
            }
        }
        ASR::CallReplacerOnExpressionsVisitor<CallVisitor>::visit_Associate(x);
    }

    // Check if an ArraySection has a right bound that is an ArrayBound UBound
    // on an assumed-size dimension (a dimension with no declared upper bound).
    static bool has_undefined_assumed_size_bound(ASR::ArraySection_t* section) {
        ASR::ttype_t* source_type = ASRUtils::expr_type(section->m_v);
        ASR::dimension_t* dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(source_type, dims);
        for ( size_t i = 0; i < section->n_args && i < n_dims; i++ ) {
            // Check if this dimension is assumed-size (no declared upper bound)
            if ( dims[i].m_length != nullptr ) continue;
            // Check if the section's right bound is ArrayBound UBound on this dim
            ASR::expr_t* right = section->m_args[i].m_right;
            if ( right && ASR::is_a<ASR::ArrayBound_t>(*right) ) {
                ASR::ArrayBound_t* bound = ASR::down_cast<ASR::ArrayBound_t>(right);
                if ( bound->m_bound == ASR::arrayboundType::UBound ) {
                    return true;
                }
            }
        }
        return false;
    }

    // Don't visit DebugCheckArrayBounds, m_dt in FunctionCall might be an array
    void visit_DebugCheckArrayBounds(const ASR::DebugCheckArrayBounds_t& x) {
        (void)x;
    }
};

void pass_replace_array_passed_in_function_call(Allocator &al, ASR::TranslationUnit_t &unit,
                        const LCompilers::PassOptions& pass_options) {
    SelectRankBlockCollector select_rank_collector;
    select_rank_collector.visit_TranslationUnit(unit);
    CallVisitor v(al, pass_options);
    v.select_rank_blocks = std::move(select_rank_collector.blocks);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor x(al);
    x.visit_TranslationUnit(unit);
}


} // namespace LCompilers
