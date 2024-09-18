#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_where.h>


namespace LCompilers {

/*
This ASR pass replaces where with do loops and array expression assignments.
The function `pass_replace_where` transforms the ASR tree in-place.

Converts:

    where(a > b)
        a = 2.0
    else where(a == 2.0)
        b = 3.0
    else where
        a = b * 2.0 / x * 3.0
    endwhere

to:

    do i = lbound(1, a), ubound(1, a)
        if (a(i) > b(i))
            a(i) = 2.0
        else if (a(i) == 2.0)
            b(i) = 3.0
        else
            a(i) = b(i) * 2.0 / x(i) * 3.0
        end if
    end do
*/

uint64_t static inline get_hash(ASR::asr_t *node)
{
    return (uint64_t)node;
}

using ASR::down_cast;
using ASR::is_a;

class ReplaceVar : public ASR::BaseExprReplacer<ReplaceVar>
{
public:
    Allocator& al;
    SymbolTable* current_scope;
    Vec<ASR::expr_t*> idx_vars;
    std::map<uint64_t, ASR::expr_t*> return_var_hash;
    ReplaceVar(Allocator &al_) : al(al_), current_scope(nullptr) {}

    void replace_Var(ASR::Var_t* x) {
        ASR::expr_t* expr_ = ASRUtils::EXPR(ASR::make_Var_t(al, x->base.base.loc, x->m_v));
        *current_expr = expr_;
        if (ASRUtils::is_array(ASRUtils::expr_type(expr_))) {
            ASR::expr_t* new_expr_ = PassUtils::create_array_ref(expr_, idx_vars, al, current_scope);
            *current_expr = new_expr_;
        }
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        ASR::BaseExprReplacer<ReplaceVar>::replace_ArrayPhysicalCast(x);
        if( !ASRUtils::is_array(ASRUtils::expr_type(x->m_arg)) ) {
            *current_expr = x->m_arg;
        }
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        uint64_t h = get_hash((ASR::asr_t*) x->m_name);
        if (return_var_hash.find(h) != return_var_hash.end()) {
            *current_expr = PassUtils::create_array_ref(return_var_hash[h], idx_vars, al, current_scope);
        }
    }

    #define BinOpReplacement(Constructor) ASR::expr_t** current_expr_copy = current_expr; \
        current_expr = const_cast<ASR::expr_t**>(&(x->m_left)); \
        this->replace_expr(x->m_left); \
        ASR::expr_t* left = *current_expr; \
        current_expr = current_expr_copy; \
        current_expr = const_cast<ASR::expr_t**>(&(x->m_right)); \
        this->replace_expr(x->m_right); \
        ASR::expr_t* right = *current_expr; \
        current_expr = current_expr_copy; \
        *current_expr = ASRUtils::EXPR(ASR::Constructor(al, x->base.base.loc, \
            left, x->m_op, right, x->m_type, nullptr)); \

    void replace_IntegerBinOp(ASR::IntegerBinOp_t* x) {
        BinOpReplacement(make_IntegerBinOp_t)
    }

    void replace_RealBinOp(ASR::RealBinOp_t* x) {
        BinOpReplacement(make_RealBinOp_t)
    }

    void replace_IntrinsicElementalFunction(ASR::IntrinsicElementalFunction_t* x) {
        Vec<ASR::expr_t*> args;
        args.reserve(al, x->n_args);
        for (size_t i=0; i<x->n_args; i++) {
            ASR::expr_t* arg = x->m_args[i];
            current_expr = const_cast<ASR::expr_t**>(&(arg));
            this->replace_expr(arg);
            args.push_back(al, *current_expr);
        }
        ASR::ttype_t* type = ASRUtils::expr_type(args[0]);
        ASR::expr_t* new_expr = ASRUtils::EXPR(
            ASRUtils::make_IntrinsicElementalFunction_t_util(al, x->base.base.loc,
            x->m_intrinsic_id, args.p, x->n_args, x->m_overload_id, type, x->m_value));
        *current_expr = new_expr;
    }

    void replace_Array(ASR::Array_t */*x*/) {
        // pass
    }
};

class VarVisitor : public ASR::CallReplacerOnExpressionsVisitor<VarVisitor>
{
public:

    Allocator &al;
    ReplaceVar replacer;
    std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash;
    std::map<uint64_t, ASR::expr_t*> &return_var_hash;
    Vec<ASR::stmt_t*> pass_result;

    VarVisitor(Allocator &al_, std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash, std::map<uint64_t, ASR::expr_t*> &return_var_hash) :
        al(al_), replacer(al_), assignment_hash(assignment_hash), return_var_hash(return_var_hash) {
        pass_result.reserve(al, 1);
    }

    void call_replacer_(Vec<ASR::expr_t*> idx_vars_) {
        replacer.current_expr = current_expr;
        replacer.current_scope = current_scope;
        replacer.idx_vars = idx_vars_;
        replacer.return_var_hash = return_var_hash;
        replacer.replace_expr(*current_expr);
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        for (size_t i=0; i<n_body; i++) {
            pass_result.n = 0;
            ASR::stmt_t* stmt_ = m_body[i];
            visit_stmt(*m_body[i]);
            if (stmt_->type == ASR::stmtType::Assignment && pass_result.size() > 0) {
                for (size_t j=0; j < pass_result.size(); j++) {
                    body.push_back(al, pass_result[j]);
                }
            } else {
                body.push_back(al, m_body[i]);
            }
        }
        m_body = body.p;
        n_body = body.size();
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        uint64_t h = get_hash((ASR::asr_t*) &x);
        if (assignment_hash.find(h) == assignment_hash.end()) {
            return;
        }
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x.m_target));
        this->call_replacer_(assignment_hash[h]);
        ASR::expr_t* target = *replacer.current_expr;
        current_expr = current_expr_copy;
        this->visit_expr(*x.m_target);
        current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
        this->call_replacer_(assignment_hash[h]);
        ASR::expr_t* value = *replacer.current_expr;
        current_expr = current_expr_copy;
        this->visit_expr(*x.m_value);
        if( !ASRUtils::is_array(ASRUtils::expr_type(target)) ) {
            if( ASR::is_a<ASR::ArrayBroadcast_t>(*value) ) {
                value = ASR::down_cast<ASR::ArrayBroadcast_t>(value)->m_array;
            }
        }
        ASR::stmt_t* tmp_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, target, value, nullptr));
        pass_result.push_back(al, tmp_stmt);
    }
};


class WhereVisitor : public PassUtils::PassVisitor<WhereVisitor>
{
public:
    std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash;
    std::map<uint64_t, ASR::expr_t*> &return_var_hash;
    WhereVisitor(Allocator &al, std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash, std::map<uint64_t, ASR::expr_t*> &return_var_hash) :
        PassVisitor(al, nullptr), assignment_hash(assignment_hash), return_var_hash(return_var_hash) {
        pass_result.reserve(al, 1);
    }

    /*
    * Converts an array section expression like `array(1, :)` to an array item
    * expression `array(1, var)` using the current `do` loop variable `var` and
    * returns it. 
    * 
    * Returns the original expression if it is not `ASR::ArraySection_t`.
    * 
    * We do this conversion in the `where` pass before the `array_op` pass to use the
    * current loop variable `var` for accessing values from the array. The `array_op` pass
    * does the above conversion using a new `do` loop which leads to an incorrect output.
    */
    ASR::expr_t* make_array_item_from_array_section(ASR::expr_t* expression, ASR::expr_t* var) {
        ASR::ArraySection_t* arr_section = nullptr;
        if (ASR::is_a<ASR::ArraySection_t>(*expression)) {
            arr_section = ASR::down_cast<ASR::ArraySection_t>(expression);
        } else {
            return expression;
        }

        ASR::expr_t* arr_section_var = arr_section->m_v;

        size_t sliced_dim_index = 0;
        for (size_t i = 0; i < arr_section->n_args; i++) {
            if (!(arr_section->m_args[i].m_left == nullptr
                  && arr_section->m_args[i].m_right != nullptr
                  && arr_section->m_args[i].m_step == nullptr)) {
                sliced_dim_index = i + 1;
            }
        }

        Vec<ASR::array_index_t> args;
        ASR::array_index_t ai;
        ai.loc = arr_section_var->base.loc;
        ai.m_left = nullptr;
        ai.m_right = nullptr;
        ai.m_step = nullptr;
        args.reserve(al, 1);

        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, arr_section_var->base.loc, 1,
                                ASRUtils::TYPE(ASR::make_Integer_t(al, arr_section_var->base.loc, 8))));
        for (size_t i = 0; i < arr_section->n_args; i++) {
            if (i + 1 == sliced_dim_index) {
                ai.m_left = one;
                ai.m_right = var;
                ai.m_step = one;
                args.push_back(al, ai);
            } else {
                args.push_back(al, arr_section->m_args[i]);
            }
        }

        ASR::expr_t* array_item = ASRUtils::EXPR(
                                    ASRUtils::make_ArrayItem_t_util(
                                        al,
                                        arr_section->m_v->base.loc,
                                        arr_section->m_v,
                                        args.p,
                                        args.size(),
                                        ASRUtils::type_get_past_array_pointer_allocatable(
                                            ASRUtils::expr_type((arr_section_var))),
                                        ASR::arraystorageType::ColMajor,
                                        nullptr));

        return array_item;
    }

    /*
    * Converts an array section assignment statement like `array(1, :) = 2.0` into
    * an array item assignment statement `array(1, var) = 2.0` using the current
    * `do` loop variable `var`.
    */
    ASR::stmt_t* convert_array_section_assignment_to_array_item_assignment(ASR::Assignment_t* assignment,
                                                                   ASR::expr_t* var) {
        ASR::expr_t* target = make_array_item_from_array_section(assignment->m_target, var);

        ASR::stmt_t* arr_item_assign = ASRUtils::STMT(
                                        ASR::make_Assignment_t(al,
                                            assignment->base.base.loc,
                                            target,
                                            assignment->m_value,
                                            assignment->m_overloaded));

        return arr_item_assign;
    }

    ASR::stmt_t* handle_If(ASR::Where_t& x, ASR::expr_t* test, ASR::expr_t* var, Location& loc, Vec<ASR::expr_t*> idx_vars) {
        ASR::IntegerCompare_t* int_cmp = nullptr;
        ASR::RealCompare_t* real_cmp = nullptr;
        ASR::LogicalBinOp_t* log_bin_op = nullptr;
        ASR::expr_t* left, *right;
        bool is_left_array = false;
        bool is_right_array = false;
        ASR::ttype_t* logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
        ASR::expr_t* test_new = nullptr;
        ASR::expr_t* left_array = nullptr;
        ASR::expr_t* right_array = nullptr;

        if (ASR::is_a<ASR::IntegerCompare_t>(*test)) {
            int_cmp = ASR::down_cast<ASR::IntegerCompare_t>(test);

            left = make_array_item_from_array_section(int_cmp->m_left, var);
            right = make_array_item_from_array_section(int_cmp->m_right, var);
        } else if (ASR::is_a<ASR::RealCompare_t>(*test)) {
            real_cmp = ASR::down_cast<ASR::RealCompare_t>(test);

            left = make_array_item_from_array_section(real_cmp->m_left, var);
            right = make_array_item_from_array_section(real_cmp->m_right, var);
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*test)) {
            log_bin_op = ASR::down_cast<ASR::LogicalBinOp_t>(test);

            left = make_array_item_from_array_section(log_bin_op->m_left, var);
            right = make_array_item_from_array_section(log_bin_op->m_right, var);
        } else {
            throw LCompilersException("Unsupported type");
        }

        if (ASRUtils::is_array(ASRUtils::expr_type(left))) {
            if (ASR::is_a<ASR::ArrayBroadcast_t>(*left)) {
                ASR::ArrayBroadcast_t* arr_broadcast = ASR::down_cast<ASR::ArrayBroadcast_t>(left);
                if (ASR::is_a<ASR::Logical_t>(*ASRUtils::expr_type(arr_broadcast->m_array))) {
                    is_left_array = false;
                }
            } else {
                is_left_array = true;
                left_array = PassUtils::create_array_ref(left, idx_vars, al, current_scope);
            }
        }

        if (ASRUtils::is_array(ASRUtils::expr_type(right))) {
            if (ASR::is_a<ASR::ArrayBroadcast_t>(*right)) {
                ASR::ArrayBroadcast_t* arr_broadcast = ASR::down_cast<ASR::ArrayBroadcast_t>(right);
                if (ASR::is_a<ASR::Logical_t>(*ASRUtils::expr_type(arr_broadcast->m_array))) {
                    is_right_array = false;
                }
            } else {
                is_right_array = true;
                right_array = PassUtils::create_array_ref(right, idx_vars, al, current_scope);
            }
        }


        if (int_cmp) {
            test_new = ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc, is_left_array ? left_array : left, int_cmp->m_op,
                        is_right_array?right_array:right, logical_type, nullptr));
        } else if (real_cmp) {
            test_new = ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc, is_left_array ? left_array : left, real_cmp->m_op,
                        is_right_array?right_array:right, logical_type, nullptr));
        } else if (log_bin_op) {
            test_new = ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc, is_left_array ? left_array : left, log_bin_op->m_op,
                        is_right_array?right_array:right, logical_type, nullptr));
        }

        Vec<ASR::stmt_t*> if_body;
        if_body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            ASR::stmt_t* stmt = x.m_body[i];
            if (stmt->type == ASR::stmtType::Assignment) {
                ASR::Assignment_t* assign_ = ASR::down_cast<ASR::Assignment_t>(stmt);
                if (ASR::is_a<ASR::ArraySection_t>(*assign_->m_target)) {
                   stmt = convert_array_section_assignment_to_array_item_assignment(assign_, var);
                   pass_result.push_back(al, stmt);
                } else {
                    uint64_t h = get_hash((ASR::asr_t*) assign_);
                    assignment_hash[h] = idx_vars;
                }
            }
            if_body.push_back(al, stmt);
        }

        Vec<ASR::stmt_t*> orelse_body;
        orelse_body.reserve(al, x.n_orelse);
        for (size_t i = 0; i < x.n_orelse; i++) {
            if (ASR::is_a<ASR::Where_t>(*x.m_orelse[i])) {
                ASR::Where_t* where = ASR::down_cast<ASR::Where_t>(x.m_orelse[i]);
                ASR::stmt_t* if_stmt = handle_If(*where, where->m_test, var, where->base.base.loc, idx_vars);
                orelse_body.push_back(al, if_stmt);
            } else {
                ASR::stmt_t* stmt = x.m_orelse[i];
                if (stmt->type == ASR::stmtType::Assignment) {
                    ASR::Assignment_t* assign_ = ASR::down_cast<ASR::Assignment_t>(stmt);
                    if (ASR::is_a<ASR::ArraySection_t>(*assign_->m_target)) {
                        stmt = convert_array_section_assignment_to_array_item_assignment(assign_, var);
                        pass_result.push_back(al, stmt);
                    } else {
                        uint64_t h = get_hash((ASR::asr_t*) assign_);
                        assignment_hash[h] = idx_vars;
                    }
                }
                orelse_body.push_back(al, stmt);
            }
        }
        ASR::stmt_t* if_stmt = ASRUtils::STMT(ASR::make_If_t(al, loc, test_new, if_body.p, if_body.size(), orelse_body.p, orelse_body.size()));
        return if_stmt;
    }

    ASR::stmt_t* nested_do_loop(Location &loc,ASR::expr_t* left_array,Vec<ASR::expr_t*> &idx_vars,Vec<ASR::stmt_t*> &do_loop_body_deep,int current_idx=0){
        // Base Case.
        if(current_idx == (int)idx_vars.size()){
            return nullptr;
        }

        // Create Head.
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = idx_vars[current_idx];
        head.m_start = PassUtils::get_bound(left_array, current_idx+1, "lbound", al);
        head.m_end = PassUtils::get_bound(left_array, current_idx+1, "ubound", al);
        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        head.m_increment = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, int32_type));

        // Create Body
        ASR::stmt_t* nested = nested_do_loop(loc, left_array, idx_vars, do_loop_body_deep, current_idx+1); 
        Vec<ASR::stmt_t*> do_loop_body;
        do_loop_body.reserve(al, 1);
        if(!nested){
            // Use the do_loop_body initialized in the caller function when we hit last idx_var.
            do_loop_body = do_loop_body_deep; 
        } else {
            do_loop_body.push_back(al, nested);
        }

        return ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr, head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
    }
    void visit_Where(const ASR::Where_t& x) {
        ASR::Where_t& xx = const_cast<ASR::Where_t&>(x);
        Location loc = x.base.base.loc;
        ASR::expr_t* test = x.m_test;
        ASR::IntegerCompare_t* int_cmp = nullptr;
        ASR::RealCompare_t* real_cmp = nullptr;
        ASR::LogicalBinOp_t* log_bin_op = nullptr;
        ASR::expr_t* left;
        ASR::expr_t* opt_left = nullptr;
        ASR::stmt_t* assign_stmt = nullptr;
        ASR::stmt_t* if_stmt = nullptr;

        // We initially handle this case for logical arrays inside the AST node visitor. We need to handle it here
        // again to work with the changes introduced during the ASR passes before this.
        if (ASRUtils::is_array(ASRUtils::expr_type(test))
            && ASR::is_a<ASR::Logical_t>(
                *ASRUtils::type_get_past_array_pointer_allocatable(ASRUtils::expr_type(test)))) {
            if (!ASR::is_a<ASR::IntegerCompare_t>(*test) && !ASR::is_a<ASR::RealCompare_t>(*test)
                && !ASR::is_a<ASR::LogicalBinOp_t>(*test)) {
                ASR::expr_t* logical_true = ASRUtils::EXPR(ASR::make_LogicalConstant_t(
                    al,
                    x.base.base.loc,
                    true,
                    ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc, 4))));
                test = ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al,
                                                               x.base.base.loc,
                                                               test,
                                                               ASR::logicalbinopType::Eqv,
                                                               logical_true,
                                                               ASRUtils::expr_type(test),
                                                               nullptr));
            }
        }

        if (ASR::is_a<ASR::IntegerCompare_t>(*test)) {
            int_cmp = ASR::down_cast<ASR::IntegerCompare_t>(test);
            left = int_cmp->m_left;
        } else if (ASR::is_a<ASR::RealCompare_t>(*test)) {
            real_cmp = ASR::down_cast<ASR::RealCompare_t>(test);
            left = real_cmp->m_left;
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*test)) {
            log_bin_op = ASR::down_cast<ASR::LogicalBinOp_t>(test);
            left = log_bin_op->m_left;
        } else {
            throw LCompilersException("Unsupported type, " + ASRUtils::type_to_str_python(ASRUtils::expr_type(test)));
        }

        // create index variables.
        Vec<ASR::expr_t*> idx_vars;
        PassUtils::create_idx_vars(idx_vars, ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(left)), loc, al, current_scope);
        ASR::expr_t* var = idx_vars[0];

        if (ASR::is_a<ASR::FunctionCall_t>(*left)) {
            // Create an assignment `return_var = left` and replace function call with return_var
            ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(left);
            uint64_t h = get_hash((ASR::asr_t*) fc->m_name);
            ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(fc->m_name);
            ASR::expr_t* return_var_expr = fn->m_return_var;
            ASR::Variable_t* return_var = ASRUtils::EXPR2VAR(return_var_expr);
            ASR::expr_t* new_return_var_expr = PassUtils::create_var(1,
                return_var->m_name, return_var->base.base.loc,
                return_var->m_type, al, current_scope);
            assign_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, loc, new_return_var_expr, left, nullptr));
            opt_left = new_return_var_expr;
            return_var_hash[h] = opt_left;
        }

        if (opt_left && ASR::is_a<ASR::IntegerCompare_t>(*test)) {
            int_cmp = ASR::down_cast<ASR::IntegerCompare_t>(test);
            int_cmp->m_left = opt_left;
        }
        if (opt_left && ASR::is_a<ASR::RealCompare_t>(*test)) {
            real_cmp = ASR::down_cast<ASR::RealCompare_t>(test);
            real_cmp->m_left = opt_left;
        }
        if (opt_left && ASR::is_a<ASR::LogicalBinOp_t>(*test)) {
            log_bin_op = ASR::down_cast<ASR::LogicalBinOp_t>(test);
            log_bin_op->m_left = opt_left;
        }

        //Create do loop.
        ASR::stmt_t* doloop = nullptr;
        Vec<ASR::stmt_t*> do_loop_body;
        do_loop_body.reserve(al, 1);

        // create an if statement
        // TO DO : fix handle_if function to handle arraySections properly while doing looping on multiple dimensions.
        if (int_cmp) {
            if_stmt = handle_If(xx, ASRUtils::EXPR((ASR::asr_t*)int_cmp), var, loc, idx_vars);
        } else if (real_cmp) {
            if_stmt = handle_If(xx, ASRUtils::EXPR((ASR::asr_t*)real_cmp), var, loc, idx_vars);
        } else if (log_bin_op) {
            if_stmt = handle_If(xx, ASRUtils::EXPR((ASR::asr_t*)log_bin_op), var, loc, idx_vars);
        }
        if (assign_stmt) {
            pass_result.push_back(al, assign_stmt);
        }

        do_loop_body.push_back(al, if_stmt);
        doloop = nested_do_loop(loc, opt_left?opt_left:left, idx_vars, do_loop_body);
        pass_result.push_back(al, doloop);
    }
};

void pass_replace_where(Allocator &al, ASR::TranslationUnit_t &unit,
                        const LCompilers::PassOptions& /*pass_options*/) {
    std::map<uint64_t, Vec<ASR::expr_t*>> assignment_hash;
    std::map<uint64_t, ASR::expr_t*> return_var_hash;
    WhereVisitor v(al, assignment_hash, return_var_hash);
    v.visit_TranslationUnit(unit);
    if (assignment_hash.size() > 0) {
        VarVisitor w(al, assignment_hash, return_var_hash);
        w.visit_TranslationUnit(unit);
        PassUtils::UpdateDependenciesVisitor x(al);
        x.visit_TranslationUnit(unit);
    }
}


} // namespace LCompilers
