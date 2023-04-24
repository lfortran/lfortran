#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/where.h>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class WhereVisitor : public PassUtils::PassVisitor<WhereVisitor>
{
public:
    std::map<ASR::symbol_t*, ASR::expr_t*> &replace_expr_map;
    ASR::expr_t* tmp;
    ASR::stmt_t* tmp_stmt;
    WhereVisitor(Allocator &al, std::map<ASR::symbol_t*, ASR::expr_t*> &replace_expr_map) : PassVisitor(al, nullptr), replace_expr_map(replace_expr_map) {
        pass_result.reserve(al, 1);
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        tmp_stmt = nullptr;
        ASR::expr_t* target = x.m_target;
        ASR::expr_t* value = x.m_value;
        visit_expr(*x.m_target);
        if (tmp) target = tmp;
        visit_expr(*x.m_value);
        if (tmp) value = tmp;
        tmp_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, target, value, nullptr));
    }

    void visit_expr(ASR::expr_t &x) {
        tmp = nullptr;
        if (ASR::is_a<ASR::Var_t>(x)) {
            ASR::Var_t* var = down_cast<ASR::Var_t>(&x);
            ASR::symbol_t* sym = var->m_v;
            if (replace_expr_map.find(sym) != replace_expr_map.end()) {
                ASR::expr_t* new_expr = replace_expr_map[sym];
                tmp = new_expr;
            }
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(x)) {
            ASR::IntegerBinOp_t* integer_ = ASR::down_cast<ASR::IntegerBinOp_t>(&x);
            visit_expr(*integer_->m_left);
            if (tmp) integer_->m_left = tmp;
            visit_expr(*integer_->m_right);
            if (tmp) integer_->m_right = tmp;
            tmp = ASRUtils::EXPR((ASR::asr_t*) integer_);
        }
        if (ASR::is_a<ASR::RealBinOp_t>(x)) {
            ASR::RealBinOp_t* real_ = ASR::down_cast<ASR::RealBinOp_t>(&x);
            visit_expr(*real_->m_left);
            if (tmp) real_->m_left = tmp;
            visit_expr(*real_->m_right);
            if (tmp) real_->m_right = tmp;
            tmp = ASRUtils::EXPR((ASR::asr_t*) real_);
        }
    }

    ASR::stmt_t* handle_If(ASR::Where_t& x, ASR::expr_t* test, ASR::expr_t* var, Location& loc) {
        ASR::IntegerCompare_t* int_cmp = nullptr;
        ASR::RealCompare_t* real_cmp = nullptr;
        ASR::expr_t* left, *right;
        bool is_right_array = false;
        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4, nullptr, 0));
        ASR::ttype_t* real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4, nullptr, 0));
        ASR::ttype_t* logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4,nullptr, 0));

        if (ASR::is_a<ASR::IntegerCompare_t>(*test)) {
            int_cmp = ASR::down_cast<ASR::IntegerCompare_t>(test);
            left = int_cmp->m_left;
            right = int_cmp->m_right;
        }

        if (ASR::is_a<ASR::RealCompare_t>(*test)) {
            real_cmp = ASR::down_cast<ASR::RealCompare_t>(test);
            left = real_cmp->m_left;
            right = real_cmp->m_right;
        }

        ASR::Var_t* left_var = ASR::down_cast<ASR::Var_t>(left);
        ASR::Var_t* right_var = nullptr;
        if (ASRUtils::is_array(ASRUtils::expr_type(right))) {
            is_right_array = true;
            if (ASR::is_a<ASR::Var_t>(*right)) {
                right_var = ASR::down_cast<ASR::Var_t>(right);
            }
        }

        ASR::array_index_t left_array_index;
        left_array_index.loc = loc;
        left_array_index.m_left = nullptr;
        left_array_index.m_right = var;
        left_array_index.m_step = nullptr;


        ASR::array_index_t right_array_index;
        right_array_index.loc = loc;
        right_array_index.m_left = nullptr;
        right_array_index.m_right = var;
        right_array_index.m_step = nullptr;

        // create ArrayItem for left and right
        Vec<ASR::array_index_t> left_array_index_vec;
        left_array_index_vec.reserve(al, 1);
        left_array_index_vec.push_back(al, left_array_index);

        Vec<ASR::array_index_t> right_array_index_vec;
        right_array_index_vec.reserve(al, 1);
        right_array_index_vec.push_back(al, right_array_index);
        ASR::expr_t* left_array = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, left, left_array_index_vec.p, left_array_index_vec.size(),
                                                real_cmp?real_type:int32_type, ASR::arraystorageType::ColMajor, nullptr));
        ASR::expr_t* right_array = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, right, right_array_index_vec.p, right_array_index_vec.size(),
                                                real_cmp?real_type:int32_type, ASR::arraystorageType::ColMajor, nullptr));
        
        ASR::expr_t* test_new = ASRUtils::EXPR(
                    real_cmp?ASR::make_RealCompare_t(al, loc, left_array, real_cmp->m_op, is_right_array?right_array:right,
                                            logical_type, nullptr):
                             ASR::make_IntegerCompare_t(al, loc, left_array, int_cmp->m_op, is_right_array?right_array:right,
                                            logical_type, nullptr));
        
        // populate replace expression map
        replace_expr_map[left_var->m_v] = left_array;
        if (right_var) {
            replace_expr_map[right_var->m_v] = right_array;
        }
        
        Vec<ASR::stmt_t*> if_body;
        if_body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            ASR::stmt_t* stmt = x.m_body[i];
            visit_stmt(*stmt);
            if (tmp_stmt) if_body.push_back(al, tmp_stmt);
            else if_body.push_back(al, stmt);
        }

        Vec<ASR::stmt_t*> orelse_body;
        orelse_body.reserve(al, x.n_orelse);
        for (size_t i = 0; i < x.n_orelse; i++) {
            if (ASR::is_a<ASR::Where_t>(*x.m_orelse[i])) {
                ASR::Where_t* where = ASR::down_cast<ASR::Where_t>(x.m_orelse[i]);
                ASR::stmt_t* if_stmt = handle_If(*where, where->m_test, var, where->base.base.loc);
                orelse_body.push_back(al, if_stmt);
            } else {
                ASR::stmt_t* stmt = x.m_orelse[i];
                visit_stmt(*stmt);
                if (tmp_stmt) orelse_body.push_back(al, tmp_stmt);
                else orelse_body.push_back(al, stmt);
            }
        }
        ASR::stmt_t* if_stmt = ASRUtils::STMT(ASR::make_If_t(al, loc, test_new, if_body.p, if_body.size(), orelse_body.p, orelse_body.size()));  
        return if_stmt;  
    }

    void visit_Where(const ASR::Where_t& x) {
        ASR::Where_t& xx = const_cast<ASR::Where_t&>(x);
        Location loc = x.base.base.loc;
        ASR::expr_t* test = x.m_test;
        ASR::IntegerCompare_t* int_cmp = nullptr;
        ASR::RealCompare_t* real_cmp = nullptr;
        ASR::expr_t* left;

        if (ASR::is_a<ASR::IntegerCompare_t>(*test)) {
            int_cmp = ASR::down_cast<ASR::IntegerCompare_t>(test);
            left = int_cmp->m_left;
        }

        if (ASR::is_a<ASR::RealCompare_t>(*test)) {
            real_cmp = ASR::down_cast<ASR::RealCompare_t>(test);
            left = real_cmp->m_left;
        }

        // Construct a do loop
        ASR::stmt_t* doloop = nullptr;

        // create a index variable
        Vec<ASR::expr_t*> idx_vars;
        PassUtils::create_idx_vars(idx_vars, 1, loc, al, current_scope);
        ASR::expr_t* var = idx_vars[0];

        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4, nullptr, 0));
        ASR::ttype_t* real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4, nullptr, 0));

        // create do loop head
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = var;
        head.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0, int32_type));
        head.m_end = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc, left, nullptr, real_cmp?real_type:int32_type, nullptr));
        head.m_increment = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, int32_type));

        // create do loop body
        Vec<ASR::stmt_t*> do_loop_body;
        do_loop_body.reserve(al, 1);

        // create an if statement
        ASR::stmt_t* if_stmt = handle_If(xx, test, var, loc);
        do_loop_body.push_back(al, if_stmt);

        doloop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, 0, head, do_loop_body.p, do_loop_body.size()));
        pass_result.push_back(al, doloop);
    }
};

void pass_replace_where(Allocator &al, ASR::TranslationUnit_t &unit,
                        const LCompilers::PassOptions& /*pass_options*/) {
    std::map<ASR::symbol_t*, ASR::expr_t*> replace_expr_map = {};
    WhereVisitor v(al, replace_expr_map);
    v.visit_TranslationUnit(unit);
}


} // namespace LCompilers
