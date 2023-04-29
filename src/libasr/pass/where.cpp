#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/where.h>


namespace LCompilers {

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
    ReplaceVar(Allocator &al_) : al(al_), 
        current_scope(nullptr) {}

    void replace_Var(ASR::Var_t* x) {
        ASR::expr_t* expr_ = ASRUtils::EXPR(ASR::make_Var_t(al, x->base.base.loc, x->m_v));
        ASR::expr_t* new_expr_ = PassUtils::create_array_ref(expr_, idx_vars, al);
        current_expr = &new_expr_;
    }

    void replace_IntegerBinOp(ASR::IntegerBinOp_t* x) {
        ASR::expr_t** curr_expr = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x->m_left));
        this->replace_expr(x->m_left);
        ASR::expr_t* left = *current_expr;
        current_expr = curr_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x->m_right));
        this->replace_expr(x->m_right);
        ASR::expr_t* right = *current_expr;
        current_expr = curr_expr;
        ASR::expr_t* new_expr = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, x->base.base.loc, left, x->m_op, right, x->m_type, nullptr));
        current_expr = &new_expr;
    }

    void replace_RealBinOp(ASR::RealBinOp_t* x) {
        ASR::expr_t** curr_expr = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x->m_left));
        this->replace_expr(x->m_left);
        ASR::expr_t* left = *current_expr;
        current_expr = curr_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x->m_right));
        this->replace_expr(x->m_right);
        ASR::expr_t* right = *current_expr;
        current_expr = curr_expr;
        ASR::expr_t* new_expr = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, x->base.base.loc, left, x->m_op, right, x->m_type, nullptr));
        current_expr = &new_expr;
    }

};

class VarVisitor : public ASR::CallReplacerOnExpressionsVisitor<VarVisitor>
{
public:

    Allocator &al;
    ReplaceVar replacer;
    std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash;
    Vec<ASR::stmt_t*> pass_result;

    VarVisitor(Allocator &al_, std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash) : 
        al(al_), replacer(al), assignment_hash(assignment_hash) {
        pass_result.reserve(al, 1);
    }

    void call_replacer_(Vec<ASR::expr_t*> idx_vars_) {
        replacer.current_expr = current_expr;
        replacer.current_scope = current_scope;
        replacer.idx_vars = idx_vars_;
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
        ASR::expr_t** curr_expr = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x.m_target));
        this->call_replacer_(assignment_hash[h]);
        ASR::expr_t* target = *replacer.current_expr;
        current_expr = curr_expr;
        this->visit_expr(*x.m_target);
        current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
        this->call_replacer_(assignment_hash[h]);
        ASR::expr_t* value = *replacer.current_expr;
        current_expr = curr_expr;
        this->visit_expr(*x.m_value);
        ASR::stmt_t* tmp_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, target, value, nullptr));
        pass_result.push_back(al, tmp_stmt);
    }
};


class WhereVisitor : public PassUtils::PassVisitor<WhereVisitor>
{
public:
    std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash;
    WhereVisitor(Allocator &al, std::map<uint64_t, Vec<ASR::expr_t*>> &assignment_hash) : PassVisitor(al, nullptr), 
        assignment_hash(assignment_hash) {
        pass_result.reserve(al, 1);
    }

    ASR::stmt_t* handle_If(ASR::Where_t& x, ASR::expr_t* test, ASR::expr_t* var, Location& loc, Vec<ASR::expr_t*> idx_vars) {
        ASR::IntegerCompare_t* int_cmp = nullptr;
        ASR::RealCompare_t* real_cmp = nullptr;
        ASR::expr_t* left, *right;
        bool is_right_array = false;
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

        if (ASRUtils::is_array(ASRUtils::expr_type(right))) {
            is_right_array = true;
        }
        
        ASR::expr_t* left_array = PassUtils::create_array_ref(left, idx_vars, al);
        ASR::expr_t* right_array = PassUtils::create_array_ref(right, idx_vars, al);

        ASR::expr_t* test_new = ASRUtils::EXPR(
                    real_cmp?ASR::make_RealCompare_t(al, loc, left_array, real_cmp->m_op, is_right_array?right_array:right,
                                            logical_type, nullptr):
                             ASR::make_IntegerCompare_t(al, loc, left_array, int_cmp->m_op, is_right_array?right_array:right,
                                            logical_type, nullptr));
        
        
        Vec<ASR::stmt_t*> if_body;
        if_body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            ASR::stmt_t* stmt = x.m_body[i];
            if (stmt->type == ASR::stmtType::Assignment) {
                ASR::Assignment_t* assign_ = ASR::down_cast<ASR::Assignment_t>(stmt);
                uint64_t h = get_hash((ASR::asr_t*) assign_);
                assignment_hash[h] = idx_vars;
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
                    uint64_t h = get_hash((ASR::asr_t*) assign_);
                    assignment_hash[h] = idx_vars;
                }
                orelse_body.push_back(al, stmt);
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

        // create do loop head
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = var;
        head.m_start = PassUtils::get_bound(left, 1, "lbound", al);
        head.m_end = PassUtils::get_bound(left, 1, "ubound", al);
        head.m_increment = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, int32_type));

        // create do loop body
        Vec<ASR::stmt_t*> do_loop_body;
        do_loop_body.reserve(al, 1);

        // create an if statement
        ASR::stmt_t* if_stmt = handle_If(xx, test, var, loc, idx_vars);
        do_loop_body.push_back(al, if_stmt);

        doloop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, 0, head, do_loop_body.p, do_loop_body.size()));
        pass_result.push_back(al, doloop);
    }
};

void pass_replace_where(Allocator &al, ASR::TranslationUnit_t &unit,
                        const LCompilers::PassOptions& /*pass_options*/) {
    std::map<uint64_t, Vec<ASR::expr_t*>> assignment_hash;
    WhereVisitor v(al, assignment_hash);
    v.visit_TranslationUnit(unit);
    if (assignment_hash.size() > 0) {
        VarVisitor w(al, assignment_hash);
        w.visit_TranslationUnit(unit);
        PassUtils::UpdateDependenciesVisitor x(al);
        x.visit_TranslationUnit(unit);
    }
}


} // namespace LCompilers
