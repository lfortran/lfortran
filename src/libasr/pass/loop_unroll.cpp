#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/loop_unroll.h>
#include <libasr/pass/pass_utils.h>

#include <cmath>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class LoopUnrollVisitor : public PassUtils::PassVisitor<LoopUnrollVisitor>
{
private:

    int64_t unroll_factor;

    ASRUtils::ExprStmtDuplicator node_duplicator;

public:

    LoopUnrollVisitor(Allocator &al_, size_t unroll_factor_) :
    PassVisitor(al_, nullptr),
    unroll_factor(unroll_factor_), node_duplicator(al_)
    {
        pass_result.reserve(al, 1);
    }

    void visit_DoLoop(const ASR::DoLoop_t& x) {
        if (!x.m_head.m_start || !x.m_head.m_end || x.n_orelse) return;
        ASR::do_loop_head_t x_head = x.m_head;
        ASR::expr_t* x_start = ASRUtils::expr_value(x_head.m_start);
        ASR::expr_t* x_end = ASRUtils::expr_value(x_head.m_end);
        ASR::expr_t* x_inc = nullptr;
        if( x_head.m_increment ) {
            x_inc = ASRUtils::expr_value(x_head.m_increment);
        } else {
            ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4));
            x_inc = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x_head.m_end->base.loc, 1, int32_type));
        }

        int64_t _start, _end, _inc;
        if( !ASRUtils::is_value_constant(x_start, _start) ||
            !ASRUtils::is_value_constant(x_end, _end) ||
            !ASRUtils::is_value_constant(x_inc, _inc) ) {
            return ;
        }
        int64_t loop_size = (_end - _start)/_inc + 1;
        int64_t unroll_factor_ = std::min(unroll_factor, loop_size);
        bool create_unrolled_loop = unroll_factor_ < loop_size;
        // Avoid unnecessary loop unrolling
        if( !create_unrolled_loop ) {
            return ;
        }
        Vec<ASR::stmt_t*> init_and_whileloop = PassUtils::replace_doloop(
            al, x, -1, current_scope);
        ASR::stmt_t* whileloop_stmt = init_and_whileloop[init_and_whileloop.size() - 1];
        ASR::WhileLoop_t* whileloop = ASR::down_cast<ASR::WhileLoop_t>(whileloop_stmt);

        Vec<ASR::stmt_t*> unrolled_loop;
        unrolled_loop.reserve(al, whileloop->n_body * unroll_factor_);
        for( size_t i = 0; i < whileloop->n_body; i++ ) {
            unrolled_loop.push_back(al, whileloop->m_body[i]);
        }

        for( int64_t j = 1; j < unroll_factor_; j++ ) {
            // Each copy includes the termination test, so a partial final
            // group exits with the same index as the original counted loop.
            for( size_t i = 0; i < whileloop->n_body; i++ ) {
                node_duplicator.success = true;
                ASR::stmt_t* m_body_copy = node_duplicator.duplicate_stmt(whileloop->m_body[i]);
                if( !node_duplicator.success ) {
                    return ;
                }
                unrolled_loop.push_back(al, m_body_copy);
            }
        }

        for (size_t i = 0; i + 1 < init_and_whileloop.size(); i++) {
            pass_result.push_back(al, init_and_whileloop[i]);
        }
        ASR::stmt_t* unrolled_whileloop = ASRUtils::STMT(ASR::make_WhileLoop_t(al, x.base.base.loc,
            whileloop->m_name, whileloop->m_test, unrolled_loop.p, unrolled_loop.size(),
            whileloop->m_orelse, whileloop->n_orelse));
        pass_result.push_back(al, unrolled_whileloop);
    }

};

void pass_loop_unroll(Allocator &al, ASR::TranslationUnit_t &unit,
                      const LCompilers::PassOptions& pass_options) {
    LoopUnrollVisitor v(al, pass_options.unroll_factor);
    v.visit_TranslationUnit(unit);
}


} // namespace LCompilers
