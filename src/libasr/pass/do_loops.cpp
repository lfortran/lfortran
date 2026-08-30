#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_do_loops.h>
#include <libasr/pass/stmt_walk_visitor.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

/*

This ASR pass replaces do loops with while loops. The function
`pass_replace_do_loops` transforms the ASR tree in-place.

Converts:

    do i = a, b, c
        ...
    end do

to:

    i = a-c
    do while (i+c <= b)
        i = i+c
        ...
    end do

The comparison is >= for c<0.
*/
class DoLoopVisitor : public ASR::StatementWalkVisitor<DoLoopVisitor>
{
public:
    bool use_loop_variable_after_loop = false;
    PassOptions pass_options;
    DoLoopVisitor(Allocator &al, PassOptions pass_options_) :
        StatementWalkVisitor(al), pass_options(pass_options_) { }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        pass_result = PassUtils::replace_doloop(al, x, -1, use_loop_variable_after_loop, this->current_scope);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (pass_options.enable_gpu_offloading) {
            return;
        }

        // Typed DO CONCURRENT controls are represented using a generated
        // Block whose symbol table owns the construct-local index variables.
        if (x.n_body == 1 && ASR::is_a<ASR::BlockCall_t>(*x.m_body[0])) {
            ASR::BlockCall_t *block_call = ASR::down_cast<ASR::BlockCall_t>(x.m_body[0]);

            if (ASR::is_a<ASR::Block_t>(*block_call->m_m)) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(block_call->m_m);
                std::string block_name = block->m_name;
                if (block_name.find("~do_concurrent_block_") == 0) {
                    Vec<ASR::stmt_t *> body;
                    body.reserve(al, block->n_body);
                    for (size_t i = 0; i < block->n_body; i++) {
                        body.push_back(al, block->m_body[i]);
                    }
                    for (int i = static_cast<int>(x.n_head) - 1;i > 0; i--) {
                        ASR::asr_t *do_loop = ASR::make_DoLoop_t(al, x.base.base.loc, s2c(al, ""), x.m_head[i], body.p, body.n, nullptr, 0);
                        body = {};
                        body.reserve(al, 1);
                        body.push_back(al, ASRUtils::STMT(do_loop));
                    }

                    ASR::asr_t *outer_do_loop = ASR::make_DoLoop_t(al, x.base.base.loc, s2c(al, ""), x.m_head[0], body.p, body.n, nullptr, 0);

                    const ASR::DoLoop_t &outer_do_loop_ref = (const ASR::DoLoop_t &)(*outer_do_loop);

                    Vec<ASR::stmt_t *> lowered_body = PassUtils::replace_doloop(al, outer_do_loop_ref, -1, use_loop_variable_after_loop, block->m_symtab);
                    block->m_body = lowered_body.p;
                    block->n_body = lowered_body.size();

                    pass_result.reserve(al, 1);
                    pass_result.push_back(al, x.m_body[0]);
                    return;
                }
            }
        }

        // Existing lowering for DO CONCURRENT loops without a generated Block.
        Vec<ASR::stmt_t *> body;
        body.reserve(al, x.n_body);

        for (size_t i = 0; i < x.n_body; i++) {
            body.push_back(al, x.m_body[i]);
        }
        for (int i = static_cast<int>(x.n_head) - 1; i > 0; i--) {
            ASR::asr_t *do_loop = ASR::make_DoLoop_t(al, x.base.base.loc, s2c(al, ""), x.m_head[i], body.p, body.n, nullptr, 0);
            body = {};
            body.reserve(al, 1);
            body.push_back(al, ASRUtils::STMT(do_loop));
        }
        ASR::asr_t *do_loop = ASR::make_DoLoop_t(al, x.base.base.loc, s2c(al, ""), x.m_head[0], body.p, body.n, nullptr, 0);
        const ASR::DoLoop_t &do_loop_ref = (const ASR::DoLoop_t &)(*do_loop);;
        pass_result = PassUtils::replace_doloop(al, do_loop_ref, -1, use_loop_variable_after_loop, this->current_scope);
    }
};

void pass_replace_do_loops(Allocator &al, ASR::TranslationUnit_t &unit,
                           const LCompilers::PassOptions& pass_options) {
    DoLoopVisitor v(al, pass_options);
    // Each call transforms only one layer of nested loops, so we call it twice
    // to transform doubly nested loops:
    v.asr_changed = true;
    v.use_loop_variable_after_loop = pass_options.use_loop_variable_after_loop;
    while( v.asr_changed ) {
        v.asr_changed = false;
        v.visit_TranslationUnit(unit);
    }
}


} // namespace LCompilers
