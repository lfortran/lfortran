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
`pass_replace_do_loops` transforms the ASR tree in-place. Loops of device
code (see ASRUtils::is_device_only_function) are kept: the device code
generators write them as C `for` loops (gpu_sequential_unroll, which runs
before this pass, unrolls the short ones for Metal).

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

The comparison is >= for c<0. With --fast, a loop with c = 1 or c = -1
tests `i < b` (`i > b` for c<0) if a is a constant for which `a-c` does not
overflow, and `i /= b` guarded by `if (i+c <= b)` otherwise, so that LLVM can
compute its trip count (see PassUtils::replace_doloop).
*/
class DoLoopVisitor : public ASR::StatementWalkVisitor<DoLoopVisitor>
{
    bool device_code = false;

public:
    bool use_loop_variable_after_loop = false;
    PassOptions pass_options;
    DoLoopVisitor(Allocator &al, PassOptions pass_options_) :
        StatementWalkVisitor(al), pass_options(pass_options_) {
        pass_result.reserve(al, 3);
    }

    void visit_Function(const ASR::Function_t &x) {
        bool saved = device_code;
        device_code = ASRUtils::is_device_only_function(x);
        StatementWalkVisitor::visit_Function(x);
        device_code = saved;
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (device_code) {
            // The device code generators write a counted loop as a C
            // `for` loop themselves.
            StatementWalkVisitor::visit_DoLoop(x);
            return;
        }
        pass_result = PassUtils::replace_doloop(al, x, -1,
            use_loop_variable_after_loop, this->current_scope, pass_options.fast);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (device_code) {
            // The device code generators write it as nested `for` loops.
            ASR::DoConcurrentLoop_t &xx =
                const_cast<ASR::DoConcurrentLoop_t&>(x);
            transform_stmts(xx.m_body, xx.n_body);
            return;
        }
        Vec<ASR::stmt_t*> body;body.reserve(al,1);
        for (int i = 0; i < static_cast<int>(x.n_body); i++) {
            body.push_back(al,x.m_body[i]);
        }
        for (int i = static_cast<int>(x.n_head) - 1; i > 0; i--) {
            ASR::asr_t* do_loop = ASRUtils::make_DoLoop_t_util(al, x.base.base.loc, s2c(al, ""), x.m_head[i], body.p, body.n, nullptr, 0);
            body={};body.reserve(al,1);
            body.push_back(al,ASRUtils::STMT(do_loop));
        }
        ASR::asr_t* do_loop = ASRUtils::make_DoLoop_t_util(al, x.base.base.loc, s2c(al, ""), x.m_head[0], body.p, body.n, nullptr, 0);
        const ASR::DoLoop_t &do_loop_ref = (const ASR::DoLoop_t&)(*do_loop);
        pass_result = PassUtils::replace_doloop(al, do_loop_ref, -1,
            use_loop_variable_after_loop, this->current_scope, pass_options.fast);
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
