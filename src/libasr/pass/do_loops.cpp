#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_do_loops.h>
#include <libasr/pass/stmt_walk_visitor.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/gpu_decline.h>

#include <set>

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

The comparison is >= for c<0. With --fast, a loop with c = 1 or c = -1
tests `i < b` (`i > b` for c<0) if a is a constant for which `a-c` does not
overflow, and `i /= b` guarded by `if (i+c <= b)` otherwise, so that LLVM can
compute its trip count (see PassUtils::replace_doloop).
*/
class DoLoopVisitor : public ASR::StatementWalkVisitor<DoLoopVisitor>
{
    bool device_code = false;
    std::set<const ASR::DoLoop_t*> counted_loops;

    bool unroll_device_loop(const ASR::DoLoop_t &x) {
        if (counted_loops.count(&x)) return true;
        int factor = gpu_device_capabilities(pass_options).sequential_loop_unroll_factor;
        if (!device_code || factor < 2 || !current_scope || !x.m_head.m_v
                || !x.m_head.m_start || !x.m_head.m_end
                || (x.m_name && x.m_name[0]) || x.n_orelse
                || x.n_body == 0 || x.n_body > 32) return false;
        ASR::ttype_t *type = ASRUtils::expr_type(x.m_head.m_v);
        if (!ASR::is_a<ASR::Integer_t>(*type)
                || ASRUtils::extract_kind_from_ttype_t(type) != 4) return false;
        for (ASR::expr_t *control : {x.m_head.m_start, x.m_head.m_end}) {
            ASR::ttype_t *control_type = ASRUtils::expr_type(control);
            if (!ASR::is_a<ASR::Integer_t>(*control_type)
                    || ASRUtils::extract_kind_from_ttype_t(control_type) != 4) return false;
        }
        int64_t start, step = 1;
        if (!ASRUtils::extract_value(ASRUtils::expr_value(x.m_head.m_start), start)
                || start < 0
                || (x.m_head.m_increment &&
                    !ASRUtils::extract_value(
                        ASRUtils::expr_value(x.m_head.m_increment), step))
                || step != 1) return false;

        // No control transfer can enter or leave one of the duplicated iterations.
        ASRUtils::ExprStmtDuplicator duplicator(al);
        std::vector<std::vector<ASR::stmt_t*>> copies(factor);
        for (int k = 0; k < factor; k++) {
            for (size_t i = 0; i < x.n_body; i++) {
                if (!ASR::is_a<ASR::Assignment_t>(*x.m_body[i])) return false;
                duplicator.success = true;
                ASR::stmt_t *copy = duplicator.duplicate_stmt(x.m_body[i]);
                if (!duplicator.success) return false;
                copies[k].push_back(copy);
            }
        }

        ASR::expr_t *start_expr = x.m_head.m_start;
        ASR::expr_t *end_expr = x.m_head.m_end;
        ASRUtils::ASRBuilder b(al, x.base.base.loc);
        ASR::stmt_t *capture;
        std::string end_name = current_scope->get_unique_name("__gpu_loop_end");
        ASR::expr_t *end = PassUtils::create_auxiliary_variable_for_expr(
            end_expr, end_name, al, current_scope, capture);
        std::string group_name = current_scope->get_unique_name("__gpu_loop_group");
        ASR::expr_t *group = PassUtils::create_auxiliary_variable(
            x.base.base.loc, group_name, al, current_scope, type,
            ASR::intentType::Local, nullptr, nullptr);
        std::vector<ASR::stmt_t*> body;
        for (int k = 0; k < factor; k++) {
            body.push_back(b.Assignment(x.m_head.m_v,
                k == 0 ? group : b.Add(group, b.i_t(k, type))));
            body.insert(body.end(), copies[k].begin(), copies[k].end());
        }
        ASR::stmt_t *main = b.DoLoop(group, start_expr,
            b.Sub(end, b.i_t(factor - 1, type)), body, b.i_t(factor, type));
        std::vector<ASR::stmt_t*> remainder(x.m_body, x.m_body + x.n_body);
        ASR::stmt_t *tail = b.DoLoop(x.m_head.m_v, group, end, remainder);
        counted_loops.insert(ASR::down_cast<ASR::DoLoop_t>(main));
        counted_loops.insert(ASR::down_cast<ASR::DoLoop_t>(tail));

        pass_result.push_back(al, capture);
        pass_result.push_back(al, b.Assignment(x.m_head.m_v, start_expr));
        // A negative zero-trip bound must not underflow when subtracting the batch size.
        pass_result.push_back(al, b.If(b.GtE(end, start_expr), {main, tail}, {}));
        return true;
    }

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
        if (unroll_device_loop(x)) return;
        pass_result = PassUtils::replace_doloop(al, x, -1,
            use_loop_variable_after_loop, this->current_scope, pass_options.fast);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        Vec<ASR::stmt_t*> body;body.reserve(al,1);
        for (int i = 0; i < static_cast<int>(x.n_body); i++) {
            body.push_back(al,x.m_body[i]);
        }
        for (int i = static_cast<int>(x.n_head) - 1; i > 0; i--) {
            ASR::asr_t* do_loop = ASR::make_DoLoop_t(al, x.base.base.loc, s2c(al, ""), x.m_head[i], body.p, body.n, nullptr, 0);
            body={};body.reserve(al,1);
            body.push_back(al,ASRUtils::STMT(do_loop));
        }
        ASR::asr_t* do_loop = ASR::make_DoLoop_t(al, x.base.base.loc, s2c(al, ""), x.m_head[0], body.p, body.n, nullptr, 0);
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
