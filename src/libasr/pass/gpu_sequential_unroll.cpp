#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/gpu_sequential_unroll.h>
#include <libasr/pass/stmt_walk_visitor.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

/*
Unrolls the short counted loops of device code (see
ASRUtils::is_device_only_function) by the device's
sequential_loop_unroll_factor: 16 for Metal, while CUDA unrolls nothing.

A loop with a 32-bit index starting at a non-negative constant, a unit step
and a body of at most 32 assignments, no name and no loop-else,

    do i = a, b
        ...
    end do

becomes, for the factor 16,

    end = b
    i = a
    if (end >= a) then
        do group = a, end - 15, 16
            i = group
            ...
            i = group + 1
            ...
            i = group + 15
            ...
        end do
        do i = group, end
            ...
        end do
    end if

so that the device compiler gets the body sixteen times in a row. The
loops it makes are not unrolled again.
*/
class SequentialUnrollVisitor
    : public ASR::StatementWalkVisitor<SequentialUnrollVisitor>
{
    bool device_code = false;

    bool unroll_device_loop(const ASR::DoLoop_t &x) {
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
        for (size_t i = 0; i < x.n_body; i++) {
            if (!ASR::is_a<ASR::Assignment_t>(*x.m_body[i])) return false;
        }
        if (!PassUtils::can_unroll_iterations(al, x)) return false;

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
        std::vector<ASR::stmt_t*> body = PassUtils::unroll_iterations(al, x,
            group, 1, factor);
        ASR::stmt_t *main = b.DoLoop(group, start_expr,
            b.Sub(end, b.i_t(factor - 1, type)), body, b.i_t(factor, type));
        std::vector<ASR::stmt_t*> remainder(x.m_body, x.m_body + x.n_body);
        ASR::stmt_t *tail = b.DoLoop(x.m_head.m_v, group, end, remainder);

        pass_result.push_back(al, capture);
        pass_result.push_back(al, b.Assignment(x.m_head.m_v, start_expr));
        // A negative zero-trip bound must not underflow when subtracting the batch size.
        pass_result.push_back(al, b.If(b.GtE(end, start_expr), {main, tail}, {}));
        return true;
    }


public:
    PassOptions pass_options;
    SequentialUnrollVisitor(Allocator &al, PassOptions pass_options_) :
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
        StatementWalkVisitor::visit_DoLoop(x);
    }
};

void pass_gpu_sequential_unroll(Allocator &al, ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions &pass_options) {
    if (gpu_device_capabilities(pass_options).sequential_loop_unroll_factor
            < 2) {
        return;
    }
    SequentialUnrollVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
