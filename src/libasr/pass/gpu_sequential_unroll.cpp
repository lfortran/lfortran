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
sequential_loop_unroll_factor: 16 for Metal, 4 for HIP (AMD), while CUDA on
NVIDIA unrolls nothing.

A loop with a 32-bit index starting at a non-negative constant or at an
expression (evaluated once), a unit step and a body of at most 32
assignments, no name and no loop-else,

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

A loop that gpu_vector_lanes shares out between the lanes of an iteration
steps by the lanes, `do i = a + lane, b, lanes` (or a positive constant
times the lanes), which the host passes as a positive kernel argument
(gpu_kernel_layout.lanes). Such a loop is unrolled the same way, its copies
taking `i = group + k*lanes`, the groups stepping by `16*lanes`: each lane
then has sixteen independent loads in flight where it had one, which is what
a reduction shared out between the lanes waited on.
*/
class SequentialUnrollVisitor
    : public ASR::StatementWalkVisitor<SequentialUnrollVisitor>
{
    bool device_code = false;
    // The lanes argument of the kernel being visited, if it has one.
    ASR::symbol_t *lanes = nullptr;

    // Whether `e` is the lanes argument, or a positive constant times it,
    // possibly converted to another integer kind: a step that is positive.
    bool lanes_step(ASR::expr_t *e) {
        if (lanes == nullptr || e == nullptr) return false;
        while (ASR::is_a<ASR::Cast_t>(*e)) {
            ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
            if (c->m_kind != ASR::cast_kindType::IntegerToInteger) {
                return false;
            }
            e = c->m_arg;
        }
        if (ASR::is_a<ASR::Var_t>(*e)) {
            return ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v) == lanes;
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
            ASR::IntegerBinOp_t *m = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            if (m->m_op != ASR::binopType::Mul) return false;
            int64_t c;
            if (ASRUtils::extract_value(ASRUtils::expr_value(m->m_left), c)) {
                return c > 0 && lanes_step(m->m_right);
            }
            if (ASRUtils::extract_value(ASRUtils::expr_value(m->m_right),
                    c)) {
                return c > 0 && lanes_step(m->m_left);
            }
        }
        return false;
    }

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
        // A start that is not a constant, `do k = first(i), first(i + 1) - 1`
        // over the entries of a sparse row, is evaluated once. It is not
        // known to be non-negative, so `end - (factor - 1)` underflows when
        // the loop starts within factor - 1 of the most negative integer.
        int64_t start = 0, step = 1;
        bool constant_start = ASRUtils::extract_value(
            ASRUtils::expr_value(x.m_head.m_start), start);
        // A step of the lanes, positive but known only at run time.
        bool strided = x.m_head.m_increment
            && lanes_step(x.m_head.m_increment);
        if ((constant_start && start < 0)
                || (!strided && x.m_head.m_increment &&
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
        if (!constant_start) {
            ASR::stmt_t *capture_start;
            std::string start_name = current_scope->get_unique_name(
                "__gpu_loop_start");
            start_expr = PassUtils::create_auxiliary_variable_for_expr(
                start_expr, start_name, al, current_scope, capture_start);
            pass_result.push_back(al, capture_start);
        }
        ASR::stmt_t *capture;
        std::string end_name = current_scope->get_unique_name("__gpu_loop_end");
        ASR::expr_t *end = PassUtils::create_auxiliary_variable_for_expr(
            end_expr, end_name, al, current_scope, capture);
        std::string group_name = current_scope->get_unique_name("__gpu_loop_group");
        ASR::expr_t *group = PassUtils::create_auxiliary_variable(
            x.base.base.loc, group_name, al, current_scope, type,
            ASR::intentType::Local, nullptr, nullptr);
        std::vector<ASR::stmt_t*> body;
        ASR::stmt_t *main = nullptr, *tail = nullptr;
        std::vector<ASR::stmt_t*> remainder(x.m_body, x.m_body + x.n_body);
        if (strided) {
            // Copy k at group + k*step; the groups step by factor*step.
            ASRUtils::ExprStmtDuplicator dup(al);
            auto times = [&](int64_t k) {
                return b.Mul(b.i_t(k, type),
                    dup.duplicate_expr(x.m_head.m_increment));
            };
            for (int64_t k = 0; k < factor; k++) {
                body.push_back(b.Assignment(x.m_head.m_v,
                    k == 0 ? group : b.Add(group, times(k))));
                for (size_t i = 0; i < x.n_body; i++) {
                    body.push_back(dup.duplicate_stmt(x.m_body[i]));
                }
            }
            main = b.DoLoop(group, start_expr, b.Sub(end, times(factor - 1)),
                body, times(factor));
            tail = b.DoLoop(x.m_head.m_v, group, end, remainder,
                dup.duplicate_expr(x.m_head.m_increment));
        } else {
            body = PassUtils::unroll_iterations(al, x, group, 1, factor);
            main = b.DoLoop(group, start_expr,
                b.Sub(end, b.i_t(factor - 1, type)), body,
                b.i_t(factor, type));
            tail = b.DoLoop(x.m_head.m_v, group, end, remainder);
        }

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
        ASR::symbol_t *saved_lanes = lanes;
        device_code = ASRUtils::is_device_only_function(x);
        lanes = x.m_gpu ? x.m_gpu->m_lanes : nullptr;
        StatementWalkVisitor::visit_Function(x);
        device_code = saved;
        lanes = saved_lanes;
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
