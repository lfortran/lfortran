#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/loop_unroll.h>
#include <libasr/pass/pass_utils.h>

#include <algorithm>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class LoopUnrollVisitor : public PassUtils::PassVisitor<LoopUnrollVisitor>
{
private:

    int64_t unroll_factor;

public:

    LoopUnrollVisitor(Allocator &al_, size_t unroll_factor_) :
    PassVisitor(al_, nullptr),
    unroll_factor(unroll_factor_)
    {
        pass_result.reserve(al, 1);
    }

    /*
    A DO loop of constant bounds and step, with n > factor iterations,

        do i = a, b, c
            ...
        end do

    becomes, with g = n / factor groups of factor iterations,

        do group = a, a + (g - 1) * factor * c, factor * c
            i = group
            ...
            i = group + c
            ...
            i = group + (factor - 1) * c
            ...
        end do
        do i = a + g * factor * c, b, c
            ...
        end do

    The second loop runs the remaining iterations and leaves i with its
    value after the original loop. A loop whose body can leave an iteration
    (EXIT, CYCLE, GO TO) is not unrolled.
    */
    void visit_DoLoop(const ASR::DoLoop_t& x) {
        ASR::do_loop_head_t head = x.m_head;
        if (!head.m_v || x.n_orelse > 0) {
            return ;
        }
        int64_t a, b, c;
        if( !ASRUtils::is_value_constant(ASRUtils::expr_value(head.m_start), a) ||
            !ASRUtils::is_value_constant(ASRUtils::expr_value(head.m_end), b) ||
            !ASRUtils::is_value_constant(ASRUtils::expr_value(head.m_increment), c) ||
            c == 0 ) {
            return ;
        }
        int64_t n = std::max<int64_t>((b - a + c) / c, 0);
        // Avoid unnecessary loop unrolling
        if( n <= unroll_factor || !PassUtils::can_unroll_iterations(al, x) ) {
            return ;
        }
        int64_t groups = n / unroll_factor;
        ASR::ttype_t* type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(head.m_v));
        std::string group_name = current_scope->get_unique_name("__loop_unroll_group");
        ASR::expr_t* group = PassUtils::create_auxiliary_variable(
            x.base.base.loc, group_name, al, current_scope, type,
            ASR::intentType::Local, nullptr, nullptr);
        ASRUtils::ASRBuilder builder(al, x.base.base.loc);
        std::vector<ASR::stmt_t*> body = PassUtils::unroll_iterations(al, x,
            group, c, unroll_factor);
        pass_result.push_back(al, builder.DoLoop(group, builder.i_t(a, type),
            builder.i_t(a + (groups - 1) * unroll_factor * c, type), body,
            builder.i_t(unroll_factor * c, type)));
        std::vector<ASR::stmt_t*> remainder(x.m_body, x.m_body + x.n_body);
        pass_result.push_back(al, builder.DoLoop(head.m_v,
            builder.i_t(a + groups * unroll_factor * c, type), head.m_end,
            remainder, head.m_increment));
    }

};

void pass_loop_unroll(Allocator &al, ASR::TranslationUnit_t &unit,
                      const LCompilers::PassOptions& pass_options) {
    LoopUnrollVisitor v(al, pass_options.unroll_factor);
    v.visit_TranslationUnit(unit);
}


} // namespace LCompilers
