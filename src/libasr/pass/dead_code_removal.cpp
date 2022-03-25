#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/dead_code_removal.h>
#include <libasr/pass/pass_utils.h>

#include <vector>
#include <map>
#include <utility>


namespace LFortran {

using ASR::down_cast;
using ASR::is_a;


class DeadCodeRemovalVisitor : public PassUtils::PassVisitor<DeadCodeRemovalVisitor>
{
private:

    std::string rl_path;

public:

    bool dead_code_removed;

    DeadCodeRemovalVisitor(Allocator &al_, const std::string& rl_path_) : PassVisitor(al_, nullptr),
    rl_path(rl_path_), dead_code_removed(false)
    {
        pass_result.reserve(al, 1);
    }

    void visit_If(const ASR::If_t& x) {
        ASR::If_t& xx = const_cast<ASR::If_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
        ASR::expr_t* m_test_value = ASRUtils::expr_value(x.m_test);
        bool m_test_bool;
        if( ASRUtils::is_value_constant(m_test_value, m_test_bool) ) {
            ASR::stmt_t** selected_part = nullptr;
            size_t n_selected_part = 0;
            if( m_test_bool ) {
                selected_part = x.m_body;
                n_selected_part = x.n_body;
            } else {
                selected_part = x.m_orelse;
                n_selected_part = x.n_orelse;
            }
            for( size_t i = 0; i < n_selected_part; i++ ) {
                pass_result.push_back(al, selected_part[i]);
            }
            dead_code_removed = true;
        }
    }

};

void pass_dead_code_removal(Allocator &al, ASR::TranslationUnit_t &unit,
                            const std::string& rl_path) {
    DeadCodeRemovalVisitor v(al, rl_path);
    v.visit_TranslationUnit(unit);
    LFORTRAN_ASSERT(asr_verify(unit));
}


} // namespace LFortran
