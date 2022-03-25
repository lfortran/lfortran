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

    DeadCodeRemovalVisitor(Allocator &al_, const std::string& rl_path_) : PassVisitor(al_, nullptr),
    rl_path(rl_path_)
    {
        pass_result.reserve(al, 1);
    }

};

void pass_dead_code_removal(Allocator &al, ASR::TranslationUnit_t &unit,
                            const std::string& rl_path) {
    DeadCodeRemovalVisitor v(al, rl_path);
    v.visit_TranslationUnit(unit);
    LFORTRAN_ASSERT(asr_verify(unit));
}


} // namespace LFortran
