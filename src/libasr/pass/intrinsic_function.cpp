#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/intrinsic_function.h>
#include <libasr/pass/pass_utils.h>

#include <vector>
#include <utility>


namespace LFortran {

using ASR::down_cast;
using ASR::is_a;

/*

This ASR pass replaces the IntrinsicFunction node with a call to an
implementation in the surface language (ASR).

Call this pass if you do not want to implement intrinsic functions directly
in the backend.

*/
class IntrinsicFunctionVisitor : public PassUtils::PassVisitor<IntrinsicFunctionVisitor>
{
private:

    std::string rl_path;

public:
    IntrinsicFunctionVisitor(Allocator &al_, const std::string& rl_path_) : PassVisitor(al_, nullptr),
    rl_path(rl_path_)
    {
        pass_result.reserve(al, 1);
    }

    void visit_IntrinsicFunction(const ASR::IntrinsicFunction_t& x) {
        switch (x.m_intrinsic_id) {
            case (static_cast<int64_t>(ASRUtils::IntrinsicFunctions::LogGamma)) : {
                for (size_t i=0; i < x.n_args; i++) {
                    // TODO: handle the case if the argument changes
                    visit_expr(*x.m_args[i]);
                }
                // TODO: here we must get access to the pure ASR implementation
                // of LogGamma, as provided by the frontend, let's say
                // it is assigned to this "fc":
                // ASR::FunctionCall_t *fc = ...;
                // TODO: Return "fc" instead of "self" here
                throw LCompilersException("TODO: change IntrinsicFunction to FunctionCall");
                break;
            }
            default : {
                throw LCompilersException("Intrinsic function not implemented");
            }
        }
    }

};

void pass_replace_intrinsic_function(Allocator &al, ASR::TranslationUnit_t &unit,
                             const LCompilers::PassOptions& pass_options) {
    std::string rl_path = pass_options.runtime_library_dir;
    IntrinsicFunctionVisitor v(al, rl_path);
    v.visit_TranslationUnit(unit);
}


} // namespace LFortran
