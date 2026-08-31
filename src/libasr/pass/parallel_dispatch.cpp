#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/parallel_dispatch.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

/*
 * Decides, for each parallel loop, which of the three lowerings runs its
 * iterations.
 *
 * By the time this pass runs every parallel loop is a `DoConcurrentLoop`,
 * whether it was written as `do concurrent`, as an `!$omp target` region, or
 * as an `!$omp parallel do` the compiler was asked to offload. Each one is
 * still `ExecAuto`, and this pass replaces that with the one target it gets.
 * Every lowering below claims only the loops assigned to it, so the choice is
 * per loop: a program may hand one loop to the device and the next to the
 * host threads.
 *
 * The policy reads the command line only. A device is preferred when one was
 * asked for, because that is the whole point of asking; the host threads come
 * next; one thread is what is left. A loop the device turns down is demoted
 * by the offload pass itself, so it lands on the host threads rather than on
 * a single one.
 */

ASR::exec_targetType host_exec_target(const PassOptions &pass_options) {
    if (pass_options.openmp) return ASR::exec_targetType::ExecHostThreads;
    return ASR::exec_targetType::ExecSerial;
}

class ParallelDispatchVisitor :
    public ASR::BaseWalkVisitor<ParallelDispatchVisitor>
{
public:
    const PassOptions &pass_options;

    ParallelDispatchVisitor(const PassOptions &pass_options_) :
        pass_options(pass_options_) {
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        ASR::DoConcurrentLoop_t &xx = const_cast<ASR::DoConcurrentLoop_t&>(x);
        if (xx.m_exec_target == ASR::exec_targetType::ExecAuto) {
            bool gpu = pass_options.gpu_offload_metal ||
                       pass_options.gpu_offload_cuda;
            xx.m_exec_target = gpu ? ASR::exec_targetType::ExecDevice
                                   : host_exec_target(pass_options);
        }
        ASR::BaseWalkVisitor<ParallelDispatchVisitor>::visit_DoConcurrentLoop(x);
    }
};

void pass_parallel_dispatch(Allocator &/*al*/, ASR::TranslationUnit_t &unit,
                            const PassOptions &pass_options) {
    ParallelDispatchVisitor v(pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
