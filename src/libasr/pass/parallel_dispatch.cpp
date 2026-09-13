#include <iostream>
#include <set>
#include <utility>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/diagnostics.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_unsupported_check.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/parallel_dispatch.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

/*
 * Decides, for each parallel loop, which of the three lowerings runs its
 * iterations.
 *
 * By the time this pass runs every parallel loop is one canonical
 * `OMPRegion`, whether it was written as `do concurrent`, as an `!$omp
 * target` region, or as an `!$omp parallel do` the compiler was asked to
 * offload. Each one is still `ExecAuto`, and this pass replaces that with the
 * one target it gets. Every lowering below claims only the regions assigned
 * to it, so the choice is per loop: a program may hand one loop to the device
 * and the next to the host threads.
 *
 * A region that does not assert the independence of its iterations is not a
 * parallel loop the canonicalization produced: it is the construct the source
 * wrote, which only the OpenMP pass lowers, and there is nothing to choose.
 *
 * When a device was asked for, a loop goes to the device unless it uses a
 * construct on the device's unsupported list (see gpu_unsupported_check.h and
 * doc/src/gpu_offloading.md). Such a loop is an error at the construct, or,
 * with `--gpu-allow-cpu-fallback`, goes to the host with a warning. Every
 * failing loop of the unit is reported before the compilation stops. Without
 * a device, the host threads come first and one thread is what is left.
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
    // What the selected device can do, which is what its unsupported list
    // is written in.
    const GpuDeviceCapabilities device_caps;
    // The constructs already reported. A loop nested in a loop that failed
    // fails on the same construct, and it is reported once.
    std::set<std::pair<uint32_t, uint32_t>> reported;

    ParallelDispatchVisitor(const PassOptions &pass_options_) :
        pass_options(pass_options_),
        device_caps(gpu_device_capabilities(pass_options_)) {
    }

    // Whether a loop that uses an unsupported construct runs on the host.
    // Showing the kernels asks for the kernels of the other loops, so it
    // does not fail on this one either.
    bool host_allowed() const {
        return pass_options.gpu_allow_cpu_fallback ||
            pass_options.gpu_kernel_source_only;
    }

    void report(const ASR::OMPRegion_t &x,
            const GpuUnsupportedConstruct &construct) {
        std::string what = gpu_unsupported_construct_message(construct,
            device_caps);
        if (pass_options.gpu_decline_stats) {
            std::cerr << "gpu-decline: unsupported: " << what << std::endl;
        }
        if (pass_options.diagnostics == nullptr) return;
        if (!reported.insert({construct.loc.first,
                construct.loc.last}).second) {
            return;
        }
        std::vector<diag::Label> labels;
        labels.push_back(diag::Label(
            gpu_unsupported_construct_label(construct), {construct.loc}));
        if (!construct.routine.empty()) {
            // The construct is in a procedure, which can be far from the
            // loop, so point at the loop as well.
            Location head = x.base.base.loc;
            head.last = head.first;
            labels.push_back(diag::Label("the parallel loop", {head},
                false));
        }
        if (host_allowed()) {
            pass_options.diagnostics->add(diag::Diagnostic(
                "parallel loop runs on the CPU: " + what,
                diag::Level::Warning, diag::Stage::ASRPass, labels));
        } else {
            pass_options.diagnostics->add(diag::Diagnostic(
                "parallel loop cannot be offloaded: " + what +
                    "; pass `--gpu-allow-cpu-fallback` to run it on the "
                    "CPU instead",
                diag::Level::Error, diag::Stage::ASRPass, labels));
        }
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        ASR::OMPRegion_t &xx = const_cast<ASR::OMPRegion_t&>(x);
        if (xx.m_exec_target == ASR::exec_targetType::ExecAuto &&
                omp_region_has_clause(x,
                    ASR::omp_clauseType::OMPIndependent)) {
            if (!device_caps.device_selected()) {
                xx.m_exec_target = host_exec_target(pass_options);
            } else {
                GpuUnsupportedConstruct construct =
                    gpu_find_unsupported_construct(x, device_caps);
                if (construct.found()) {
                    report(x, construct);
                    xx.m_exec_target = host_exec_target(pass_options);
                } else {
                    xx.m_exec_target = ASR::exec_targetType::ExecDevice;
                }
            }
        }
        ASR::BaseWalkVisitor<ParallelDispatchVisitor>::visit_OMPRegion(x);
    }
};

void pass_parallel_dispatch(Allocator &/*al*/, ASR::TranslationUnit_t &unit,
                            const PassOptions &pass_options) {
    ParallelDispatchVisitor v(pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
