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
#include <libasr/pass/stmt_walk_visitor.h>

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
 *
 * A procedure that is only declared in this unit -- one read from a module
 * file under separate compilation -- has its loops decided, offloaded and
 * reported by the compilation that generates its code, and nothing here
 * emits or runs them. When a device was asked for, a loop of such a
 * procedure that the device can run is therefore unwrapped here into the
 * loop it runs, so that neither the offload nor, with `--openmp`, the OpenMP
 * pass lowers it again. A loop the device cannot run is given to the host
 * without a diagnostic, as that compilation did with
 * `--gpu-allow-cpu-fallback`: lowering it for the host threads is what
 * reshapes the procedure's interface there, and the calls here must agree
 * with it. A kernel that calls such a procedure runs its loops serially, as
 * it does any loop of a procedure it calls. Without a device the loops are
 * dispatched as any other.
 */

ASR::exec_targetType host_exec_target(const PassOptions &pass_options) {
    if (pass_options.openmp) return ASR::exec_targetType::ExecHostThreads;
    return ASR::exec_targetType::ExecSerial;
}

// Unwraps the parallel loops assigned to run serially into the loops they
// run. The constructs the canonicalization did not produce, and the loops
// assigned elsewhere, are left as they are.
class SerialParallelLoopUnwrapper :
    public ASR::StatementWalkVisitor<SerialParallelLoopUnwrapper>
{
public:
    explicit SerialParallelLoopUnwrapper(Allocator &al) :
        StatementWalkVisitor(al) {}

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        ASR::OMPRegion_t &xx = const_cast<ASR::OMPRegion_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        if (xx.m_exec_target != ASR::exec_targetType::ExecSerial ||
                !omp_region_has_clause(x,
                    ASR::omp_clauseType::OMPIndependent)) {
            return;
        }
        remove_original_stmt = true;
        pass_result.reserve(al, xx.n_body);
        for (size_t i = 0; i < xx.n_body; i++) {
            pass_result.push_back(al, xx.m_body[i]);
        }
    }
};

class ParallelDispatchVisitor :
    public ASR::BaseWalkVisitor<ParallelDispatchVisitor>
{
public:
    Allocator &al;
    const PassOptions &pass_options;
    // What the selected device can do, which is what its unsupported list
    // is written in.
    const GpuDeviceCapabilities device_caps;
    // The constructs already reported. A loop nested in a loop that failed
    // fails on the same construct, and it is reported once.
    std::set<std::pair<uint32_t, uint32_t>> reported;
    // The bodies of the separate module procedures a loop calls.
    GpuProcedureImplementations implementations;
    // Whether the procedure being walked is compiled elsewhere.
    bool compiled_elsewhere = false;

    ParallelDispatchVisitor(Allocator &al, ASR::TranslationUnit_t &unit,
            const PassOptions &pass_options_) :
        al(al), pass_options(pass_options_),
        device_caps(gpu_device_capabilities(pass_options_)),
        implementations(al, unit, pass_options_) {
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
        if (pass_options.diagnostics == nullptr) return;
        if (!reported.insert({construct.loc.first,
                construct.loc.last}).second) {
            return;
        }
        std::vector<diag::Label> labels;
        if (construct.routine_elsewhere) {
            // The procedure was compiled separately: point at the loop.
            Location head = x.base.base.loc;
            head.last = head.first;
            labels.push_back(diag::Label(
                gpu_unsupported_construct_label(construct), {head}));
        } else {
            labels.push_back(diag::Label(
                gpu_unsupported_construct_label(construct),
                {construct.loc}));
        }
        if (!construct.routine.empty() && !construct.routine_elsewhere) {
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

    // A procedure read from a module file under separate compilation is
    // only declared here: its code, and the decision for each of its
    // loops, belong to the compilation of its module.
    void visit_Function(const ASR::Function_t &x) {
        ASR::FunctionType_t *ftype = ASRUtils::get_FunctionType(x);
        if (compiled_elsewhere || !device_caps.device_selected()
                || ftype->m_abi != ASR::abiType::ExternalUndefined
                || ftype->m_deftype != ASR::deftypeType::Implementation) {
            ASR::BaseWalkVisitor<ParallelDispatchVisitor>::visit_Function(x);
            return;
        }
        compiled_elsewhere = true;
        ASR::BaseWalkVisitor<ParallelDispatchVisitor>::visit_Function(x);
        compiled_elsewhere = false;
        // Its contained procedures included.
        SerialParallelLoopUnwrapper(al).visit_Function(x);
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
                    gpu_find_unsupported_construct(x, device_caps,
                        implementations);
                if (construct.found()) {
                    if (!compiled_elsewhere) report(x, construct);
                    xx.m_exec_target = host_exec_target(pass_options);
                } else if (compiled_elsewhere) {
                    xx.m_exec_target = ASR::exec_targetType::ExecSerial;
                } else {
                    xx.m_exec_target = ASR::exec_targetType::ExecDevice;
                }
            }
        }
        ASR::BaseWalkVisitor<ParallelDispatchVisitor>::visit_OMPRegion(x);
    }
};

void pass_parallel_dispatch(Allocator &al, ASR::TranslationUnit_t &unit,
                            const PassOptions &pass_options) {
    ParallelDispatchVisitor v(al, unit, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
