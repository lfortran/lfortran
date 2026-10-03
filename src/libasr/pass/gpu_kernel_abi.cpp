#include <libasr/asr_builder.h>
#include <libasr/pass/gpu_kernel_abi.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

namespace {

class KernelArgumentBinder :
        public ASR::BaseExprReplacer<KernelArgumentBinder> {
    const std::map<ASR::symbol_t*, ASR::expr_t*> &arguments;
public:
    bool valid = true;

    explicit KernelArgumentBinder(
            const std::map<ASR::symbol_t*, ASR::expr_t*> &arguments)
        : arguments(arguments) {}

    void replace_Var(ASR::Var_t *x) {
        ASR::symbol_t *symbol = ASRUtils::symbol_get_past_external(x->m_v);
        auto arg = arguments.find(symbol);
        if (arg != arguments.end()) {
            *current_expr = arg->second;
        } else if (ASR::is_a<ASR::Variable_t>(*symbol) &&
                ASR::down_cast<ASR::Variable_t>(symbol)->m_storage ==
                    ASR::storage_typeType::Parameter) {
            ASR::expr_t *value =
                ASR::down_cast<ASR::Variable_t>(symbol)->m_value;
            if (value) *current_expr = value;
            else valid = false;
        } else {
            valid = false;
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        ASR::BaseExprReplacer<KernelArgumentBinder>
            ::replace_StructInstanceMember(x);
        ASR::symbol_t *type =
            ASRUtils::get_struct_sym_from_struct_expr(x->m_v);
        ASR::symbol_t *member = gpu_struct_lookup_member(type,
            ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(x->m_m)));
        if (!member) {
            valid = false;
            return;
        }
        x->m_m = member;
        x->m_type = ASRUtils::symbol_type(member);
    }
};

// Lays out each kernel once shared lowering has given it its final shape,
// and passes the workspace extents that layout needs from every launch. A
// kernel the layout cannot be built for is an error at its launch.
class FinalizeGpuKernels : public PassUtils::PassVisitor<FinalizeGpuKernels> {
    const PassOptions &options;

public:
    FinalizeGpuKernels(Allocator &al, const PassOptions &options)
        : PassVisitor(al, nullptr), options(options) {}

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        ASR::Function_t &kernel =
            *ASR::down_cast<ASR::Function_t>(x.m_kernel);
        if (!kernel.m_gpu) {
            GpuDecline decline;
            if (!gpu_create_kernel_layout(al, kernel, x.m_args, x.n_args,
                    decline)) {
                report_gpu_decline(options, x.base.base.loc, decline);
                return;
            }
        }
        const ASR::gpu_kernel_layout_t &layout = *kernel.m_gpu;
        if (x.n_args == kernel.n_args) return;
        LCOMPILERS_ASSERT(x.n_args == (size_t)layout.m_source_argument_count);
        ASRUtils::ASRBuilder b(al, x.base.base.loc);
        Vec<ASR::call_arg_t> args;
        args.reserve(al, kernel.n_args);
        for (size_t i = 0; i < x.n_args; i++) args.push_back(al, x.m_args[i]);
        pass_result.reserve(al, kernel.n_args - x.n_args + 1);
        for (size_t w = 0; w < layout.n_workspaces; w++) {
            const ASR::gpu_workspace_t &workspace = layout.m_workspaces[w];
            for (size_t d = 0; d < workspace.n_dims; d++) {
                const ASR::gpu_workspace_dimension_t &dim = workspace.m_dims[d];
                if (!dim.m_parameter) continue;
                ASR::expr_t *value = gpu_bind_kernel_expression(al, kernel,
                    x.m_args, x.n_args, dim.m_extent);
                LCOMPILERS_ASSERT(value != nullptr);
                ASR::expr_t *extent = b.Variable(current_scope,
                    current_scope->get_unique_name("__gpu_extent"),
                    ASRUtils::expr_type(value), ASR::intentType::Local);
                pass_result.push_back(al, b.Assignment(extent, value));
                ASR::call_arg_t arg;
                arg.loc = x.base.base.loc;
                arg.m_value = extent;
                args.push_back(al, arg);
            }
        }
        LCOMPILERS_ASSERT(args.n == kernel.n_args);
        auto &launch = const_cast<ASR::GpuKernelLaunch_t&>(x);
        launch.m_args = args.p;
        launch.n_args = args.n;
        pass_result.push_back(al, &launch.base);
    }
};

} // namespace

ASR::expr_t* gpu_bind_kernel_expression(Allocator &al,
        const ASR::Function_t &kernel, ASR::call_arg_t *args, size_t n_args,
        ASR::expr_t *expression) {
    std::map<ASR::symbol_t*, ASR::expr_t*> arguments;
    LCOMPILERS_ASSERT(n_args <= kernel.n_args);
    for (size_t i = 0; i < n_args; i++) {
        arguments.emplace(ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v,
            args[i].m_value);
    }
    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.success = true;
    ASR::expr_t *result = duplicator.duplicate_expr(expression);
    if (!duplicator.success) return nullptr;
    KernelArgumentBinder binder(arguments);
    binder.current_expr = &result;
    binder.replace_expr(result);
    return binder.valid ? result : nullptr;
}

void pass_gpu_kernel_finalize(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &options) {
    if (!gpu_device_capabilities(options).device_selected()) return;
    FinalizeGpuKernels finalizer(al, options);
    finalizer.visit_TranslationUnit(unit);
}

} // namespace LCompilers
