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

    // Where a thread is in the grid exists only on the device.
    void replace_GpuThreadIndex(ASR::GpuThreadIndex_t */*x*/) {
        valid = false;
    }

    void replace_GpuBlockIndex(ASR::GpuBlockIndex_t */*x*/) {
        valid = false;
    }

    void replace_GpuBlockSize(ASR::GpuBlockSize_t */*x*/) {
        valid = false;
    }

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

ASR::expr_t* duplicate_expression(Allocator &al, ASR::expr_t *e) {
    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.success = true;
    ASR::expr_t *result = duplicator.duplicate_expr(e);
    return duplicator.success ? result : nullptr;
}

// Rewrites an extent a routine the kernel calls writes over its own dummy
// arguments into the kernel's names, by the actual arguments of the call.
class CalleeArgumentBinder :
        public ASR::BaseExprReplacer<CalleeArgumentBinder> {
    Allocator &al;
    const GpuExtentContext &ctx;

    // Whether `sym` is a dummy of the callee, and which one.
    bool is_dummy(ASR::symbol_t *sym, size_t &index) {
        for (size_t i = 0; i < ctx.callee->n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*ctx.callee->m_args[i]) &&
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(ctx.callee->m_args[i])->m_v)
                    == sym) {
                index = i;
                return true;
            }
        }
        return false;
    }

    // A dummy array has the shape it is declared with, which need not be
    // the shape of the actual argument, so its extents are not read off
    // the actual.
    bool names_dummy_array(ASR::expr_t *e) {
        ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
        size_t index;
        return ASR::is_a<ASR::Var_t>(*v) && is_dummy(
            ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(v)->m_v), index);
    }

public:
    bool valid = true;

    CalleeArgumentBinder(Allocator &al, const GpuExtentContext &ctx)
        : al(al), ctx(ctx) {}

    void replace_Var(ASR::Var_t *x) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x->m_v);
        size_t index;
        if (is_dummy(sym, index)) {
            ASR::expr_t *actual = index < ctx.n_args
                ? ctx.args[index].m_value : nullptr;
            actual = actual ? duplicate_expression(al, actual) : nullptr;
            if (actual) *current_expr = actual;
            else valid = false;
        } else if (ASRUtils::symbol_parent_symtab(sym) ==
                ctx.callee->m_symtab) {
            ASR::Variable_t *var = ASR::is_a<ASR::Variable_t>(*sym)
                ? ASR::down_cast<ASR::Variable_t>(sym) : nullptr;
            if (var && var->m_storage == ASR::storage_typeType::Parameter
                    && var->m_value) {
                *current_expr = var->m_value;
            } else {
                valid = false;
            }
        }
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        if (names_dummy_array(x->m_v)) {
            valid = false;
            return;
        }
        ASR::BaseExprReplacer<CalleeArgumentBinder>::replace_ArraySize(x);
    }

    void replace_ArrayBound(ASR::ArrayBound_t *x) {
        if (names_dummy_array(x->m_v)) {
            valid = false;
            return;
        }
        ASR::BaseExprReplacer<CalleeArgumentBinder>::replace_ArrayBound(x);
    }
};

// Rewrites an extent written in the kernel's names into one over the
// kernel's parameters and the names in `kept`: a local the kernel gives one
// value is replaced by that value, and an extent of a pointer the kernel
// associates with a section by the extent the section's ranges span.
class KernelLocalResolver :
        public ASR::BaseExprReplacer<KernelLocalResolver> {
    Allocator &al;
    const ASR::Function_t &kernel;
    const std::map<ASR::symbol_t*, ASR::expr_t*> &kept;
    int depth = 0;

    bool is_parameter(ASR::symbol_t *sym) {
        for (size_t i = 0; i < kernel.n_args; i++) {
            if (ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v == sym) {
                return true;
            }
        }
        return false;
    }

    // The value `e` stands for, itself resolved.
    ASR::expr_t* resolved(ASR::expr_t *e) {
        ASR::expr_t *copy = duplicate_expression(al, e);
        if (!copy || depth > 8) {
            valid = false;
            return e;
        }
        ASR::expr_t **saved = current_expr;
        depth++;
        current_expr = &copy;
        replace_expr(copy);
        current_expr = saved;
        depth--;
        return copy;
    }

    // The section a pointer local of the kernel is associated with, when
    // the kernel associates it with exactly one.
    ASR::expr_t* bound_section(ASR::expr_t *array) {
        ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(array);
        if (!ASR::is_a<ASR::Var_t>(*v)) return nullptr;
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(v)->m_v;
        if (is_parameter(ASRUtils::symbol_get_past_external(sym))) {
            return nullptr;
        }
        GpuExtentContext ctx;
        ctx.kernel = &kernel;
        ASR::expr_t *target = ctx.associated_target(sym);
        if (!target) return nullptr;
        target = ASRUtils::get_past_array_physical_cast(target);
        return ASR::is_a<ASR::ArraySection_t>(*target) ? target : nullptr;
    }

    // The number of elements `range` spans, never below zero.
    ASR::expr_t* range_extent(ASR::array_index_t *range,
            ASR::ttype_t *type) {
        ASRUtils::ASRBuilder b(al, range->loc);
        ASR::expr_t *lo = b.i2i_t(resolved(range->m_left), type);
        ASR::expr_t *hi = b.i2i_t(resolved(range->m_right), type);
        ASR::expr_t *step = range->m_step
            ? b.i2i_t(resolved(range->m_step), type)
            : b.i_t(1, type);
        ASR::expr_t *count = b.Div(b.Add(b.Sub(hi, lo), step), step);
        return b.Max(count, b.i_t(0, type));
    }

    // The element count of the ranges of `section` that `dim` selects, or
    // nullptr when the section has no such shape.
    ASR::expr_t* section_extent(ASR::expr_t *section, ASR::expr_t *dim,
            ASR::ttype_t *type) {
        std::vector<ASR::array_index_t*> ranges =
            gpu_section_extent_ranges(section, dim);
        if (ranges.empty()) return nullptr;
        ASRUtils::ASRBuilder b(al, section->base.loc);
        ASR::expr_t *count = nullptr;
        for (ASR::array_index_t *range : ranges) {
            ASR::expr_t *one = range_extent(range, type);
            count = count ? b.Mul(count, one) : one;
        }
        return count;
    }

public:
    bool valid = true;

    KernelLocalResolver(Allocator &al, const ASR::Function_t &kernel,
            const std::map<ASR::symbol_t*, ASR::expr_t*> &kept)
        : al(al), kernel(kernel), kept(kept) {}

    void replace_Var(ASR::Var_t *x) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x->m_v);
        if (kept.count(sym) || is_parameter(sym)) return;
        ASR::expr_t *value = gpu_local_scalar_binding(sym, kernel.m_body,
            kernel.n_body);
        if (value) {
            ASR::expr_t *replacement = resolved(value);
            *current_expr = replacement;
        }
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        ASR::expr_t *section = bound_section(x->m_v);
        if (!section) {
            ASR::BaseExprReplacer<KernelLocalResolver>::replace_ArraySize(x);
            return;
        }
        ASR::expr_t *count = section_extent(section, x->m_dim, x->m_type);
        if (count) *current_expr = count;
        else valid = false;
    }

    // A pointer associated with a section is counted from one.
    void replace_ArrayBound(ASR::ArrayBound_t *x) {
        ASR::expr_t *section = bound_section(x->m_v);
        if (!section) {
            ASR::BaseExprReplacer<KernelLocalResolver>::replace_ArrayBound(x);
            return;
        }
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        if (x->m_bound == ASR::arrayboundType::LBound) {
            *current_expr = b.i_t(1, x->m_type);
            return;
        }
        ASR::expr_t *count = x->m_dim
            ? section_extent(section, x->m_dim, x->m_type) : nullptr;
        if (count) *current_expr = count;
        else valid = false;
    }

    // A call may not have a counterpart on the host.
    void replace_FunctionCall(ASR::FunctionCall_t */*x*/) {
        valid = false;
    }
};

} // namespace

std::vector<ASR::expr_t*> gpu_host_member_extents(Allocator &al,
        const ASR::Function_t &kernel, ASR::call_arg_t *args, size_t n_args,
        const GpuMemberShape &shape,
        const std::vector<ASR::expr_t*> &subscripts) {
    // The iteration that writes the element is the one whose loop index
    // equals the subscript the element is picked by, so a local subscript
    // is the host's own subscript of the element.
    std::map<ASR::symbol_t*, ASR::expr_t*> host_values;
    if (shape.element && shape.element->n_args == subscripts.size()) {
        for (size_t d = 0; d < shape.element->n_args; d++) {
            ASR::expr_t *sub = shape.element->m_args[d].m_right;
            if (!sub || shape.element->m_args[d].m_left ||
                    shape.element->m_args[d].m_step ||
                    !ASR::is_a<ASR::Var_t>(*sub)) {
                continue;
            }
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(sub)->m_v);
            if (ASRUtils::symbol_parent_symtab(sym) != kernel.m_symtab) {
                continue;
            }
            host_values.emplace(sym, subscripts[d]);
        }
    }
    std::vector<ASR::expr_t*> extents;
    for (size_t d = 0; d < shape.shape->n_dims; d++) {
        ASR::expr_t *extent = shape.shape->m_dims[d].m_length;
        extent = extent ? duplicate_expression(al, extent) : nullptr;
        if (!extent) return {};
        if (shape.ctx.callee) {
            CalleeArgumentBinder binder(al, shape.ctx);
            binder.current_expr = &extent;
            binder.replace_expr(extent);
            if (!binder.valid) return {};
        }
        KernelLocalResolver resolver(al, kernel, host_values);
        resolver.current_expr = &extent;
        resolver.replace_expr(extent);
        if (!resolver.valid) return {};
        extent = gpu_bind_kernel_expression(al, kernel, args, n_args, extent,
            host_values);
        if (!extent) return {};
        extent = duplicate_expression(al, extent);
        if (!extent) return {};
        extents.push_back(extent);
    }
    return extents;
}

ASR::expr_t* gpu_bind_kernel_expression(Allocator &al,
        const ASR::Function_t &kernel, ASR::call_arg_t *args, size_t n_args,
        ASR::expr_t *expression,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &host_values) {
    std::map<ASR::symbol_t*, ASR::expr_t*> arguments = host_values;
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
