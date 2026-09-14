#include <memory>
#include <set>

#include <libasr/asr_builder.h>
#include <libasr/pass/gpu_kernel_abi.h>
#include <libasr/pass/gpu_offload_rewrite.h>
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

// Whether a kernel's parameters include `sym`.
bool is_kernel_parameter(const ASR::Function_t &kernel, ASR::symbol_t *sym) {
    for (size_t i = 0; i < kernel.n_args; i++) {
        if (ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v == sym) {
            return true;
        }
    }
    return false;
}

ASR::expr_t* duplicate_expression(Allocator &al, ASR::expr_t *e) {
    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.success = true;
    ASR::expr_t *result = duplicator.duplicate_expr(e);
    return duplicator.success ? result : nullptr;
}

// The number of elements the ranges of `section` that `dim` selects span,
// each range never below zero, with each bound read through `value`, or
// nullptr when the section has no such shape or a bound has no value.
ASR::expr_t* section_element_count(Allocator &al, ASR::expr_t *section,
        ASR::expr_t *dim, ASR::ttype_t *type,
        const std::function<ASR::expr_t*(ASR::expr_t*)> &value) {
    std::vector<ASR::array_index_t*> ranges =
        gpu_section_extent_ranges(section, dim);
    if (ranges.empty()) return nullptr;
    ASRUtils::ASRBuilder b(al, section->base.loc);
    ASR::expr_t *count = nullptr;
    for (ASR::array_index_t *range : ranges) {
        ASR::expr_t *lo = range->m_left ? value(range->m_left) : nullptr;
        ASR::expr_t *hi = range->m_right ? value(range->m_right) : nullptr;
        ASR::expr_t *step = range->m_step ? value(range->m_step)
            : b.i_t(1, type);
        if (!lo || !hi || !step) return nullptr;
        lo = b.i2i_t(lo, type);
        hi = b.i2i_t(hi, type);
        if (range->m_step) step = b.i2i_t(step, type);
        ASR::expr_t *one = b.Max(b.Div(b.Add(b.Sub(hi, lo), step), step),
            b.i_t(0, type));
        count = count ? b.Mul(count, one) : one;
    }
    return count;
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

    // The section a pointer local of the callee is associated with, when
    // the callee associates it with exactly one.
    ASR::expr_t* associated_section(ASR::expr_t *array) {
        ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(array);
        if (!ASR::is_a<ASR::Var_t>(*v)) return nullptr;
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(v)->m_v);
        size_t index;
        if (ASRUtils::symbol_parent_symtab(sym) != ctx.callee->m_symtab ||
                is_dummy(sym, index)) {
            return nullptr;
        }
        ASR::expr_t *target = ctx.associated_target(sym);
        if (!target) return nullptr;
        target = ASRUtils::get_past_array_physical_cast(target);
        return ASR::is_a<ASR::ArraySection_t>(*target) ? target : nullptr;
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        if (names_dummy_array(x->m_v)) {
            valid = false;
            return;
        }
        ASR::expr_t *section = associated_section(x->m_v);
        if (!section) {
            ASR::BaseExprReplacer<CalleeArgumentBinder>::replace_ArraySize(x);
            return;
        }
        // The section's bounds read the callee's names, bound in turn.
        ASR::expr_t *count = section_element_count(al, section, x->m_dim,
            x->m_type, [&](ASR::expr_t *e) {
                return duplicate_expression(al, e);
            });
        if (!count) {
            valid = false;
            return;
        }
        *current_expr = count;
        replace_expr(*current_expr);
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
        return is_kernel_parameter(kernel, sym);
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

    // The element count of the ranges of `section` that `dim` selects, or
    // nullptr when the section has no such shape.
    ASR::expr_t* section_extent(ASR::expr_t *section, ASR::expr_t *dim,
            ASR::ttype_t *type) {
        return section_element_count(al, section, dim, type,
            [&](ASR::expr_t *e) { return resolved(e); });
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

// Whether an expression reads where a thread is in the grid.
class GridPositionReader : public ASR::BaseWalkVisitor<GridPositionReader> {
public:
    bool found = false;

    void visit_GpuThreadIndex(const ASR::GpuThreadIndex_t &/*x*/) {
        found = true;
    }

    void visit_GpuBlockIndex(const ASR::GpuBlockIndex_t &/*x*/) {
        found = true;
    }

    void visit_GpuBlockSize(const ASR::GpuBlockSize_t &/*x*/) {
        found = true;
    }
};

bool reads_grid_position(ASR::expr_t *e) {
    GridPositionReader reader;
    reader.visit_expr(*e);
    return reader.found;
}

// Whether an expression calls a procedure, which need not have a
// counterpart on the host.
class CallReader : public ASR::BaseWalkVisitor<CallReader> {
public:
    bool found = false;

    void visit_FunctionCall(const ASR::FunctionCall_t &/*x*/) {
        found = true;
    }
};

bool reads_call(ASR::expr_t *e) {
    CallReader reader;
    reader.visit_expr(*e);
    return reader.found;
}

// Replaces where a thread is in the grid by the number of the iteration the
// host goes through, counted from zero: the thread of that number in a grid
// of one block runs the same iteration as every thread the device numbers
// the same.
class GridPositionReplacer :
        public ASR::BaseExprReplacer<GridPositionReplacer> {
    Allocator &al;
    ASR::expr_t *number;
public:
    bool valid = true;

    GridPositionReplacer(Allocator &al, ASR::expr_t *number)
        : al(al), number(number) {}

    void replace_GpuThreadIndex(ASR::GpuThreadIndex_t *x) {
        if (x->m_dim != 0) {
            valid = false;
            return;
        }
        *current_expr = number;
    }

    void replace_GpuBlockIndex(ASR::GpuBlockIndex_t *x) {
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        *current_expr = b.i_t(0, x->m_type);
    }

    void replace_GpuBlockSize(ASR::GpuBlockSize_t *x) {
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        *current_expr = b.i_t(1, x->m_type);
    }
};

ASR::expr_t* as_type(ASRUtils::ASRBuilder &b, ASR::expr_t *e,
        ASR::ttype_t *type) {
    if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e)) ==
            ASRUtils::extract_kind_from_ttype_t(type)) {
        return e;
    }
    return b.i2i_t(e, type);
}

// Rewrites each subscript of an element of an array the kernel is handed so
// that it picks the same element of the array the launch hands over:
// `lbound(a, d) + (subscript - lower)`, with `lower` the lower bound the
// kernel declares. The kernel's array is the host's laid end to end.
class KernelElementRebaser :
        public ASR::BaseExprReplacer<KernelElementRebaser> {
    Allocator &al;
    const ASR::Function_t &kernel;
    ASR::call_arg_t *args;
    size_t n_args;
public:
    bool valid = true;

    KernelElementRebaser(Allocator &al, const ASR::Function_t &kernel,
            ASR::call_arg_t *args, size_t n_args)
        : al(al), kernel(kernel), args(args), n_args(n_args) {}

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        ASR::BaseExprReplacer<KernelElementRebaser>::replace_ArrayItem(x);
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(x->m_v);
        if (!ASR::is_a<ASR::Var_t>(*base)) {
            valid = false;
            return;
        }
        ASR::symbol_t *parameter = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(base)->m_v);
        size_t position = kernel.n_args;
        for (size_t i = 0; i < kernel.n_args; i++) {
            if (ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v ==
                    parameter) {
                position = i;
            }
        }
        if (position >= n_args || !args[position].m_value ||
                !ASR::is_a<ASR::Variable_t>(*parameter)) {
            valid = false;
            return;
        }
        ASR::dimension_t *dims = nullptr;
        size_t rank = ASRUtils::extract_dimensions_from_ttype(
            ASR::down_cast<ASR::Variable_t>(parameter)->m_type, dims);
        if (rank == 0 || rank != x->n_args || rank !=
                (size_t)ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(args[position].m_value))) {
            valid = false;
            return;
        }
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::ttype_t *index_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
            x->base.base.loc, 4));
        for (size_t d = 0; d < rank; d++) {
            ASR::array_index_t &index = x->m_args[d];
            if (!index.m_right || index.m_left || index.m_step ||
                    !dims[d].m_start) {
                valid = false;
                return;
            }
            // The lower bound of the array bound to the parameter, read at
            // run time: the kernel's own type need not carry it.
            ASR::expr_t *lower = ASRUtils::EXPR(ASR::make_ArrayBound_t(al,
                x->base.base.loc, base, b.i32((int)d + 1), index_type,
                ASR::arrayboundType::LBound, nullptr));
            index.m_right = b.Add(lower,
                b.Sub(as_type(b, index.m_right, index_type),
                    as_type(b, dims[d].m_start, index_type)));
        }
    }

    void replace_ArraySection(ASR::ArraySection_t */*x*/) {
        valid = false;
    }
};

// Replaces the dummy arguments of a function in an expression of its own by
// the actual arguments of a call to it. Any other variable of the function,
// or one it is not handed, has no counterpart.
class DummyArgumentSubstituter :
        public ASR::BaseExprReplacer<DummyArgumentSubstituter> {
    const ASR::Function_t &callee;
    const ASR::FunctionCall_t &call;
public:
    bool valid = true;

    DummyArgumentSubstituter(const ASR::Function_t &callee,
            const ASR::FunctionCall_t &call)
        : callee(callee), call(call) {}

    void replace_Var(ASR::Var_t *x) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x->m_v);
        for (size_t i = 0; i < callee.n_args; i++) {
            if (ASR::down_cast<ASR::Var_t>(callee.m_args[i])->m_v == sym) {
                *current_expr = call.m_args[i].m_value;
                return;
            }
        }
        valid = false;
    }
};

// Replaces a call to a function whose body is a single assignment of its
// result from its scalar dummy arguments, such as the ones intrinsics like
// `mod` are lowered to, by that expression over the actual arguments. Such a
// body reads nothing else, so its value is the same wherever it is
// evaluated. The kernel calls the device's copy of the function, which the
// host cannot call.
class ExpressionFunctionInliner :
        public ASR::BaseExprReplacer<ExpressionFunctionInliner> {
    Allocator &al;
    int depth = 0;
public:
    bool valid = true;

    explicit ExpressionFunctionInliner(Allocator &al) : al(al) {}

    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        ASR::BaseExprReplacer<ExpressionFunctionInliner>
            ::replace_FunctionCall(x);
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x->m_name);
        if (!valid || !sym || !ASR::is_a<ASR::Function_t>(*sym) ||
                depth > 8) {
            valid = false;
            return;
        }
        ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(sym);
        if (!callee->m_return_var ||
                !ASR::is_a<ASR::Var_t>(*callee->m_return_var) ||
                callee->n_body != 1 ||
                !ASR::is_a<ASR::Assignment_t>(*callee->m_body[0]) ||
                x->n_args != callee->n_args) {
            valid = false;
            return;
        }
        ASR::Assignment_t *result =
            ASR::down_cast<ASR::Assignment_t>(callee->m_body[0]);
        if (!ASR::is_a<ASR::Var_t>(*result->m_target) ||
                ASR::down_cast<ASR::Var_t>(result->m_target)->m_v !=
                    ASR::down_cast<ASR::Var_t>(callee->m_return_var)->m_v) {
            valid = false;
            return;
        }
        for (size_t i = 0; i < x->n_args; i++) {
            if (!x->m_args[i].m_value ||
                    ASRUtils::is_array(ASRUtils::expr_type(
                        x->m_args[i].m_value)) ||
                    !ASR::is_a<ASR::Var_t>(*callee->m_args[i])) {
                valid = false;
                return;
            }
        }
        ASR::expr_t *value = duplicate_expression(al, result->m_value);
        if (!value) {
            valid = false;
            return;
        }
        DummyArgumentSubstituter substituter(*callee, *x);
        substituter.current_expr = &value;
        substituter.replace_expr(value);
        if (!substituter.valid) {
            valid = false;
            return;
        }
        value = duplicate_expression(al, value);
        if (!value) {
            valid = false;
            return;
        }
        ASR::expr_t **saved = current_expr;
        depth++;
        current_expr = &value;
        replace_expr(value);
        current_expr = saved;
        depth--;
        *current_expr = value;
    }
};

} // namespace

bool gpu_host_iterations(Allocator &al, const ASR::Function_t &kernel,
        ASR::call_arg_t *args, size_t n_args,
        const std::function<ASR::expr_t*(const std::string&, ASR::ttype_t*)>
            &new_local,
        GpuHostIterations &iterations) {
    // The kernel starts by returning from a thread past the last
    // iteration: `if (position >= count) return`.
    if (kernel.n_body == 0 || !ASR::is_a<ASR::If_t>(*kernel.m_body[0])) {
        return false;
    }
    ASR::If_t *guard = ASR::down_cast<ASR::If_t>(kernel.m_body[0]);
    if (guard->n_body != 1 || guard->n_orelse != 0 ||
            !ASR::is_a<ASR::Return_t>(*guard->m_body[0]) ||
            !ASR::is_a<ASR::IntegerCompare_t>(*guard->m_test)) {
        return false;
    }
    ASR::IntegerCompare_t *past_end =
        ASR::down_cast<ASR::IntegerCompare_t>(guard->m_test);
    if (past_end->m_op != ASR::cmpopType::GtE ||
            !reads_grid_position(past_end->m_left) ||
            reads_grid_position(past_end->m_right)) {
        return false;
    }
    ASR::expr_t *count = gpu_bind_kernel_expression(al, kernel, args,
        n_args, past_end->m_right);
    if (!count) return false;
    ASR::ttype_t *position_type = ASRUtils::expr_type(past_end->m_left);
    iterations.counter = new_local("gpu_iteration", position_type);
    iterations.count = count;

    // Then it works out the loop indices from its position, one scalar
    // at a time. What it reads has to be the same before the launch: the
    // position, the scalars already worked out, and what the kernel does
    // not change.
    std::shared_ptr<GpuIterationVaryingSymbols> changed =
        gpu_symbols_changed_in(kernel.m_body, kernel.n_body);
    std::map<ASR::symbol_t*, ASR::expr_t*> host;
    // The iteration number is the host's own already.
    std::map<ASR::symbol_t*, ASR::expr_t*> bound{{ASR::down_cast<ASR::Var_t>(
        iterations.counter)->m_v, iterations.counter}};
    std::set<ASR::symbol_t*> worked_out;
    size_t end = 1;
    for (; end < kernel.n_body; end++) {
        if (!ASR::is_a<ASR::Assignment_t>(*kernel.m_body[end])) break;
        ASR::Assignment_t *step =
            ASR::down_cast<ASR::Assignment_t>(kernel.m_body[end]);
        if (!ASR::is_a<ASR::Var_t>(*step->m_target)) {
            // An element or a component holds none of the indices.
            continue;
        }
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(step->m_target)->m_v);
        ASR::Variable_t *var = ASR::is_a<ASR::Variable_t>(*target)
            ? ASR::down_cast<ASR::Variable_t>(target) : nullptr;
        if (!var || ASRUtils::symbol_parent_symtab(target) !=
                kernel.m_symtab || is_kernel_parameter(kernel, target) ||
                ASRUtils::is_array(var->m_type) ||
                !ASR::is_a<ASR::Integer_t>(*var->m_type) ||
                reads_call(step->m_value) ||
                gpu_reads_changed(*changed, step->m_value, &worked_out)) {
            break;
        }
        ASR::expr_t *value = duplicate_expression(al, step->m_value);
        if (!value) break;
        GridPositionReplacer position(al, iterations.counter);
        position.current_expr = &value;
        position.replace_expr(value);
        if (!position.valid) break;
        value = gpu_bind_kernel_expression(al, kernel, args, n_args, value,
            bound);
        if (!value) break;
        auto local = host.find(target);
        if (local == host.end()) {
            local = host.emplace(target, new_local(
                std::string("gpu_") + var->m_name, var->m_type)).first;
            bound.emplace(target, local->second);
        }
        ASRUtils::ASRBuilder b(al, step->base.base.loc);
        iterations.prologue.push_back(b.Assignment(local->second, value));
        worked_out.insert(target);
    }
    // A scalar the rest of the kernel changes holds that value only at its
    // start.
    std::shared_ptr<GpuIterationVaryingSymbols> changed_later =
        gpu_symbols_changed_in(kernel.m_body + end, kernel.n_body - end);
    for (auto &value : host) {
        ASR::expr_t *name = ASRUtils::EXPR(ASR::make_Var_t(al,
            kernel.base.base.loc, value.first));
        if (!gpu_reads_changed(*changed_later, name)) {
            iterations.indices.emplace(value.first, value.second);
        }
    }
    return !iterations.indices.empty();
}

std::vector<ASR::expr_t*> gpu_host_element_subscripts(Allocator &al,
        const ASR::Function_t &kernel, ASR::call_arg_t *args, size_t n_args,
        const GpuMemberShape &shape, ASR::expr_t *array,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &indices) {
    if (!shape.element) return {};
    ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(
        shape.element->m_v);
    if (!ASR::is_a<ASR::Var_t>(*base)) return {};
    ASR::symbol_t *parameter = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(base)->m_v);
    if (!is_kernel_parameter(kernel, parameter) ||
            !ASR::is_a<ASR::Variable_t>(*parameter)) {
        return {};
    }
    ASR::dimension_t *dims = nullptr;
    size_t rank = ASRUtils::extract_dimensions_from_ttype(
        ASR::down_cast<ASR::Variable_t>(parameter)->m_type, dims);
    if (rank == 0 || rank != shape.element->n_args || rank !=
            (size_t)ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(array))) {
        return {};
    }
    std::shared_ptr<GpuIterationVaryingSymbols> changed =
        gpu_symbols_changed_in(kernel.m_body, kernel.n_body);
    std::set<ASR::symbol_t*> ignored;
    for (auto &index : indices) ignored.insert(index.first);
    ASRUtils::ASRBuilder b(al, shape.element->base.base.loc);
    ASR::ttype_t *index_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
        shape.element->base.base.loc, 4));
    std::vector<ASR::expr_t*> subscripts;
    for (size_t d = 0; d < rank; d++) {
        const ASR::array_index_t &index = shape.element->m_args[d];
        if (!index.m_right || index.m_left || index.m_step ||
                !dims[d].m_start || reads_call(index.m_right) ||
                gpu_reads_changed(*changed, index.m_right, &ignored)) {
            return {};
        }
        ASR::expr_t *subscript = gpu_bind_kernel_expression(al, kernel,
            args, n_args, index.m_right, indices);
        ASR::expr_t *lower = gpu_bind_kernel_expression(al, kernel, args,
            n_args, dims[d].m_start);
        if (!subscript || !lower) return {};
        // The kernel's array is the host's laid end to end, so an element
        // is as far from the first one on either side.
        subscripts.push_back(b.Add(b.GetLBound(array, d + 1),
            b.Sub(as_type(b, subscript, index_type),
                as_type(b, lower, index_type))));
    }
    return subscripts;
}

bool gpu_host_write_conditions(Allocator &al, const ASR::Function_t &kernel,
        ASR::call_arg_t *args, size_t n_args, const GpuMemberShape &shape,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &indices,
        std::vector<ASR::expr_t*> &tests) {
    if (!shape.conditions_known || kernel.n_body == 0) return false;
    // Past its first statement, `if (position >= count) return`, an
    // iteration that stops early skips the writes after that point.
    if (gpu_transfers_control(kernel.m_body + 1, kernel.n_body - 1, true)) {
        return false;
    }
    std::shared_ptr<GpuIterationVaryingSymbols> changed =
        gpu_symbols_changed_in(kernel.m_body, kernel.n_body);
    std::set<ASR::symbol_t*> ignored;
    for (auto &index : indices) ignored.insert(index.first);
    for (auto &condition : shape.conditions) {
        ASR::expr_t *test = duplicate_expression(al, condition.first);
        if (!test) return false;
        ExpressionFunctionInliner inliner(al);
        inliner.current_expr = &test;
        inliner.replace_expr(test);
        // The host evaluates the test before the loop runs.
        if (!inliner.valid || reads_call(test) || reads_grid_position(test) ||
                gpu_reads_changed(*changed, test, &ignored)) {
            return false;
        }
        KernelElementRebaser rebaser(al, kernel, args, n_args);
        rebaser.current_expr = &test;
        rebaser.replace_expr(test);
        if (!rebaser.valid) return false;
        test = gpu_bind_kernel_expression(al, kernel, args, n_args, test,
            indices);
        if (!test) return false;
        ASRUtils::ASRBuilder b(al, test->base.loc);
        tests.push_back(condition.second ? test : b.Not(test));
    }
    return true;
}

std::vector<ASR::expr_t*> gpu_host_member_extents(Allocator &al,
        const ASR::Function_t &kernel, ASR::call_arg_t *args, size_t n_args,
        const GpuMemberShape &shape,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &indices) {
    std::shared_ptr<GpuIterationVaryingSymbols> changed =
        gpu_symbols_changed_in(kernel.m_body, kernel.n_body);
    std::set<ASR::symbol_t*> ignored;
    for (auto &index : indices) ignored.insert(index.first);
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
        KernelLocalResolver resolver(al, kernel, indices);
        resolver.current_expr = &extent;
        resolver.replace_expr(extent);
        if (!resolver.valid) return {};
        // The host evaluates the extent before the loop runs, so a value
        // the kernel writes, even earlier in the same iteration, is not
        // yet the one the extent reads.
        if (gpu_reads_changed(*changed, extent, &ignored)) return {};
        extent = gpu_bind_kernel_expression(al, kernel, args, n_args, extent,
            indices);
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
