#include <libasr/asr_builder.h>
#include <libasr/pass/gpu_kernel_abi.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/gpu_memory_space.h>
#include <libasr/pass/gpu_device_struct_storage.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/pass_utils.h>

#include <utility>
#include <algorithm>

namespace LCompilers {

namespace {

ASR::expr_t* duplicate_expression(Allocator &al, ASR::expr_t *e) {
    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.success = true;
    ASR::expr_t *copy = duplicator.duplicate_expr(e);
    return duplicator.success ? copy : nullptr;
}

// Writes an expression of a kernel over the kernel's parameters and its
// thread index alone: a scalar the prelude gives a value is replaced by
// that value, and, with `through_bindings`, a scalar the kernel binds once
// by the value it binds. Anything else is not something every thread and
// the host can evaluate alike, and leaves the writer invalid.
class GpuThreadExpressionWriter :
        public ASR::BaseExprReplacer<GpuThreadExpressionWriter> {
    Allocator &al;
    const ASR::Function_t &kernel;
    const std::map<ASR::symbol_t*, ASR::expr_t*> &values;
    std::set<ASR::symbol_t*> parameters, expanding;
    bool through_bindings;

public:
    bool valid = true;

    GpuThreadExpressionWriter(Allocator &al, const ASR::Function_t &kernel,
            const std::map<ASR::symbol_t*, ASR::expr_t*> &values,
            bool through_bindings)
        : al(al), kernel(kernel), values(values),
          through_bindings(through_bindings) {
        for (size_t i = 0; i < kernel.n_args; i++) {
            parameters.insert(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v));
        }
    }

    ASR::expr_t* write(ASR::expr_t *e) {
        ASR::expr_t *copy = duplicate_expression(al, e);
        if (copy == nullptr) return nullptr;
        current_expr = &copy;
        replace_expr(copy);
        return valid ? copy : nullptr;
    }

    // A type says what the value is, not how to compute it.
    void replace_ttype(ASR::ttype_t *) {}

    // Replaces the current expression by `value`, itself written out, in
    // place of what `symbol` stands for.
    void expand(ASR::symbol_t *symbol, ASR::expr_t *value) {
        ASR::expr_t *copy = value && !expanding.count(symbol)
            ? duplicate_expression(al, value) : nullptr;
        if (copy == nullptr) {
            valid = false;
            return;
        }
        expanding.insert(symbol);
        ASR::expr_t **outer = current_expr;
        current_expr = &copy;
        replace_expr(copy);
        current_expr = outer;
        *current_expr = copy;
        expanding.erase(symbol);
    }

    // The extent of an array of the kernel's own is the one it is
    // declared or allocated with.
    void replace_ArraySize(ASR::ArraySize_t *x) {
        ASR::expr_t *array = ASRUtils::get_past_array_physical_cast(x->m_v);
        if (!ASR::is_a<ASR::Var_t>(*array)) {
            ASR::BaseExprReplacer<GpuThreadExpressionWriter>
                ::replace_ArraySize(x);
            return;
        }
        ASR::symbol_t *symbol = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(array)->m_v);
        if (parameters.count(symbol)) {
            ASR::BaseExprReplacer<GpuThreadExpressionWriter>
                ::replace_ArraySize(x);
            return;
        }
        GpuExtentScope scope;
        scope.kernel = &kernel;
        scope.symtab = kernel.m_symtab;
        scope.body = kernel.m_body;
        scope.n_body = kernel.n_body;
        std::string name = ASRUtils::symbol_name(symbol);
        size_t rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::symbol_type(symbol));
        size_t first = 0, last = rank;
        if (x->m_dim != nullptr) {
            int64_t d = 0;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(x->m_dim), d)
                    || d < 1 || (size_t)d > rank) {
                valid = false;
                return;
            }
            first = d - 1;
            last = d;
        }
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::expr_t *size = nullptr;
        for (size_t d = first; d < last; d++) {
            ASR::expr_t *extent = gpu_scope_local_array_extent(scope, name,
                d);
            if (extent == nullptr) {
                valid = false;
                return;
            }
            extent = b.i2i_t(extent, x->m_type);
            size = size ? b.Mul(size, extent) : extent;
        }
        expand(symbol, size);
    }

    void replace_Var(ASR::Var_t *x) {
        ASR::symbol_t *symbol = ASRUtils::symbol_get_past_external(x->m_v);
        if (parameters.count(symbol)) return;
        if (ASR::is_a<ASR::Variable_t>(*symbol) &&
                ASR::down_cast<ASR::Variable_t>(symbol)->m_storage ==
                    ASR::storage_typeType::Parameter) {
            return;
        }
        auto value = values.find(symbol);
        if (value != values.end()) {
            *current_expr = duplicate_expression(al, value->second);
            return;
        }
        expand(symbol, through_bindings ? gpu_local_scalar_binding(symbol,
            kernel.m_body, kernel.n_body) : nullptr);
    }
};

// Whether an expression is made only of what the host can evaluate before
// a launch, for any thread: the operations asr_verify accepts in a
// workspace extent.
class GpuHostEvaluableCheck :
        public ASR::BaseWalkVisitor<GpuHostEvaluableCheck> {
public:
    bool evaluable = true;
    void visit_expr(const ASR::expr_t &e) {
        switch (e.type) {
            case ASR::exprType::IntegerConstant:
            case ASR::exprType::LogicalConstant:
            case ASR::exprType::Var:
            case ASR::exprType::IntegerBinOp:
            case ASR::exprType::IntegerUnaryMinus:
            case ASR::exprType::IntegerCompare:
            case ASR::exprType::IfExp:
            case ASR::exprType::Cast:
            case ASR::exprType::ArraySize:
            case ASR::exprType::ArrayBound:
            case ASR::exprType::ArrayItem:
            case ASR::exprType::ArrayPhysicalCast:
            case ASR::exprType::StructInstanceMember:
            case ASR::exprType::GpuThreadIndex:
            case ASR::exprType::GpuBlockIndex:
            case ASR::exprType::GpuBlockSize:
                ASR::BaseWalkVisitor<GpuHostEvaluableCheck>::visit_expr(e);
                break;
            default: evaluable = false;
        }
    }
    void visit_ttype(const ASR::ttype_t &) {}
};

// Whether an expression reads, as a value, something the kernel writes.
// The shape of an array is not such a value: a kernel cannot allocate, so
// every array the launch hands it keeps the shape the host gave it, and
// only the subscripts that pick an element are read.
class GpuKernelWrittenValueReader :
        public ASR::BaseWalkVisitor<GpuKernelWrittenValueReader> {
    const GpuIterationVaryingSymbols &changed;

    void visit_shape_of(ASR::expr_t *e) {
        while (e != nullptr) {
            if (ASR::is_a<ASR::Var_t>(*e)) return;
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(e);
                for (size_t i = 0; i < item->n_args; i++) {
                    visit_array_index(item->m_args[i]);
                }
                e = item->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                ASR::ArraySection_t *section =
                    ASR::down_cast<ASR::ArraySection_t>(e);
                for (size_t i = 0; i < section->n_args; i++) {
                    visit_array_index(section->m_args[i]);
                }
                e = section->m_v;
            } else {
                visit_expr(*e);
                return;
            }
        }
    }

public:
    bool found = false;

    explicit GpuKernelWrittenValueReader(
        const GpuIterationVaryingSymbols &changed) : changed(changed) {}

    void visit_Var(const ASR::Var_t &x) {
        if (gpu_writes_symbol(changed,
                ASRUtils::symbol_get_past_external(x.m_v))) {
            found = true;
        }
    }

    void visit_ArraySize(const ASR::ArraySize_t &x) {
        visit_shape_of(x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        visit_shape_of(x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_ttype(const ASR::ttype_t &) {}
};

// Replays a per-thread expression on the host for one thread of a launch:
// the thread index and the block index are the host's loop variables over
// the launch's threads, and the block size is the launch's own.
class GpuThreadReplay : public ASR::BaseExprReplacer<GpuThreadReplay> {
    Allocator &al;
    ASR::expr_t *block, *thread, *block_size;

    ASR::expr_t* as(ASR::expr_t *e, ASR::ttype_t *type) {
        ASRUtils::ASRBuilder b(al, e->base.loc);
        return b.i2i_t(e, type);
    }

public:
    GpuThreadReplay(Allocator &al, ASR::expr_t *block, ASR::expr_t *thread,
            ASR::expr_t *block_size)
        : al(al), block(block), thread(thread), block_size(block_size) {}

    ASR::expr_t* replay(ASR::expr_t *e) {
        ASR::expr_t *copy = duplicate_expression(al, e);
        LCOMPILERS_ASSERT(copy != nullptr);
        current_expr = &copy;
        replace_expr(copy);
        return copy;
    }

    void replace_GpuThreadIndex(ASR::GpuThreadIndex_t *x) {
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        *current_expr = x->m_dim == 0 ? as(thread, x->m_type)
            : b.i_t(0, x->m_type);
    }

    void replace_GpuBlockIndex(ASR::GpuBlockIndex_t *x) {
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        *current_expr = x->m_dim == 0 ? as(block, x->m_type)
            : b.i_t(0, x->m_type);
    }

    void replace_GpuBlockSize(ASR::GpuBlockSize_t *x) {
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        *current_expr = x->m_dim == 0 ? as(block_size, x->m_type)
            : b.i_t(1, x->m_type);
    }
};

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
                if (gpu_reads_thread_index(value)) {
                    largest_over_threads(x, kernel, value, extent);
                } else {
                    pass_result.push_back(al, b.Assignment(extent, value));
                }
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

    // Sets `largest` to the largest value the per-thread extent `value`
    // has in a thread of the launch that runs the kernel's body. The host
    // goes over the launch's `thread_count` threads as the kernel numbers
    // them -- not over the whole blocks it starts -- and skips a thread the
    // kernel's prelude sends back before the body: such a thread has no
    // iteration, and its extent may read past the end of what the
    // iterations read.
    void largest_over_threads(const ASR::GpuKernelLaunch_t &x,
            const ASR::Function_t &kernel, ASR::expr_t *value,
            ASR::expr_t *largest) {
        const Location &loc = x.base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *index_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 8));
        ASR::expr_t *flat = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_replay_thread_id"),
            index_type, ASR::intentType::Local);
        ASR::expr_t *block = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_replay_block"),
            index_type, ASR::intentType::Local);
        ASR::expr_t *thread = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_replay_thread"),
            index_type, ASR::intentType::Local);
        ASR::expr_t *block_size = b.i2i_t(x.m_block_size, index_type);
        GpuThreadReplay replay(al, block, thread, block_size);
        ASR::ttype_t *type = ASRUtils::expr_type(value);
        ASR::expr_t *one_value = replay.replay(value);
        std::vector<ASR::stmt_t*> body = {b.If(b.Gt(one_value, largest),
            {b.Assignment(largest, one_value)}, {})};
        GpuKernelPrelude prelude = gpu_kernel_prelude(al, kernel);
        for (size_t i = prelude.exits.size(); i-- > 0;) {
            ASR::expr_t *exit = gpu_bind_kernel_expression(al, kernel,
                x.m_args, x.n_args, prelude.exits[i]);
            LCOMPILERS_ASSERT(exit != nullptr);
            ASR::stmt_t *guarded = b.If(replay.replay(exit), {},
                std::move(body));
            body = {guarded};
        }
        body.insert(body.begin(), {
            b.Assignment(block, b.Div(flat, block_size)),
            b.Assignment(thread, b.Sub(flat, b.Mul(block, block_size)))});
        pass_result.push_back(al, b.Assignment(largest, b.i_t(0, type)));
        pass_result.push_back(al, b.DoLoop(flat, b.i_t(0, index_type),
            b.Sub(b.i2i_t(x.m_thread_count, index_type),
                b.i_t(1, index_type)), std::move(body)));
    }
};

// The storage an actual argument of a launch designates: a variable and
// the chain of components of it the actual selects, outermost first. Only
// storage no other designator can reach is tracked: a variable that is not
// a pointer, not a target, not in a common block or equivalence and not an
// associate name, reached through components that are not pointers either.
// Two such designators of different variables never share storage, and a
// variable of conforming code is not also reached through a dummy argument
// it is associated with where either is written (Fortran 2023, the
// restrictions on entities associated with dummy arguments).
struct GpuArgumentStorage {
    ASR::symbol_t *variable = nullptr;
    std::vector<ASR::Variable_t*> components;

    explicit GpuArgumentStorage(ASR::expr_t *e) {
        std::vector<ASR::Variable_t*> chain;
        while (e != nullptr) {
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                auto *member = ASR::down_cast<ASR::StructInstanceMember_t>(e);
                ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                    member->m_m);
                if (!ASR::is_a<ASR::Variable_t>(*m)) return;
                auto *component = ASR::down_cast<ASR::Variable_t>(m);
                if (ASRUtils::is_pointer(component->m_type)) return;
                chain.push_back(component);
                e = member->m_v;
            } else if (ASR::is_a<ASR::Var_t>(*e)) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                if (!ASR::is_a<ASR::Variable_t>(*s)) return;
                auto *var = ASR::down_cast<ASR::Variable_t>(s);
                SymbolTable *scope = var->m_parent_symtab;
                if (ASRUtils::is_pointer(var->m_type) || var->m_target_attr
                        || var->m_common_block != nullptr
                        || (scope->asr_owner &&
                            ASR::is_a<ASR::symbol_t>(*scope->asr_owner) &&
                            ASR::is_a<ASR::AssociateBlock_t>(
                                *ASR::down_cast<ASR::symbol_t>(
                                    scope->asr_owner)))) {
                    return;
                }
                variable = s;
                components.assign(chain.rbegin(), chain.rend());
                return;
            } else {
                return;
            }
        }
    }

    bool tracked() const { return variable != nullptr; }

    // Whether this and `other` are known to share no storage: different
    // variables, or components of one variable that part ways, two
    // components of the same derived type. A parent component is declared
    // in the extension, not alongside the components it holds.
    bool disjoint(const GpuArgumentStorage &other) const {
        if (!tracked() || !other.tracked()) return false;
        if (variable != other.variable) return true;
        size_t n = std::min(components.size(), other.components.size());
        for (size_t i = 0; i < n; i++) {
            ASR::Variable_t *a = components[i], *b = other.components[i];
            if (a == b) continue;
            return a->m_parent_symtab == b->m_parent_symtab;
        }
        return false;
    }
};

// Marks each buffer of `kernel` that no other buffer can alias in any of
// its `launches`, so the device code may say so (`__restrict__`). A buffer
// that is only read may share storage with another one that is only read.
// Every buffer of one actual argument -- an array of a derived type and the
// data of its components -- counts as that actual's storage.
void gpu_mark_noalias_buffers(ASR::Function_t &kernel,
        const std::vector<const ASR::GpuKernelLaunch_t*> &launches) {
    ASR::gpu_kernel_layout_t &layout = *kernel.m_gpu;
    for (size_t i = 0; i < layout.n_buffers; i++) {
        layout.m_buffers[i].m_noalias = !launches.empty();
    }
    for (const ASR::GpuKernelLaunch_t *launch : launches) {
        std::vector<GpuArgumentStorage> storage;
        for (size_t i = 0; i < layout.n_buffers; i++) {
            size_t index = layout.m_buffers[i].m_argument_index;
            LCOMPILERS_ASSERT(index < launch->n_args);
            storage.emplace_back(launch->m_args[index].m_value);
        }
        for (size_t i = 0; i < layout.n_buffers; i++) {
            ASR::gpu_kernel_argument_t &arg = layout.m_buffers[i];
            if (!storage[i].tracked()) {
                arg.m_noalias = false;
                continue;
            }
            for (size_t j = 0; j < layout.n_buffers && arg.m_noalias; j++) {
                const ASR::gpu_kernel_argument_t &other = layout.m_buffers[j];
                if (j == i || (!arg.m_written && !other.m_written)) continue;
                if (other.m_argument_index == arg.m_argument_index
                        || !storage[i].disjoint(storage[j])) {
                    arg.m_noalias = false;
                }
            }
        }
    }
}

class GpuKernelLaunches : public ASR::BaseWalkVisitor<GpuKernelLaunches> {
public:
    std::map<ASR::Function_t*,
        std::vector<const ASR::GpuKernelLaunch_t*>> launches;

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        launches[ASR::down_cast<ASR::Function_t>(x.m_kernel)].push_back(&x);
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

// Follows the statements of a kernel's prelude from `body` on, adding to
// `values` and `exits` (see GpuKernelPrelude), and returns how many it
// followed: it stops at the first statement that is not part of one.
static size_t gpu_follow_prelude(Allocator &al, const ASR::Function_t &kernel,
        ASR::stmt_t **body, size_t n_body,
        std::map<ASR::symbol_t*, ASR::expr_t*> &values,
        std::vector<ASR::expr_t*> &exits) {
    size_t n = 0;
    for (; n < n_body; n++) {
        ASR::stmt_t *stmt = body[n];
        // Where a component of a derived-type local is stored says nothing
        // about the iteration (see gpu_device_struct_storage.h).
        ASR::StructInstanceMember_t *component = nullptr;
        ASR::expr_t *storage = nullptr;
        if (gpu_struct_component_storage(stmt, component, storage)) continue;
        if (ASR::is_a<ASR::If_t>(*stmt)) {
            ASR::If_t *test = ASR::down_cast<ASR::If_t>(stmt);
            GpuThreadExpressionWriter writer(al, kernel, values, false);
            ASR::expr_t *condition = writer.write(test->m_test);
            if (condition == nullptr) break;
            if (test->n_body == 1 && test->n_orelse == 0 &&
                    ASR::is_a<ASR::Return_t>(*test->m_body[0])) {
                exits.push_back(condition);
                continue;
            }
            // A choice the launch makes for every thread alike, such as
            // the numbering of a tiled launch (gpu_offload.cpp): each
            // branch has to be a prelude of its own. A scalar then has the
            // value of the branch taken, and a branch's exit applies when
            // that branch is taken.
            std::map<ASR::symbol_t*, ASR::expr_t*> then_values = values,
                else_values = values;
            std::vector<ASR::expr_t*> then_exits, else_exits;
            if (gpu_follow_prelude(al, kernel, test->m_body, test->n_body,
                        then_values, then_exits) != test->n_body
                    || gpu_follow_prelude(al, kernel, test->m_orelse,
                        test->n_orelse, else_values, else_exits)
                        != test->n_orelse) {
                break;
            }
            const Location &loc = stmt->base.loc;
            ASR::ttype_t *logical_type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, loc, 4));
            ASR::expr_t *otherwise = ASRUtils::EXPR(ASR::make_LogicalNot_t(
                al, loc, condition, logical_type, nullptr));
            for (ASR::expr_t *exit : then_exits) {
                exits.push_back(ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al,
                    loc, condition, ASR::logicalbinopType::And, exit,
                    logical_type, nullptr)));
            }
            for (ASR::expr_t *exit : else_exits) {
                exits.push_back(ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al,
                    loc, otherwise, ASR::logicalbinopType::And, exit,
                    logical_type, nullptr)));
            }
            std::set<ASR::symbol_t*> symbols;
            for (auto &value : then_values) symbols.insert(value.first);
            for (auto &value : else_values) symbols.insert(value.first);
            for (auto &value : values) symbols.insert(value.first);
            for (ASR::symbol_t *symbol : symbols) {
                auto in_then = then_values.find(symbol);
                auto in_else = else_values.find(symbol);
                if (in_then == then_values.end()
                        || in_else == else_values.end()) {
                    values.erase(symbol);
                } else if (in_then->second == in_else->second) {
                    values[symbol] = in_then->second;
                } else {
                    values[symbol] = ASRUtils::EXPR(ASR::make_IfExp_t(al,
                        loc, condition, in_then->second, in_else->second,
                        ASRUtils::expr_type(in_then->second), nullptr));
                }
            }
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) break;
        // The statements so far run one after the other in every thread,
        // so a scalar holds the last value assigned to it. An element or a
        // component written here is not a scalar of the prelude.
        ASR::Assignment_t *assignment = ASR::down_cast<ASR::Assignment_t>(stmt);
        if (!ASR::is_a<ASR::Var_t>(*assignment->m_target)) continue;
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(assignment->m_target)->m_v);
        ASR::expr_t *value = nullptr;
        if (!ASRUtils::is_array(ASRUtils::expr_type(assignment->m_target))) {
            GpuThreadExpressionWriter writer(al, kernel, values, false);
            value = writer.write(assignment->m_value);
        }
        values.erase(target);
        if (value != nullptr) values[target] = value;
    }
    return n;
}

GpuKernelPrelude gpu_kernel_prelude(Allocator &al,
        const ASR::Function_t &kernel) {
    GpuKernelPrelude prelude;
    std::map<ASR::symbol_t*, ASR::expr_t*> values;
    size_t n = gpu_follow_prelude(al, kernel, kernel.m_body, kernel.n_body,
        values, prelude.exits);
    auto rest = gpu_symbols_changed_in(kernel.m_body + n, kernel.n_body - n);
    for (auto &value : values) {
        if (!gpu_writes_symbol(*rest, value.first)) {
            prelude.values.insert(value);
        }
    }
    return prelude;
}

ASR::expr_t* gpu_kernel_per_thread_extent(Allocator &al,
        const ASR::Function_t &kernel, ASR::expr_t *extent) {
    if (extent == nullptr) return nullptr;
    GpuKernelPrelude prelude = gpu_kernel_prelude(al, kernel);
    GpuThreadExpressionWriter writer(al, kernel, prelude.values, true);
    ASR::expr_t *out = writer.write(extent);
    if (out == nullptr) return nullptr;
    GpuHostEvaluableCheck evaluable;
    evaluable.visit_expr(*out);
    if (!evaluable.evaluable) return nullptr;
    auto changed = gpu_symbols_changed_in(kernel.m_body, kernel.n_body);
    GpuKernelWrittenValueReader reader(*changed);
    reader.visit_expr(*out);
    if (reader.found) return nullptr;
    return out;
}

void pass_gpu_kernel_storage(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &options) {
    const auto caps = gpu_device_capabilities(options);
    if (!caps.device_selected()) return;
    // A shared aggregate passed by reference to an updating routine needs
    // a thread-local copy. Materialize it before measuring local storage.
    // Scalar reference copies depend on the chosen address spaces and
    // remain part of the frame reserve.
    gpu_materialize_struct_arguments(al, unit);
    // Leave room for the device compiler's frames and register spills.
    const int64_t budget = std::max<int64_t>(caps.thread_stack_bytes - 8192, 0);
    for (auto &item : unit.m_symtab->get_scope()) {
        if (!ASRUtils::is_device_kernel(item.second)) continue;
        auto &kernel = *ASR::down_cast<ASR::Function_t>(item.second);
        auto arrays = collect_gpu_vla_workspaces(kernel, 0, true);

        // These are the actual remaining device routines, not a prediction
        // of the routines gpu_offload might inline. Count each scope once.
        int64_t remaining = budget;
        auto array_bytes = [&](const GpuVlaWorkspace &ws) {
            int64_t bytes = ws.elem_size;
            for (const auto &dim : ws.dims) {
                if (!dim.is_constant) return int64_t(-1);
                int64_t extent = std::max<int64_t>(dim.constant_value, 0);
                bytes = extent > 0 && bytes > budget / extent
                    ? budget + 1 : bytes * extent;
            }
            return bytes;
        };
        auto non_workspace_storage = [&](ASR::Function_t &fn,
                const std::vector<GpuVlaWorkspace> &workspaces,
                bool include_value_parameters) {
            std::set<ASR::symbol_t*> workspace_vars;
            for (const auto &ws : workspaces) workspace_vars.insert(ws.var);
            std::function<void(SymbolTable*)> scope = [&](SymbolTable *table) {
                for (const auto &item : table->get_scope()) {
                    if (ASR::is_a<ASR::Block_t>(*item.second)) {
                        scope(ASR::down_cast<ASR::Block_t>(
                            item.second)->m_symtab);
                    } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                        scope(ASR::down_cast<ASR::AssociateBlock_t>(
                            item.second)->m_symtab);
                    } else if (ASR::is_a<ASR::Variable_t>(*item.second) &&
                            !workspace_vars.count(item.second)) {
                        auto *var = ASR::down_cast<ASR::Variable_t>(item.second);
                        // Read-only scalar aggregates are values in the
                        // device ABI, unlike array and updating parameters.
                        bool value_parameter = include_value_parameters &&
                            ASR::is_a<ASR::StructType_t>(*var->m_type) &&
                            var->m_intent != ASR::intentType::Out &&
                            var->m_intent != ASR::intentType::InOut;
                        if ((var->m_intent != ASR::intentType::Local &&
                                var->m_intent != ASR::intentType::ReturnVar &&
                                !value_parameter) ||
                                var->m_storage != ASR::storage_typeType::Default ||
                                ASRUtils::is_allocatable_or_pointer(var->m_type)) {
                            continue;
                        }
                        ASR::expr_t *value = ASRUtils::EXPR(ASR::make_Var_t(
                            al, var->base.base.loc, item.second));
                        ASR::ttype_t *type = gpu_size_of_type_argument(al,
                            value, var->m_type);
                        int64_t bytes = ASRUtils::compute_type_size_align(type).first;
                        // Storage not representable by a numeric workspace
                        // (notably aggregates) is never room for another array.
                        remaining = bytes < 0 ? 0
                            : std::max<int64_t>(remaining - bytes, 0);
                    }
                }
            };
            scope(fn.m_symtab);
        };
        non_workspace_storage(kernel, arrays, false);
        std::set<ASR::Function_t*> visited;
        std::function<void(ASR::Function_t*)> callees =
            [&](ASR::Function_t *fn) {
            if (!visited.insert(fn).second) return;
            if (fn != &kernel) {
                auto locals = collect_gpu_vla_workspaces(*fn, 0, true);
                non_workspace_storage(*fn, locals, true);
                for (const auto &ws : locals) {
                    int64_t bytes = array_bytes(ws);
                    if (bytes >= 0) remaining = std::max<int64_t>(remaining - bytes, 0);
                }
            }
            for (auto *callee : gpu_callees(fn->m_body, fn->n_body, true)) {
                callees(callee);
            }
        };
        callees(&kernel);
        std::vector<std::pair<int64_t, ASR::symbol_t*>> fixed;
        for (const auto &ws : arrays) {
            int64_t bytes = array_bytes(ws);
            if (bytes >= 0) fixed.emplace_back(bytes, ws.var);
        }
        std::sort(fixed.begin(), fixed.end(), [](const auto &a, const auto &b) {
            if (a.first != b.first) return a.first < b.first;
            return std::string(ASRUtils::symbol_name(a.second))
                < std::string(ASRUtils::symbol_name(b.second));
        });
        for (auto &array : fixed) {
            auto *var = ASR::down_cast<ASR::Variable_t>(array.second);
            var->m_type = ASRUtils::duplicate_type(al, var->m_type);
            auto *type = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_allocatable(var->m_type));
            // Global here means private storage in a workspace, not a
            // shared value. The ABI planner records its per-thread slice.
            type->m_memory_space = array.first <= remaining
                ? ASR::memory_spaceType::Thread : ASR::memory_spaceType::Global;
            if (array.first <= remaining) remaining -= array.first;
        }
    }
}

void pass_gpu_kernel_finalize(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &options) {
    if (!gpu_device_capabilities(options).device_selected()) return;
    FinalizeGpuKernels finalizer(al, options);
    finalizer.visit_TranslationUnit(unit);

    GpuKernelLaunches launches;
    launches.visit_TranslationUnit(unit);
    for (auto &kernel : launches.launches) {
        if (kernel.first->m_gpu) {
            gpu_mark_noalias_buffers(*kernel.first, kernel.second);
        }
    }

    // The host code that sizes a workspace can call a function, such as
    // the routine shared lowering gives `maxval`.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
