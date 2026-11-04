#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_overwrite.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

namespace {

// `stmt`, or `if (present(sym)) stmt` for an optional dummy argument `sym`.
ASR::stmt_t *gpu_if_present(Allocator &al, const Location &loc,
        ASR::symbol_t *sym, ASR::stmt_t *stmt) {
    ASR::symbol_t *v = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Variable_t>(*v) || ASR::down_cast<ASR::Variable_t>(
            v)->m_presence != ASR::presenceType::Optional) {
        return stmt;
    }
    Vec<ASR::expr_t*> args;
    args.reserve(al, 1);
    args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym)));
    ASR::expr_t *present = ASRUtils::EXPR(
        ASR::make_IntrinsicElementalFunction_t(al, loc,
            static_cast<int64_t>(
                ASRUtils::IntrinsicElementalFunctions::Present),
            args.p, args.n, 0,
            ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
    Vec<ASR::stmt_t*> then;
    then.reserve(al, 1);
    then.push_back(al, stmt);
    return ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr, present, then.p,
        then.n, nullptr, 0));
}

// Appends to `out` the statements that set `count` to the number of
// iterations of a nest whose dimensions run from starts[d] to ends[d],
// expressions of the type of `count` that may be evaluated more than once:
// the product of their extents, or 0 when one of them is empty. When the
// product of the extents of a nest without an empty one does not fit in
// that type, the statements stop the program with an error first, before
// anything of the launch is done: such a nest could not finish running
// anyway. The extent of a dimension whose bounds were widened to the type
// (`widened[d]`) fits in it; that of one whose bounds have the type itself
// may not, as -huge(0_8):huge(0_8) does not. `count` is assigned once.
void gpu_checked_iteration_count(Allocator &al, const Location &loc,
        const std::vector<ASR::expr_t*> &starts,
        const std::vector<ASR::expr_t*> &ends,
        const std::vector<bool> &widened, ASR::expr_t *count,
        Vec<ASR::stmt_t*> &out) {
    ASRUtils::ASRBuilder b(al, loc);
    ASR::ttype_t *type = ASRUtils::expr_type(count);
    int kind = ASRUtils::extract_kind_from_ttype_t(type);
    int64_t huge = kind >= 8 ? INT64_MAX
        : (int64_t(1) << (8 * kind - 1)) - 1;
    auto constant = [&](int64_t value) { return b.i_t(value, type); };
    auto stop = [&]() {
        std::string message = "a gpu kernel launch has more than "
            + std::to_string(huge) + " iterations";
        return ASRUtils::STMT(ASR::make_ErrorStop_t(al, loc,
            b.StringConstant(message, b.String(b.i32(message.size()),
                ASR::ExpressionLength))));
    };
    // The checks made when no dimension is empty, and the test that none
    // is, nullptr when every bound is a constant.
    std::vector<ASR::stmt_t*> checks;
    ASR::expr_t *nonempty = nullptr;
    // The product of the constant extents, and the extents that are not
    // constants. A constant product that does not fit is `overflow`.
    int64_t fixed = 1;
    bool overflow = false;
    std::vector<ASR::expr_t*> extents;
    for (size_t d = 0; d < starts.size(); d++) {
        ASR::expr_t *start = starts[d], *end = ends[d];
        int64_t start_value = 0, end_value = 0;
        bool start_known = ASRUtils::extract_value(
            ASRUtils::expr_value(start), start_value);
        bool end_known = ASRUtils::extract_value(
            ASRUtils::expr_value(end), end_value);
        if (start_known && end_known) {
            if (end_value < start_value) {
                // The nest is empty, whatever the other bounds are.
                out.push_back(al, b.Assignment(count, constant(0)));
                return;
            }
            uint64_t span = static_cast<uint64_t>(end_value)
                - static_cast<uint64_t>(start_value);
            if (span >= static_cast<uint64_t>(huge)
                    || static_cast<int64_t>(span) + 1 > huge / fixed) {
                overflow = true;
            } else {
                fixed *= static_cast<int64_t>(span) + 1;
            }
            continue;
        }
        ASR::expr_t *test = b.GtE(end, start);
        nonempty = nonempty ? b.And(nonempty, test) : test;
        // end - start + 1 > huge exactly when start <= 0 and
        // end >= huge + start, which does not overflow then; a positive
        // start leaves fewer than huge values up to huge.
        if (!widened[d] && !(start_known && start_value > 0)) {
            ASR::stmt_t *check = b.If(b.GtE(end,
                b.Add(constant(huge), start)), {stop()}, {});
            checks.push_back(start_known ? check
                : b.If(b.LtE(start, constant(0)), {check}, {}));
        }
        extents.push_back(b.Add(b.Sub(end, start), constant(1)));
    }
    ASR::expr_t *product = nullptr;
    if (overflow) {
        checks.push_back(stop());
    } else {
        // product * extent > huge exactly when product > huge / extent,
        // for a positive extent; the product so far fits, as its factors
        // passed these checks.
        product = fixed != 1 || extents.empty() ? constant(fixed) : nullptr;
        for (ASR::expr_t *extent : extents) {
            if (product != nullptr) {
                checks.push_back(b.If(b.Gt(product,
                    b.Div(constant(huge), extent)), {stop()}, {}));
            }
            product = product ? b.Mul(product, extent) : extent;
        }
    }
    if (nonempty == nullptr) {
        for (ASR::stmt_t *check : checks) out.push_back(al, check);
        out.push_back(al, b.Assignment(count,
            product ? product : constant(0)));
        return;
    }
    if (!checks.empty()) out.push_back(al, b.If(nonempty, checks, {}));
    out.push_back(al, b.Assignment(count, ASRUtils::EXPR(ASR::make_IfExp_t(
        al, loc, nonempty, product ? product : constant(0), constant(0),
        type, nullptr))));
}

} // namespace

// Replaces the region with the launch of the kernel the draft above built:
// the copies the host makes on either side of the dispatch and the dispatch
// itself. Everything it needs is in `plan`; the region was committed to the
// device before this pass ran, so this is also where what the launch cannot
// honour is reported.
void GpuOffloadVisitor::build_kernel_launch(const ASR::OMPRegion_t &region,
        const ParallelLoopNest &work, const Location &loc,
        GpuLaunchPlan &plan) {
    ASRUtils::ASRBuilder b(al, loc);
    // The kernel is built, so this is where a clause the launch cannot
    // honour is reported: a loop that is an error is not also warned about.
    for (size_t i = 0; i < region.n_clauses; i++) {
        std::string clause_name = unhonoured_clause(region.m_clauses[i]);
        if (!clause_name.empty()) {
            report_clause_ignored(region.m_clauses[i]->base.loc,
                clause_name);
        }
    }

    pass_plain_dummy_lower_bounds(*ASR::down_cast<ASR::Function_t>(
        ASR::down_cast<ASR::symbol_t>(plan.kernel_func)));

    plan.tu_symtab->add_symbol(plan.kernel_name,
        ASR::down_cast<ASR::symbol_t>(plan.kernel_func));

    // Pre-allocate host-side allocatable arrays that are assigned
    // from a FunctionCall inside the loop body. The GPU
    // kernel receives the buffer pointer at launch time, so the
    // array must already be allocated on the host before dispatch.
    Vec<ASR::stmt_t*> pre_launch_stmts;
    pre_launch_stmts.reserve(al, 4);
    for (size_t si = 0; si < work.n_body; si++) {
        ASR::stmt_t *stmt = work.body[si];
        // Unwrap BlockCall to inspect block body statements
        ASR::stmt_t **stmts_to_scan = &stmt;
        size_t n_stmts_to_scan = 1;
        SymbolTable *block_symtab = nullptr;
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t *bc =
                ASR::down_cast<ASR::BlockCall_t>(stmt);
            if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                ASR::Block_t *blk =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                stmts_to_scan = blk->m_body;
                n_stmts_to_scan = blk->n_body;
                block_symtab = blk->m_symtab;
            }
        }
        for (size_t sj = 0; sj < n_stmts_to_scan; sj++) {
            if (!ASR::is_a<ASR::Assignment_t>(*stmts_to_scan[sj]))
                continue;
            ASR::Assignment_t *asgn =
                ASR::down_cast<ASR::Assignment_t>(stmts_to_scan[sj]);
            if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) continue;
            if (!ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value))
                continue;

            ASR::Var_t *target_var =
                ASR::down_cast<ASR::Var_t>(asgn->m_target);
            ASR::symbol_t *orig_sym =
                ASRUtils::symbol_get_past_external(target_var->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*orig_sym)) continue;
            ASR::Variable_t *var =
                ASR::down_cast<ASR::Variable_t>(orig_sym);
            if (!ASRUtils::is_allocatable(var->m_type)) continue;
            // A local of a BLOCK of the loop body is the kernel's own,
            // which the host has no storage for.
            if (block_symtab && var->m_parent_symtab == block_symtab) {
                continue;
            }

            ASR::FunctionCall_t *fc =
                ASR::down_cast<ASR::FunctionCall_t>(asgn->m_value);
            ASR::symbol_t *fn_sym =
                ASRUtils::symbol_get_past_external(fc->m_name);
            if (!ASR::is_a<ASR::Function_t>(*fn_sym)) continue;

            ASR::Function_t *fn =
                ASR::down_cast<ASR::Function_t>(fn_sym);
            std::string ret_name;
            if (fn->m_return_var &&
                    ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                ret_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        fn->m_return_var)->m_v);
            }
            if (ret_name.empty()) continue;

            // Find the Allocate statement for the return variable
            // in the function body and use its dimensions.
            bool alloc_found = false;
            for (size_t bi = 0;
                    bi < fn->n_body && !alloc_found; bi++) {
                if (!ASR::is_a<ASR::Allocate_t>(*fn->m_body[bi]))
                    continue;
                ASR::Allocate_t *fn_alloc =
                    ASR::down_cast<ASR::Allocate_t>(fn->m_body[bi]);
                for (size_t ai = 0; ai < fn_alloc->n_args; ai++) {
                    if (!fn_alloc->m_args[ai].m_a ||
                            !ASR::is_a<ASR::Var_t>(
                                *fn_alloc->m_args[ai].m_a))
                        continue;
                    std::string aname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            fn_alloc->m_args[ai].m_a)->m_v);
                    if (aname != ret_name) continue;

                    ASRUtils::ExprStmtDuplicator dup(al);
                    dup.success = true;
                    ASR::alloc_arg_t host_arg;
                    host_arg.loc = loc;
                    host_arg.m_a = asgn->m_target;
                    host_arg.n_dims =
                        fn_alloc->m_args[ai].n_dims;
                    host_arg.m_dims =
                        al.allocate<ASR::dimension_t>(
                            host_arg.n_dims);
                    for (size_t d = 0; d < host_arg.n_dims; d++) {
                        host_arg.m_dims[d].loc = loc;
                        host_arg.m_dims[d].m_start =
                            fn_alloc->m_args[ai].m_dims[d].m_start
                            ? dup.duplicate_expr(
                                fn_alloc->m_args[ai]
                                    .m_dims[d].m_start)
                            : nullptr;
                        host_arg.m_dims[d].m_length =
                            fn_alloc->m_args[ai].m_dims[d].m_length
                            ? dup.duplicate_expr(
                                fn_alloc->m_args[ai]
                                    .m_dims[d].m_length)
                            : nullptr;
                    }
                    host_arg.m_len_expr = nullptr;
                    host_arg.m_sym_subclass = nullptr;
                    host_arg.m_type = nullptr;
                    host_arg.m_codims = nullptr;
                    host_arg.n_codims = 0;

                    Vec<ASR::alloc_arg_t> alloc_vec;
                    alloc_vec.reserve(al, 1);
                    alloc_vec.push_back(al, host_arg);
                    pre_launch_stmts.push_back(al,
                        ASRUtils::STMT(ASR::make_Allocate_t(
                            al, loc, alloc_vec.p, alloc_vec.n,
                            nullptr, nullptr, nullptr)));
                    alloc_found = true;
                    break;
                }
            }
        }
    }

    // 7. Replace the region with its GpuKernelLaunch
    // Collect all launch-related statements into a temporary Vec.
    Vec<ASR::stmt_t*> launch_stmts;
    launch_stmts.reserve(al, plan.gather_stmts.n + pre_launch_stmts.n
        + plan.scatter_stmts.n + plan.liveout_scalars.size() + 2
        + plan.liveout_scalars.size() + plan.private_shapes.n);
    Vec<ASR::stmt_t*> prelude;
    prelude.reserve(al, 2 * plan.n_dims + 2);
    // The limits of the loops are evaluated once, as the loop evaluates
    // them before its first iteration, into host variables that everything
    // below reads: the component fit, the kernel's arguments and the grid.
    std::vector<ASR::expr_t*> starts, ends;
    {
        auto evaluate_once = [&](ASR::expr_t *&limit,
                const std::string &name) {
            if (ASR::is_a<ASR::IntegerConstant_t>(*limit)) return;
            ASR::symbol_t *sym = gpu_new_variable(al, loc, current_scope,
                current_scope->get_unique_name(name),
                ASRUtils::duplicate_type(al,
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASRUtils::expr_type(limit))));
            prelude.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym)), limit,
                    nullptr, false, false, false, false)));
            for (size_t i = 0; i < plan.call_args.n; i++) {
                if (plan.call_args.p[i].m_value == limit) {
                    plan.call_args.p[i].m_value =
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
                }
            }
            limit = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        };
        for (size_t d = 0; d < plan.n_dims; d++) {
            evaluate_once(plan.dim_info[d].host_start, "__gpu_loop_start");
            evaluate_once(plan.dim_info[d].host_end, "__gpu_loop_end");
            starts.push_back(plan.dim_info[d].host_start);
            ends.push_back(plan.dim_info[d].host_end);
        }
    }
    // The number of threads the launch runs, evaluated once into the
    // variable that guards the launch below. Bounds are still evaluated for
    // a zero-trip loop, but gathers, allocations and readbacks belong only
    // to actual offload work. The iterations are counted in integer(8),
    // whatever the kernel numbers them in (gpu_iteration_number_type): a
    // nest of integer(4) loops can have more than huge(0) of them, which a
    // launch then runs in chunks the kernel numbers in 32 bits. The count
    // is positive exactly when every dimension is non-empty.
    ASR::expr_t *thread_count = nullptr;
    ASR::ttype_t *count_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
        8));
    // Whether the launch may run in chunks (GpuKernelLaunch.chunk_dimensions).
    bool chunked = plan.chunk_fixed_iterations > 0;
    auto widen = [&](ASR::expr_t *limit) {
        ASRUtils::ExprStmtDuplicator copy(al);
        copy.success = true;
        ASR::expr_t *value = copy.duplicate_expr(limit);
        LCOMPILERS_ASSERT(copy.success && value);
        return ASRUtils::extract_kind_from_ttype_t(
                ASRUtils::expr_type(value))
                == ASRUtils::extract_kind_from_ttype_t(count_type)
            ? value : b.i2i_t(value, count_type);
    };
    auto extent = [&](size_t d) {
        return gpu_loop_extent(al, loc, widen(starts[d]),
            widen(ends[d]), count_type);
    };
    // The iteration count, the product of the extents (0 when one of them
    // is): a launch whose count does not fit in its type stops instead
    // (gpu_checked_iteration_count).
    ASR::expr_t *iteration_count = b.Variable(current_scope,
        current_scope->get_unique_name("__gpu_iteration_count"),
        count_type, ASR::intentType::Local);
    {
        std::vector<ASR::expr_t*> count_starts, count_ends;
        std::vector<bool> widened;
        for (size_t d = 0; d < plan.n_dims; d++) {
            count_starts.push_back(widen(starts[d]));
            count_ends.push_back(widen(ends[d]));
            widened.push_back(ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(starts[d]))
                    < ASRUtils::extract_kind_from_ttype_t(count_type)
                && ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(ends[d]))
                    < ASRUtils::extract_kind_from_ttype_t(count_type));
        }
        gpu_checked_iteration_count(al, loc, count_starts, count_ends,
            widened, iteration_count, prelude);
    }
    if (plan.tiled_arg >= 0) {
        // A launch that can run in tiles (gpu_offload.cpp) runs whole tiles
        // of the device's tile width by the rest of a block, unless that
        // pads the nest by more than a quarter, as a short dimension does:
        // then it runs one thread per iteration, numbered flat. Padding can
        // take the count past the kind of the loop indices; it is counted
        // in integer(8) as well. A tiled nest has no reduction (that keeps
        // the flat numbering), so nothing below needs the iteration count.
        // Whole tiles start at most `threads_per_block` threads per
        // iteration, so the padded count, and the test below that compares
        // it with the iterations, cannot overflow while there are at most
        // `few` iterations and at least one: an empty dimension outside the
        // tile does not bound the product of the tile dimensions it is
        // multiplied by. A launch of more or of none does not form the
        // padded count; one of more runs flat.
        int64_t few = INT64_MAX
            / (4 * int64_t(device_caps.threads_per_block));
        auto count = [&](int64_t value) {
            return b.i_t(value, count_type);
        };
        auto ceil_div = [&](ASR::expr_t *n, int64_t size) {
            return b.Div(b.Add(n, count(size - 1)), count(size));
        };
        size_t across = plan.tile_dims[0], down = plan.tile_dims[1];
        int64_t width = device_caps.tile_width;
        int64_t height = device_caps.threads_per_block / width;
        ASR::expr_t *tile_count = b.Mul(b.Mul(
            ceil_div(extent(across), width), ceil_div(extent(down), height)),
            count(device_caps.threads_per_block));
        for (size_t d = 0; d < plan.n_dims; d++) {
            if (d != across && d != down) {
                tile_count = b.Mul(tile_count, extent(d));
            }
        }
        LCOMPILERS_ASSERT(std::none_of(plan.reductions.begin(),
            plan.reductions.end(), [](const auto &r) {
                return r.host_buf_sym != nullptr; }));
        ASR::expr_t *tile_threads = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_tile_threads"),
            count_type, ASR::intentType::Local);
        prelude.push_back(al, b.Assignment(tile_threads, ASRUtils::EXPR(
            ASR::make_IfExp_t(al, loc, b.And(b.Gt(iteration_count, count(0)),
                b.LtE(iteration_count, count(few))),
                tile_count, iteration_count, count_type, nullptr))));
        ASR::expr_t *tiled = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_tiled"),
            ASRUtils::duplicate_type(al, plan.int_type),
            ASR::intentType::Local);
        // One launch starts at most the chunk limit's threads, whether the
        // nest may run in chunks or its iterations are known to fit the
        // limit: whole tiles that pad the nest past it run flat instead,
        // one thread per iteration, and a launch of more iterations than
        // the limit runs in chunks, which split the nest into boxes the
        // kernel numbers flat.
        ASR::expr_t *flat = b.Or(b.Or(b.Gt(iteration_count, count(few)),
            b.Gt(b.Mul(count(4), b.Sub(tile_threads, iteration_count)),
                iteration_count)),
            b.Gt(tile_threads, count(pass_options.gpu_chunk_limit)));
        prelude.push_back(al, b.Assignment(tiled,
            ASRUtils::EXPR(ASR::make_IfExp_t(al, loc, flat,
                b.i_t(0, plan.int_type), b.i_t(1, plan.int_type),
                plan.int_type, nullptr))));
        plan.call_args.p[plan.tiled_arg].m_value = tiled;
        thread_count = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_launch_threads"),
            count_type, ASR::intentType::Local);
        prelude.push_back(al, b.Assignment(thread_count,
            ASRUtils::EXPR(ASR::make_IfExp_t(al, loc,
                b.Eq(tiled, b.i_t(0, plan.int_type)), iteration_count,
                tile_threads, count_type, nullptr))));
    } else {
        // One thread per iteration.
        thread_count = iteration_count;
        if (plan.strided) {
            // The kernel runs a grid-stride loop of at most
            // gpu_reduction_threads threads. A launch of more iterations
            // than the chunk limit keeps one thread per iteration, since it
            // runs in chunks: in each, the threads past the stride return.
            ASR::expr_t *limit = b.i_t(gpu_reduction_threads, count_type);
            thread_count = b.Variable(current_scope,
                current_scope->get_unique_name("__gpu_launch_threads"),
                count_type, ASR::intentType::Local);
            prelude.push_back(al, b.Assignment(thread_count,
                ASRUtils::EXPR(ASR::make_IfExp_t(al, loc,
                    b.And(b.Gt(iteration_count, limit),
                        b.LtE(iteration_count,
                            b.i_t(pass_options.gpu_chunk_limit, count_type))),
                    limit, iteration_count, count_type, nullptr))));
        }
    }
    for (size_t i = 0; i < plan.private_shapes.n; i++) {
        launch_stmts.push_back(al, plan.private_shapes[i]);
    }
    // The components the kernel writes are made ready from the loop as the
    // source wrote it, which the rewrites above left untouched, and before
    // any element is gathered into a copy.
    // The launch names the block that does it, which the device heap
    // later reads and changes.
    ASR::symbol_t *component_fit = nullptr;
    // The arrays the kernel assigns whole before it reads them, and when it
    // does: what the host holds in them then need not reach the device.
    Vec<ASR::expr_t*> overwritten, overwritten_if;
    overwritten.reserve(al, 0);
    overwritten_if.reserve(al, 0);
    {
        ParallelLoopNest source;
        bool have_source = parallel_loop_nest(region, source);
        if (have_source) {
            component_fit = build_component_fit(source, starts, ends, loc);
            if (component_fit != nullptr) {
                launch_stmts.push_back(al, ASRUtils::STMT(
                    ASR::make_BlockCall_t(al, loc, -1, component_fit)));
            }
        }
        for (size_t i = 0; i < plan.call_args.n; i++) {
            ASR::expr_t *arg = plan.call_args.p[i].m_value;
            if (arg == nullptr) continue;
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.success = true;
            ASR::expr_t *condition = have_source
                ? gpu_overwrite_condition(al, loc, source, starts, ends, arg)
                : nullptr;
            // Otherwise what a loop transformation recorded from the loop
            // it rewrote, which runs the same iterations.
            if (condition == nullptr) {
                condition = gpu_recorded_overwrite(region, arg);
                if (condition != nullptr) {
                    condition = dup.duplicate_expr(condition);
                }
            }
            if (condition == nullptr) continue;
            overwritten.push_back(al, dup.duplicate_expr(arg));
            overwritten_if.push_back(al, condition);
        }
    }
    for (size_t gi = 0; gi < plan.gather_stmts.n; gi++) {
        launch_stmts.push_back(al, plan.gather_stmts.p[gi]);
    }
    for (size_t pi = 0; pi < pre_launch_stmts.n; pi++) {
        launch_stmts.push_back(al, pre_launch_stmts.p[pi]);
    }

    // Copy liveout scalars into their 1-element array buffers
    // before the kernel launch so the buffer has the initial value
    for (auto &ls : plan.liveout_scalars) {
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *buf_item = gpu_liveout_slot(al, loc, ls.host_buf_sym,
            ls.scalar_type);
        launch_stmts.push_back(al, gpu_if_present(al, loc,
            ls.orig_scalar_sym, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, buf_item, scalar_var,
                    nullptr, false, false, false, false))));
    }

    ASR::expr_t *block_size_const = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, device_caps.threads_per_block, plan.int_type,
            ASR::integerbozType::Decimal));

    // The dimensions a launch that may run in chunks splits, in the order
    // the kernel numbers its iterations.
    std::vector<size_t> split_dims;
    if (chunked) {
        for (size_t d : plan.order) {
            if (plan.start_parameters[d] != nullptr) split_dims.push_back(d);
        }
    }

    // One accumulator per iteration, sized now that the iteration count
    // is known. The kernel sets each to the identity, so nothing is
    // copied in. A launch of more iterations than the chunk limit runs in
    // chunks (device_launch_expand.cpp), which number their threads from
    // zero: it has one per iteration of its largest chunk, the first, the
    // only one whose threads start them.
    ASR::expr_t *slots = thread_count;
    if (plan.first_chunk_parameter != nullptr) {
        LCOMPILERS_ASSERT(chunked);
        auto local = [&](const std::string &name) {
            return b.Variable(current_scope,
                current_scope->get_unique_name(name),
                ASRUtils::duplicate_type(al, count_type),
                ASR::intentType::Local);
        };
        std::vector<ASR::expr_t*> extents, pieces;
        for (size_t d : split_dims) {
            extents.push_back(extent(d));
            pieces.push_back(local("__gpu_chunk_piece"));
        }
        ASR::expr_t *largest = local("__gpu_chunk_threads");
        // What gpu_chunk_pieces adds, then the number of slots.
        Vec<ASR::stmt_t*> split;
        split.reserve(al, 4 * extents.size() + 2);
        gpu_chunk_pieces(al, loc, split, extents,
            plan.chunk_fixed_iterations, pass_options.gpu_chunk_limit,
            pieces, largest);
        slots = local("__gpu_reduction_slots");
        launch_stmts.push_back(al, b.Assignment(slots, thread_count));
        split.push_back(al, b.Assignment(slots, largest));
        launch_stmts.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc,
            nullptr, b.Gt(thread_count, b.i_t(pass_options.gpu_chunk_limit,
                count_type)), split.p, split.n, nullptr, 0)));
        if (plan.strided) {
            // Only the first gpu_reduction_threads threads of a chunk start
            // and fill a slot (the rest return).
            ASR::expr_t *limit = b.i_t(gpu_reduction_threads, count_type);
            launch_stmts.push_back(al, b.Assignment(slots,
                ASRUtils::EXPR(ASR::make_IfExp_t(al, loc,
                    b.Gt(slots, limit), limit, slots, count_type,
                    nullptr))));
        }
    }
    for (auto &r : plan.reductions) {
        if (!r.host_buf_sym) continue;
        Vec<ASR::alloc_arg_t> alloc_args;
        alloc_args.reserve(al, 1);
        ASR::alloc_arg_t arg;
        arg.loc = loc;
        arg.m_a = ASRUtils::EXPR(ASR::make_Var_t(al, loc, r.host_buf_sym));
        Vec<ASR::dimension_t> alloc_dims;
        alloc_dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = b.i_t(1, count_type);
        dim.m_length = slots;
        alloc_dims.push_back(al, dim);
        arg.m_dims = alloc_dims.p;
        arg.n_dims = alloc_dims.n;
        arg.m_codims = nullptr;
        arg.n_codims = 0;
        arg.m_len_expr = nullptr;
        arg.m_sym_subclass = nullptr;
        arg.m_type = nullptr;
        alloc_args.push_back(al, arg);
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Allocate_t(al, loc, alloc_args.p, alloc_args.n,
                nullptr, nullptr, nullptr)));
    }

    Vec<ASR::gpu_launch_dimension_t> chunk_dimensions;
    chunk_dimensions.reserve(al, split_dims.size());
    for (size_t d : split_dims) {
        ASR::gpu_launch_dimension_t dim;
        dim.loc = loc;
        dim.m_start_parameter = plan.start_parameters[d];
        dim.m_end_parameter = plan.end_parameters[d];
        chunk_dimensions.push_back(al, dim);
    }

    launch_stmts.push_back(al, ASRUtils::STMT(
        ASR::make_GpuKernelLaunch_t(al, loc,
            ASR::down_cast<ASR::symbol_t>(plan.kernel_func),
            thread_count, block_size_const,
            plan.call_args.p, plan.call_args.n, component_fit, nullptr,
            0, nullptr, nullptr, 0, overwritten.p, overwritten.n,
            overwritten_if.p, overwritten_if.n, chunk_dimensions.p,
            chunk_dimensions.n, plan.chunk_fixed_iterations,
            plan.first_chunk_parameter)));

    // The launch does not wait for the kernel. What follows reads what the
    // kernel wrote, so the `gpu_launch_sync` pass puts the GpuSync that
    // waits for it in front of it.

    // Put every gathered element the kernel wrote into back over the
    // original, before anything on the host can read it again.
    for (size_t si = 0; si < plan.scatter_stmts.n; si++) {
        launch_stmts.push_back(al, plan.scatter_stmts.p[si]);
    }

    // Copy liveout scalar results back from the 1-element array
    // buffers after the kernel has completed
    for (auto &ls : plan.liveout_scalars) {
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *buf_item = gpu_liveout_slot(al, loc, ls.host_buf_sym,
            ls.scalar_type);
        launch_stmts.push_back(al, gpu_if_present(al, loc,
            ls.orig_scalar_sym, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, scalar_var, buf_item,
                    nullptr, false, false, false, false))));
    }

    // Fold the accumulators into the scalar the source named. It still
    // holds whatever it held before the loop, which is what a Fortran
    // reduction accumulates onto, so the fold starts from it rather than
    // from the identity.
    for (auto &r : plan.reductions) {
        if (!r.host_buf_sym) continue;
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, r.orig_scalar_sym));
        std::string index_name = current_scope->get_unique_name("__gpu_fold_i");
        ASR::symbol_t *index_sym = gpu_new_variable(al, loc, current_scope,
            index_name, ASRUtils::duplicate_type(al, count_type));
        ASR::expr_t *index_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, index_sym));
        ASR::expr_t *slot = gpu_reduction_slot(al, loc, r.host_buf_sym,
            index_var, r.scalar_type);

        Vec<ASR::stmt_t*> fold_body;
        fold_body.reserve(al, 1);
        if (gpu_reduction_folds_by_compare(r.op)) {
            // MIN and MAX keep whichever of the two they are named for.
            ASR::expr_t *keep = ASRUtils::EXPR(ASRUtils::make_Cmpop_util(
                al, loc,
                r.op == ASR::reduction_opType::ReduceMIN
                    ? ASR::cmpopType::Lt : ASR::cmpopType::Gt,
                slot, scalar_var,
                ASRUtils::duplicate_type(al, r.scalar_type)));
            Vec<ASR::stmt_t*> keep_body;
            keep_body.reserve(al, 1);
            keep_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, scalar_var, slot,
                    nullptr, false, false, false, false)));
            Vec<ASR::stmt_t*> keep_else;
            keep_else.reserve(al, 0);
            fold_body.push_back(al, ASRUtils::STMT(
                ASR::make_If_t(al, loc, nullptr, keep,
                    keep_body.p, keep_body.n, keep_else.p, keep_else.n)));
        } else {
            fold_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, scalar_var,
                    ASRUtils::EXPR(ASRUtils::make_Binop_util(al, loc,
                        gpu_reduction_fold_binop(r.op), scalar_var, slot,
                        ASRUtils::duplicate_type(al, r.scalar_type))),
                    nullptr, false, false, false, false)));
        }

        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = index_var;
        head.m_start = b.i_t(1, count_type);
        head.m_end = slots;
        head.m_increment = nullptr;
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASRUtils::make_DoLoop_t_util(al, loc, nullptr, head,
                fold_body.p, fold_body.n, nullptr, 0)));

        Vec<ASR::expr_t*> dealloc_args;
        dealloc_args.reserve(al, 1);
        dealloc_args.push_back(al,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, r.host_buf_sym)));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_ExplicitDeallocate_t(al, loc,
                dealloc_args.p, dealloc_args.n, nullptr, nullptr)));
    }

    prelude.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr,
        b.Gt(thread_count, b.i_t(0, ASRUtils::expr_type(thread_count))),
        launch_stmts.p, launch_stmts.n, nullptr, 0)));

    pass_result.reserve(al, prelude.n);
    for (size_t i = 0; i < prelude.n; i++) {
        pass_result.push_back(al, prelude.p[i]);
    }
}

} // namespace LCompilers
