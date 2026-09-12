#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_undo.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

// Replaces the region with the launch of the kernel the draft above built:
// the copies the host makes on either side of the dispatch, the dispatch
// itself, and the GpuOffload node that still carries the host loop as its
// alternative. Everything it needs is in `plan`; from here the offload is
// certain, so this is also where what the launch cannot honour is reported.
void GpuOffloadVisitor::build_kernel_launch(const ASR::OMPRegion_t &region,
        const ParallelLoopNest &work, const Location &loc,
        GpuLaunchPlan &plan) {
    // The loop is offloaded from here on, so this is where a clause the
    // launch cannot honour is reported: before this every exit still
    // leaves the loop on the host, where the clause is honoured.
    for (size_t i = 0; i < region.n_clauses; i++) {
        std::string clause_name = unhonoured_clause(region.m_clauses[i]);
        if (!clause_name.empty()) {
            report_clause_ignored(region.m_clauses[i]->base.loc,
                clause_name);
        }
    }

    // A `stop` the kernel runs as a trap is reported for the same reason:
    // the launch does halt where the program said to halt, but the stop
    // code and the kind of termination do not survive the crossing. A
    // device with no trap never gets here -- the loop was declined above
    // and runs on the host, where the statement means all of what it says.
    {
        GpuStopStatementFinder stops;
        for (size_t i = 0; i < work.n_body; i++) {
            stops.visit_stmt(*work.body[i]);
        }
        for (ASR::Function_t *fn : reachable_routines(work.body,
                work.n_body)) {
            for (size_t i = 0; i < fn->n_body; i++) {
                stops.visit_stmt(*fn->m_body[i]);
            }
        }
        for (auto &stop : stops.stops) {
            report_stop_degraded(stop.second, stop.first);
        }
    }

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
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t *bc =
                ASR::down_cast<ASR::BlockCall_t>(stmt);
            if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                ASR::Block_t *blk =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                stmts_to_scan = blk->m_body;
                n_stmts_to_scan = blk->n_body;
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

    // 7. Replace the region with GpuKernelLaunch + GpuSync
    // Collect all launch-related statements into a temporary Vec.
    // If any involved variable is optional, wrap them in a
    // present() guard so the host never reads a null descriptor.
    plan.gather_guard->commit();
    Vec<ASR::stmt_t*> launch_stmts;
    launch_stmts.reserve(al, plan.gather_stmts.n + pre_launch_stmts.n
        + plan.scatter_stmts.n + plan.liveout_scalars.size() + 2
        + plan.liveout_scalars.size());
    for (size_t gi = 0; gi < plan.gather_stmts.n; gi++) {
        launch_stmts.push_back(al, plan.gather_stmts.p[gi]);
    }
    for (size_t pi = 0; pi < pre_launch_stmts.n; pi++) {
        launch_stmts.push_back(al, pre_launch_stmts.p[pi]);
    }

    // Copy liveout scalars into their 1-element array buffers
    // before the kernel launch so the buffer has the initial value
    for (auto &ls : plan.liveout_scalars) {
        ASR::expr_t *buf_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.host_buf_sym));
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *idx_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, plan.int_type,
                ASR::integerbozType::Decimal));
        Vec<ASR::array_index_t> ai_args;
        ai_args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = idx_one;
        ai.m_step = nullptr;
        ai_args.push_back(al, ai);
        ASR::expr_t *buf_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, buf_var,
                ai_args.p, 1, ls.scalar_type,
                ASR::arraystorageType::ColMajor, nullptr));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, buf_item, scalar_var,
                nullptr, false, false)));
    }

    ASR::expr_t *block_size_const = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, 256, plan.int_type,
            ASR::integerbozType::Decimal));

    // Compute host-side total_elements = product of (end_d - start_d + 1)
    ASR::expr_t *host_one = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, 1, plan.int_type,
            ASR::integerbozType::Decimal));
    ASR::expr_t *host_total = nullptr;
    for (size_t d = 0; d < plan.n_dims; d++) {
        ASR::expr_t *dim_range = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc,
                ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    plan.dim_info[d].host_end, ASR::binopType::Sub,
                    plan.dim_info[d].host_start, plan.int_type, nullptr)),
                ASR::binopType::Add, host_one, plan.int_type, nullptr));
        if (host_total == nullptr) {
            host_total = dim_range;
        } else {
            host_total = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    host_total, ASR::binopType::Mul,
                    dim_range, plan.int_type, nullptr));
        }
    }

    // grid_size = (total + 255) / 256
    ASR::expr_t *grid_padded = ASRUtils::EXPR(
        ASR::make_IntegerBinOp_t(al, loc, host_total, ASR::binopType::Add,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 255, plan.int_type,
                ASR::integerbozType::Decimal)),
            plan.int_type, nullptr));
    ASR::expr_t *grid_size = ASRUtils::EXPR(
        ASR::make_IntegerBinOp_t(al, loc, grid_padded, ASR::binopType::Div,
            block_size_const, plan.int_type, nullptr));

    // One accumulator per iteration, sized now that the iteration count
    // is known. The kernel sets each to the identity, so nothing is
    // copied in.
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
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
            plan.int_type, ASR::integerbozType::Decimal));
        dim.m_length = host_total;
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

    launch_stmts.push_back(al, ASRUtils::STMT(
        ASR::make_GpuKernelLaunch_t(al, loc,
            ASR::down_cast<ASR::symbol_t>(plan.kernel_func),
            grid_size, block_size_const,
            plan.call_args.p, plan.call_args.n)));

    launch_stmts.push_back(al, ASRUtils::STMT(
        ASR::make_GpuSync_t(al, loc)));

    // Put every gathered element the kernel wrote into back over the
    // original, before anything on the host can read it again.
    for (size_t si = 0; si < plan.scatter_stmts.n; si++) {
        launch_stmts.push_back(al, plan.scatter_stmts.p[si]);
    }

    // Copy liveout scalar results back from the 1-element array
    // buffers after the kernel has completed
    for (auto &ls : plan.liveout_scalars) {
        ASR::expr_t *buf_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.host_buf_sym));
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *idx_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, plan.int_type,
                ASR::integerbozType::Decimal));
        Vec<ASR::array_index_t> ai_args;
        ai_args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = idx_one;
        ai.m_step = nullptr;
        ai_args.push_back(al, ai);
        ASR::expr_t *buf_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, buf_var,
                ai_args.p, 1, ls.scalar_type,
                ASR::arraystorageType::ColMajor, nullptr));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, scalar_var, buf_item,
                nullptr, false, false)));
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
            index_name, ASRUtils::duplicate_type(al, plan.int_type));
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
                    nullptr, false, false)));
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
                    nullptr, false, false)));
        }

        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = index_var;
        head.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
            plan.int_type, ASR::integerbozType::Decimal));
        head.m_end = host_total;
        head.m_increment = nullptr;
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_DoLoop_t(al, loc, nullptr, head,
                fold_body.p, fold_body.n, nullptr, 0)));

        Vec<ASR::expr_t*> dealloc_args;
        dealloc_args.reserve(al, 1);
        dealloc_args.push_back(al,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, r.host_buf_sym)));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_ExplicitDeallocate_t(al, loc,
                dealloc_args.p, dealloc_args.n)));
    }

    // If any involved variable is optional, wrap the whole kernel
    // launch block in if(present(v1) .and. present(v2) ...) so
    // the host never tries to read a null descriptor or compute
    // ArraySize on an absent argument.
    Vec<ASR::stmt_t*> device_body;
    device_body.reserve(al, launch_stmts.n);
    if (!plan.optional_syms.empty()) {
        ASR::ttype_t *log_type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, loc, 4));
        ASR::expr_t *guard = nullptr;
        for (ASR::symbol_t *opt_sym : plan.optional_syms) {
            Vec<ASR::expr_t*> present_args;
            present_args.reserve(al, 1);
            present_args.push_back(al, ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, opt_sym)));
            ASR::expr_t *present_call = ASRUtils::EXPR(
                ASR::make_IntrinsicElementalFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicElementalFunctions::Present),
                    present_args.p, present_args.n, 0,
                    log_type, nullptr));
            if (guard == nullptr) {
                guard = present_call;
            } else {
                guard = ASRUtils::EXPR(
                    ASR::make_LogicalBinOp_t(al, loc, guard,
                        ASR::logicalbinopType::And, present_call,
                        log_type, nullptr));
            }
        }
        Vec<ASR::stmt_t*> empty_else;
        empty_else.reserve(al, 0);
        device_body.push_back(al, ASRUtils::STMT(
            ASR::make_If_t(al, loc, nullptr, guard,
                launch_stmts.p, launch_stmts.n,
                empty_else.p, empty_else.n)));
    } else {
        for (size_t i = 0; i < launch_stmts.n; i++) {
            device_body.push_back(al, launch_stmts.p[i]);
        }
    }
    Vec<ASR::stmt_t*> fallback;
    fallback.reserve(al, 1);
    fallback.push_back(al, const_cast<ASR::stmt_t*>(&region.base));
    pass_result.reserve(al, 1);
    pass_result.push_back(al, ASRUtils::STMT(ASR::make_GpuOffload_t(al,
        loc, ASR::down_cast<ASR::symbol_t>(plan.kernel_func),
        device_body.p, device_body.n, fallback.p, fallback.n)));
}

} // namespace LCompilers
