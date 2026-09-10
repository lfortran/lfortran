#include <map>
#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/scoped_inlining.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

// A submodule `module procedure` reaches its callers through the
// parent module's interface declaration, whose body is empty. Return
// the Implementation function that actually carries the body,
// loading the submodule from its `.smod` file when it is not already
// part of this translation unit (--separate-compilation). Returns
// `fn` unchanged when it already is an implementation, or when no
// implementation can be found.
ASR::Function_t* GpuOffloadVisitor::resolve_function_implementation(
        ASR::Function_t *fn) {
    if (!fn) return nullptr;
    ASR::FunctionType_t *fn_ft = ASR::down_cast<ASR::FunctionType_t>(
        fn->m_function_signature);
    if (fn_ft->m_deftype != ASR::deftypeType::Interface) return fn;
    auto cached = function_implementations.find(fn);
    if (cached != function_implementations.end()) return cached->second;
    ASR::Function_t *impl = find_function_implementation(fn);
    function_implementations[fn] = impl;
    return impl;
}

// The search behind resolve_function_implementation(), which caches
// its result.
ASR::Function_t* GpuOffloadVisitor::find_function_implementation(
        ASR::Function_t *fn) {
    std::string pname = fn->m_name;
    for (auto &tu_item : tu.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Module_t>(*tu_item.second)) continue;
        ASR::Module_t *mod = ASR::down_cast<ASR::Module_t>(
            tu_item.second);
        ASR::symbol_t *impl_sym = mod->m_symtab->get_symbol(pname);
        if (!impl_sym || !ASR::is_a<ASR::Function_t>(*impl_sym)) continue;
        ASR::Function_t *impl_func = ASR::down_cast<ASR::Function_t>(
            impl_sym);
        if (ASR::down_cast<ASR::FunctionType_t>(
                impl_func->m_function_signature)->m_deftype ==
                ASR::deftypeType::Implementation) {
            return impl_func;
        }
    }
    // Not in this translation unit: load the submodule from disk.
    SymbolTable *parent_st = fn->m_symtab->parent;
    if (!parent_st || !parent_st->asr_owner ||
            parent_st->asr_owner->type != ASR::asrType::symbol) {
        return fn;
    }
    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
        parent_st->asr_owner);
    if (!ASR::is_a<ASR::Module_t>(*owner)) return fn;
    std::string smod_prefix = std::string(
        ASR::down_cast<ASR::Module_t>(owner)->m_name) + "@";
    std::vector<std::filesystem::path> mod_dirs;
    mod_dirs.push_back(pass_options.runtime_library_dir);
    mod_dirs.push_back(pass_options.mod_files_dir);
    mod_dirs.insert(mod_dirs.end(), pass_options.include_dirs.begin(),
        pass_options.include_dirs.end());
    for (auto &dir : mod_dirs) {
        if (dir.empty()) dir = ".";
        if (!std::filesystem::is_directory(dir)) continue;
        for (auto &file : std::filesystem::directory_iterator(dir)) {
            std::string fname = file.path().filename().string();
            if (!startswith(fname, smod_prefix) ||
                    !endswith(fname, ".smod")) continue;
            std::string content;
            if (!read_file(file.path().string(), content) ||
                    content.empty()) continue;
            LocationManager lm_tmp;
            auto res = load_modfile(al, content, false, *tu.m_symtab,
                lm_tmp);
            if (!res.ok) continue;
            load_submodule_deps(*res.result);
            fix_external_symbols(*res.result, *tu.m_symtab);
            lower_loaded_implied_do_loops(*res.result);
            ASR::Module_t *submod = ASRUtils::extract_module(
                *res.result);
            ASR::symbol_t *impl_sym = submod->m_symtab->get_symbol(
                pname);
            if (!impl_sym || !ASR::is_a<ASR::Function_t>(*impl_sym)) {
                continue;
            }
            ASR::Function_t *impl_func = ASR::down_cast<ASR::Function_t>(
                impl_sym);
            if (ASR::down_cast<ASR::FunctionType_t>(
                    impl_func->m_function_signature)->m_deftype !=
                    ASR::deftypeType::Implementation) continue;
            return impl_func;
        }
    }
    return fn;
}

// The function the inliner must reason about for a call: its
// implementation, never the interface declaration that stands in for
// a submodule `module procedure` at the call site. The interface has
// an empty body, so reasoning about it would silently conclude that
// the callee needs nothing.
ASR::Function_t* GpuOffloadVisitor::resolve_device_function(
        ASR::symbol_t *sym) {
    if (!sym) return nullptr;
    ASR::symbol_t *r = ASRUtils::symbol_get_past_external(sym);
    if (!r) return nullptr;
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*r)) {
        r = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(r)->m_proc);
    }
    if (!r || !ASR::is_a<ASR::Function_t>(*r)) return nullptr;
    return resolve_function_implementation(
        ASR::down_cast<ASR::Function_t>(r));
}

void GpuOffloadVisitor::mark_regions_device_code_runs() {
    GpuParallelRegionCollector all_loops;
    all_loops.visit_TranslationUnit(tu);
    std::vector<ASR::Function_t*> pending;
    auto add_callees = [&](ASR::stmt_t **body, size_t n_body) {
        GpuFunctionCollector fc;
        for (size_t i = 0; i < n_body; i++) {
            fc.visit_stmt(*body[i]);
        }
        for (auto &item : fc.functions) {
            ASR::Function_t *callee = resolve_device_function(
                item.second);
            if (callee &&
                    device_reachable_functions.insert(callee).second) {
                pending.push_back(callee);
            }
        }
    };
    GpuParallelRegionCollector blocked;
    for (auto &item : tu.m_symtab->get_scope()) {
        if (!ASRUtils::is_device_kernel(item.second)) continue;
        ASR::Function_t *k =
            ASR::down_cast<ASR::Function_t>(item.second);
        add_callees(k->m_body, k->n_body);
        GpuParallelRegionCollector in_kernel;
        for (size_t i = 0; i < k->n_body; i++) {
            in_kernel.visit_stmt(*k->m_body[i]);
        }
        blocked.loops.insert(in_kernel.loops.begin(),
            in_kernel.loops.end());
    }
    for (ASR::OMPRegion_t *loop : all_loops.loops) {
        add_callees(loop->m_body, loop->n_body);
    }
    // A procedure stays device-reachable once its caller's loop has
    // been rewritten into a launch, so the set accumulates across
    // rounds rather than being rebuilt from the loops still present.
    for (ASR::Function_t *fn : device_reachable_functions) {
        pending.push_back(fn);
    }
    while (!pending.empty()) {
        ASR::Function_t *fn = pending.back();
        pending.pop_back();
        add_callees(fn->m_body, fn->n_body);
    }
    for (ASR::Function_t *fn : device_reachable_functions) {
        GpuParallelRegionCollector in_fn;
        for (size_t i = 0; i < fn->n_body; i++) {
            in_fn.visit_stmt(*fn->m_body[i]);
        }
        blocked.loops.insert(in_fn.loops.begin(), in_fn.loops.end());
    }
    for (ASR::OMPRegion_t *loop : blocked.loops) {
        serial_regions.insert(loop);
    }
    GpuSerialRegionMarker::run(serial_regions);
}

// Strip the physical-type casts wrapping an actual argument. The
// uncast expression keeps its declared shape, which is what makes
// `size(dummy,1)` fold to a constant after substitution.
ASR::expr_t* GpuOffloadVisitor::strip_array_casts(ASR::expr_t *e) {
    while (e && ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
    }
    return e;
}

// The right-hand side of `target = f(...)` -- the only position a
// call can be spliced from without inventing a temporary.
const ASR::FunctionCall_t* GpuOffloadVisitor::spliceable_call(
        ASR::stmt_t *stmt) {
    if (!ASR::is_a<ASR::Assignment_t>(*stmt)) return nullptr;
    ASR::expr_t *value = strip_array_casts(
        ASR::down_cast<ASR::Assignment_t>(stmt)->m_value);
    if (!value || !ASR::is_a<ASR::FunctionCall_t>(*value)) return nullptr;
    return ASR::down_cast<ASR::FunctionCall_t>(value);
}

bool GpuOffloadVisitor::can_inline_device_function(ASR::Function_t *fn,
        const ASR::FunctionCall_t *call) {
    return fn && fn->m_return_var &&
        PassUtils::can_inline_in_block(*fn, call->m_args, call->n_args);
}

// True when `fn` itself needs a run-time sized temporary, or reaches
// a function that does. Also check the results of callees that will remain
// out of line, before later passes create their caller-owned temporaries.
// Memoized; `visiting` breaks call cycles.
bool GpuOffloadVisitor::device_function_needs_inlining(ASR::Function_t *fn,
        std::map<ASR::Function_t*, bool> &memo,
        std::set<ASR::Function_t*> &visiting,
        GpuDecline &decline) {
    auto it = memo.find(fn);
    if (it != memo.end()) return it->second;
    if (visiting.count(fn)) return false;
    visiting.insert(fn);
    GpuDeviceFunctionArrayTempChecker checker;
    checker.check_function(fn);
    bool result = checker.has_runtime_sized_temp;
    GpuFunctionCollector fc;
    for (size_t i = 0; i < fn->n_body; i++) {
        fc.visit_stmt(*fn->m_body[i]);
    }
    for (auto &[name, sym] : fc.functions) {
        ASR::Function_t *callee = resolve_device_function(sym);
        if (callee && callee != fn) {
            bool callee_needs_inlining = device_function_needs_inlining(
                callee, memo, visiting, decline);
            if (decline.declined()) {
                visiting.erase(fn);
                return false;
            }
            result = result || callee_needs_inlining;
        }
    }
    visiting.erase(fn);
    if (!result && !gpu_function_result_allocation_is_supported(*fn)) {
        decline = GpuDecline(GpuDeclineReason::FunctionResultAllocation,
            fn->m_name);
        return false;
    }
    memo[fn] = result;
    return result;
}

// Walk the statements that will become the kernel body and work out
// which callees have to be spliced in. Purely analytical: nothing is
// rewritten here, so the offload decision stays ahead of any
// destructive change. A callee that cannot be spliced or whose out-of-line
// result allocation is unsupported keeps the loop on the host.
bool GpuOffloadVisitor::plan_device_function_inlining(
        ASR::stmt_t **stmts, size_t n_stmts,
        std::map<ASR::Function_t*, bool> &memo,
        std::set<ASR::Function_t*> &on_stack,
        GpuDecline &decline,
        bool spliceable) {
    for (size_t si = 0; si < n_stmts; si++) {
        ASR::stmt_t *stmt = stmts[si];
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                if (!plan_device_function_inlining(blk->m_body,
                        blk->n_body, memo, on_stack, decline, spliceable)) {
                    return false;
                }
            }
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                ASR::AssociateBlock_t *blk =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                // Splice rewrites an assignment in the current
                // scope. An ASSOCIATE-local actual would leave a
                // BlockCall whose Vars point outside that table.
                if (!plan_device_function_inlining(blk->m_body,
                        blk->n_body, memo, on_stack, decline, false)) {
                    return false;
                }
            }
            continue;
        }
        const ASR::FunctionCall_t *top = spliceable_call(stmt);
        GpuCallSiteCollector csc;
        csc.visit_stmt(*stmt);
        for (const ASR::FunctionCall_t *call : csc.calls) {
            ASR::Function_t *callee = resolve_device_function(
                call->m_name);
            if (!callee) continue;
            bool needs_inlining = device_function_needs_inlining(callee,
                memo, on_stack, decline);
            if (decline.declined()) return false;
            if (!needs_inlining) continue;
            // Only a call that *is* the assignment's value can be
            // spliced; one nested inside a larger expression would
            // need a temporary the caller does not have. ASSOCIATE
            // is the same: splice cannot rewrite that scope.
            if (!spliceable || call != top) {
                return false;
            }
            if (on_stack.count(callee)) {
                return false;
            }
            if (!can_inline_device_function(callee, call)) return false;
            functions_to_inline.insert(callee);
            on_stack.insert(callee);
            bool ok = plan_device_function_inlining(callee->m_body,
                callee->n_body, memo, on_stack, decline, spliceable);
            on_stack.erase(callee);
            if (!ok) return false;
        }
    }
    return true;
}

// Copy a sectioned actual argument into a contiguous array owned by
// the spliced block, and return that array; nullptr when the actual is
// not a section, when the callee never sections the dummy, or when the
// section is not one the copy loops can walk.
//
// The callee's own sections of the dummy are what force this: splicing
// substitutes the actual for the dummy, so `c(1:k)` over an actual
// `a(:,j)` becomes a section of a section.  A device pointer carries a
// base and a count and nothing else, so the inner section's stride
// would simply be dropped.  Copying first leaves the dummy standing for
// an ordinary contiguous array, which the callee may section freely.
//
// The array lives in the spliced block, so kernel extraction gives it a
// per-thread workspace buffer rather than one buffer shared by every
// thread; an extent the host cannot work out is caught by the workspace
// pre-flight, which declines the loop.
ASR::expr_t* GpuOffloadVisitor::gather_section_actual(const Location &loc,
        SymbolTable *block_scope, ASR::Function_t *fn,
        ASR::symbol_t *dummy, ASR::expr_t *actual, bool writable,
        std::vector<ASR::stmt_t*> &gathers,
        std::vector<ASR::stmt_t*> &scatters) {
    if (!actual || !ASR::is_a<ASR::ArraySection_t>(*actual)) {
        return nullptr;
    }
    ASR::ArraySection_t *as =
        ASR::down_cast<ASR::ArraySection_t>(actual);
    // The copy loops index the base as a designator, so it has to be
    // one they can write down.
    if (!ASR::is_a<ASR::Var_t>(*as->m_v)
            && !ASR::is_a<ASR::StructInstanceMember_t>(*as->m_v)) {
        return nullptr;
    }
    std::vector<int> range_dims;
    for (size_t d = 0; d < as->n_args; d++) {
        if (as->m_args[d].m_left && as->m_args[d].m_right
                && as->m_args[d].m_step) {
            range_dims.push_back((int)d);
        }
    }
    if (range_dims.empty()) return nullptr;
    GpuDummySectionFinder finder(dummy);
    for (size_t i = 0; i < fn->n_body; i++) {
        finder.visit_stmt(*fn->m_body[i]);
    }
    if (!finder.found) return nullptr;
    // Gathering is what lets this callee be spliced at all, so it is
    // also where the shader has to be judged buildable. A callee that
    // holds an implied-do reaches the Metal code generator with no
    // rendering for it, and the driver is handed a shader that will
    // not compile -- worse than leaving the loop on the host. Declining
    // to gather leaves the nested section standing, and the loop is
    // then declined further down exactly as before.
    GpuImpliedDoFinder implied_do;
    for (size_t i = 0; i < fn->n_body; i++) {
        implied_do.visit_stmt(*fn->m_body[i]);
    }
    if (implied_do.found) return nullptr;

    Vec<ASR::expr_t*> extents;
    extents.reserve(al, range_dims.size());
    for (int d : range_dims) {
        extents.push_back(al, section_extent(loc, as->m_args[d]));
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(as->m_v));
    ASR::expr_t *tmp = declare_temp_array(loc, block_scope, elem_type,
        extents.p, extents.n, "__gpu_arg");
    gathers.push_back(build_section_copy_loops(loc, block_scope, as,
        range_dims, tmp, true));
    if (writable) {
        scatters.push_back(build_section_copy_loops(loc, block_scope,
            as, range_dims, tmp, false));
    }
    return tmp;
}

// Prepare device-specific section arguments, then use the shared scoped
// inliner. The block keeps per-call locals separate from captured arguments;
// final kernel planning assigns their workspace storage.
ASR::stmt_t* GpuOffloadVisitor::splice_device_function(ASR::Function_t *fn,
        const ASR::FunctionCall_t *fc, ASR::expr_t *target,
        const Location &loc) {
    SymbolTable *block_scope = al.make_new<SymbolTable>(current_scope);

    std::map<ASR::symbol_t*, ASR::expr_t*> subst;
    std::vector<ASR::stmt_t*> arg_gathers, arg_scatters;
    for (size_t i = 0; i < fn->n_args; i++) {
        ASR::symbol_t *d = ASR::down_cast<ASR::Var_t>(
            fn->m_args[i])->m_v;
        ASR::expr_t *actual = strip_array_casts(fc->m_args[i].m_value);
        if (!actual) return nullptr;
        // A sectioned actual whose dummy the callee sections in turn
        // would leave a section of a section behind, which no device
        // pointer can express. Copy it into a contiguous array of the
        // spliced block first and let the dummy stand for that.
        ASR::expr_t *gathered = gather_section_actual(loc, block_scope,
            fn, d, actual, dummy_is_written(fn, i), arg_gathers,
            arg_scatters);
        subst[d] = gathered ? gathered : actual;
    }

    ASR::stmt_t *call = PassUtils::inline_in_block(al, loc, *fn,
        block_scope, std::move(subst), target, arg_gathers, arg_scatters);
    if (!call) return nullptr;
    kernel_blocks.push_back(ASR::down_cast<ASR::BlockCall_t>(call)->m_m);
    return call;
}

// Splice every planned callee into `stmts`, repeating until no call
// is left to inline (a callee's own calls surface only once its body
// has been spliced in). The planner has already proved this
// terminates: it rejects any call cycle.
bool GpuOffloadVisitor::inline_device_function_calls(ASR::stmt_t **&stmts,
        size_t &n_stmts) {
    if (functions_to_inline.empty()) return true;
    for (size_t round = 0; round < functions_to_inline.size() + 1;
            round++) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_stmts * 4);
        bool changed = false;
        for (size_t si = 0; si < n_stmts; si++) {
            ASR::stmt_t *stmt = stmts[si];
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
                if (b && ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    SymbolTable *outer_scope = current_scope;
                    current_scope = blk->m_symtab;
                    bool inlined = inline_device_function_calls(blk->m_body,
                        blk->n_body);
                    current_scope = outer_scope;
                    if (!inlined) return false;
                }
                new_body.push_back(al, stmt);
                continue;
            }
            const ASR::FunctionCall_t *call = spliceable_call(stmt);
            ASR::Function_t *callee = call
                ? resolve_device_function(call->m_name) : nullptr;
            if (!callee || !functions_to_inline.count(callee)) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::Assignment_t *asgn =
                ASR::down_cast<ASR::Assignment_t>(stmt);
            ASR::stmt_t *spliced = splice_device_function(callee,
                call, asgn->m_target, stmt->base.loc);
            if (!spliced) return false;
            new_body.push_back(al, spliced);
            changed = true;
        }
        if (!changed) break;
        stmts = new_body.p;
        n_stmts = new_body.n;
    }
    return true;
}

} // namespace LCompilers
