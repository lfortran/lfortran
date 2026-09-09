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

// The BLOCK or ASSOCIATE construct `stmt` enters, or nullptr when
// the statement does not enter one.
ASR::symbol_t* GpuOffloadVisitor::nested_scope_entered(ASR::stmt_t *stmt) {
    if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
        if (b && ASR::is_a<ASR::Block_t>(*b)) return b;
    } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
        if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) return b;
    }
    return nullptr;
}

// The symbol table and body of a BLOCK or ASSOCIATE construct.
void GpuOffloadVisitor::nested_scope_contents(
        ASR::symbol_t *b, SymbolTable *&st,
        ASR::stmt_t **&body, size_t &n_body) {
    if (ASR::is_a<ASR::Block_t>(*b)) {
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
        st = blk->m_symtab;
        body = blk->m_body;
        n_body = blk->n_body;
    } else {
        ASR::AssociateBlock_t *blk =
            ASR::down_cast<ASR::AssociateBlock_t>(b);
        st = blk->m_symtab;
        body = blk->m_body;
        n_body = blk->n_body;
    }
}

// Collect, in body order, the nested BLOCK and ASSOCIATE scopes that
// the splice will flatten into the single kernel-level block.
// Returns false when one is entered from a position the flattening
// walk cannot rebuild -- inside an IF or a loop, where the ASR holds
// a single statement rather than a statement list.
bool GpuOffloadVisitor::collect_flattened_scopes(ASR::stmt_t **stmts, size_t n,
        std::vector<ASR::symbol_t*> &scopes) {
    for (size_t i = 0; i < n; i++) {
        ASR::symbol_t *b = nested_scope_entered(stmts[i]);
        if (b) {
            scopes.push_back(b);
            SymbolTable *st = nullptr;
            ASR::stmt_t **body = nullptr;
            size_t n_body = 0;
            nested_scope_contents(b, st, body, n_body);
            if (!collect_flattened_scopes(body, n_body, scopes)) {
                return false;
            }
            continue;
        }
        GpuNestedScopeCounter nc;
        nc.visit_stmt(*stmts[i]);
        if (nc.count > 0) return false;
    }
    return true;
}

// Can this callee's body be spliced verbatim into the caller?
bool GpuOffloadVisitor::can_inline_device_function(ASR::Function_t *fn,
        const ASR::FunctionCall_t *fc) {
    if (!fn || !fn->m_return_var || fn->n_body == 0) {
        return false;
    }
    ASR::FunctionType_t *ft = ASR::down_cast<ASR::FunctionType_t>(
        fn->m_function_signature);
    if (ft->m_abi != ASR::abiType::Source) {
        return false;
    }
    if (ft->m_deftype != ASR::deftypeType::Implementation) {
        return false;
    }
    if (fn->n_args != fc->n_args) {
        return false;
    }
    for (size_t i = 0; i < fc->n_args; i++) {
        // An absent optional actual has no expression to substitute.
        if (!fc->m_args[i].m_value) {
            return false;
        }
    }
    for (size_t i = 0; i < fn->n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
            return false;
        }
    }
    std::vector<ASR::symbol_t*> nested;
    if (!collect_flattened_scopes(fn->m_body, fn->n_body, nested)) {
        return false;
    }
    std::set<ASR::symbol_t*> flattened(nested.begin(), nested.end());
    // Every symbol the callee owns, in its own scope and in each
    // nested scope, must be something the splice can carry over.
    std::vector<SymbolTable*> scopes;
    scopes.push_back(fn->m_symtab);
    for (ASR::symbol_t *b : nested) {
        SymbolTable *st = nullptr;
        ASR::stmt_t **body = nullptr;
        size_t n_body = 0;
        nested_scope_contents(b, st, body, n_body);
        scopes.push_back(st);
    }
    for (SymbolTable *st : scopes) {
        for (auto &item : st->get_scope()) {
            // An ExternalSymbol only names an entity owned by another
            // module -- a derived-type component, a type, a
            // procedure. It resolves through that module from
            // wherever the cloned body ends up, so it needs no
            // re-homing.
            if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
            // A nested scope is re-homed by flattening its variables
            // and statements into the spliced block, but only when
            // the walk above reached it.
            if (ASR::is_a<ASR::Block_t>(*item.second) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                if (!flattened.count(item.second)) {
                    return false;
                }
                continue;
            }
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
                return false;
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                item.second);
            // SAVE state must persist across calls; inlining would
            // give every call site its own copy.
            if (v->m_storage == ASR::storage_typeType::Save) {
                return false;
            }
        }
    }
    // A `return` anywhere but as the final statement needs control
    // flow the splice cannot express. A `return` inside a nested
    // scope is one such place: flattening would drop it silently.
    GpuReturnCounter rc;
    for (size_t i = 0; i < fn->n_body; i++) {
        rc.visit_stmt(*fn->m_body[i]);
    }
    for (ASR::symbol_t *b : nested) {
        SymbolTable *st = nullptr;
        ASR::stmt_t **body = nullptr;
        size_t n_body = 0;
        nested_scope_contents(b, st, body, n_body);
        for (size_t i = 0; i < n_body; i++) {
            rc.visit_stmt(*body[i]);
        }
    }
    if (rc.count > 1) return false;
    if (rc.count == 1 &&
            !ASR::is_a<ASR::Return_t>(*fn->m_body[fn->n_body - 1])) {
        return false;
    }
    return true;
}

// True when `fn` itself needs a run-time sized temporary, or reaches
// a function that does. Memoized; `visiting` breaks call cycles.
bool GpuOffloadVisitor::device_function_needs_inlining(ASR::Function_t *fn,
        std::map<ASR::Function_t*, bool> &memo,
        std::set<ASR::Function_t*> &visiting) {
    auto it = memo.find(fn);
    if (it != memo.end()) return it->second;
    if (visiting.count(fn)) return false;
    visiting.insert(fn);
    GpuDeviceFunctionArrayTempChecker checker;
    checker.check_function(fn);
    bool result = checker.has_runtime_sized_temp;
    if (!result) {
        GpuFunctionCollector fc;
        for (size_t i = 0; i < fn->n_body; i++) {
            fc.visit_stmt(*fn->m_body[i]);
        }
        for (auto &[name, sym] : fc.functions) {
            ASR::Function_t *callee = resolve_device_function(sym);
            if (callee && callee != fn &&
                    device_function_needs_inlining(callee, memo,
                        visiting)) {
                result = true;
                break;
            }
        }
    }
    visiting.erase(fn);
    memo[fn] = result;
    return result;
}

// Walk the statements that will become the kernel body and work out
// which callees have to be spliced in. Purely analytical: nothing is
// rewritten here, so the offload decision stays ahead of any
// destructive change. Returns false when some callee that must be
// inlined cannot be, in which case the loop is not offloaded.
bool GpuOffloadVisitor::plan_device_function_inlining(
        ASR::stmt_t **stmts, size_t n_stmts,
        std::map<ASR::Function_t*, bool> &memo,
        std::set<ASR::Function_t*> &on_stack,
        bool spliceable) {
    for (size_t si = 0; si < n_stmts; si++) {
        ASR::stmt_t *stmt = stmts[si];
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                if (!plan_device_function_inlining(blk->m_body,
                        blk->n_body, memo, on_stack, spliceable)) {
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
                        blk->n_body, memo, on_stack, false)) {
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
            if (!device_function_needs_inlining(callee, memo,
                    on_stack)) continue;
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
                callee->n_body, memo, on_stack, spliceable);
            on_stack.erase(callee);
            if (!ok) return false;
        }
    }
    return true;
}

// Rewrite the dimension expressions of a cloned local's type through
// `subst`, so an extent written in terms of the callee's dummies is
// expressed in terms of the actual arguments instead.
void GpuOffloadVisitor::substitute_in_type(ASR::ttype_t *t,
        std::map<ASR::symbol_t*, ASR::expr_t*> &subst) {
    if (!t) return;
    ASR::ttype_t *bare = ASRUtils::type_get_past_allocatable_pointer(t);
    if (!bare || !ASR::is_a<ASR::Array_t>(*bare)) return;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(bare);
    AssociateVarResolver resolver(al, subst);
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (arr->m_dims[d].m_start) {
            resolver.current_expr = &(arr->m_dims[d].m_start);
            resolver.replace_expr(arr->m_dims[d].m_start);
        }
        if (arr->m_dims[d].m_length) {
            resolver.current_expr = &(arr->m_dims[d].m_length);
            resolver.replace_expr(arr->m_dims[d].m_length);
        }
    }
}

// Clone `stmts` into `out`, flattening every BLOCK and ASSOCIATE it
// enters into the same statement list. An ASSOCIATE construct opens
// its body with plain assignments that define its associate names,
// so once those names are cloned as ordinary locals of the spliced
// block the body needs no further rewriting.
bool GpuOffloadVisitor::flatten_device_function_body(
        ASR::stmt_t **stmts, size_t n_stmts,
        ASRUtils::ExprStmtDuplicator &dup, Vec<ASR::stmt_t*> &out) {
    for (size_t i = 0; i < n_stmts; i++) {
        ASR::symbol_t *b = nested_scope_entered(stmts[i]);
        if (b) {
            SymbolTable *st = nullptr;
            ASR::stmt_t **body = nullptr;
            size_t n_body = 0;
            nested_scope_contents(b, st, body, n_body);
            if (!flatten_device_function_body(body, n_body, dup, out)) {
                return false;
            }
            continue;
        }
        if (ASR::is_a<ASR::Return_t>(*stmts[i])) continue;
        dup.success = true;
        ASR::stmt_t *c = dup.duplicate_stmt(stmts[i]);
        if (!c || !dup.success) return false;
        out.push_back(al, c);
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

// Splice `fn`'s body into a BLOCK, rewritten for this call site,
// and assign its result to `target` inside that block.
//
// The BLOCK is what makes this work: the callee's locals land in the
// block's own symbol table, so after kernel extraction they are
// block-scope locals of the kernel -- exactly where
// analyze_gpu_vla_workspaces() looks for run-time sized arrays and
// binds a device buffer for each. Putting them in the enclosing
// scope instead would make them kernel *arguments*, and an ALLOCATE
// of a kernel argument is not valid ASR.
//
// That machinery only inspects the symbol table of a top-level
// block, so the callee's own nested BLOCK and ASSOCIATE scopes are
// flattened into this one block rather than rebuilt inside it: a
// run-time sized temporary left one level down would be invisible to
// it and reach the shader as a variable-length array.
ASR::stmt_t* GpuOffloadVisitor::splice_device_function(ASR::Function_t *fn,
        const ASR::FunctionCall_t *fc, ASR::expr_t *target,
        const Location &loc) {
    SymbolTable *block_scope = al.make_new<SymbolTable>(current_scope);

    std::map<ASR::symbol_t*, ASR::expr_t*> subst;
    std::set<ASR::symbol_t*> dummies;
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
        dummies.insert(d);
    }

    // The callee's own scope plus every nested BLOCK and ASSOCIATE
    // scope, all of which are flattened into this one block.
    std::vector<ASR::symbol_t*> nested;
    if (!collect_flattened_scopes(fn->m_body, fn->n_body, nested)) {
        return nullptr;
    }
    std::vector<SymbolTable*> scopes;
    scopes.push_back(fn->m_symtab);
    for (ASR::symbol_t *b : nested) {
        SymbolTable *st = nullptr;
        ASR::stmt_t **body = nullptr;
        size_t n_body = 0;
        nested_scope_contents(b, st, body, n_body);
        scopes.push_back(st);
    }

    // Clone the callee's locals (its result variable included) into
    // the block. Two phases, so that an extent written in terms of
    // another local is substituted too.
    ASR::symbol_t *ret_sym = ASR::down_cast<ASR::Var_t>(
        fn->m_return_var)->m_v;
    std::vector<ASR::symbol_t*> cloned_locals;
    for (SymbolTable *st : scopes) {
        for (auto &item : st->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (dummies.count(sym)) continue;
            // ExternalSymbols keep resolving through their owning
            // module; the cloned body may reference them as they are.
            if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) continue;
            // The nested scopes themselves are dissolved by the
            // flattening, so nothing stands in for them here.
            if (ASR::is_a<ASR::Block_t>(*sym) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*sym)) continue;
            if (!ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            // A named constant carries its value on the declaration
            // rather than at every reference, so a clone that drops
            // it leaves the name standing for nothing -- neither the
            // shape resolver nor the backend can say what it is. A
            // value that is not a self-contained constant would name
            // the callee's own symbols, so only a folded one is
            // carried over.
            ASR::expr_t *param_value = nullptr;
            if (v->m_storage == ASR::storage_typeType::Parameter &&
                    v->m_value != nullptr &&
                    ASRUtils::is_value_constant(v->m_value)) {
                ASRUtils::ExprStmtDuplicator value_dup(al);
                param_value = value_dup.duplicate_expr(v->m_value);
            }
            // The block the splice creates is nested inside the
            // scope the call was made from, so a clone that keeps a
            // name something enclosing already uses shadows it in the
            // device source: the callee's result variable `faces`
            // would hide the caller's array of the same name, and the
            // copy-out would write the per-thread workspace instead
            // of the array. get_unique_name only looks at the block's
            // own scope, so the enclosing chain is asked as well.
            std::string name = block_scope->get_unique_name(v->m_name);
            for (int attempt = 1;
                    block_scope->resolve_symbol(name) != nullptr;
                    attempt++) {
                name = block_scope->get_unique_name(
                    std::string(v->m_name) + "_"
                    + std::to_string(attempt));
            }
            ASR::symbol_t *ns = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, loc, block_scope,
                    s2c(al, name), nullptr, 0, ASR::intentType::Local,
                    param_value, param_value, v->m_storage,
                    ASRUtils::duplicate_type(al, v->m_type),
                    v->m_type_declaration, ASR::abiType::Source,
                    ASR::accessType::Public, ASR::presenceType::Required,
                    false));
            block_scope->add_symbol(name, ns);
            subst[sym] = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ns));
            cloned_locals.push_back(ns);
        }
    }
    for (ASR::symbol_t *ns : cloned_locals) {
        substitute_in_type(
            ASR::down_cast<ASR::Variable_t>(ns)->m_type, subst);
    }

    ASRUtils::ExprStmtDuplicator dup(al);
    Vec<ASR::stmt_t*> cloned;
    cloned.reserve(al, fn->n_body + arg_gathers.size()
        + arg_scatters.size() + 1);
    // The gathers read the caller's own expressions, so they are not
    // subject to the dummy substitution and go in ahead of it.
    for (ASR::stmt_t *g : arg_gathers) cloned.push_back(al, g);
    size_t body_start = cloned.n;
    if (!flatten_device_function_body(fn->m_body, fn->n_body, dup,
            cloned)) {
        return nullptr;
    }
    AssociateVarResolverVisitor resolver(al, subst);
    for (size_t i = body_start; i < cloned.n; i++) {
        resolver.visit_stmt(*cloned[i]);
    }
    for (ASR::stmt_t *sc : arg_scatters) cloned.push_back(al, sc);

    auto rit = subst.find(ret_sym);
    if (rit == subst.end()) return nullptr;
    cloned.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
        al, loc, target, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            ASR::down_cast<ASR::Var_t>(rit->second)->m_v)),
        nullptr, false, false)));

    std::string block_name = current_scope->get_unique_name(
        "__gpu_inl_" + std::string(fn->m_name));
    ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
        s2c(al, block_name), cloned.p, cloned.n);
    block_scope->asr_owner = block;
    ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
    current_scope->add_symbol(block_name, block_sym);
    kernel_blocks.push_back(block_sym);
    return ASRUtils::STMT(ASR::make_BlockCall_t(al, loc, -1,
        block_sym));
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
                    if (!inline_device_function_calls(blk->m_body,
                            blk->n_body)) return false;
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
