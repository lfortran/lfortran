#ifndef LIBASR_PASS_GPU_OFFLOAD_VISITOR_H
#define LIBASR_PASS_GPU_OFFLOAD_VISITOR_H

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/utils.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_undo.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/stmt_walk_visitor.h>

namespace LCompilers {

// The GPU offload pass itself: it walks every parallel region, decides
// whether the selected device can run it, canonicalises the body it found
// into a shape a kernel can be built from, and replaces the loop with a
// kernel plus its launch. The work is spread over several translation
// units; this is the one place its members are all named.
class GpuOffloadVisitor : public ASR::StatementWalkVisitor<GpuOffloadVisitor>
{
public:
    PassOptions pass_options;
    // What the selected device can do. Every question this pass asks about
    // the device goes through here, so that none of it names a dialect.
    GpuDeviceCapabilities device_caps;
    ASR::TranslationUnit_t &tu;
    // Scalar variables that receive the result of an inlined all()
    // reduction. These need to be passed back from the GPU kernel.
    std::set<std::string> all_reduction_targets;

    GpuOffloadVisitor(Allocator &al, PassOptions pass_options_,
                      ASR::TranslationUnit_t &tu_)
        : StatementWalkVisitor(al), pass_options(pass_options_),
          device_caps(gpu_device_capabilities(pass_options_)), tu(tu_) {}

    void load_submodule_deps(ASR::TranslationUnit_t &sub_tu);

    void lower_loaded_implied_do_loops(ASR::TranslationUnit_t &sub_tu);

    ASR::expr_t* dup_expr_to_scope(ASR::expr_t *expr, SymbolTable *scope);

    void fixup_struct_refs_in_scope(SymbolTable *scope,
            SymbolTable *kernel_scope,
            char *kernel_fn_name = nullptr);

    ASR::symbol_t* find_kernel_struct(SymbolTable *kernel_scope,
            const std::string &struct_name,
            const std::string &member_name);

    ASR::symbol_t* import_struct_def(ASR::Struct_t *orig_struct,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const Location &loc);

    ASR::symbol_t* import_struct_type(ASR::symbol_t *orig_sym,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const Location &loc);

    void find_array_section_bounds(ASR::expr_t *e,
            ASR::expr_t *&loop_start, ASR::expr_t *&loop_end);

    void find_array_section_all_bounds(ASR::expr_t *e,
            std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> &dim_bounds);

    ASR::expr_t* elementize_mask(ASR::expr_t *e, ASR::expr_t *loop_var,
            ASR::ttype_t *logical_type, const Location &loc);

    ASR::expr_t* elementize_mask_multi(ASR::expr_t *e,
            std::vector<ASR::expr_t*> &loop_vars,
            ASR::ttype_t *logical_type, const Location &loc);

    ASR::expr_t* inline_single_all(ASR::IntrinsicArrayFunction_t *iaf,
            const Location &loc, Vec<ASR::stmt_t*> &preamble);

    bool contains_intrinsic_all(ASR::expr_t *e);

    ASR::expr_t* replace_all_in_expr(ASR::expr_t *e, const Location &loc,
            Vec<ASR::stmt_t*> &preamble);

    // ---------------------------------------------------------------
    // Inlining device functions into the kernel body
    //
    // Metal shaders have neither variable-length arrays nor a heap, so
    // an `inline` device function cannot declare a local whose extent is
    // only known at run time. A kernel *can* have one: the extent is
    // evaluated on the host at launch and a device buffer is bound for
    // it (`analyze_gpu_vla_workspaces`). So instead of teaching the
    // device-function boundary to carry such a workspace through every
    // address-space overload, splice the callee's body into the loop
    // body: its locals become kernel-scope locals and the existing
    // workspace machinery applies unchanged, while its dummy arguments
    // are replaced by the actual arguments -- which often makes the
    // extent a compile-time constant outright.
    // ---------------------------------------------------------------

    // Functions whose bodies must be spliced into the loop body for this
    // loop to be offloadable. Filled by plan_device_function_inlining()
    // during the (non-destructive) eligibility decision and consumed by
    // inline_device_function_calls() afterwards.
    std::set<ASR::Function_t*> functions_to_inline;

    // The implementation already found for an interface declaration.
    // Loading a submodule from disk builds a fresh copy of its symbol
    // table every time, so without this the same `module procedure`
    // would resolve to a different Function_t at every call. The
    // inliner identifies a planned callee by pointer, so the plan and
    // the splice would then disagree and the callee would silently stay
    // an out-of-line device function.
    std::map<ASR::Function_t*, ASR::Function_t*> function_implementations;

    ASR::Function_t* resolve_function_implementation(ASR::Function_t *fn);

    ASR::Function_t* find_function_implementation(ASR::Function_t *fn);

    ASR::Function_t* resolve_device_function(ASR::symbol_t *sym);

    // A `do concurrent` inside a procedure that device code can reach
    // has to stay an ordinary sequential loop. Offloading it rewrites it
    // into a host-side kernel launch, and a kernel launch has no meaning
    // inside a kernel: the device copy of that procedure is then emitted
    // with an empty body -- silently, because the GPU backends have no
    // lowering for a launch -- and the caller reads uninitialised
    // memory. The same holds for a loop already lifted into a kernel.
    // Both are sequentialized here, before this round rewrites anything,
    // so the decision is made on intact ASR.
    std::set<ASR::OMPRegion_t*> serial_regions;
    std::set<ASR::Function_t*> device_reachable_functions;

    void mark_regions_device_code_runs();

    static ASR::expr_t* strip_array_casts(ASR::expr_t *e);

    static const ASR::FunctionCall_t* spliceable_call(ASR::stmt_t *stmt);

    static ASR::symbol_t* nested_scope_entered(ASR::stmt_t *stmt);

    static void nested_scope_contents(ASR::symbol_t *b, SymbolTable *&st,
            ASR::stmt_t **&body, size_t &n_body);

    static bool collect_flattened_scopes(ASR::stmt_t **stmts, size_t n,
            std::vector<ASR::symbol_t*> &scopes);

    static bool can_inline_device_function(ASR::Function_t *fn,
            const ASR::FunctionCall_t *fc);

    bool device_function_needs_inlining(ASR::Function_t *fn,
            std::map<ASR::Function_t*, bool> &memo,
            std::set<ASR::Function_t*> &visiting);

    bool plan_device_function_inlining(ASR::stmt_t **stmts, size_t n_stmts,
            std::map<ASR::Function_t*, bool> &memo,
            std::set<ASR::Function_t*> &on_stack,
            bool spliceable = true);

    void substitute_in_type(ASR::ttype_t *t,
            std::map<ASR::symbol_t*, ASR::expr_t*> &subst);

    bool flatten_device_function_body(ASR::stmt_t **stmts, size_t n_stmts,
            ASRUtils::ExprStmtDuplicator &dup, Vec<ASR::stmt_t*> &out);

    ASR::expr_t* gather_section_actual(const Location &loc,
            SymbolTable *block_scope, ASR::Function_t *fn,
            ASR::symbol_t *dummy, ASR::expr_t *actual, bool writable,
            std::vector<ASR::stmt_t*> &gathers,
            std::vector<ASR::stmt_t*> &scatters);

    ASR::stmt_t* splice_device_function(ASR::Function_t *fn,
            const ASR::FunctionCall_t *fc, ASR::expr_t *target,
            const Location &loc);

    bool inline_device_function_calls(ASR::stmt_t **&stmts,
            size_t &n_stmts);

    void inline_intrinsic_all(ParallelLoopNest &nest);

    ASR::IntrinsicArrayFunction_t* match_statement_matmul(
            ASR::expr_t *value, ASR::expr_t *&binop_other,
            ASR::binopType &binop_op, bool &matmul_is_left);

    void inline_matmul_stmts(ASR::stmt_t** &body, size_t &n_body);

    void hoist_nested_matmuls(ParallelLoopNest &nest);

    void hoist_nested_matmuls_in_body(ASR::stmt_t** &body, size_t &n_body,
            SymbolTable *var_scope, bool scope_has_workspaces,
            bool at_loop_top);

    void hoist_matmuls_from_assignment(ASR::Assignment_t *asgn,
            Vec<ASR::stmt_t*> &out, SymbolTable *var_scope,
            bool scope_has_workspaces);

    bool matmul_operand_aliases_target(ASR::IntrinsicArrayFunction_t *mm,
            ASR::expr_t *target);

    ASR::expr_t* matmul_temp_section(ASR::expr_t *tmp, ASR::expr_t *target);

    bool declared_constant_dims(ASR::expr_t *e, Vec<ASR::dimension_t> &out);

    ASR::expr_t* make_matmul_result_temp(ASR::IntrinsicArrayFunction_t *mm,
            const Location &loc, SymbolTable *var_scope,
            bool scope_has_workspaces, ASR::expr_t *target = nullptr);

    bool intrinsic_array_result_dims(ASR::expr_t *e,
            Vec<ASR::dimension_t> &dims);

    ASR::dimension_t dim_or_runtime_extent(ASR::expr_t *operand,
            ASR::dimension_t *dims, size_t d);

    void inline_intrinsic_matmul(ParallelLoopNest &nest);

    ASR::expr_t* index_array_expr(ASR::expr_t *expr,
            ASR::array_index_t *idx_p, size_t idx_n,
            ASR::ttype_t *elem_type, const Location &loc);

    ASR::IntrinsicArrayFunction_t* find_array_intrinsic_in_expr(
            ASR::expr_t *expr, ASRUtils::IntrinsicArrayFunctions which,
            ASR::IntrinsicArrayFunction_t *skip = nullptr);

    bool replace_array_intrinsic_in_expr(ASR::expr_t* &expr,
            ASR::IntrinsicArrayFunction_t *target,
            ASR::expr_t *replacement);

    void extract_nested_sums(ASR::stmt_t** &stmts, size_t &n_stmts,
                             SymbolTable *scope);

    void inline_sum_in_stmts(ASR::stmt_t** &stmts, size_t &n_stmts,
                             SymbolTable *scope);

    void inline_intrinsic_sum(ParallelLoopNest &nest);

    ASR::expr_t* dot_product_operand_element(ASR::expr_t *arg,
            ASR::expr_t *k, ASR::ttype_t *elem_type, const Location &loc);

    ASR::expr_t* dot_product_extent(ASR::expr_t *arg, const Location &loc,
            bool allow_bound);

    void inline_dot_product_in_stmts(ASR::stmt_t** &stmts, size_t &n_stmts,
                                     SymbolTable *scope);

    void inline_intrinsic_dot_product(ParallelLoopNest &nest);

    void inline_intrinsic_transpose(ParallelLoopNest &nest);

    static bool is_int_literal(ASR::expr_t *e, int64_t value);

    ASR::expr_t *int32_const(const Location &loc, int64_t n);

    ASR::expr_t *to_int32(const Location &loc, ASR::expr_t *e);

    ASR::expr_t *section_extent(const Location &loc,
            const ASR::array_index_t &d);

    ASR::expr_t *section_index(const Location &loc,
            const ASR::array_index_t &d, ASR::expr_t *counter);

    bool eval_int_literal(ASR::expr_t *e, int64_t &out);

    ASR::expr_t *declare_temp_array(const Location &loc,
            SymbolTable *var_scope, ASR::ttype_t *elem_type,
            ASR::expr_t **extents, size_t n_extents,
            const std::string &prefix);

    ASR::expr_t *self_aliasing_target(ASR::Assignment_t *asgn);

    bool alias_temp_is_fixed_size(ASR::expr_t *target);

    bool alias_temp_extents(ASR::expr_t *target,
            Vec<ASR::expr_t*> &extents);

    bool alias_temp_extents_resolvable(ASR::expr_t *target,
            ASR::expr_t **extents, size_t n_extents,
            const std::vector<std::string> &arg_names);

    bool body_needs_unsupported_alias_temp(ASR::stmt_t **body,
            size_t n_body, bool top_level,
            const std::vector<std::string> &arg_names);

    void size_scope_array_temporaries(ASR::stmt_t **body, size_t n_body,
            std::vector<ScopeArrayDims> &undo);

    void materialize_runtime_alias_blocks(ParallelLoopNest &nest);

    void materialize_aliased_assignments(ParallelLoopNest &nest);

    void materialize_aliased_in_body(ASR::stmt_t** &body, size_t &n_body,
            bool &changed);

    void inline_array_section_assignment(ParallelLoopNest &nest);

    void inline_array_section_in_body(ASR::stmt_t** &body, size_t &n_body,
            bool &changed);


    // ---- gather/scatter for a strided section actual argument ----
    //
    // A strided array section passed as an actual argument --
    // `s3(a(j:j+4:2))` -- reaches a device function as a bare base pointer
    // plus an element count. A device pointer cannot express a stride, so
    // the callee reads the contiguous run a(j), a(j+1), a(j+2) instead of
    // every second element: finite numbers, wrong answers, no diagnostic.
    //
    // Gather the section into a contiguous temporary before the call, pass
    // the temporary, and scatter it back afterwards when the dummy may be
    // written. The temporary is an ordinary loop-body local, so each thread
    // gets its own copy through the machinery that already sizes and slices
    // kernel locals.
    //
    // A unit-stride section is left alone: base pointer plus element count
    // describes it exactly, and that path must stay as cheap as it is.
    // Linear form of an integer expression: a constant plus integer
    // multiples of scalar variables. It exists to fold `(j + 4) - j` to 4,
    // which makes the extent of a section like `a(j:j+4:2)` a compile-time
    // constant even though neither of its bounds is one.
    struct LinearForm {
        int64_t constant = 0;
        std::map<ASR::symbol_t*, int64_t> terms;
    };

    bool linear_form(ASR::expr_t *e, LinearForm &f, int64_t scale);

    bool const_section_extent(const ASR::array_index_t &d, int64_t &n);

    static bool section_is_strided(const ASR::ArraySection_t *as);

    static bool dummy_is_written(ASR::Function_t *fn, size_t arg_index);

    ASR::stmt_t* build_section_copy_loops(const Location &loc,
            SymbolTable *var_scope, ASR::ArraySection_t *as,
            const std::vector<int> &range_dims, ASR::expr_t *tmp,
            bool to_temp);

    static ASR::ArraySection_t* strided_section_actual(ASR::expr_t *e);

    bool strided_section_is_gatherable(ASR::ArraySection_t *as);

    bool gather_strided_section_arg(const Location &loc,
            SymbolTable *block_scope, ASR::expr_t **slot, bool writable,
            std::vector<ASR::stmt_t*> &before,
            std::vector<ASR::stmt_t*> &after);

    bool gather_strided_sections_in_stmt(ASR::stmt_t *stmt,
            SymbolTable *block_scope, std::vector<ASR::stmt_t*> &before,
            std::vector<ASR::stmt_t*> &after);

    bool body_has_ungatherable_strided_section(ASR::stmt_t **body,
            size_t n_body);

    void gather_strided_section_arguments(ParallelLoopNest &nest);

    void gather_strided_sections_in_body(ASR::stmt_t** &body,
            size_t &n_body, SymbolTable *scope);

    void inline_elemental_array_var_assignment(ParallelLoopNest &nest);

    void inline_elemental_array_var_in_body(ASR::stmt_t** &body,
            size_t &n_body, bool &changed);

    // Collects every symbol the kernel would have to reference for `x`:
    // the symbols appearing in the loop head and body, plus the symbols
    // that only appear inside the array-dimension expressions of those
    // symbols' types (e.g. `tmp(size(b))` pulls in `b`).
    // The names the kernel arguments will carry, in the spelling the
    // workspace extent resolver expects: every symbol the loop involves,
    // plus the synthetic per-dimension extent scalar the kernel
    // extraction adds for each dimension of an array argument.
    // A parallel loop is ordinary Fortran, so a loop that cannot be
    // offloaded must still compile and run. Report why it was left on the
    // host rather than build a kernel that would quietly do something else.
    // The loop whose offload is being decided, and the loops already
    // reported. pass_replace_gpu_offload re-walks the translation unit until
    // nothing changes, so a declined loop is visited again on every sweep;
    // without this the user is told about it once per sweep rather than once
    // per loop.
    const ASR::OMPRegion_t *region_being_decided = nullptr;
    std::set<const ASR::OMPRegion_t*> reported_regions;

    // Names the region a decline is about for as long as the decision lasts,
    // whichever of the dozen exits it leaves by.
    struct DecisionScope {
        GpuOffloadVisitor &v;
        const ASR::OMPRegion_t *saved;
        DecisionScope(GpuOffloadVisitor &v_, const ASR::OMPRegion_t *r)
            : v(v_), saved(v_.region_being_decided) {
            v.region_being_decided = r;
        }
        ~DecisionScope() { v.region_being_decided = saved; }
    };

    void report_not_offloaded(const Location &where,
            const GpuDecline &decline);

    void report_clause_ignored(const Location &where, const std::string &name);

    std::string unhonoured_clause(const ASR::omp_clause_t *clause);

    void collect_kernel_arg_names(const ParallelLoopNest &nest,
            const std::set<SymbolTable*> &enclosing_block_scopes,
            std::vector<std::string> &arg_names);

    void collect_involved_syms(const ParallelLoopNest &nest,
            const std::set<SymbolTable*> &enclosing_block_scopes,
            std::map<std::string,
                std::pair<ASR::ttype_t*, ASR::expr_t*>> &involved_syms);

    ASR::stmt_t* wrap_assoc_scope_in_block(ASR::AssociateBlock_t *ab,
            Vec<ASR::stmt_t*> &resolved_stmts, SymbolTable *parent_scope);

    void migrate_inlined_assoc_symbols(ASR::AssociateBlock_t *ab,
            SymbolTable *parent_scope);

    bool hoist_struct_element_gathers(const ParallelLoopNest &nest,
            Vec<ASR::stmt_t*> &gather_stmts,
            Vec<ASR::stmt_t*> &scatter_stmts,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo,
            std::vector<std::string> &temp_names);

    bool host_nameable(ASR::symbol_t *sym);

    // The BLOCKs and ASSOCIATEs the kernel was given copies of, so that a
    // declined offload can drop them again, and so that only a copy this
    // pass made is moved into the kernel.
    //
    // The blocks are held as the symbols themselves rather than as their
    // names. A name says which entry of a scope a block is filed under
    // today, which is not the same question as which block it is: a scope
    // renames what it takes in, and one spelling can belong to two
    // different blocks in two different scopes.
    std::vector<ASR::symbol_t*> kernel_blocks;

    ASR::stmt_t* copy_loop_stmt(ASR::stmt_t *stmt,
            ASRUtils::ExprStmtDuplicator &dup);

    void retarget_local_extents(SymbolTable *scope);

    void remap_associate_to_own_scope(ASR::AssociateBlock_t *ab);

    void remap_block_to_own_scope(ASR::Block_t *block);

    void retarget_nested_calls_in(ASR::symbol_t *owner, SymbolTable *scope,
            ASR::stmt_t **body, size_t n_body);

    void retarget_block_calls_in(ASR::Block_t *block);

    void decline(const ASR::OMPRegion_t &x);

    void visit_OMPRegion(const ASR::OMPRegion_t &region);
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_VISITOR_H
