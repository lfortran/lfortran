#ifndef LIBASR_PASS_GPU_LOOP_UTILS_H
#define LIBASR_PASS_GPU_LOOP_UTILS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>

#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
 * Shared pieces of the loop transformations that run on parallel loops
 * assigned to the device before they are offloaded (gpu_loop_fusion.cpp,
 * gpu_index_set_split.cpp): recognizing a loop nest, copying statements
 * while substituting variables, and inlining calls of pure functions into
 * a loop body.
 */

// Which of the transformations run (`--gpu-loop-transforms`).
enum GpuLoopTransform {
    GpuLoopFuse = 1,
    GpuLoopSwap = 2,
    GpuLoopSplit = 4,
    GpuLoopJam = 8,
};
bool gpu_loop_transform_enabled(const PassOptions &pass_options,
    GpuLoopTransform t);
// Whether `value` is a valid value of `--gpu-loop-transforms`: `all`,
// `none`, or a comma separated list of the names of the transformations.
// Otherwise `message` says why not.
bool gpu_loop_transforms_valid(const std::string &value,
    std::string &message);
// Whether the loop transformations run at all: for CUDA (and its CPU
// emulation), where they have been measured; other devices keep their
// loops as written.
bool gpu_loop_transforms_apply(const PassOptions &pass_options);

// The parallel loops a transformation keeps for an execution that rarely
// happens (the original loops of a fusion, which run in the last iteration
// of the DO loops around them only): the transformations after it leave
// them as written, so that the offload pipeline still recognizes, for
// example, that they assign an array whole and need not copy it to the
// device first. One run of the transformations (pass_gpu_loop_transforms)
// owns the set and hands it from the fusion to the splitting.
typedef std::set<ASR::stmt_t*> GpuRarelyRun;

// The size in bytes of the device's last-level cache, as an integer(8)
// expression: the --gpu-cache-bytes option, or a call of the runtime's
// lfortran_gpu_cache_bytes() (declared in `unit_scope` if needed).
ASR::expr_t *gpu_cache_bytes(Allocator &al, SymbolTable *unit_scope,
    const PassOptions &pass_options, const Location &loc);

// A `do concurrent` nest (or an offloaded `!$omp parallel do`) that the
// dispatch assigned to the device: a ParallelDo OMPRegion whose body is
// `collapse` perfectly nested DO loops of unit step.
struct GpuLoopNest {
    ASR::OMPRegion_t *region = nullptr;
    std::vector<ASR::DoLoop_t*> loops;          // outermost first
    std::vector<ASR::symbol_t*> vars;
    std::vector<ASR::expr_t*> starts, ends;
    std::vector<ASR::symbol_t*> privates;
    ASR::DoLoop_t *innermost() const { return loops.back(); }
    size_t rank() const { return loops.size(); }
    // The position of a loop variable in the nest, or -1.
    int position(ASR::symbol_t *v) const;
};

// Whether `s` is such a nest; only the clauses OMPIndependent,
// OMPCollapse and OMPPrivate are accepted.
bool gpu_loop_nest(ASR::stmt_t *s, GpuLoopNest &nest);

// A new nest with the loop heads and clauses of `nest` and `body` as its
// innermost body, with `extra_privates` added to its private variables.
ASR::stmt_t *gpu_loop_nest_with_body(Allocator &al, const GpuLoopNest &nest,
    const std::vector<ASR::expr_t*> &starts,
    const std::vector<ASR::expr_t*> &ends, Vec<ASR::stmt_t*> &body,
    const std::vector<ASR::symbol_t*> &extra_privates = {});

// A parallel loop nest assigned to the device that copies every element of
// the array `src` into the array `dest` of the same bounds, with index
// variables of its own in `scope`.
ASR::stmt_t *gpu_device_copy_nest(Allocator &al, const Location &loc,
    SymbolTable *scope, ASR::expr_t *dest, ASR::expr_t *src);

// The designator an array is named by (a variable or a chain of
// non-pointer components of one), as a key, or "".
std::string gpu_array_key(ASR::expr_t *e);

// Copies expressions and statements, substituting:
// * `symbol_map`: a variable for another one,
// * `value_map`: an expression for a scalar variable,
// * `array_map`: an array designator for an assumed-shape array dummy,
//   whose subscripts are shifted by the difference of the lower bounds.
// A BLOCK the copied statements run is copied too, with variables of its
// own, in `scope`. Anything it cannot copy faithfully clears `ok`.
class GpuDuplicator : public ASR::BaseExprStmtDuplicator<GpuDuplicator> {
public:
    struct ArrayActual {
        ASR::expr_t *actual = nullptr;
        // Added to each subscript: the lower bound of the actual minus
        // that of the dummy.
        std::vector<ASR::expr_t*> shift;
        // The lower bound of each dimension of the dummy.
        std::vector<ASR::expr_t*> lower;
    };
    std::map<ASR::symbol_t*, ASR::symbol_t*> symbol_map;
    std::map<ASR::symbol_t*, ASR::expr_t*> value_map;
    std::map<ASR::symbol_t*, ArrayActual> array_map;
    SymbolTable *scope;
    bool ok = true;
    std::vector<std::pair<SymbolTable*, std::string>> *added = nullptr;

    GpuDuplicator(Allocator &al, SymbolTable *scope_)
        : BaseExprStmtDuplicator(al), scope(scope_) {}

    ASR::asr_t* duplicate_Var(ASR::Var_t *x);
    ASR::asr_t* duplicate_ArrayItem(ASR::ArrayItem_t *x);
    ASR::asr_t* duplicate_ArraySize(ASR::ArraySize_t *x);
    ASR::asr_t* duplicate_ArrayBound(ASR::ArrayBound_t *x);
    ASR::asr_t* duplicate_FunctionCall(ASR::FunctionCall_t *x);
    ASR::asr_t* duplicate_BlockCall(ASR::BlockCall_t *x);
    // The duplicate of the subscripts of an element, shifted for an array
    // dummy.
    ASR::array_index_t* duplicate_indices(ASR::ArrayItem_t *x,
        const ArrayActual *map);
    // Copies a whole list of statements.
    void duplicate_body(ASR::stmt_t **body, size_t n, Vec<ASR::stmt_t*> &out);
};

// Inlines calls of pure functions that return a scalar into the statements
// of a loop body. A statement with such calls becomes a BLOCK whose
// variables hold the results and the locals of the inlined bodies, so they
// are private to an iteration. `select` picks the calls; a picked call that
// cannot be inlined (see gpu_function_inlinable) makes `run` fail and leave
// the scope as it was.
class GpuInliner {
public:
    GpuInliner(Allocator &al_,
        std::function<bool(ASR::FunctionCall_t*)> select_)
        : al(al_), select(select_) {}
    // Inlines in place; false when a picked call cannot be inlined.
    bool run(SymbolTable *scope, ASR::stmt_t **&body, size_t &n_body);
    // Removes every symbol `run` added (after a failure).
    void rollback();
    std::vector<std::pair<SymbolTable*, std::string>> added;

private:
    Allocator &al;
    std::function<bool(ASR::FunctionCall_t*)> select;
    int depth = 0;
    bool inline_stmts(SymbolTable *scope, ASR::stmt_t **&body,
        size_t &n_body);
    bool inline_stmt(SymbolTable *scope, ASR::stmt_t *&s);
    ASR::expr_t *inline_call(ASR::FunctionCall_t *call, SymbolTable *block,
        Vec<ASR::stmt_t*> &pre);
    friend class GpuCallReplacer;
};

// Whether calls of `fn` can be inlined: a pure function with a body and a
// scalar result, scalar dummies and assumed-shape array dummies, scalar
// locals without initialization, and no RETURN, STOP, input/output or
// subroutine call.
bool gpu_function_inlinable(ASR::Function_t *fn);

// Whether a function names no variable but its own dummy arguments and
// locals (named constants aside) and calls only procedures that do the
// same: a call of it then reads and writes nothing but what its actual
// arguments name, which the analyses of a loop body see.
bool gpu_function_self_contained(ASR::Function_t *fn);
// Whether every procedure an expression or a statement list (with the
// BLOCKs it runs) calls is self-contained.
bool gpu_calls_self_contained(ASR::expr_t *e);
bool gpu_calls_self_contained(ASR::stmt_t **body, size_t n);

// Whether `e` reads no function result: duplicating it does not change
// what the program does.
bool gpu_expr_duplicable(ASR::expr_t *e);

// What a statement list may define, as computed by gpu_collect_effects.
struct GpuEffects {
    // The variables defined as a whole (a scalar, a whole array, a
    // derived-type object), and the variable a whole array or derived-type
    // component that is defined belongs to (an allocation in it may
    // change).
    std::set<ASR::symbol_t*> scalars;
    // The gpu_array_key of every array or derived-type object defined in
    // part or as a whole.
    std::set<std::string> arrays;
    // The variable each defined designator starts from (`x` for `x`,
    // `x(k)`, `x%c` or `x%a(k)%c`): an expression reading that variable may
    // evaluate differently afterwards.
    std::set<ASR::symbol_t*> bases;
    // How many times each variable is defined as a whole, a DO variable
    // counting twice.
    std::map<ASR::symbol_t*, int> definitions;
    // Whether the statements may define something else: the sets above
    // are then incomplete.
    bool unknown = false;
};

// Adds to `effects` what a statement list may define, descending into the
// statements nested in it and the BLOCKs it runs. Every statement and every
// expression is classified by its kind in one place: assignments, DO
// variables, the actual arguments of procedures whose dummies are not
// INTENT(IN), internal files written and the variables of implied DO loops
// outside input/output are definitions. A pure procedure (or a
// self-contained one without input/output) is modeled by its dummies; the
// control constructs and output statements without specifiers that define
// anything define nothing themselves. Any other kind (a call of another
// procedure, input, an allocation, a pointer association, IOSTAT=, an
// implied DO in input/output, a namelist, ...) sets `unknown`.
void gpu_collect_effects(ASR::stmt_t **body, size_t n, GpuEffects &effects);

// Every array designator key an expression or statement list reads (as an
// element, a whole array or an actual argument), and the key of every
// derived-type object it reads as a whole (an actual argument, the value
// of an assignment), which reads all its components.
void gpu_collect_array_reads(ASR::expr_t *e, std::set<std::string> &arrays);
void gpu_collect_array_reads(ASR::stmt_t **body, size_t n,
    std::set<std::string> &arrays);

// Whether a set of keys from the collections above touches the array of
// `key`: it holds `key`, or the key of a derived-type object the array is
// a component of (`x` or `x%c` for `x%c%a`), which is read or assigned as
// a whole. gpu_key_touched_whole only checks the second.
bool gpu_key_touched(const std::set<std::string> &keys,
    const std::string &key);
bool gpu_key_touched_whole(const std::set<std::string> &keys,
    const std::string &key);

// Whether a loop body only computes values of intrinsic scalar types:
// assignments of such scalars and array elements, IF, SELECT CASE, DO and
// BLOCK constructs with scalar locals, and calls of pure functions with
// scalar results. The loop transformations leave any other body alone,
// since the offload pipeline analyses some constructs (allocatable
// components, derived-type assignments) on the loop as written.
bool gpu_simple_body(ASR::stmt_t **body, size_t n);

// Replaces, in every expression of a statement list and of the BLOCKs it
// runs, each subexpression for which `f` returns an expression.
void gpu_replace_exprs(ASR::stmt_t **body, size_t n,
    const std::function<ASR::expr_t*(ASR::expr_t*)> &f);

// Every BLOCK a statement list runs, at any depth.
void gpu_collect_blocks(ASR::stmt_t **body, size_t n,
    std::set<ASR::symbol_t*> &blocks);

// Every variable referenced.
void gpu_collect_symbols(ASR::expr_t *e, std::set<ASR::symbol_t*> &syms);
void gpu_collect_symbols(ASR::stmt_t **body, size_t n,
    std::set<ASR::symbol_t*> &syms);
// Every variable whose value an expression reads: as gpu_collect_symbols,
// but without the arrays of which it takes only bounds or the size
// (`size(x)`, `ubound(x%a, 1)`), which writes to their elements do not
// change.
void gpu_collect_value_symbols(ASR::expr_t *e,
    std::set<ASR::symbol_t*> &syms);

// The number of arithmetic operations and loads of an expression,
// counting a call of a function that can be inlined by its body, the
// heaviest branch of an IF.
int64_t gpu_expr_cost(ASR::expr_t *e, int depth = 0);
int64_t gpu_stmts_cost(ASR::stmt_t **body, size_t n, int depth = 0);

ASR::expr_t *gpu_i32(Allocator &al, const Location &loc, int64_t v);
ASR::expr_t *gpu_int_add(Allocator &al, ASR::expr_t *a, ASR::expr_t *b);
ASR::expr_t *gpu_int_sub(Allocator &al, ASR::expr_t *a, ASR::expr_t *b);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_LOOP_UTILS_H
