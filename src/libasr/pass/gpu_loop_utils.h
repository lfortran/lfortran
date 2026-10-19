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
 * gpu_index_set_split.cpp, gpu_unroll_jam.cpp): recognizing a loop nest,
 * copying statements while substituting variables, and inlining calls of
 * pure functions into a loop body.
 */

// Which of the transformations run (`--gpu-loop-transforms`).
enum GpuLoopTransform {
    GpuLoopFuse = 1,
};
bool gpu_loop_transform_enabled(const PassOptions &pass_options,
    GpuLoopTransform t);
// Whether the loop transformations run at all: for CUDA (and its CPU
// emulation), where they have been measured; other devices keep their
// loops as written.
bool gpu_loop_transforms_apply(const PassOptions &pass_options);

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
// innermost body.
ASR::stmt_t *gpu_loop_nest_with_body(Allocator &al, const GpuLoopNest &nest,
    const std::vector<ASR::expr_t*> &starts,
    const std::vector<ASR::expr_t*> &ends, Vec<ASR::stmt_t*> &body,
    const std::vector<ASR::symbol_t*> &extra_privates = {});

// The designator an array is named by (a variable or a chain of
// non-pointer components of one), as a key, or "".
std::string gpu_array_key(ASR::expr_t *e);

// Copies expressions and statements, substituting:
// * `symbol_map`: a variable for another one,
// * `value_map`: an expression for a scalar variable,
// * `array_map`: an array designator for an assumed-shape array dummy,
//   whose subscripts are shifted by the difference of the lower bounds,
// * `item_hook`: anything for an array element, when it returns non-null.
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
    std::function<ASR::expr_t*(ASR::ArrayItem_t*, GpuDuplicator&)> item_hook;
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
    int inlined = 0;

private:
    Allocator &al;
    std::function<bool(ASR::FunctionCall_t*)> select;
    int depth = 0;
    bool failed = false;
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

// Whether `e` reads no function result: duplicating it does not change
// what the program does.
bool gpu_expr_duplicable(ASR::expr_t *e);

// Every variable a statement list assigns (scalars, and arrays through
// their elements or as a whole), descending into nested statements and
// BLOCKs, keyed by gpu_array_key for arrays.
void gpu_collect_assigned(ASR::stmt_t **body, size_t n,
    std::set<ASR::symbol_t*> &scalars, std::set<std::string> &arrays,
    bool &unknown);

// Every array designator key an expression or statement list reads (as an
// element, a whole array or an actual argument).
void gpu_collect_array_reads(ASR::expr_t *e, std::set<std::string> &arrays);
void gpu_collect_array_reads(ASR::stmt_t **body, size_t n,
    std::set<std::string> &arrays);

// Whether a loop body only computes values of intrinsic scalar types:
// assignments of such scalars and array elements, IF, SELECT CASE, DO and
// BLOCK constructs with scalar locals, and calls of pure functions with
// scalar results. The loop transformations leave any other body alone,
// since the offload pipeline analyses some constructs (allocatable
// components, derived-type assignments) on the loop as written.
bool gpu_simple_body(ASR::stmt_t **body, size_t n);

// Every variable referenced.
void gpu_collect_symbols(ASR::expr_t *e, std::set<ASR::symbol_t*> &syms);
void gpu_collect_symbols(ASR::stmt_t **body, size_t n,
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
