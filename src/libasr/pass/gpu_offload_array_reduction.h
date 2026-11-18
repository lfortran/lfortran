#ifndef LIBASR_PASS_GPU_OFFLOAD_ARRAY_REDUCTION_H
#define LIBASR_PASS_GPU_OFFLOAD_ARRAY_REDUCTION_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

#include <functional>
#include <map>
#include <set>
#include <vector>

namespace LCompilers {

/*
A REDUCE of an array on the device (gpu_offload.cpp).

A loop with `reduce(+: s)` where `s` is an array, whose iterations each add
to elements of `s` that every iteration names the same -- `s(1)`, `s(k)` for
a `k` the loop does not change -- has one accumulator per such element, as a
reduction of a scalar has one: each thread accumulates into private scalars,
which the launch combines on the device (GpuReduceCombine). An element an
iteration chooses (`s(i)`, a histogram) is not one: two iterations would add
to the same element at once, which needs atomic updates, so such a loop is
declined, as is any other reference to the array (the whole array, a
section, an inquiry, an actual argument).
*/

// Whether every reference the statements make to `array` is an element of
// it whose subscripts are the same in every iteration: integer expressions
// of constants and of scalars the statements neither assign nor pass to a
// procedure, which are not `indices` (the loop's) nor locals of a BLOCK of
// the statements.
bool gpu_array_reduction_elements_invariant(ASR::symbol_t *array,
    ASR::stmt_t **body, size_t n_body,
    const std::set<ASR::symbol_t*> &indices);

// Replaces every element of `array` in the statements, BLOCKs included,
// with the accumulator of the first element `elements` holds with the same
// subscripts. An element with subscripts not seen before is appended to
// `elements` (a copy), and `new_accumulator` is asked for its accumulator,
// a Var, given its index. What the statements still name of `array` after
// it is nothing, if gpu_array_reduction_elements_invariant held of them.
void gpu_replace_array_reduction_elements(Allocator &al,
    ASR::symbol_t *array, ASR::stmt_t **body, size_t n_body,
    std::vector<ASR::ArrayItem_t*> &elements,
    std::vector<ASR::expr_t*> &accumulators,
    const std::function<ASR::expr_t*(size_t)> &new_accumulator);

// Whether combining values of `type` with `op` gives the same result
// whatever the order and the grouping: a sum of integers (which wraps the
// same way in any order), and the largest or the smallest of integers or of
// reals. Such a reduction of an element is combined into the element with
// atomic updates, which leave `a(k)` before the loop combined with every
// contribution, as Fortran says, and need nothing else.
inline bool gpu_reduction_order_free(ASR::reduction_opType op,
        ASR::ttype_t *type) {
    switch (op) {
        case ASR::reduction_opType::ReduceAdd:
        case ASR::reduction_opType::ReduceSub:
            return ASRUtils::is_integer(*type);
        case ASR::reduction_opType::ReduceMIN:
        case ASR::reduction_opType::ReduceMAX:
            return ASRUtils::is_integer(*type) || ASRUtils::is_real(*type);
        default:
            return false;
    }
}

// Whether a GpuReduceCombine combines `item` into its target with atomic
// updates: an order-free operation, with no initial value to start from
// (which would have to be stored before the first update).
inline bool gpu_reduce_item_atomic(const ASR::gpu_reduce_item_t &item) {
    return item.m_initial == nullptr && gpu_reduction_order_free(item.m_op,
        ASRUtils::expr_type(item.m_target));
}

// An element of an array of a REDUCE that the statement right before the
// loop assigns a constant: `s(1) = 0` before a loop that adds to `s(1)`.
// The host would have to wait for the launches pending on `s` to assign
// it, so the combination of the launch starts from the constant instead
// (GpuReduceCombine, gpu_reduce_item.initial), and the host assigns it
// only when the loop has no iterations and no launch is made.
struct GpuReductionInitializer {
    // The assignment, which the host no longer runs before the loop.
    ASR::stmt_t *assignment;
    ASR::symbol_t *array;
    ASR::ArrayItem_t *element;
    // The constant, of the element's type.
    ASR::expr_t *value;
};

// Takes out of their lists, for every parallel loop the device runs, the
// assignments of constants to elements of its arrays of a REDUCE of `+`,
// `-`, `*`, `max` or `min` that come right before it, one per element,
// whose subscripts are constants that the loop names an element at, and
// records them for the loop. An element combined with atomic updates
// (gpu_reduction_order_free) keeps its assignment: the updates start from
// what it holds.
void gpu_take_reduction_initializers(ASR::TranslationUnit_t &unit,
    std::map<const ASR::OMPRegion_t*, std::vector<GpuReductionInitializer>>
        &initializers);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_ARRAY_REDUCTION_H
