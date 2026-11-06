#ifndef LIBASR_PASS_UPDATE_ARRAY_DIM_INTRINSIC_CALLS_H
#define LIBASR_PASS_UPDATE_ARRAY_DIM_INTRINSIC_CALLS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <set>

namespace LCompilers {

    void pass_update_array_dim_intrinsic_calls(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Replaces, in the declaration of `sym` (a variable, or a procedure and
    // the declarations in it), the inquiries LBOUND, UBOUND and SIZE about
    // one of `arrays` by the bounds and extents the array is declared with.
    // Each of `arrays` is declared with explicit bounds.
    void replace_declared_array_inquiries(Allocator &al, ASR::symbol_t *sym,
                                const std::set<ASR::symbol_t*> &arrays);

    // The same in `type`
    void replace_declared_array_inquiries(Allocator &al, ASR::ttype_t *type,
                                const std::set<ASR::symbol_t*> &arrays);

} // namespace LCompilers

#endif // LIBASR_PASS_UPDATE_ARRAY_DIM_INTRINSIC_CALLS_H
