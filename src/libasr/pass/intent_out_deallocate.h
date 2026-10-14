#ifndef LIBASR_PASS_INTENT_OUT_DEALLOCATE_H
#define LIBASR_PASS_INTENT_OUT_DEALLOCATE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Select step 1 of finalization (F2018 7.5.6.2): an exact-rank FINAL,
    // otherwise an elemental FINAL. A nonelemental scalar never handles arrays.
    ASR::Function_t *select_final_procedure(ASR::Struct_t *st, int rank);

    void pass_intent_out_deallocate(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Give the dummy argument `result` of `fn`, which holds what used to be
    // the result of a function (see subroutine_from_function), the state a
    // function result starts out in: its default-initialized components are
    // defined and its allocatable components are unallocated. Unlike an
    // intent(out) dummy argument, it is not finalized on entry (F2018
    // 7.5.6.3 finalizes a function result after the statement that references
    // the function, not when the function is invoked). The statements are
    // put at the start of the body of `fn`.
    void initialize_function_result_on_entry(Allocator &al,
                                ASR::Function_t &fn, ASR::expr_t *result);

    // Append to `out` the statements that finalize `entity`, a nonpolymorphic
    // scalar of derived type (F2018 7.5.6.2): the final subroutines of its
    // type, then its finalizable components, as for an intent(out) dummy
    // argument on entry to a procedure.
    void finalize_entity(Allocator &al, ASR::expr_t *entity,
                         SymbolTable *scope, Vec<ASR::stmt_t*> &out);

    // A private procedure `name`, `subroutine name(object)` with
    // `type(T), intent(out) :: object` for the derived type `type`, in a new
    // scope under `parent` (the caller adds the symbol). Its body is what
    // this pass puts at the start of a procedure with such a dummy: the
    // finalization of the object, the deallocation of its allocatable
    // components and the part of its default initialization that follows
    // finalization; the code generator applies the rest of the default
    // initialization of an intent(out) dummy on entry. It resets an object
    // of exactly that type, which a code generator dispatches to for a
    // polymorphic intent(out) dummy of that dynamic type (see
    // PassOptions::dynamic_intent_out_reset).
    ASR::Function_t *make_intent_out_reset_procedure(Allocator &al,
                         ASR::Struct_t *type, SymbolTable *parent,
                         const std::string &name);

} // namespace LCompilers

#endif // LIBASR_PASS_INTENT_OUT_DEALLOCATE_H
