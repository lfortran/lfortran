#ifndef LIBASR_PASS_GLOBAL_INIT_H
#define LIBASR_PASS_GLOBAL_INIT_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Startup ("global") initializers.
    //
    // A `Module`, a `Program` and the `TranslationUnit` itself can each name
    // one initializer function through their `global_init` member. The named
    // function takes no arguments, returns nothing, lives in the symbol table
    // of the owner that names it and runs exactly once before any user code
    // observes the state it sets up. Everything a backend has to know is that
    // link: no backend may recognise an initializer by its name.
    //
    // The function body is always one guarded block
    //
    //     if (.not. already_run) then
    //         already_run = .true.
    //         <dependency initializer calls>
    //         <initialization statements>
    //     end if
    //
    // so an initializer stays correct however many times it is called, which
    // is what lets a module initializer call the initializers of the modules
    // it uses instead of relying on link order or constructor priority.

    namespace ASRUtils {

        // Return the initializer `owner` names, creating it if `owner` has
        // none yet. `owner` is a `Module_t*`, a `Program_t*` or the
        // `TranslationUnit_t*`.
        ASR::Function_t* get_or_create_global_init(Allocator &al,
            ASR::TranslationUnit_t &unit, ASR::asr_t *owner);

        // Append `stmt` inside the run-once guard of `fn`.
        void global_init_append_stmt(Allocator &al, ASR::Function_t *fn,
            ASR::stmt_t *stmt);

        // Prepend `stmts` inside the run-once guard of `fn`, after the
        // statement that marks the initializer as run.
        void global_init_prepend_stmts(Allocator &al, ASR::Function_t *fn,
            const std::vector<ASR::stmt_t*> &stmts);

    } // namespace ASRUtils

    void pass_global_init(Allocator &al, ASR::TranslationUnit_t &unit,
                          const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GLOBAL_INIT_H
