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
    //         <initialization statements>
    //     end if
    //
    // so an initializer stays correct however many times it is called. The
    // program calls each of them once, in `determine_module_dependencies`
    // order, so neither link order nor a target's constructor priority can
    // change when they run; the guard is what keeps the translation unit's
    // own initializer correct, which a target startup hook calls with no
    // ordering at all.

    namespace ASRUtils {

        // Return the initializer `owner` names, creating it if `owner` has
        // none yet. `owner` is a `Module_t*`, a `Program_t*` or the
        // `TranslationUnit_t*`.
        //
        // `defined_elsewhere` names it without defining it, for a module this
        // translation unit only uses: the object file the module was compiled
        // into holds the one definition, and defining a second one here would
        // clash with it at link time.
        ASR::Function_t* get_or_create_global_init(Allocator &al,
            ASR::TranslationUnit_t &unit, ASR::asr_t *owner,
            bool defined_elsewhere = false);

        // Append `stmt` inside the run-once guard of `fn`.
        void global_init_append_stmt(Allocator &al, ASR::Function_t *fn,
            ASR::stmt_t *stmt);

        // Prepend `stmts` inside the run-once guard of `fn`, after the
        // statement that marks the initializer as run.
        void global_init_prepend_stmts(Allocator &al, ASR::Function_t *fn,
            const std::vector<ASR::stmt_t*> &stmts);

    } // namespace ASRUtils

    // Lower the declaration initializers no target can lay out as static
    // data into the initializers of the units that own them.
    void pass_global_init(Allocator &al, ASR::TranslationUnit_t &unit,
                          const PassOptions &pass_options);

    // Connect the initializers: a program's calls every module initializer it
    // can observe, in dependency order, and then runs before the program's
    // first statement. It is a pass of its own because it has to run after
    // every pass that can create an initializer, `coarray` among them.
    void pass_global_init_wire(Allocator &al, ASR::TranslationUnit_t &unit,
                               const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GLOBAL_INIT_H
