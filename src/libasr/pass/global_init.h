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
    //
    // An initializer nothing calls twice drops its guard under `--fast`, so
    // that the body is the initialization statements themselves. A module
    // whose `global_init_at_startup` is set is the exception: a target's
    // startup runs that one as well as the call chain, and the guard is what
    // makes those two into one initialization. The guard at the top of a
    // procedure or block body is a different thing — it is the save attribute
    // of an initialized local, so it decides behaviour rather than repeating a
    // call that cannot happen — and is kept in every mode.

    namespace ASRUtils {

        // Whether where in a target's startup an initializer runs can be told
        // apart from the order the call chain gives it.
        //
        // `OrderInsensitive` is an assignment or an association that reads a
        // constant or the address of a variable with `save`. No other
        // initializer can change either of those, so no program can observe
        // which of two such initializers ran first.
        //
        // `Ordered` is everything else, and a call above all: where in the
        // startup a given object file's hook runs is chosen by the linker and
        // cannot be predicted from the source, so a call from there can reach
        // a library whose own startup has not run yet.
        enum class InitOrdering {
            OrderInsensitive,
            Ordered,
        };

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

        // Append `stmt` inside the run-once guard of `fn`. `ordering` says
        // what `stmt` is, and a module's `global_init_at_startup` holds only
        // while everything put into its initializer is `OrderInsensitive`.
        void global_init_append_stmt(Allocator &al, ASR::Function_t *fn,
            ASR::stmt_t *stmt, InitOrdering ordering);

        // Prepend `stmts` inside the run-once guard of `fn`, after the
        // statement that marks the initializer as run. `ordering` is read as
        // for `global_init_append_stmt`.
        void global_init_prepend_stmts(Allocator &al, ASR::Function_t *fn,
            const std::vector<ASR::stmt_t*> &stmts, InitOrdering ordering);

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
