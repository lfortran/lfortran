#ifndef LIBASR_PASS_GLOBAL_INIT_H
#define LIBASR_PASS_GLOBAL_INIT_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <string>
#include <vector>

namespace LCompilers {

    // Startup ("global") initialization.
    //
    // A `Module`, a `Program` and the `TranslationUnit` can each own one
    // initializer: the function their `global_init` points to, which takes
    // no arguments and lives in the owner's own symbol table, and the state
    // word their `global_init_state` points to, a saved `integer(4)` variable
    // of the same symbol table. That link is the only way anything recognises
    // an initializer or its state; nothing may go by their names.
    //
    // Every user module owns an initializer from the moment semantics creates
    // it, whatever its initialization later lowers to, so that its `.mod`
    // file carries it and every translation unit that depends on the module
    // calls that one definition. A program, the translation unit and a module
    // a pass creates get theirs on demand.
    //
    // While the passes lower initialization, an initializer's body is a plain
    // list of statements. `global_init_wire` then gives each initializer
    // defined in this translation unit its final shape:
    //
    //     if (_lcompilers_init_begin(state) /= 0) then
    //         call <initializer of each dependency, by stable id>
    //         GlobalInitStorage(<storage set up at run time>)
    //         <the statements>
    //         call _lcompilers_init_end(state)
    //     end if
    //
    // Statements that do collective work, the allocation of saved coarrays,
    // are preceded by `call _lcompilers_init_require_collective()`, put there
    // by the pass that adds them. An owner that is collective only because
    // something it depends on is has no such call, so once its dependencies
    // are ready it initializes like any other.
    //
    // The state is ready only once the whole body has run. The runtime engine
    // (runtime/lcompilers_init.h) serializes initialization, reports a cycle,
    // and runs the initializer of every module and translation unit of every
    // loaded image, discovered from the records the code generators emit, in
    // stable id order before user code runs; `GlobalInitDispatch`, the first
    // statement of a program, is where generated code enters it. A host
    // without a Fortran main program enters it through
    // `lfortran_initialize` instead. A program's own initializer is frame
    // local and is called by the program itself, after the dispatch.

    namespace ASRUtils {

        // One initializer of a module or the translation unit that this
        // translation unit defines: a root the engine runs.
        struct GlobalInitRoot {
            std::string stable_id;
            ASR::Function_t *ensure;
            // A `Module_t*` or the `TranslationUnit_t*`.
            ASR::asr_t *owner;
            ASR::Variable_t *state;
            // Only a collective startup boundary may run it.
            bool collective;
            // The translation unit's collective bootstrap, which has no
            // state and no guard: run at the collective boundary before
            // every collective root, outside every guard.
            bool bootstrap = false;
        };

        // The procedure `unit.m_global_init_bootstrap` points to, or nullptr.
        ASR::Function_t* get_global_init_bootstrap(ASR::TranslationUnit_t &unit);
        // Whether `fn` is the collective bootstrap of its translation unit.
        bool is_global_init_bootstrap(const ASR::Function_t *fn);

        // The initializer and the state of `owner`, or nullptr. `owner`
        // is a `Module_t*`, a `Program_t*` or a `TranslationUnit_t*`.
        ASR::Function_t* get_global_init(ASR::asr_t *owner);
        ASR::Variable_t* get_global_init_state(ASR::asr_t *owner);

        // The owner whose `global_init` is `fn`, or nullptr when `fn` is
        // not an initializer.
        ASR::asr_t* global_init_owner(const ASR::Function_t *fn);
        // The symbol table of that owner, or nullptr.
        SymbolTable* global_init_owner_scope(const ASR::Function_t *fn);
        bool is_owner_global_init(const ASR::Function_t *fn);

        // Whether this translation unit holds the definition of initializer
        // `fn`, rather than a declaration of the one in another object file.
        bool global_init_defined_here(const ASR::Function_t *fn);

        // Whether `owner` needs a collective startup boundary: it allocates a
        // saved coarray, or something it depends on does.
        bool global_init_is_collective(ASR::asr_t *owner);

        // The id that orders `owner` among the roots, the same on every
        // image and in every compilation: `m:<module>`, `s:<ancestor>:<name>`
        // for a submodule, `t:<initializer>` for the translation unit.
        std::string global_init_stable_id(ASR::asr_t *owner);

        // The roots of `unit`, by stable id.
        std::vector<GlobalInitRoot> global_init_roots(
            ASR::TranslationUnit_t &unit);

        // A call to initializer `ensure` written in `scope`, through an
        // import when it belongs to another scope.
        ASR::stmt_t* make_global_init_call(Allocator &al, SymbolTable *scope,
            ASR::Function_t *ensure, const Location &loc);

        // Whether module `m` has no initializer of its own, so that the
        // translation unit's sets up its storage, and its teardown frees it:
        // a COMMON block above all, which every translation unit that uses
        // it defines.
        bool global_init_borrows_storage(const ASR::Module_t &m);

        // The scopes whose variables `owner`'s initializer sets up and its
        // teardown frees: its own, and for the translation unit also those
        // of the modules whose storage it borrows.
        std::vector<SymbolTable*> global_init_storage_scopes(ASR::asr_t *owner);

        // Whether `v`, a variable of a module or of the translation unit,
        // has storage a backend creates at run time, so that the owner's
        // `GlobalInitStorage` names it.
        bool needs_runtime_storage_setup(const ASR::Variable_t &v);

        // Where an object file puts its initialization table; see
        // runtime/lcompilers_init_abi.h.
        enum class InitObjectFormat { ELF, MachO, COFF, Wasm };
        InitObjectFormat init_object_format(Platform platform);

        // The runtime interface of an initializer's guard.
        enum class InitRuntimeFn { Begin, End, RequireCollective };
        ASR::symbol_t* get_init_runtime_function(Allocator &al,
            ASR::TranslationUnit_t &unit, InitRuntimeFn kind);
        bool is_init_runtime_function(const ASR::symbol_t *sym);

        // Give the new module `m` its initializer and its state. Called by
        // semantics for every user module and submodule.
        ASR::Function_t* create_module_global_init(Allocator &al,
            ASR::Module_t *m);

        // The initializer of `owner`, creating it (and its state) if
        // `owner` has none yet, named `name` when that is given. The name
        // of the translation unit's initializer is its stable id, so a pass
        // that creates one derives the name from what it initializes.
        ASR::Function_t* get_or_create_global_init(Allocator &al,
            ASR::TranslationUnit_t &unit, ASR::asr_t *owner);
        ASR::Function_t* get_or_create_global_init(Allocator &al,
            ASR::TranslationUnit_t &unit, ASR::asr_t *owner,
            const std::string &name);

        // Append `stmt` to the statements of initializer `fn`.
        void global_init_append_stmt(Allocator &al, ASR::Function_t *fn,
            ASR::stmt_t *stmt);
        // Prepend `stmts` to the statements of initializer `fn`.
        void global_init_prepend_stmts(Allocator &al, ASR::Function_t *fn,
            const std::vector<ASR::stmt_t*> &stmts);

        // Compute `global_init_collective` of every module `unit` defines: a
        // module that declares a saved coarray anywhere in its scope, and one
        // whose parent or a module it uses is collective. Semantics calls it
        // before `.mod` files are written, so a module read back from one
        // carries its own. The translation unit's own flag is set by the pass
        // that gives it collective work.
        void update_global_init_collective(ASR::TranslationUnit_t &unit,
            bool coarrays);

        // For a target that sees the whole program: replace each
        // `GlobalInitDispatch` by calls to every root, the local ones first,
        // then the collective bootstraps and the collective ones, and each
        // guard by plain code on its state, so that nothing of the runtime
        // engine is left.
        void expand_closed_world_dispatch(Allocator &al,
            ASR::TranslationUnit_t &unit);

    } // namespace ASRUtils

    // Lower what static data does not hold into statements of the initializer
    // of the unit that owns it (for a procedure or a block, at the top of its
    // own body).
    void pass_global_init(Allocator &al, ASR::TranslationUnit_t &unit,
                          const PassOptions &pass_options);

    // Give every initializer its final guarded shape and start the program
    // with the dispatch. It runs after every pass that can put statements
    // into an initializer, `coarray` among them.
    void pass_global_init_wire(Allocator &al, ASR::TranslationUnit_t &unit,
                               const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GLOBAL_INIT_H
