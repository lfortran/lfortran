#ifndef LCOMPILERS_INIT_H
#define LCOMPILERS_INIT_H

/*
 * The startup initialization engine: one mechanism for every definition that
 * has to be initialized before user code observes it.
 *
 * A definition's initializer is an ordinary compiled function guarded by a
 * state word:
 *
 *     if (_lcompilers_init_begin(&state)) {
 *         <initializers of what it depends on>
 *         <storage set up and initialization>
 *         _lcompilers_init_end(&state);
 *     }
 *
 * An initializer's body and a teardown do not call the loader (dlopen,
 * dlclose, dlsym, dladdr, LoadLibrary, FreeLibrary, ...): they run holding
 * the engine's lock, which a constructor that runs under the loader's lock
 * can be waiting for. A runtime prerequisite that has to call it goes into
 * a bootstrap, which runs outside every initializer; see
 * lcompilers_init_abi.h.
 *
 * `_lcompilers_init_begin` returns 0 once the state is ready. Otherwise it
 * takes the engine's lock, which is recursive, marks the state initializing
 * and returns 1; a thread that finds a state another thread is initializing
 * waits for it, and a state already initializing on the calling thread is a
 * cycle, which is reported and ends the process. `_lcompilers_init_end` marks
 * the state ready and releases the lock.
 *
 * The engine discovers the records of every loaded image (see
 * lcompilers_init_abi.h) plus those added by `_lcompilers_init_add_records`,
 * and a dispatch calls their initializers in `stable_id` order: the local
 * phase every record that is not collective, and the collective phase, only
 * at a collective boundary, the collective ones. A dispatch is entered from
 * an object file's constructor, from a foreign entry point, from a Fortran
 * main program, from `lcompilers_initialize` or after a batch of records was
 * added.
 */

#include "lcompilers_init_abi.h"
/* LFORTRAN_API; that header includes this one at its end. */
#ifndef LFORTRAN_API
#include "lfortran_intrinsics.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

LFORTRAN_API int32_t _lcompilers_init_begin(int32_t *state);
LFORTRAN_API void _lcompilers_init_end(int32_t *state);
/* Ends the process unless the collective phase is running on this thread.
 * An initializer that performs collective work calls it first. */
LFORTRAN_API void _lcompilers_init_require_collective(void);

/* The constructor of an object file, passed its table: the image was
 * loaded, so a table at that address that was unloaded before is live again,
 * and it is discovered from now on until the object file's destructor
 * passes it to `_lcompilers_init_unload`, whether or not the loader lists it.
 * It may decline to dispatch where the loader cannot safely run initializers
 * (a DLL's constructor on Windows); a later entry then does the work.
 *
 * On Windows a DLL's records are discovered only once its own constructor
 * has run (a DLL still being loaded, or whose load fails, is not taken in),
 * so a dispatch sees the records of the executable and of every DLL whose
 * attach has run, not those of a DLL that is still loading. What a foreign
 * entry point of such a DLL needs, it runs itself, through its explicit
 * calls of the initializers; the host's lcompilers_initialize() runs
 * everything attached by then. */
LFORTRAN_API void _lcompilers_init_ctor(const lcompilers_init_table *table);
/* `lcompilers_init_dispatch_local` or `lcompilers_init_dispatch_collective`. */
LFORTRAN_API void _lcompilers_init_dispatch(int32_t phase);
/* The local dispatch of a foreign entry point. `entered` is a word of the
 * entry point's own, zero-initialized, writable data, zero again in every
 * new mapping of its image, which the engine alone uses: once a dispatch
 * through it has completed, a call returns after two atomic loads for as
 * long as no image with records is taken in or goes away and no host batch
 * is added or removed. An entry point of an image still being loaded, called
 * from one of the image's own constructors, thus dispatches in full, which
 * finds the records of its image (on Windows, those of the DLLs attached
 * already; see `_lcompilers_init_ctor`). */
LFORTRAN_API void _lcompilers_init_enter(uint64_t *entered);
/* The collective boundary a host calls, on every image, before user code
 * runs; equivalent to the one a Fortran main program enters. */
LFORTRAN_API void lcompilers_initialize(void);

/* Records a loader cannot discover, such as those of JIT code. A batch is
 * identified by its address and has to be removed before its code or data
 * is unmapped. Removing it waits for the initializers of that batch other
 * threads are running, and for no others. */
LFORTRAN_API void _lcompilers_init_add_records(
    const lcompilers_init_table *table);
LFORTRAN_API void _lcompilers_init_remove_records(
    const lcompilers_init_table *table);

/* The destructor of an object file: an image that is unloaded tears down
 * what its records' owners hold, if that has not happened yet, and is
 * forgotten, before its code and data are unmapped. It waits for the
 * initializers of that image other threads are running, and for no others;
 * the loader may list the image for a while after, and its records are not
 * used again unless it is loaded again. */
LFORTRAN_API void _lcompilers_init_unload(const lcompilers_init_table *table);

/* Runs the teardown of every ready record, in the reverse of the order they
 * became ready. */
LFORTRAN_API void _lcompilers_init_teardown_all(void);

#ifdef __cplusplus
}
#endif

#endif /* LCOMPILERS_INIT_H */
