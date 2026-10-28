#ifndef LIBASR_PASS_EXPLICIT_SHAPE_EXTENT_H
#define LIBASR_PASS_EXPLICIT_SHAPE_EXTENT_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <map>

namespace LCompilers {

    void pass_explicit_shape_extent(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Replaces each reference to a local initialized on entry in the
    // procedure or BLOCK `owner`, the procedures it contains and its BLOCKs
    // by the expression the local is initialized with, and removes the
    // local. When `owner` is a copy of `original` whose statements may still
    // name the locals of `original`, those references are replaced too.
    // Device code sizes its explicit-shape objects from their declared
    // bounds, so the GPU offload passes give this to every procedure and
    // BLOCK they copy into device code.
    void inline_entry_initialized_locals(Allocator &al, ASR::symbol_t* owner,
                                ASR::symbol_t* original);

    // Adds to `substitutions`, which replaces the dummies of `fn` by the
    // actual arguments of a call to it, each local initialized on entry of
    // `fn` (of the procedures it contains and its BLOCKs) with its
    // initializer written over those actual arguments. The GPU offload
    // passes use it to inline `fn` into device code.
    void substitute_entry_initialized_locals(Allocator &al, ASR::Function_t &fn,
                                std::map<ASR::symbol_t*, ASR::expr_t*> &substitutions);

} // namespace LCompilers

#endif // LIBASR_PASS_EXPLICIT_SHAPE_EXTENT_H
