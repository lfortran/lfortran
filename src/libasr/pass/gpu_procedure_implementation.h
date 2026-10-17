#ifndef LIBASR_PASS_GPU_PROCEDURE_IMPLEMENTATION_H
#define LIBASR_PASS_GPU_PROCEDURE_IMPLEMENTATION_H

#include <map>
#include <set>

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

/*
The body of a separate module procedure, which device code runs.

A call reaches a `module procedure` implemented in a submodule through the
interface its module declares, which has no body. The body is in a submodule
of this translation unit, or, with separate compilation, in the `.smod` file
the submodule was compiled to, which is read when it is first needed. Whatever
asks a question of what device code runs -- the unsupported-construct check
that decides whether a loop goes to the device, and the offload pipeline that
builds the kernel -- asks it of that body, so that the two cannot disagree
about a procedure.
*/
class GpuProcedureImplementations {
public:
    GpuProcedureImplementations(Allocator &al, ASR::TranslationUnit_t &tu,
        const PassOptions &pass_options)
        : al(al), tu(tu), pass_options(pass_options) {}

    // The implementation of `fn`: `fn` itself when it has a body, or when
    // no body can be found. The same Function_t every time it is asked:
    // a submodule read from disk is a fresh copy every time it is read.
    ASR::Function_t* implementation(ASR::Function_t *fn);

    // Whether `fn` was read from a module file: its locations are in a file
    // this compilation does not have.
    bool read_from_module_file(ASR::Function_t *fn) const {
        return read_from_disk.count(fn) > 0;
    }

    // Loads the modules a submodule read from disk uses into this
    // translation unit, so that its external symbols resolve.
    void load_submodule_deps(ASR::TranslationUnit_t &sub_tu);

    // A submodule read from disk still has the array constructors the
    // shared lowering, which ran over this translation unit, removed.
    void lower_loaded_implied_do_loops(ASR::TranslationUnit_t &sub_tu);

private:
    Allocator &al;
    ASR::TranslationUnit_t &tu;
    PassOptions pass_options;
    std::map<ASR::Function_t*, ASR::Function_t*> implementations;
    std::set<ASR::Function_t*> read_from_disk;

    ASR::Function_t* find(ASR::Function_t *fn);
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_PROCEDURE_IMPLEMENTATION_H
