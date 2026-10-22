#ifndef LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H
#define LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H

#include <vector>

#include <libasr/asr.h>
#include <libasr/containers.h>

namespace LCompilers {

/*
The procedures a kernel calls are copied into the kernel's scope. The copy of
an internal procedure names the variables of its host -- the procedure the
loop is in -- and those names now resolve to the kernel's own variables: its
arguments, and the named constants it copies. Device code has no host
association, so each such procedure is given what it names of the kernel
instead:

* A named constant becomes a named constant of the procedure.
* A variable becomes a dummy argument of the procedure, which every call
  hands the variable, as the caller names it. A procedure that calls one
  that is handed a variable is handed it too, to pass it on, and so is the
  variable a shape of such a dummy names.

Nothing in device code then reaches into an enclosing procedure, so the
shared pass for nested procedures leaves it alone.
*/
void gpu_lift_host_association(Allocator &al, ASR::Function_t &kernel);

/*
A variable of a common block has no storage of its own: it is the storage of
the block, which device code cannot reach. The kernel's copy of a procedure
that names one would otherwise declare it as a local. The host procedure the
loop is in is given a view of the block that places its variables as the
procedure's view does, and every such variable the procedure names becomes a
variable of the kernel, which the launch hands the host's variable for it.
gpu_lift_host_association then hands it to the procedure.
*/
void gpu_pass_common_blocks(Allocator &al, const Location &loc,
    const std::vector<ASR::Function_t*> &procedures,
    SymbolTable *kernel_scope, SymbolTable *host_scope,
    Vec<ASR::expr_t*> &kernel_args, Vec<ASR::call_arg_t> &call_args);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H
