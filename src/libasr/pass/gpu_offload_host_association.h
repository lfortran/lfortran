#ifndef LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H
#define LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H

#include <vector>

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_decline.h>

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
loop is in is given a view of the block that places the variables the
procedure names as the procedure's view does, and every such variable becomes
a variable of the kernel, which the launch hands the host's variable for it.
gpu_lift_host_association then hands it to the procedure.

Only a numeric or logical variable that the view places as one of its members
is handed over this way. gpu_common_block_decline says why a procedure names
one that is not: a variable EQUIVALENCE associates with the block, a pointer,
a character variable, or a block with a derived-type variable among those
that place it.
*/
GpuDecline gpu_common_block_decline(ASR::Function_t &fn);

/*
Why the variables of common blocks that the loop `body` and the procedures
`routines` it calls name cannot be handed to the kernel together, if they
cannot: an external procedure names one (the kernel calls the procedure, not
a copy of it); a procedure defines a scalar one (the kernel is handed its
value); or a variable is defined through one view of a block and another view
places a variable it names differently (each reaches the kernel as memory of
its own).
*/
GpuDecline gpu_common_block_access_decline(ASR::stmt_t **body, size_t n_body,
    const std::vector<ASR::Function_t*> &routines);

void gpu_pass_common_blocks(Allocator &al, const Location &loc,
    const std::vector<ASR::Function_t*> &procedures,
    SymbolTable *kernel_scope, SymbolTable *host_scope,
    Vec<ASR::expr_t*> &kernel_args, Vec<ASR::call_arg_t> &call_args);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H
