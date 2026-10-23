#ifndef LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H
#define LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H

#include <libasr/asr.h>

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
The type of a dummy argument in a procedure signature, which must not name the
symbols of a scope: a bound of an array that is not a compile-time constant is
left out. A constant bound is kept, so a "FixedSizeArray" keeps the constant
dimensions its physical type requires.
*/
ASR::ttype_t* gpu_signature_type(Allocator &al, ASR::ttype_t *type);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_HOST_ASSOCIATION_H
