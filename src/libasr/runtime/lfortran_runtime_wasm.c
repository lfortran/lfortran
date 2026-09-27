/* The WebAssembly runtimes are one object file each, so their sources are
 * compiled together. The engine comes first: lfortran_intrinsics.c forbids
 * the C allocator in everything after its own allocator. */
#include "lcompilers_init.c"
#include "lfortran_intrinsics.c"
