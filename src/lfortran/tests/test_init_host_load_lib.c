/*
 * The C part of the library of run_host_load_test.cmake. Unless
 * NO_CONSTRUCTOR is defined, a C constructor calls the library's bind(c)
 * procedure while the library is loaded, before the library's Fortran
 * constructors have run, and so calls lfortran_initialize() first, as the
 * contract of ISO_Fortran_binding.h asks of every caller that may come
 * before the runtime is started: one of priority 101, which runs ahead of
 * every constructor of default priority, or, with DEFAULT_PRIORITY, one of
 * default priority linked ahead of the Fortran object files.
 */
#include <ISO_Fortran_binding.h>

void test_init_host_load_e(int *r);

#if defined(NO_CONSTRUCTOR)
/* What the program checks: nothing is called before the load completes. */
int test_init_host_load_in_ctor = 7;
#else
int test_init_host_load_in_ctor = -1;

#if defined(DEFAULT_PRIORITY)
__attribute__((constructor))
#else
__attribute__((constructor(101)))
#endif
static void test_init_host_load_early(void) {
    lfortran_initialize(0, NULL);
    test_init_host_load_e(&test_init_host_load_in_ctor);
}
#endif
