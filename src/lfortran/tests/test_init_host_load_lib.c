/*
 * A C constructor of the library of run_entry_load_test.cmake that calls a
 * bind(c) entry point of its own library while the library is loaded,
 * before the library's Fortran constructors have run: one of priority 101,
 * which runs ahead of every constructor of default priority, or, with
 * DEFAULT_PRIORITY, one of default priority linked ahead of the Fortran
 * object files.
 */
void test_init_entry_load_e(int *r);

int test_init_entry_load_in_ctor = -1;

#if defined(DEFAULT_PRIORITY)
__attribute__((constructor))
#else
__attribute__((constructor(101)))
#endif
static void test_init_entry_load_early(void) {
    test_init_entry_load_e(&test_init_entry_load_in_ctor);
}
