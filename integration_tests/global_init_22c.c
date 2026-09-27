/*
 * Linked into one shared library with global_init_20_b and global_init_20_a,
 * ahead of them, so that this constructor calls into the library's Fortran
 * code before the library's own startup hooks have run: on ELF because of
 * its priority, on Mach-O because constructors of one image run in link
 * order. The modules must be initialized by the time the first call gets
 * there, and nothing that runs later may undo what this constructor
 * changes. global_init_22.c checks both.
 */
int global_init_20_check_initial(void);
void global_init_20_mutate(void);

static int status = -1;

#if defined(__APPLE__)
__attribute__((constructor)) static void call_before_module_startup(void);
#else
__attribute__((constructor(101))) static void call_before_module_startup(void);
#endif

static void call_before_module_startup(void) {
    status = global_init_20_check_initial();
    if (status == 0) global_init_20_mutate();
}

int global_init_22_status(void) {
    return status;
}
