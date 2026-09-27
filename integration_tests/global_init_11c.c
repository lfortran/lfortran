int global_init_11_check(int changed);
void global_init_11_change(void);

/*
 * Changes the module's static data before the startup hook of the object
 * file that defines the module has run, which must not undo the change. The
 * constructor is registered the way global_init_10c.c describes, which is
 * what makes it run ahead of that hook.
 */
#if defined(_MSC_VER)
static void change_before_module_startup(void);
#pragma section(".CRT$XCT", read)
__declspec(allocate(".CRT$XCT")) void (*global_init_11_crt_initializer)(void) =
    change_before_module_startup;
#pragma comment(linker, "/include:global_init_11_crt_initializer")
#elif defined(__APPLE__)
__attribute__((constructor)) static void change_before_module_startup(void);
#else
__attribute__((constructor(101))) static void change_before_module_startup(void);
#endif

static void change_before_module_startup(void) {
    global_init_11_change();
}

int main(void) {
    return global_init_11_check(1) != 0;
}
