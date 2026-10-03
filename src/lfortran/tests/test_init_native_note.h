/*
 * The ELF note of one object file of the engine's native tests, spelled as
 * compiled code spells it (see lcompilers_init_abi.h): allocated, read-only,
 * and naming the object's table, whose assembler name is
 * `test_init_native_table`, by its offset from the note.
 * Included once, at file scope, by an object file that defines that table.
 */
#if __SIZEOF_POINTER__ == 8
__asm__(".pushsection .note.lcompilers.init,\"a\",%note\n"
    ".balign 4\n"
    "1: .long 4, 8, 1\n"
    ".asciz \"LCP\"\n"
    ".quad test_init_native_table - 1b\n"
    ".popsection");
#else
__asm__(".pushsection .note.lcompilers.init,\"a\",%note\n"
    ".balign 4\n"
    "1: .long 4, 4, 1\n"
    ".asciz \"LCP\"\n"
    ".long test_init_native_table - 1b\n"
    ".popsection");
#endif
