/*
 * Linked with the C-backend object files of test_init_c_note_a.f90 and
 * test_init_c_note_b.f90: the engine has to read the notes of both, however
 * the compiler and the linker laid them out. See run_c_note_test.cmake.
 */
#include <stdint.h>
#include <stdio.h>

uint64_t _lcompilers_init_test_note_tables(void);

int main(void) {
    uint64_t n = _lcompilers_init_test_note_tables();
    printf("tables %llu\n", (unsigned long long)n);
    return n == 2 ? 0 : 1;
}
