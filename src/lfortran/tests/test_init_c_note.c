/*
 * Linked with the C-backend object files of test_init_c_note_a.f90 and
 * test_init_c_note_b.f90: the engine has to read the notes of both, however
 * the compiler and the linker laid them out, and the entry point of the
 * first has to work. See run_c_note_test.cmake.
 */
#include <stdint.h>
#include <stdio.h>

uint64_t _lcompilers_init_test_note_tables(void);
void test_init_c_note_get(int *r);

int main(void) {
    uint64_t n = _lcompilers_init_test_note_tables();
    int first = -1, second = -1;
    test_init_c_note_get(&first);
    test_init_c_note_get(&second);
    printf("tables %llu, entry %d %d\n", (unsigned long long)n, first, second);
    return n == 2 && first == 11 && second == 11 ? 0 : 1;
}
