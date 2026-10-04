/*
 * Linked with an object file LFortran compiled from a module with startup
 * records: its constructor, which enters the engine, has run by the time
 * main starts, whatever linker linked it. Nothing else of this program
 * enters the engine before main. See run_show_llvm_test.cmake.
 */
#include <stdint.h>
#include <stdio.h>

uint64_t _lcompilers_init_test_generation_reads(void);

int main(void) {
    int ran = _lcompilers_init_test_generation_reads() > 0;
    printf("constructor %s\n", ran ? "ran" : "did not run");
    return ran ? 0 : 1;
}
