/* lfortran_initialize(), with LFortran; see ISO_Fortran_binding.h. */
#include <ISO_Fortran_binding.h>

struct local_type {
    int value;
};
void global_init_29_fill(struct local_type *);

int main(int argc, char **argv)
{
    struct local_type x = {0};
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_initialize(argc, argv);
#else
    (void)argc;
    (void)argv;
#endif
    global_init_29_fill(&x);
#ifdef LFORTRAN_HAS_INITIALIZE
    lfortran_finalize();
#endif
    return x.value != 4;
}
