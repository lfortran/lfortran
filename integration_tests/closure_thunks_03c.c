#include <stdint.h>

/* A legacy C routine called from Fortran through an implicit interface
   (GFortran's convention for non-bind(c) procedures): the procedure
   argument is a raw code pointer. */
void call_twice_(void (*f)(int32_t *), int32_t *v)
{
    f(v);
    f(v);
}
