#include <stddef.h>
#include <stdint.h>

/* GFortran's calling convention for non-bind(c) procedures: a type(c_ptr)
   dummy argument without VALUE is passed by reference (void **), whatever
   its intent. */

/* Returns the integer that the c_ptr points to, or -1 for a null c_ptr. */
int32_t cptr_c_get_(void **p)
{
    if (p == NULL) return -100;
    if (*p == NULL) return -1;
    return *(int32_t *)*p;
}

typedef int32_t (*cptr_f_get_t)(void **);

/* Calls a Fortran procedure with an intent(in) type(c_ptr) dummy. */
int32_t cptr_c_call_(cptr_f_get_t f, int32_t *x)
{
    void *p = x;
    void *q = NULL;
    if (f(&q) != -1) return 1;
    if (f(&p) != *x) return 2;
    if (p != x) return 3;
    return 0;
}

typedef int32_t (*scale_t)(int32_t);

/* Calls the procedure that a type(c_funptr), intent(in) dummy (also passed
   by reference) holds, or returns -1 for a null c_funptr. */
int32_t cptr_c_funcall_(void (**f)(void), int32_t *x)
{
    if (f == NULL) return -100;
    if (*f == NULL) return -1;
    return ((scale_t)*f)(*x);
}
