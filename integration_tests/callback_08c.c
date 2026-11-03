#include <stdint.h>

/* C routines without bind(c) that call Fortran procedure values, passed
   through implicit interfaces as plain code pointers. Both the GFortran
   (trailing underscore) and the LFortran names are defined. */

typedef void (*sub_t)(int32_t *, int32_t *);
typedef int32_t (*fun_t)(int32_t *);
typedef double (*real_t)(double *);

static void call_sub(sub_t f, int32_t *x, int32_t *r)
{
    int32_t copy = *x;
    f(&copy, r);
}

void callback_08_call_sub(sub_t f, int32_t *x, int32_t *r)
{
    call_sub(f, x, r);
}

void callback_08_call_sub_(sub_t f, int32_t *x, int32_t *r)
{
    call_sub(f, x, r);
}

int32_t callback_08_call_fun(fun_t f, int32_t *x)
{
    return f(x);
}

int32_t callback_08_call_fun_(fun_t f, int32_t *x)
{
    return f(x);
}

double callback_08_call_real(real_t f, double *x)
{
    return f(x);
}

double callback_08_call_real_(real_t f, double *x)
{
    return f(x);
}
