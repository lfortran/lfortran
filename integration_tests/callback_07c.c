#include <stdint.h>

/* A non-bind(c) Fortran subroutine with EXTERNAL dummies, called from C. */
typedef void (*sub_t)(int32_t *, int32_t *);
typedef int32_t (*fun_t)(int32_t *);

void callback_07_apply_(sub_t f, fun_t g, int32_t *x, int32_t *r);

static void add3(int32_t *a, int32_t *b)
{
    *b = *a + 3;
}

static void twice(int32_t *a, int32_t *b)
{
    *b = 2 * *a;
}

static int32_t cube(int32_t *a)
{
    return *a * *a * *a;
}

static int32_t negate(int32_t *a)
{
    return -*a;
}

void callback_07_driver_(int32_t *status)
{
    int32_t x = 5, r = 0;
    callback_07_apply_(add3, cube, &x, &r);
    if (r != 5 + 3 + 3 + 125) { *status = 1; return; }
    x = 7;
    callback_07_apply_(twice, negate, &x, &r);
    if (r != 4 * 7 - 7) { *status = 2; return; }
    if (x != 7) { *status = 3; return; }
    *status = 0;
}
