#include <stdint.h>

/* GFortran's calling convention for non-bind(c) procedures: scalar integer,
   real and logical VALUE dummy arguments are passed by value. */

int32_t value_c_check_(int8_t a1, int16_t a2, int32_t a4, int64_t a8,
    float r4, double r8, int8_t l1, int32_t l4)
{
    if (a1 != -3) return 1;
    if (a2 != 1234) return 2;
    if (a4 != 7) return 3;
    if (a8 != 123456789012LL) return 4;
    if (r4 != 1.5f) return 5;
    if (r8 != -2.25) return 6;
    if (l1 != 0) return 7;
    if (l4 != 1) return 8;
    return 0;
}

typedef int32_t (*value_f_check_t)(int32_t, int64_t, float, double, int32_t);

/* Calls a Fortran procedure with VALUE dummy arguments. */
int32_t value_c_call_(value_f_check_t f)
{
    return f(7, 123456789012LL, 1.5f, -2.25, 1);
}
