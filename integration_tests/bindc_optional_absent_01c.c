#include <stddef.h>

typedef struct {
    int a, b;
} pt;

void bindc_optional_absent_01_ext(const int *x, const int *y, int *s)
{
    *s = *x;
    if (y != NULL) *s += *y;
}

int bindc_optional_absent_01_real(float *r)
{
    if (r == NULL) return 0;
    *r *= 2.0f;
    return 1;
}

void bindc_optional_absent_01_pt(const pt *t, int *s)
{
    *s = t == NULL ? -1 : t->a + t->b;
}

void bindc_optional_absent_01_cptr(void **p, int *s)
{
    *s = p != NULL;
}
