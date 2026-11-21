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

void bindc_optional_absent_01_proc(void (*cb)(int), int *s)
{
    if (cb != NULL) cb(5);
    *s = cb != NULL;
}

int bindc_optional_absent_01_proc_f(void (*cb)(int))
{
    if (cb == NULL) return 0;
    cb(5);
    return 1;
}

void bindc_optional_absent_01_arr(int n, const int *a, int *s)
{
    int i;
    if (a == NULL) {
        *s = -1;
        return;
    }
    *s = 0;
    for (i = 0; i < n; i++) *s += a[i];
}

void bindc_optional_absent_01_asz(const int *a, int *s)
{
    *s = a == NULL ? -1 : a[0] + a[1] + a[2];
}

/* `a` is a C descriptor (CFI_cdesc_t *) when present. */
void bindc_optional_absent_01_ash(const void *a, int *s)
{
    *s = a == NULL ? -1 : 1;
}
