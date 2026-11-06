#include <stdint.h>
#include <stdlib.h>

/* Stores Fortran callbacks obtained with c_funloc and calls them later,
   while the Fortran activations that made them are still live. */

typedef int32_t (*callback_t)(int32_t);

enum { MAX_DEPTH = 16 };

static callback_t registry[MAX_DEPTH + 1];

void callback_09_register(int32_t depth, callback_t f)
{
    if (depth < 1 || depth > MAX_DEPTH || f == NULL) abort();
    registry[depth] = f;
}

int32_t callback_09_call(int32_t depth, int32_t x)
{
    if (depth < 1 || depth > MAX_DEPTH || registry[depth] == NULL) abort();
    return registry[depth](x);
}

int32_t callback_09_sum(int32_t n, int32_t x)
{
    int32_t s = 0;
    int32_t d;
    for (d = 1; d <= n; d++) s += callback_09_call(d, x);
    return s;
}

/* Returns 1 if the callbacks of activations 1..n have distinct addresses. */
int32_t callback_09_distinct(int32_t n)
{
    int32_t i, j;
    for (i = 1; i <= n; i++) {
        for (j = 1; j < i; j++) {
            if (registry[i] == registry[j]) return 0;
        }
    }
    return 1;
}
