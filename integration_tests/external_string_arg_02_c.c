#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* `call check(x(1), x(2))` with `character :: x(2)`: the data pointers of
   the two adjacent elements, then their lengths. */
static void check_impl(char *a, char *b, int64_t la, int64_t lb)
{
    printf("b-a = %td, *a,*b = %d %d, la,lb = %lld %lld\n", b - a,
        (unsigned char)*a, (unsigned char)*b, (long long)la, (long long)lb);
    if (b - a != 1) abort();
    if (*a != 'A' || *b != 'B') abort();
    if (la != 1 || lb != 1) abort();
}

/* LFortran */
void check(char *a, char *b, int64_t la, int64_t lb)
{
    check_impl(a, b, la, lb);
}

/* GFortran */
void check_(char *a, char *b, int64_t la, int64_t lb)
{
    check_impl(a, b, la, lb);
}
