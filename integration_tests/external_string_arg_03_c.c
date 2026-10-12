#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* The convention of gfortran for procedures without bind(c): each character
   argument is a pointer to its data, and the lengths follow all the
   arguments, by value, in the order of the character arguments. Both the
   GFortran (trailing underscore) and the LFortran names are defined. */

static void show_impl(char *a, int32_t *n, char *b, int64_t la, int64_t lb)
{
    printf("show: la=%lld lb=%lld a='%.*s' n=%d b='%c'\n", (long long)la,
        (long long)lb, (int)la, a, (int)*n, *b);
    if (la != 5 || lb != 1) abort();
    if (memcmp(a, "Hello", 5) != 0) abort();
    if (*n != 42) abort();
    if (*b != 'B') abort();
    a[0] = 'J';
    *b = 'Z';
}

void show(char *a, int32_t *n, char *b, int64_t la, int64_t lb)
{
    show_impl(a, n, b, la, lb);
}

void show_(char *a, int32_t *n, char *b, int64_t la, int64_t lb)
{
    show_impl(a, n, b, la, lb);
}

static void show_arr_impl(char *names, int32_t *n, int64_t len)
{
    printf("show_arr: n=%d len=%lld names='%.12s'\n", (int)*n,
        (long long)len, names);
    if (*n != 3 || len != 4) abort();
    if (memcmp(names, "abcdefghijkl", 12) != 0) abort();
}

void show_arr(char *names, int32_t *n, int64_t len)
{
    show_arr_impl(names, n, len);
}

void show_arr_(char *names, int32_t *n, int64_t len)
{
    show_arr_impl(names, n, len);
}

typedef void (*show_t)(char *, int32_t *, char *, int64_t, int64_t);

/* Calls the Fortran procedure `f` with the same arguments as `show`. */
static void c_call_show_impl(show_t f, int32_t *status)
{
    char a[5];
    char b = 'B';
    int32_t n = 42;
    memcpy(a, "Hello", 5);
    f(a, &n, &b, 5, 1);
    if (memcmp(a, "Jello", 5) != 0) abort();
    if (b != 'Z') abort();
    if (n != 1) abort();
    *status = 1;
}

void c_call_show(show_t f, int32_t *status)
{
    c_call_show_impl(f, status);
}

void c_call_show_(show_t f, int32_t *status)
{
    c_call_show_impl(f, status);
}
