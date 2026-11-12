#include <string.h>

int bindc_char_typestar_01_check(const void *buf, const void *expected, int n)
{
    return memcmp(buf, expected, (size_t) n) != 0;
}
