#include <stddef.h>
#include <string.h>

int bindc_optional_char_01_check(int expected, const void *x)
{
    if (expected == 0) return x != NULL;
    if (x == NULL) return 1;
    if (expected == 1) return memcmp(x, "ABCD", 4) != 0;
    return memcmp(x, "CD", 2) != 0;
}
