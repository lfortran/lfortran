#include <stddef.h>

int bindc_optional_associate_01_check(int expected, const int *x)
{
    if (expected == 0) return x != NULL;
    return x == NULL || x[0] != 3 || x[1] != 5;
}
