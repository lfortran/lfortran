#include <stdint.h>

void c_external(int32_t *value)
{
    *value = 1;
}

void legacy_external_(int32_t *value)
{
    *value = 2;
}
