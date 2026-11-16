#include <string.h>

int bindc_char_section_01_matches(const void *buf, const char *expected, int n)
{
    return memcmp(buf, expected, (size_t) n) != 0;
}

void bindc_char_section_01_overwrite(void *buf, const char *value, int n)
{
    if (n != 0) memcpy(buf, value, (size_t) n);
}
