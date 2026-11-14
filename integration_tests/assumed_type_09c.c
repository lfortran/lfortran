#include <stdint.h>
#include <string.h>

int assumed_type_09_first(int type, const void *buf)
{
    return (int) *(const int64_t *) buf;
}

int assumed_type_09_chars(int type, const void *buf, const char *expected, int n)
{
    return memcmp(buf, expected, (size_t) n) != 0;
}
