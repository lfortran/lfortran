#include <stdint.h>
#include <stdio.h>
#include <string.h>

void
redirect_stdin_to_file(const char* path, int64_t path_len)
{
    char buffer[256];
    int64_t len = path_len;
    while (len > 0 && path[len - 1] == ' ') {
        len--;
    }
    if (len >= (int64_t) sizeof(buffer)) {
        len = (int64_t) sizeof(buffer) - 1;
    }
    memcpy(buffer, path, (size_t) len);
    buffer[len] = '\0';
    freopen(buffer, "r", stdin);
}
