#include <stdint.h>
void ext_check_char(const char* c, const int32_t* expected_code,
                    int32_t* status)
{
    if ((unsigned char) *c == (unsigned char) *expected_code) {
        *status = 0;
    } else {
        *status = 1;
    }
}
