#include <stddef.h>
#include <stdint.h>

void verify_one_character(const char *value, int32_t *status,
                          size_t value_length)
{
    *status = value[0] == 'N' && value_length == 1 ? 0 : 1;
}

void verify_two_characters(const char *first, const int32_t *number,
                           const char *second, int32_t *status,
                           size_t first_length, size_t second_length)
{
    *status = first[0] == 'N' && *number == 42 && second[0] == 'Z' &&
              first_length == 1 && second_length == 1 ? 0 : 1;
}

void verify_character_result(char *result, size_t result_length,
                             const int32_t *number, const char *value,
                             size_t value_length)
{
    static const char expected[5] = {'R', '4', '2', 'Z', '!'};
    size_t i;

    if (result_length != 5 || *number != 42 ||
        value_length != 1 || value[0] != 'Z') {
        for (i = 0; i < result_length; i++) {
            result[i] = '?';
        }
        return;
    }
    for (i = 0; i < result_length; i++) {
        result[i] = expected[i];
    }
}

void verify_no_character(const int32_t *number, int32_t *status)
{
    *status = *number == 42 ? 0 : 1;
}
