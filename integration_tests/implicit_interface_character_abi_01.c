#include <stddef.h>
#include <stdint.h>

int32_t character_lengths(const char *first, const char *second,
                          size_t first_length, size_t second_length)
{
    return first[0] == 'A' && second[0] == 'B'
        ? (int32_t)(10 * first_length + second_length) : -1;
}

void character_result(char *result, size_t result_length,
                      const char *value, size_t value_length)
{
    result[0] = 'B';
    result[1] = 'A';
    result[2] = 'D';
    if (result_length == 3 && value_length == 1 && value[0] == 'Z') {
        result[0] = 'O';
        result[1] = 'K';
        result[2] = 'Z';
    }
}

void wide_character_result(uint32_t *result, size_t result_length)
{
    result[0] = result_length == 3 ? 'A' : 'X';
    result[1] = result_length == 3 ? 'B' : 'X';
    result[2] = result_length == 3 ? 'C' : 'X';
}
