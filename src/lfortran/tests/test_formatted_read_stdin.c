#include <stdint.h>
#include <stdio.h>

extern void _lfortran_formatted_read(
    int32_t unit_num, int32_t* iostat, int32_t* chunk,
    char* advance, int64_t advance_length,
    char* fmt, int64_t fmt_len,
    int32_t no_of_args,
    char* pad, int64_t pad_len, ...);

/*
 * A formatted read from stdin (unit_num == -1) has no connection, so it must
 * use the default BLANK='NULL': the blank in "4 2 " is ignored and (I4) gives
 * 42.  BLANK='ZERO' would read 4020 instead.
 */
int main(void)
{
    const char *path = "test_formatted_read_stdin_data.txt";

    FILE *in = fopen(path, "w");
    if (!in) {
        fprintf(stderr, "cannot create %s\n", path);
        return 2;
    }
    fputs("4 2 \n", in);
    fclose(in);
    if (!freopen(path, "r", stdin)) {
        fprintf(stderr, "cannot redirect stdin to %s\n", path);
        return 2;
    }

    int32_t i = 0, iostat = 0;
    char advance[] = "yes";
    char fmt[] = "(I4)";

    _lfortran_formatted_read(-1, &iostat, NULL,
        advance, (int64_t)(sizeof(advance) - 1),
        fmt, (int64_t)(sizeof(fmt) - 1),
        1, NULL, 0,
        (int32_t)0, (int32_t)2, &i);

    remove(path);

    if (iostat != 0) {
        fprintf(stderr, "expected iostat == 0, got %d\n", iostat);
        return 1;
    }
    if (i != 42) {
        fprintf(stderr, "expected 42 (BLANK='NULL'), got %d\n", i);
        return 1;
    }
    return 0;
}
