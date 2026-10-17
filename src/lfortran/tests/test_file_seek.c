#include <stdint.h>
#include <stdio.h>

extern int64_t _lfortran_open(int32_t unit_num,
    char* f_name, int64_t f_name_len,
    char* status, int64_t status_len,
    char* form, int64_t form_len,
    char* access, int64_t access_len,
    char* iomsg, int64_t iomsg_len,
    int32_t *iostat,
    char* action, int64_t action_len,
    char* delim, int64_t delim_len,
    char* position, int64_t position_len,
    char* blank, int64_t blank_len,
    char* encoding, int64_t encoding_len,
    int32_t *recl,
    char* sign, int64_t sign_len,
    char* decimal, int64_t decimal_len,
    char* round, int64_t round_len,
    char* pad, int64_t pad_len);
extern void _lfortran_file_seek(int32_t unit_num, int64_t pos, int32_t* iostat);
extern void _lfortran_close(int32_t unit_num, char* status, int64_t status_len,
    int32_t* iostat);

// Seeking a stream beyond 2**31 bytes must not be truncated to a 32-bit
// long (as on Windows). Seeking past end of file writes nothing, so the
// scratch file stays empty.
int main(void) {
    const int32_t unit = 10;
    int32_t iostat = -1;
    FILE *fp = (FILE*)_lfortran_open(unit, NULL, 0, "scratch", 7,
        NULL, 0, "stream", 6, NULL, 0, &iostat, NULL, 0, NULL, 0, NULL, 0,
        NULL, 0, NULL, 0, NULL, NULL, 0, NULL, 0, NULL, 0, NULL, 0);
    if (iostat != 0 || fp == NULL) {
        fprintf(stderr, "open failed: iostat=%d\n", (int)iostat);
        return 1;
    }

    iostat = -1;
    // POS= is 1-based, so this seeks to byte offset 3000000000.
    _lfortran_file_seek(unit, INT64_C(3000000001), &iostat);
    if (iostat != 0) {
        fprintf(stderr, "seek past 2**31 failed: iostat=%d\n", (int)iostat);
        _lfortran_close(unit, NULL, 0, NULL);
        return 1;
    }

    if (fseek(fp, 0, SEEK_END) != 0 || ftell(fp) != 0) {
        fprintf(stderr, "scratch file is not empty\n");
        _lfortran_close(unit, NULL, 0, NULL);
        return 1;
    }

    iostat = -1;
    _lfortran_close(unit, NULL, 0, &iostat);
    if (iostat != 0) {
        fprintf(stderr, "close failed: iostat=%d\n", (int)iostat);
        return 1;
    }
    return 0;
}
