#include <stddef.h>
#include <libasr/runtime/lfortran_intrinsics.h>
#include <stdio.h>
#include <string.h>

// INQUIRE(POS=) on a formatted sequential unit subtracts the record
// terminators before the current position. It must count them only in the
// bytes written or read since the previous INQUIRE, not by rereading the
// whole file each time (lfortran/lfortran#16796), and it must count them
// from the start again after the unit is repositioned (REWIND, BACKSPACE).
// POS= in a data transfer statement is allowed only for a stream unit, so a
// seek on this sequential unit must fail without moving it (#16828).

static int failures;
static void check(bool condition, const char *message) {
    if (!condition) {
        fprintf(stderr, "%s\n", message);
        failures++;
    }
}

static const int32_t unit = 10;
static const char *file_name = "test_runtime_inquire_pos.txt";

static int64_t inquire_pos(void) {
    int64_t pos = -1;
    _lfortran_inquire(NULL, 0, NULL, unit, NULL, NULL, &pos,
        NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0,
        NULL, NULL, NULL,
        NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0,
        NULL, NULL,
        NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0,
        NULL,
        NULL, 0, NULL, 0, NULL, 0, NULL, 0);
    return pos;
}

static void write_record(FILE *fp, const char *s) {
    fputs(s, fp);
    fputc('\n', fp);
}

int main(void) {
    int32_t iostat = -1;
    FILE *fp = (FILE*)_lfortran_open(unit, (char*)file_name,
        (int64_t)strlen(file_name), "replace", 7, "formatted", 9,
        "sequential", 10, NULL, 0, &iostat, NULL, 0, NULL, 0, NULL, 0,
        NULL, 0, NULL, 0, NULL, NULL, 0, NULL, 0, NULL, 0, NULL, 0,
        NULL, 0);
    if (iostat != 0 || fp == NULL) {
        fprintf(stderr, "open failed: iostat=%d\n", (int)iostat);
        return 1;
    }

    write_record(fp, "Line 1");
    check(inquire_pos() == 6, "first record");
    write_record(fp, "Line 2");
    check(inquire_pos() == 12, "second record");
    long bytes = 14, records = 2;
    for (int i = 0; i < 1000; i++) {
        write_record(fp, "abcd");
        bytes += 5;
        records++;
        if (inquire_pos() != bytes - records) {
            check(false, "record in a write loop");
            break;
        }
    }

    // Turn the first byte of the file into a record terminator behind the
    // unit's back. A rescan from the start of the file would count it.
    FILE *other = fopen(file_name, "r+");
    check(other != NULL && fputc('\n', other) == '\n' && fclose(other) == 0,
        "modify the first byte");
    write_record(fp, "x");
    bytes += 2;
    records++;
    check(inquire_pos() == bytes - records,
        "INQUIRE rescanned records before the previous INQUIRE");

    // After REWIND, overwrite the first record and every terminator after
    // it: no record terminator remains before the position.
    _lfortran_rewind(unit, &iostat, NULL, 0);
    check(iostat == 0 && inquire_pos() == 1, "rewind");
    for (long i = 0; i < bytes + 10; i++) fputc('y', fp);
    check(inquire_pos() == bytes + 11, "write past the old end after rewind");

    // After BACKSPACE, overwrite the terminator of the last record.
    write_record(fp, "");
    write_record(fp, "abc");
    check(inquire_pos() == bytes + 13, "records after rewind");
    _lfortran_backspace(unit, &iostat, NULL, 0);
    check(iostat == 0, "backspace");
    fputs("abcdefgh", fp);
    check(inquire_pos() == bytes + 18, "overwrite the last terminator");

    // POS= moves back to the start, but it is allowed only on a stream unit:
    // on this sequential unit it must fail without moving the unit.
    _lfortran_rewind(unit, &iostat, NULL, 0);
    fputs("aaaa\nbbbb\n", fp);
    check(iostat == 0 && inquire_pos() == 8, "records before failed POS=");
    long pos_before = ftell(fp);
    _lfortran_file_seek(unit, 1, &iostat);
    check(iostat == 5002, "POS= on a sequential unit must fail");
    check(ftell(fp) == pos_before, "failed POS= must not move the unit");

    _lfortran_close(unit, "delete", 6, &iostat);
    check(iostat == 0, "close");
    return failures ? 1 : 0;
}
