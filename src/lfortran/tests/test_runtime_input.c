#include <stddef.h>
#include <libasr/runtime/lfortran_intrinsics.h>
#include <libasr/runtime/lfortran_stdin.h>
#include <stdio.h>
#include <string.h>

struct input {
    const char **records;
    size_t count, next;
    int calls, end;
};

static int provide(void *context, const char **data, size_t *length) {
    struct input *input = context;
    input->calls++;
    if (input->next == input->count) return input->end;
    *data = input->records[input->next++];
    *length = strlen(*data);
    return 0;
}

static int failures;
static void check(bool condition, const char *message) {
    if (!condition) {
        fprintf(stderr, "%s\n", message);
        failures++;
    }
}

int main(void) {
    const char *records[] = {"11 12 ignored", "", "13", "go"};
    struct input input = {records, 4, 0, 0, -1};
    int32_t a = 0, status = 0;
    _lfortran_stdin_install(provide, &input);
    _lfortran_read_int32(&a, -1, &status);
    check(status == 0 && a == 11 && input.calls == 1, "first scalar");
    _lfortran_read_int32(&a, 5, &status);
    check(status == 0 && a == 12 && input.calls == 1, "shared explicit-unit cursor");
    _lfortran_empty_read(-1, &status, 0);
    check(status == 0 && input.calls == 1, "drain only current record");
    _lfortran_empty_read(-2, &status, 1);
    check(status == 0 && input.calls == 2, "blank record is not EOF");
    _lfortran_read_int32(&a, 5, &status);
    _lfortran_empty_read(5, &status, 0);
    check(status == 0 && a == 13 && input.calls == 3, "unit 5 drain");
    _lfortran_pause_string("input provider", 14);
    check(input.calls == 4, "PAUSE must use provider");
    _lfortran_read_int32(&a, -1, &status);
    check(status == -1 && input.calls == 5, "EOF");
    _lfortran_read_int32(&a, 5, &status);
    check(status == -1 && input.calls == 5, "sticky EOF");
    _lfortran_stdin_reset();

    const char *fresh[] = {"99"};
    struct input reset = {fresh, 1, 0, 0, -1};
    _lfortran_stdin_install(provide, &reset);
    _lfortran_read_int32(&a, 5, &status);
    check(status == 0 && a == 99 && reset.calls == 1, "execution reset after EOF");
    _lfortran_stdin_reset();

    struct input unavailable = {NULL, 0, 0, 0, 1};
    _lfortran_stdin_install(provide, &unavailable);
    _lfortran_read_int32(&a, -1, &status);
    check(status > 0, "unavailable input is a positive IOSTAT");
    _lfortran_read_int32(&a, 5, &status);
    check(status > 0 && unavailable.calls == 1, "sticky unavailable input");
    _lfortran_stdin_reset();

    // No-provider fallback reads the real stdio stream, never the old callback.
    const char *filename = "test_runtime_input_stdio.dat";
    FILE *file = fopen(filename, "w");
    if (!file) return 1;
    fputs("71\n72\n", file);
    fclose(file);
    if (!freopen(filename, "r", stdin)) return 1;
    _lfortran_read_int32(&a, -1, &status);
    _lfortran_empty_read(-1, &status, 0);
    check(status == 0 && a == 71, "normal stdin without provider");
    _lfortran_read_int32(&a, 5, &status);
    check(status == 0 && a == 72, "normal unit 5 without provider");
    _lfortran_empty_read(5, &status, 0);
    _lfortran_read_int32(&a, 5, &status);
    check(status == -1, "normal stdio EOF");
    reset.next = 0;
    _lfortran_stdin_install(provide, &reset);
    _lfortran_read_int32(&a, -1, &status);
    check(status == 0 && a == 99, "provider ignores libc EOF latch");
    _lfortran_stdin_reset();
    _lfortran_read_int32(&a, 5, &status);
    check(status == -1, "provider must not clear libc EOF latch");
    fclose(stdin);
    remove(filename);
    return failures ? 1 : 0;
}
