extern void bindc_75_explicit(char *a);
extern void bindc_75_default(char *a);
extern void bindc_75_matrix(char *a);
extern void bindc_75_single(char *a);
extern void bindc_75_assumed(char *a);
extern void bindc_75_scalar(char *a);

int bindc_75_check(void) {
    char explicit_chars[] = {'L', 'x', 'y', 'z', 'R'};
    char default_chars[] = {'L', 'x', 'y', 'z', 'R'};
    char matrix[] = {'L', 'a', 'b', 'c', 'd', 'R'};
    char single[] = {'L', '?', 'R'};
    char scalar = '?';
    bindc_75_explicit(explicit_chars + 1);
    if (explicit_chars[0] != 'L' || explicit_chars[1] != 'a' ||
        explicit_chars[2] != 'y' || explicit_chars[3] != 'x' ||
        explicit_chars[4] != 'R') return 1;
    bindc_75_default(default_chars + 1);
    if (default_chars[0] != 'L' || default_chars[1] != 'a' ||
        default_chars[2] != 'y' || default_chars[3] != 'x' ||
        default_chars[4] != 'R') return 2;
    bindc_75_matrix(matrix + 1);
    if (matrix[0] != 'L' || matrix[1] != 'a' || matrix[2] != 'b' ||
        matrix[3] != 'X' || matrix[4] != 'c' || matrix[5] != 'R') return 3;
    bindc_75_single(single + 1);
    if (single[0] != 'L' || single[1] != 's' || single[2] != 'R') return 4;
    bindc_75_assumed(explicit_chars + 1);
    if (explicit_chars[0] != 'L' || explicit_chars[1] != 'a' ||
        explicit_chars[2] != 'a' || explicit_chars[3] != 'x' ||
        explicit_chars[4] != 'R') return 5;
    bindc_75_scalar(&scalar);
    if (scalar != 't') return 6;
    return 0;
}
