/* C side of bindc_69: accesses the BIND(C) module character arrays through
   their binding names. Fortran g(3,2) is column-major, so g(i,j) is
   grid[j-1][i-1] in C. */
#include <string.h>

extern char bindc_69_init[3];
extern char bindc_69_grid[2][3];
extern char bindc_69_buf[5];

int bindc_69_check(void) {
    int i, j;
    if (memcmp(bindc_69_init, "xyz", 3) != 0) return 1;
    for (j = 0; j < 2; j++) {
        for (i = 0; i < 3; i++) {
            if (bindc_69_grid[j][i] != 'A' + i + 3*j) return 2;
        }
    }
    if (memcmp(bindc_69_buf, "12345", 5) != 0) return 3;
    return 0;
}

void bindc_69_fill(void) {
    int i, j;
    bindc_69_init[1] = 'Y';
    for (j = 0; j < 2; j++) {
        for (i = 0; i < 3; i++) {
            bindc_69_grid[j][i] = 'a' + i + 3*j;
        }
    }
    memcpy(bindc_69_buf, "hello", 5);
}
