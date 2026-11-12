#include <stdio.h>
#include <stdlib.h>

void bindc_optional_array_01_check(int expect_present, const long *md)
{
    if (expect_present == 0 && md != NULL) {
        printf("absent optional array arrived as non-NULL\n");
        exit(1);
    }
    if (expect_present != 0 && (md == NULL || md[0] != 7 || md[1] != 8)) {
        printf("present optional array lost its data\n");
        exit(2);
    }
}

void bindc_optional_array_01_check_scalar(int expect_present, const long *md)
{
    if (expect_present == 0 && md != NULL) {
        printf("absent optional scalar arrived as non-NULL\n");
        exit(3);
    }
    if (expect_present != 0 && (md == NULL || *md != 7)) {
        printf("present optional scalar lost its data\n");
        exit(4);
    }
}
