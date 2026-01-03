#include <stdio.h>
#include <stdlib.h>

#include "bindc_02c.h"

void driver() {
    float A[12];
    printf("driver: initializing A[i] = i\n");
    for (int i=0; i < 12; i++) {
        A[i] = i;
    }
    printf("driver: calling callback1(12, A)\n");
    callback1(12, A);

    printf("driver: calling callback1b(12, A)\n");
    callback1b(12, A);

    int n = 12;
    printf("driver: calling callback2(&n, A)\n");
    callback2(&n, A);

    printf("driver: calling callback2b(&n, A)\n");
    callback2b(&n, A);
}

void print_ptr1(int n, float *A) {
    printf("print_ptr1: n = %d\n", n);
    printf("[ ");
    for (int i=0; i < n; i++) {
        printf("%f ", A[i]);
    }
    printf("]\n");
}

void print_ptr2(int *n, float *A) {
    printf("print_ptr2: n = %d\n", *n);
    printf("[ ");
    for (int i=0; i < *n; i++) {
        printf("%f ", A[i]);
    }
    printf("]\n");
}
