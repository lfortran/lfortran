#include <stdio.h>
#include <stdlib.h>

#include "bindc_02c.h"

void driver() {
    float A[12];
    printf("driver: initializing A[i] = i\n");
    for (int i=0; i < 12; i++) {
        A[i] = i;
    }
    printf("driver: calling callback(12, A)\n");
    callback(12, A);
}

void print_ptr(int n, float *A) {
    printf("print_ptr: n = %d\n", n);
    printf("[ ");
    for (int i=0; i < n; i++) {
        printf("%f ", A[i]);
    }
    printf("]\n");
}
