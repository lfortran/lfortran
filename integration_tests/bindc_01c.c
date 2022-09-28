#include "bindc_01c.h"

#include <stdio.h>
#include <stdlib.h>

void ret_ptr_c(void **p) { *p = malloc(sizeof(float) * 12); }

void print_ptr_c(int n, void *p) {
  float *a = p;
  printf("[ ");
  for (int i = 0; i < n; i++) {
    printf("%f ", a[i]);
  }
  printf("]\n");
}
