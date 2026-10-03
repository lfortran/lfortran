#include <stdio.h>
#include <stdlib.h>

void c_callback(int x) {
    if (x != 42) {
        printf("ERROR: expected 42, got %d\n", x);
        exit(1);   
    }
}