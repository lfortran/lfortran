#include <stdio.h>

void a_func(void *func(void*)) {
    float array[5];
    for (int i = 0; i < 5; i++) {
        array[i] = i;
    }
    struct thread_data {
        float* a;
        int n;
    } data;
    
    data.a = array;
    // print &array as integer
    printf("Array address in C: %ld\n", (long int) array);
    printf("Array[0] address in C: %ld\n", (long int) &array[0]);
    data.n = 5;
    func(&data);
}

void b_func(void *func(void*)) {
    float array[5];
    for (int i = 0; i < 5; i++) {
        array[i] = i;
    }
    float *data = array;
    // print &array as integer
    printf("Array address in C for b_func: %ld\n", (long int) array);
    func(data);
}

void c_func(void *array) {
    printf("Printing array in C for c_func\n");
    float *data = array;
    for (int i = 0; i < 5; i++) {
        printf("%f\n", data[i]);
    }
}
