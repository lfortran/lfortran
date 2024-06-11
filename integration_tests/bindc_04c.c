#include <stdio.h>

void a_func(void *func(void*)) {
    float array[5];
    for (int i = 0; i < 5; i++) {
        array[i] = i;
    }
    struct thread_data {
        float* a;
    } data;
    
    data.a = array;
    func(&data);
}
