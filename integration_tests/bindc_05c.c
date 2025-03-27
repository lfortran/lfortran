#include <stdio.h>
#include <stdlib.h>

void* ax(int comm_f)
{
    int* p = (int*) malloc(sizeof(int));
    if (p == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return NULL;
    }
    *p = comm_f;
    printf("C function received: %d, returning pointer: %p\n", comm_f, (void*) p);
    return (void*) p;
}