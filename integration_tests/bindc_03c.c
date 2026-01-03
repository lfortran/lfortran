#include <stdio.h>


void execute_function( void (*func)() ) {
    printf("Executing function in C\n");
    func();
}

void execute_function_with_arg( void (*func)(int *), int* arg ) {
    printf("Executing function with arg in C\n");
    func(arg);
}
