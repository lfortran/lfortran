#include <stdio.h>

extern int counter;
extern double pi_value;

void check_counter(void) {
    if (counter != 42) {
        printf("FAIL: counter = %d, expected 42\n", counter);
        return;
    }
    printf("C sees counter=%d\n", counter);
}

void check_pi(void) {
    if (pi_value < 3.13 || pi_value > 3.15) {
        printf("FAIL: pi_value = %f, expected ~3.14\n", pi_value);
        return;
    }
    printf("C sees pi_value=%f\n", pi_value);
}
