#include <unistd.h>

void module_parameter_pointer_01_init_(
    void (*cb)(int *), void (*cb_real)(double *), void (*cb_arr)(int *));
void module_parameter_pointer_01_touch_(void);

static int *held;
static double *held_real;
static int *held_arr;

static void cb(int *p) { held = p; }
static void cb_real(double *p) { held_real = p; }
static void cb_arr(int *p) { held_arr = p; }

int main(void) {
    module_parameter_pointer_01_init_(cb, cb_real, cb_arr);
    module_parameter_pointer_01_touch_();
    if (held == 0 || *held != 100) {
        _exit(1);
    }
    if (held_real == 0 || *held_real != 2.5) {
        _exit(2);
    }
    if (held_arr == 0 || held_arr[0] != 10 || held_arr[1] != 20 || held_arr[2] != 30) {
        _exit(3);
    }
    return 0;
}
