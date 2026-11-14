#include <unistd.h>

void module_parameter_pointer_01_init_(void (*cb)(int *));
void module_parameter_pointer_01_touch_(void);

static int *held;

static void cb(int *p) { held = p; }

int main(void) {
    module_parameter_pointer_01_init_(cb);
    module_parameter_pointer_01_touch_();
    if (held == 0 || *held != 100) {
        _exit(1);
    }
    return 0;
}
