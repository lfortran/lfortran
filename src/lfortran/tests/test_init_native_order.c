/*
 * A program linked with two shared libraries, test_init_native_lb.c needing
 * test_init_native_la.c, each with records and a constructor. The loader
 * starts them one after the other, in its own order, and discovering their
 * records starts none of them early: no constructor runs inside another.
 * The dispatch of the first constructor already initializes the records of
 * both, in stable-id order.
 */
#include <stdio.h>
#include <string.h>

extern char test_init_order_events[16][32];
extern int test_init_order_nevents;
int test_init_order_lb_ready(void);

int main(void) {
    static const char *const expected[] = {
        "constructor m:la", "ready m:la", "ready m:lb", "constructor m:lb"};
    int ok = test_init_order_lb_ready() && test_init_order_nevents == 4;
    for (int i = 0; ok && i < 4; i++) {
        ok = strcmp(test_init_order_events[i], expected[i]) == 0;
    }
    if (!ok) {
        printf("FAIL; got:\n");
        for (int i = 0; i < test_init_order_nevents; i++) {
            printf("    %s\n", test_init_order_events[i]);
        }
        return 1;
    }
    printf("ok\n");
    return 0;
}
