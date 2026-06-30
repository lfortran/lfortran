#include <stdio.h>

void
bump_value(int* val);

int
main()
{
    int v = 41;
    bump_value(&v);
    if (v != 42) {
        printf("Failed! v = %d\n", v);
        return 1;
    }
    printf("Success!\n");
    return 0;
}
