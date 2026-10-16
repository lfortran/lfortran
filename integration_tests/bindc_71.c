char bindc_71_default = 'Q';
char bindc_71_named = 'R';
int bindc_71_integer = 5;
extern char bindc_71_initialized;

int bindc_71_check(void) {
    if (bindc_71_default != 'A') return 1;
    if (bindc_71_named != 'B') return 2;
    if (bindc_71_initialized != 'I') return 3;
    if (bindc_71_integer != 5) return 4;
    return 0;
}

void bindc_71_fill(void) {
    bindc_71_default = 'X';
    bindc_71_named = 'Y';
    bindc_71_initialized = 'Z';
}
