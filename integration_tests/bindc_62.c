/* C caller for bindc_62: calls the Fortran bind(c) procedures, which have
   interface blocks in their specification part, through their binding
   labels. */
void bindc_62_capply(int (*f)(int), int k, int *r);
void bindc_62_ctwice(int x, int *r);
int bindc_62_cthrice(int x);
void bindc_62_cmcall(void (*f)(void), int k, int *r);

static int plus_one(int i) { return i + 1; }

static int ncalls = 0;
static void count_call(void) { ncalls += 100; }

int bindc_62_call_from_c(void) {
    int a = -1, b = -1, c = -1;
    bindc_62_capply(plus_one, 5, &a);
    bindc_62_ctwice(21, &b);
    bindc_62_cmcall(count_call, 7, &c);
    return a + b + bindc_62_cthrice(4) + c + ncalls;
}
