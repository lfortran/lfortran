int bindc_76_concat(int x) { return x + 1; }
int bindc_76_proc(int x) { return x + 2; }
int bindc_76_used(int x) { return x + 3; }
int bindc_76_trim(int x) { return x + 4; }
int bindc_76_substr(int x) { return x + 5; }

extern int bindc_76_var;
extern int bindc_76_w;
extern struct { int cv; } bindc_76_blk;
extern int bindc_76_twice(int x);
extern void bindc_76_setcv(int x);

int bindc_76_get_var(void) { return bindc_76_var; }
int bindc_76_get_w(void) { return bindc_76_w; }
int bindc_76_get_cv(void) { return bindc_76_blk.cv; }
int bindc_76_call_twice(int x) { return bindc_76_twice(x); }
void bindc_76_call_setcv(int x) { bindc_76_setcv(x); }
