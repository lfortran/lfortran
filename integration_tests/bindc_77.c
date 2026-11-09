int bindc_77_lit(int x) { return x + 1; }
int bindc_77_expr(int x) { return x + 2; }
int bindc_77_proc(int x) { return x + 3; }

extern int bindc_77_var;
extern int bindc_77_w;
extern struct { int cv; } bindc_77_blk;
extern int bindc_77_twice(int x);

int bindc_77_get_var(void) { return bindc_77_var; }
int bindc_77_get_w(void) { return bindc_77_w; }
int bindc_77_get_cv(void) { return bindc_77_blk.cv; }
int bindc_77_call_twice(int x) { return bindc_77_twice(x); }
