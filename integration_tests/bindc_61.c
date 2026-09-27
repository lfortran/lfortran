/* C caller for bindc_61: calls the Fortran bind(c) procedures, which have
   internal procedures, through their binding labels. */
void bindc_61_csub(int x, double y, int *z);
float bindc_61_cfun(float x);

int bindc_61_call_from_c(void) {
    int z = -1;
    bindc_61_csub(5, 2.0, &z);
    return z + (int)bindc_61_cfun(3.5f);
}
