long bindc_section_01_sum(int n, const long *x)
{
    long s = 0;
    for (int i = 0; i < n; i++) s = s * 10 + x[i];
    return s;
}

void bindc_section_01_double(int n, int *x)
{
    for (int i = 0; i < n; i++) x[i] *= 2;
}
