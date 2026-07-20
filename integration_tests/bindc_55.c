/* Companion C definition for bindc_55.f90.
   Sums the first n character codes of the buffer passed from Fortran as an
   assumed-size CHARACTER(KIND=C_CHAR) :: op(*) argument. */
int nc_sum_att_text(const char *op, int n) {
    int sum = 0;
    int i;
    for (i = 0; i < n; i++) {
        sum += (unsigned char)op[i];
    }
    return sum;
}
