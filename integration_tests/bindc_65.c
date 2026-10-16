/* C callee for bindc_65: receives an allocatable array component of a
   bind(c) derived type through an assumed-size dummy, as a plain pointer
   to its first element. */
typedef struct {
    int flag;
    double value;
} bc_t;

void bindc_65_sum(int n, const bc_t *geometries, double *total) {
    *total = 0.0;
    for (int i = 0; i < n; i++) {
        *total += geometries[i].flag * geometries[i].value;
    }
}
