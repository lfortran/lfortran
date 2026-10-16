/* C side of bindc_68: reads the BIND(C) module character arrays through
   their C names. */
extern char x[1];
extern char mpi_fortran_argv_null[1];
extern char mpi_fortran_argvs_null[1][1];
char bindc_68_get(int which) {
    if (which == 0) return x[0];
    if (which == 1) return mpi_fortran_argv_null[0];
    return mpi_fortran_argvs_null[0][0];
}
