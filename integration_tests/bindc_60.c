/* Reference C callees for bindc_60. Each simply returns the byte it was
   passed so the Fortran caller can check the correct value crossed the
   BIND(C) boundary. */
int echo_byte_lenc(const char *op) {
    return (int)(unsigned char)op[0];
}

int echo_byte_len1(const char *op) {
    return (int)(unsigned char)op[0];
}

int echo_byte_bare(const char *op) {
    return (int)(unsigned char)op[0];
}

int echo_byte_kindc(const char *op) {
    return (int)(unsigned char)op[0];
}
