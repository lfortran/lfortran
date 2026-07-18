/* Companion C definition for bindc_59.f90 / bindc_59b.f90.
   Mimics netCDF-C's nc_inq_enum_member: it receives a plain C pointer
   (char *value) forwarded from the external Fortran wrapper get_member_value
   and writes an integer member value into it. */
#include <string.h>

void write_member_value(char *value) {
    int v = 42;
    memcpy(value, &v, sizeof(int));
}
