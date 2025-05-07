#include <string.h>

#include "modules_18c.h"

int f_int_float(int *a, float *b) {
    return *a + *b;
}

int f_int_double(int *a, double *b) {
    return *a + *b;
}

int f_int_float_complex(int *a, float_complex_t *b) {
    return *a + crealf(*b) + cimagf(*b);
}

int f_int_double_complex(int *a, double_complex_t *b) {
    return *a + creal(*b) + cimag(*b);
}

int f_int_float_complex_value(int a, float_complex_t b) {
    return a + crealf(b) + cimagf(b);
}

int f_int_double_complex_value(int a, double_complex_t b) {
    return a + creal(b) + cimag(b);
}

float_complex_t f_float_complex_value_return(float_complex_t b) {
    float_complex_t r;
#if _WIN32
    r = _FCmulcr(b, 2.0);
#else
    r = b * 2;
#endif
    return r;
}

double_complex_t f_double_complex_value_return(double_complex_t b) {
    double_complex_t r;
#if _WIN32
    r = _Cmulcr(b, 2.0);
#else
    r = b * 2;
#endif
    return r;
}

int f_int_float_value(int a, float b) {
    return a + b;
}

int f_int_double_value(int a, double b) {
    return a + b;
}

int f_int_intarray(int n, int *b) {
    int i;
    int s;
    s = 0;
    for (i=0; i < n; i++) {
        s += b[i];
    }
    return s;
}

float f_int_floatarray(int n, float *b) {
    int i;
    float s;
    s = 0;
    for (i=0; i < n; i++) {
        s += b[i];
    }
    return s;
}

double f_int_doublearray(int n, double *b) {
    int i;
    double s;
    s = 0;
    for (i=0; i < n; i++) {
        s += b[i];
    }
    return s;
}

float f_int_floatarray_star(int n, float *b) {
    int i;
    float s;
    s = 0;
    for (i=0; i < n; i++) {
        s += b[i];
    }
    return s;
}

// --------------------------------------------------------------------

void sub_int_float(int *a, float *b, int *r) {
    *r = *a + *b;
}

void sub_int_double(int *a, double *b, int *r) {
    *r = *a + *b;
}

void sub_int_float_complex(int *a, float_complex_t *b, int *r) {
    *r = *a + crealf(*b) + cimagf(*b);
}

void sub_int_double_complex(int *a, double_complex_t *b, int *r) {
    *r = *a + creal(*b) + cimag(*b);
}

void sub_int_float_value(int a, float b, int *r) {
    *r = a + b;
}

void sub_int_double_value(int a, double b, int *r) {
    *r = a + b;
}

void sub_int_float_complex_value(int a, float_complex_t b, int *r) {
    *r = a + crealf(b) + cimagf(b);
}

void sub_int_double_complex_value(int a, double_complex_t b, int *r) {
    *r = a + creal(b) + cimag(b);
}

void sub_int_intarray(int n, int *b, int *r) {
    int i;
    int s;
    s = 0;
    for (i=0; i < n; i++) {
        s += b[i];
    }
    *r = s;
}

void sub_int_floatarray(int n, float *b, float *r) {
    int i;
    float s;
    s = 0;
    for (i=0; i < n; i++) {
        s += b[i];
    }
    *r = s;
}

void sub_int_doublearray(int n, double *b, double *r) {
    int i;
    double s;
    s = 0;
    for (i=0; i < n; i++) {
        s += b[i];
    }
    *r = s;
}

int f_string(char *s) {
    return strlen(s);
}

int32_t call_fortran_i32(int32_t i) {
    return fortran_i32(&i);
}

int32_t call_fortran_i32_value(int32_t i) {
    return fortran_i32_value(i);
}

int64_t call_fortran_i64(int64_t i) {
    return fortran_i64(&i);
}

int64_t call_fortran_i64_value(int64_t i) {
    return fortran_i64_value(i);
}

float call_fortran_f32(float i) {
    return fortran_f32(&i);
}

float call_fortran_f32_value(float i) {
    return fortran_f32_value(i);
}

double call_fortran_f64(double i) {
    return fortran_f64(&i);
}

double call_fortran_f64_value(double i) {
    return fortran_f64_value(i);
}
