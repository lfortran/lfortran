#ifndef MODULES_18C
#define MODULES_18C

#include <stdint.h>
#include <complex.h>

#if _WIN32
typedef _Fcomplex float_complex_t;
typedef _Dcomplex double_complex_t;
#else
typedef float _Complex float_complex_t;
typedef double _Complex double_complex_t;
#endif


int f_int_float(int *a, float *b);
int f_int_double(int *a, double *b);
int f_int_float_complex(int *a, float_complex_t *b);
int f_int_double_complex(int *a, double_complex_t *b);
int f_int_float_complex_value(int a, float_complex_t b);
int f_int_double_complex_value(int a, double_complex_t b);
float_complex_t f_float_complex_value_return(float_complex_t b);
double_complex_t f_double_complex_value_return(double_complex_t b);
int f_int_float_value(int a, float b);
int f_int_double_value(int a, double b);
int f_int_intarray(int n, int *b);
float f_int_floatarray(int n, float *b);
double f_int_doublearray(int n, double *b);
float f_int_floatarray_star(int n, float *b);



void sub_int_float(int *a, float *b, int *r);
void sub_int_double(int *a, double *b, int *r);
void sub_int_float_complex(int *a, float_complex_t *b, int *r);
void sub_int_double_complex(int *a, double_complex_t *b, int *r);
void sub_int_float_value(int a, float b, int *r);
void sub_int_double_value(int a, double b, int *r);
void sub_int_float_complex_value(int a, float_complex_t b, int *r);
void sub_int_double_complex_value(int a, double_complex_t b, int *r);
void sub_int_intarray(int n, int *b, int *r);
void sub_int_floatarray(int n, float *b, float *r);
void sub_int_doublearray(int n, double *b, double *r);

int f_string(char *s);

int32_t call_fortran_i32(int32_t i);
int32_t call_fortran_i32_value(int32_t i);
int64_t call_fortran_i64(int64_t i);
int64_t call_fortran_i64_value(int64_t i);
float call_fortran_f32(float i);
float call_fortran_f32_value(float i);
double call_fortran_f64(double i);
double call_fortran_f64_value(double i);

int32_t fortran_i32(int32_t *i);
int32_t fortran_i32_value(int32_t i);
int64_t fortran_i64(int64_t *i);
int64_t fortran_i64_value(int64_t i);
float fortran_f32(float *i);
float fortran_f32_value(float i);
double fortran_f64(double *i);
double fortran_f64_value(double i);


#endif // MODULES_18C
