/*
 * lf_float128.h — Complete IEEE 754 binary128 runtime for LFortran.
 *
 * Portable, self-contained implementation of real(16) / REAL128 operations.
 *
 * Provides:
 *   - Arithmetic:    __addtf3, __subtf3, __multf3, __divtf3, __negtf2
 *   - Conversions:   __extenddftf2, __extendsftf2, __floatsitf, __floatditf,
 *                    __floatunditf, __trunctfdf2, __trunctfsf2,
 *                    __fixtfsi, __fixtfdi
 *   - Comparisons:   __eqtf2, __netf2, __lttf2, __letf2, __gttf2, __getf2,
 *                    __unordtf2
 *   - Math:          lf_f128_sqrt, lf_f128_abs, lf_f128_floor, lf_f128_ceiling,
 *                    lf_f128_mod, lf_f128_pow, lf_f128_exp, lf_f128_log,
 *                    lf_f128_log10, lf_f128_sin, lf_f128_cos, lf_f128_tan,
 *                    lf_f128_asin, lf_f128_acos, lf_f128_atan, lf_f128_atan2
 *   - Formatting:    lf_float128_to_str   (Dragon4, 34 digits)
 *   - Parsing:       lf_float128_from_str (decimal string -> binary128)
 */

#ifndef LF_FLOAT128_H
#define LF_FLOAT128_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 * Core type
 * ======================================================================== */

/*
 * lf_float128 — 16-byte struct holding an IEEE 754 binary128 value.
 *
 * Memory layout (little-endian host):
 *   bytes[0..7]  = low  64 bits
 *   bytes[8..15] = high 64 bits
 *     bit 127 (bytes[15] bit 7) = sign
 *     bits 126:112              = biased exponent (15 bits, bias = 16383)
 *     bits 111:0                = mantissa (no explicit leading bit for normals)
 */
typedef struct { uint8_t bytes[16]; } lf_float128;

lf_float128 __addtf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __subtf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __multf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __divtf3_lf_impl(lf_float128 a, lf_float128 b);
lf_float128 __negtf2_lf_impl(lf_float128 a);

/* On non-ARM64 (and non-sret) platforms the public symbols are thin wrappers */
#if !defined(__aarch64__)
lf_float128 __addtf3(lf_float128 a, lf_float128 b);
lf_float128 __subtf3(lf_float128 a, lf_float128 b);
lf_float128 __multf3(lf_float128 a, lf_float128 b);
lf_float128 __divtf3(lf_float128 a, lf_float128 b);
lf_float128 __negtf2(lf_float128 a);
#endif

/* ========================================================================
 * Conversions
 * ======================================================================== */

/* fp128 <-> floating-point */
lf_float128 __extenddftf2_lf_impl(double a);   /* double  -> fp128 */
lf_float128 __extendsftf2_lf_impl(float  a);   /* float   -> fp128 */
double       __trunctfdf2(lf_float128 a);       /* fp128   -> double */
float        __trunctfsf2(lf_float128 a);       /* fp128   -> float  */

/* fp128 <-> integer */
lf_float128 __floatsitf_lf_impl(int32_t  a);   /* int32   -> fp128 */
lf_float128 __floatditf_lf_impl(int64_t  a);   /* int64   -> fp128 */
lf_float128 __floatunditf_lf_impl(uint64_t a); /* uint64  -> fp128 */
int32_t      __fixtfsi_lf_impl(lf_float128 a);      /* fp128   -> int32  */
int64_t      __fixtfdi_lf_impl(lf_float128 a);      /* fp128   -> int64  */

#if !defined(__aarch64__)
int32_t      __fixtfsi(lf_float128 a);              /* fp128   -> int32  */
int64_t      __fixtfdi(lf_float128 a);              /* fp128   -> int64  */
#endif

#if !defined(__aarch64__)
lf_float128 __extenddftf2(double a);
lf_float128 __extendsftf2(float  a);
lf_float128 __floatsitf(int32_t  a);
lf_float128 __floatditf(int64_t  a);
lf_float128 __floatunditf(uint64_t a);
#endif

/* ========================================================================
 * Comparisons  (return int, no sret — same on all platforms)
 * ======================================================================== */
int __eqtf2   (lf_float128 a, lf_float128 b); /* 0 if a==b,   else 1  */
int __netf2   (lf_float128 a, lf_float128 b); /* 0 if a==b,   else 1  */
int __lttf2   (lf_float128 a, lf_float128 b); /* <0 if a<b            */
int __letf2   (lf_float128 a, lf_float128 b); /* ≤0 if a≤b            */
int __gttf2   (lf_float128 a, lf_float128 b); /* >0 if a>b            */
int __getf2   (lf_float128 a, lf_float128 b); /* ≥0 if a≥b            */
int __unordtf2(lf_float128 a, lf_float128 b); /* 1 if a or b is NaN   */

/* ========================================================================
 * Math — Fortran intrinsics for real(16)
 * ======================================================================== */
lf_float128 lf_f128_abs    (lf_float128 a);
lf_float128 lf_f128_sqrt   (lf_float128 a);
lf_float128 lf_f128_floor  (lf_float128 a);
lf_float128 lf_f128_ceiling(lf_float128 a);
lf_float128 lf_f128_mod    (lf_float128 a, lf_float128 b);  /* a - floor(a/b)*b */
lf_float128 lf_f128_pow    (lf_float128 a, lf_float128 b);  /* a**b */
lf_float128 lf_f128_exp    (lf_float128 a);
lf_float128 lf_f128_log    (lf_float128 a);   /* natural log */
lf_float128 lf_f128_log10  (lf_float128 a);
lf_float128 lf_f128_log2   (lf_float128 a);
lf_float128 lf_f128_sin    (lf_float128 a);
lf_float128 lf_f128_cos    (lf_float128 a);
lf_float128 lf_f128_tan    (lf_float128 a);
lf_float128 lf_f128_asin   (lf_float128 a);
lf_float128 lf_f128_acos   (lf_float128 a);
lf_float128 lf_f128_atan   (lf_float128 a);
lf_float128 lf_f128_atan2  (lf_float128 y, lf_float128 x);
lf_float128 lf_f128_sinh   (lf_float128 a);
lf_float128 lf_f128_cosh   (lf_float128 a);
lf_float128 lf_f128_tanh   (lf_float128 a);

/* ========================================================================
 * Formatting and parsing
 * ======================================================================== */

/*
 * lf_float128_to_str — Dragon4 binary128 -> decimal string (34 digits).
 * buf must be at least 48 bytes.
 * Output: [-]d.ddd...dddE±eee
 */
void lf_float128_to_str(char *buf, const uint8_t bytes[16]);

/*
 * lf_float128_from_str — decimal string -> binary128.
 * Parses standard Fortran/C decimal notation including E/e/D/d exponent.
 * Returns zero on parse failure.
 */
lf_float128 lf_float128_from_str(const char *s);

/* Convenience wrappers using the struct directly */
static inline void lf_f128_to_str(char *buf, lf_float128 v) {
    lf_float128_to_str(buf, v.bytes);
}

/* These are defined in lf_float128_to_str.c (both HAVE and !HAVE paths) */
lf_float128 lf_f128_from_double(double d);
double       lf_f128_to_double(lf_float128 v);
int lf_f128_isnan   (lf_float128 v);
int lf_f128_isinf   (lf_float128 v);
int lf_f128_signbit (lf_float128 v);
int lf_f128_cmp     (lf_float128 a, lf_float128 b);
int lf_f128_eq      (lf_float128 a, lf_float128 b);

/*
 * format_float128_fortran — format a real(16) value for Fortran print.
 * Output: [-]d.dddddddddddddddddddddddddddddddddE+eee  (34 sig digits)
 * buf must be >= 64 bytes.
 */
void format_float128_fortran(char *result, lf_float128 val);

#ifdef __cplusplus
}
#endif

#endif /* LF_FLOAT128_H */