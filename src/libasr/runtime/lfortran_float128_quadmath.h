/*
 * lfortran_float128_quadmath.h
 *
 * Functions called directly by LFortran's own C/C++ code:
 *   - lfortran_intrinsics.c  (format_float128_fortran, lf_f128_to_double)
 *   - asr_to_llvm.cpp        (lf_float128_to_str, lf_float128_from_str)
 *
 * API mirrors libquadmath so that on Linux (LFORTRAN_HAVE_REAL128=1) we
 * delegate to the system quadmath functions, and on macOS/Windows/WASM
 * we use our own portable implementations from lfortran_float128.c.
 *
 * Mapping to quadmath equivalents:
 *   lf_float128_from_str     ←→  strtoflt128
 *   lf_float128_to_str       ←→  quadmath_snprintf("%.33Qe")
 *   format_float128_fortran  ←→  quadmath_snprintf (with Fortran E formatting)
 *   lf_f128_to_double        ←→  (double) cast
 *   lf_f128_from_double      ←→  (__float128) cast
 *   lf_f128_sqrt             ←→  sqrtq
 *   lf_f128_exp              ←→  expq
 *   lf_f128_log              ←→  logq
 *   lf_f128_log10            ←→  log10q
 *   lf_f128_sin              ←→  sinq
 *   lf_f128_cos              ←→  cosq
 *   lf_f128_tan              ←→  tanq
 *   lf_f128_asin             ←→  asinq
 *   lf_f128_acos             ←→  acosq
 *   lf_f128_atan             ←→  atanq
 *   lf_f128_atan2            ←→  atan2q
 *   lf_f128_sinh             ←→  sinhq
 *   lf_f128_cosh             ←→  coshq
 *   lf_f128_tanh             ←→  tanhq
 *   lf_f128_abs              ←→  fabsq
 *   lf_f128_floor            ←→  floorq
 *   lf_f128_ceiling          ←→  ceilq
 *   lf_f128_mod              ←→  fmodq
 *   lf_f128_pow              ←→  powq
 */

#ifndef LFORTRAN_FLOAT128_QUADMATH_H
#define LFORTRAN_FLOAT128_QUADMATH_H

#include <stdint.h>
#include "lfortran_float128_llvm.h"   /* for lf_float128 type */

#ifdef __cplusplus
extern "C" {
#endif

/* ── Formatting and parsing ─────────────────────────────────────────────── */

/*
 * lf_float128_to_str — binary128 → decimal string, 34 significant digits.
 * buf must be >= 48 bytes.
 * Output format: [-]d.ddd...dddE±eee
 * Uses quadmath_snprintf on Linux, Dragon4 elsewhere.
 */
void lf_float128_to_str(char *buf, const uint8_t bytes[16]);

/*
 * lf_float128_from_str — decimal string → binary128.
 * Accepts Fortran/C notation: E/e/D/d/Q/q exponent letters.
 * Uses strtoflt128 on Linux, big-integer parser elsewhere.
 */
lf_float128 lf_float128_from_str(const char *s);

/*
 * format_float128_fortran — format for Fortran PRINT *.
 * Output: [-]d.dddddddddddddddddddddddddddddddddE+eeee  (34 sig digits)
 * buf must be >= 64 bytes.
 */
void format_float128_fortran(char *result, lf_float128 val);

/* Convenience inline: struct → string */
static inline void lf_f128_to_str(char *buf, lf_float128 v) {
    lf_float128_to_str(buf, v.bytes);
}

/* ── Conversions ─────────────────────────────────────────────────────────── */
lf_float128 lf_f128_from_double(double d);
lf_float128 lf_f128_from_int64(int64_t i);
double       lf_f128_to_double(lf_float128 v);

/* ── Basic arithmetic (kind-aware compile-time folding) ──────────────────── */
lf_float128 lf_f128_add(lf_float128 a, lf_float128 b);
lf_float128 lf_f128_sub(lf_float128 a, lf_float128 b);
lf_float128 lf_f128_mul(lf_float128 a, lf_float128 b);
lf_float128 lf_f128_div(lf_float128 a, lf_float128 b);
lf_float128 lf_f128_neg(lf_float128 a);

/* ── Predicates ──────────────────────────────────────────────────────────── */
int lf_f128_isnan  (lf_float128 v);
int lf_f128_isinf  (lf_float128 v);
int lf_f128_signbit(lf_float128 v);
int lf_f128_cmp    (lf_float128 a, lf_float128 b);
int lf_f128_eq     (lf_float128 a, lf_float128 b);

/* ── Math (mirrors libquadmath API) ─────────────────────────────────────── */
lf_float128 lf_f128_abs    (lf_float128 a);           /* fabsq   */
lf_float128 lf_f128_sqrt   (lf_float128 a);           /* sqrtq   */
lf_float128 lf_f128_floor  (lf_float128 a);           /* floorq  */
lf_float128 lf_f128_ceiling(lf_float128 a);           /* ceilq   */
lf_float128 lf_f128_mod    (lf_float128 a, lf_float128 b); /* fmodq  */
lf_float128 lf_f128_pow    (lf_float128 a, lf_float128 b); /* powq   */
lf_float128 lf_f128_exp    (lf_float128 a);           /* expq    */
lf_float128 lf_f128_log    (lf_float128 a);           /* logq    */
lf_float128 lf_f128_log10  (lf_float128 a);           /* log10q  */
lf_float128 lf_f128_log2   (lf_float128 a);           /* log2q   */
lf_float128 lf_f128_sin    (lf_float128 a);           /* sinq    */
lf_float128 lf_f128_cos    (lf_float128 a);           /* cosq    */
lf_float128 lf_f128_tan    (lf_float128 a);           /* tanq    */
lf_float128 lf_f128_asin   (lf_float128 a);           /* asinq   */
lf_float128 lf_f128_acos   (lf_float128 a);           /* acosq   */
lf_float128 lf_f128_atan   (lf_float128 a);           /* atanq   */
lf_float128 lf_f128_atan2  (lf_float128 y, lf_float128 x); /* atan2q */
lf_float128 lf_f128_sinh   (lf_float128 a);           /* sinhq   */
lf_float128 lf_f128_cosh   (lf_float128 a);           /* coshq   */
lf_float128 lf_f128_tanh   (lf_float128 a);           /* tanhq   */

#ifdef __cplusplus
}
#endif

#endif /* LFORTRAN_FLOAT128_QUADMATH_H */