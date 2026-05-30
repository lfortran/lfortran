/*
 * lf_float128_to_str.c — Complete IEEE 754 binary128 runtime for LFortran.
 *
 * Implements all real(16) operations:
 *   arithmetic, conversions, comparisons, math functions,
 *   decimal formatting (Dragon4), decimal parsing,
 *   ARM64 sret calling-convention shims.
 *
 * Design:
 *   All values are passed as lf_float128 (struct{uint8_t[16]}).
 *   Internally unpacked to {sign, biased_exp, u128 mant}.
 *   Requires __uint128_t (GCC/Clang on all 64-bit targets).
 *
 * Sections:
 *   A. u128 helpers
 *   B. Pack / Unpack
 *   C. Specials (NaN, Inf, Zero)
 *   D. Comparisons
 *   E. Negation / Abs
 *   F. Addition / Subtraction
 *   G. Multiplication
 *   H. Division
 *   I. Conversions
 *   J. Math: sqrt, floor, ceiling, mod, pow
 *   K. Math: exp, log, log2, log10
 *   L. Math: sin, cos, tan, asin, acos, atan, atan2
 *   M. Math: sinh, cosh, tanh
 *   N. Formatting (Dragon4)
 *   O. Parsing
 *   P. ARM64 sret shims
 */

#include "lfortran_float128_llvm.h"
#include "lfortran_float128_quadmath.h"
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>   /* atoi */
#include <math.h>
#if defined(LFORTRAN_HAVE_CONFIG_H) || __has_include(<libasr/config.h>)
#  include <libasr/config.h>
#endif
#if LFORTRAN_HAVE_REAL128
#  include <quadmath.h>
#else
#  if defined(_MSC_VER)
#    include <intrin.h>
#  endif
#endif

/* ========================================================================
 * A. u128 helpers
 * ======================================================================== */

/* ========================================================================
 * A. u128 helpers — portable struct-based 128-bit integer.
 *    Pure C99. No __int128. Works on MSVC, Clang, GCC.
 *    Only used in the !LFORTRAN_HAVE_REAL128 (macOS + Windows) path.
 * ======================================================================== */

typedef struct { uint64_t lo; uint64_t hi; } u128;
typedef struct { uint64_t lo;  int64_t hi; } s128;

/* Construction and extraction */
static inline u128 u128_make(uint64_t hi, uint64_t lo) {
    u128 r; r.lo = lo; r.hi = hi; return r;
}
static inline uint64_t u128_hi(u128 v) { return v.hi; }
static inline uint64_t u128_lo(u128 v) { return v.lo; }

/* Conversion from scalar */
static inline u128 u128_from_u64(uint64_t v) {
    u128 r; r.lo = v; r.hi = 0; return r;
}

/* Constants */
static inline u128 u128_zero(void) { u128 r; r.lo = 0; r.hi = 0; return r; }
static inline u128 u128_one (void) { u128 r; r.lo = 1; r.hi = 0; return r; }

/* Arithmetic */
static inline u128 u128_add(u128 a, u128 b) {
    u128 r;
    r.lo = a.lo + b.lo;
    r.hi = a.hi + b.hi + (r.lo < a.lo ? 1u : 0u);
    return r;
}
static inline u128 u128_sub(u128 a, u128 b) {
    u128 r;
    r.lo = a.lo - b.lo;
    r.hi = a.hi - b.hi - (a.lo < b.lo ? 1u : 0u);
    return r;
}
static inline u128 u128_inc(u128 a) {
    u128 r;
    r.lo = a.lo + 1;
    r.hi = a.hi + (r.lo == 0 ? 1u : 0u);
    return r;
}

/* Bitwise */
static inline u128 u128_and(u128 a, u128 b) {
    u128 r; r.lo = a.lo & b.lo; r.hi = a.hi & b.hi; return r;
}
static inline u128 u128_or(u128 a, u128 b) {
    u128 r; r.lo = a.lo | b.lo; r.hi = a.hi | b.hi; return r;
}
static inline u128 u128_xor(u128 a, u128 b) {
    u128 r; r.lo = a.lo ^ b.lo; r.hi = a.hi ^ b.hi; return r;
}
static inline u128 u128_not(u128 a) {
    u128 r; r.lo = ~a.lo; r.hi = ~a.hi; return r;
}

/* Shifts */
static inline u128 u128_shl(u128 a, int n) {
    u128 r;
    if (n <= 0)   { return a; }
    if (n >= 128) { r.lo = 0; r.hi = 0; return r; }
    if (n >= 64)  { r.hi = a.lo << (n - 64); r.lo = 0; return r; }
    r.hi = (a.hi << n) | (a.lo >> (64 - n));
    r.lo = a.lo << n;
    return r;
}
static inline u128 u128_shr(u128 a, int n) {
    u128 r;
    if (n <= 0)   { return a; }
    if (n >= 128) { r.lo = 0; r.hi = 0; return r; }
    if (n >= 64)  { r.lo = a.hi >> (n - 64); r.hi = 0; return r; }
    r.lo = (a.lo >> n) | (a.hi << (64 - n));
    r.hi = a.hi >> n;
    return r;
}

/* Comparisons — return int (0 or 1) */
static inline int u128_gt(u128 a, u128 b) {
    return a.hi > b.hi || (a.hi == b.hi && a.lo > b.lo);
}
static inline int u128_gte(u128 a, u128 b) {
    return a.hi > b.hi || (a.hi == b.hi && a.lo >= b.lo);
}
static inline int u128_lt(u128 a, u128 b)  { return u128_gt(b, a); }
static inline int u128_lte(u128 a, u128 b) { return u128_gte(b, a); }
static inline int u128_eq(u128 a, u128 b)  {
    return a.hi == b.hi && a.lo == b.lo;
}
static inline int u128_is_zero(u128 a) { return !a.lo && !a.hi; }

/* Check if bit N is set (0-indexed from LSB) */
static inline int u128_bit(u128 a, int n) {
    if (n >= 64) return (int)((a.hi >> (n - 64)) & 1);
    return (int)((a.lo >> n) & 1);
}

/* Set bit N */
static inline u128 u128_set_bit(u128 a, int n) {
    if (n >= 64) { a.hi |= (uint64_t)1 << (n - 64); }
    else         { a.lo |= (uint64_t)1 << n; }
    return a;
}

/* Right shift by 1, fast */
static inline u128 u128_shr1(u128 a) {
    u128 r;
    r.lo = (a.lo >> 1) | (a.hi << 63);
    r.hi = a.hi >> 1;
    return r;
}

/* Left shift by 1, fast */
static inline u128 u128_shl1(u128 a) {
    u128 r;
    r.hi = (a.hi << 1) | (a.lo >> 63);
    r.lo = a.lo << 1;
    return r;
}

/* Count leading zeros — only #ifdef in the entire file */
static inline int lf_clz64(uint64_t v) {
#if defined(_MSC_VER)
    unsigned long idx;
    return _BitScanReverse64(&idx, v) ? (int)(63 - idx) : 64;
#else
    return v ? __builtin_clzll(v) : 64;
#endif
}
static inline int lf_clz32(uint32_t v) {
#if defined(_MSC_VER)
    unsigned long idx;
    return _BitScanReverse(&idx, v) ? (int)(31 - idx) : 32;
#else
    return v ? __builtin_clz(v) : 32;
#endif
}
static inline int u128_clz(u128 v) {
    if (v.hi) return lf_clz64(v.hi);
    if (v.lo) return 64 + lf_clz64(v.lo);
    return 128;
}

/* Get value of top N bits shifted down (for reading specific bit ranges) */
static inline uint64_t u128_top_u64(u128 a) { return a.hi; }

/* 64x64 -> 128-bit multiply (schoolbook via 32-bit halves) */
static inline u128 mul64x64(uint64_t a, uint64_t b) {
    uint64_t a_lo = a & 0xFFFFFFFFull, a_hi = a >> 32;
    uint64_t b_lo = b & 0xFFFFFFFFull, b_hi = b >> 32;

    uint64_t p0 = a_lo * b_lo;
    uint64_t p1 = a_lo * b_hi;
    uint64_t p2 = a_hi * b_lo;
    uint64_t p3 = a_hi * b_hi;

    uint64_t mid = (p0 >> 32)
                 + (p1 & 0xFFFFFFFFull)
                 + (p2 & 0xFFFFFFFFull);
    u128 r;
    r.lo = (mid << 32) | (p0 & 0xFFFFFFFFull);
    r.hi = p3 + (p1 >> 32) + (p2 >> 32) + (mid >> 32);
    return r;
}

/* 128x128 -> 256-bit multiply. Result in (hi_out, lo_out) each 128 bits. */
static void u128_mul128(u128 a, u128 b, u128 *hi_out, u128 *lo_out) {
    u128 c00 = mul64x64(a.lo, b.lo);
    u128 c01 = mul64x64(a.lo, b.hi);
    u128 c10 = mul64x64(a.hi, b.lo);
    u128 c11 = mul64x64(a.hi, b.hi);

    /* mid = c00.hi + c01.lo + c10.lo  (tracking carry) */
    uint64_t m0 = c00.hi;
    uint64_t m1 = m0 + c01.lo; int mc1 = (m1 < m0) ? 1 : 0;
    uint64_t m2 = m1 + c10.lo; int mc2 = (m2 < m1) ? 1 : 0;

    lo_out->lo = c00.lo;
    lo_out->hi = m2;

    uint64_t h0 = c11.lo;
    uint64_t h1 = h0 + c01.hi; int hc1 = (h1 < h0) ? 1 : 0;
    uint64_t h2 = h1 + c10.hi; int hc2 = (h2 < h1) ? 1 : 0;
    uint64_t h3 = h2 + (uint64_t)(mc1 + mc2); int hc3 = (h3 < h2) ? 1 : 0;

    hi_out->lo = h3;
    hi_out->hi = c11.hi + (uint64_t)(hc1 + hc2 + hc3);
}

/* ========================================================================
 * B. Pack / Unpack
 * ======================================================================== */

/* f128 bit layout (little-endian struct):
 *   bytes[0..7]  = low  64 bits (mantissa low)
 *   bytes[8..15] = high 64 bits
 *     bit  127    = sign
 *     bits 126:112 = biased exponent (15 bits, bias=16383)
 *     bits 111:0   = mantissa (no explicit leading 1 for normals)
 */
typedef struct {
    int      sign;    /* 0 or 1 */
    int32_t  exp;     /* true (unbiased) exponent; for normals: biased-16383 */
    u128     mant;    /* full 113-bit significand: implicit 1 in bit 112 for normals */
    int      is_nan;
    int      is_inf;
    int      is_zero;
} f128_parts;

static f128_parts f128_unpack(lf_float128 v) {
    f128_parts p = {0};
    uint64_t lo, hi;
    memcpy(&lo, v.bytes,   8);
    memcpy(&hi, v.bytes+8, 8);

    p.sign = (int)(hi >> 63);
    int32_t biased = (int32_t)((hi >> 48) & 0x7FFFu);
    uint64_t mhi   = hi & 0x0000FFFFFFFFFFFFull;
    uint64_t mlo   = lo;

    if (biased == 0x7FFF) {
        if (mhi || mlo) { p.is_nan = 1; return p; }
        p.is_inf = 1;
        return p;
    }
    if (!biased && !mhi && !mlo) { p.is_zero = 1; return p; }

    if (biased) {
        /* normal: implicit leading 1 in bit 112 */
        p.mant = u128_make(mhi | (1ull << 48), mlo);
        p.exp  = biased - 16383;
    } else {
        /* subnormal: no leading 1 */
        p.mant = u128_make(mhi, mlo);
        p.exp  = -16382;  /* fixed exponent for subnormals */
    }
    return p;
}

/* Normalise mant so that bit 112 is set, adjusting exp.
 * Then pack into lf_float128. */
static lf_float128 f128_pack_parts(int sign, int32_t exp, u128 mant) {
    lf_float128 r;
    uint64_t lo, hi;

    if (u128_is_zero(mant)) {
        /* zero */
        hi = (uint64_t)sign << 63;
        lo = 0;
        memcpy(r.bytes,   &lo, 8);
        memcpy(r.bytes+8, &hi, 8);
        return r;
    }

    /* Normalise: shift mant so bit 112 is the leading 1 */
    int clz = u128_clz(mant);
    int shift = clz - (127 - 112);  /* = clz - 15 */

    if (shift > 0) {
        mant = u128_shl(mant, shift);
        exp  -= shift;
    } else if (shift < 0) {
        /* rounding shift right */
        int rsh = -shift;
        u128 sticky = u128_and(mant, u128_sub(u128_shl(u128_one(), rsh), u128_one()));
        mant = u128_shr(mant, rsh);
        /* round to nearest-even */
        u128 half = u128_shl(u128_one(), rsh - 1);
        u128 lsb  = u128_one();
        if (u128_gt(sticky, half) || (u128_eq(sticky, half) && !u128_is_zero(u128_and(mant, lsb))))
            mant = u128_inc(mant);
        /* re-check normalisation after rounding */
        if (!u128_is_zero(u128_shr(mant, 113))) { mant = u128_shr(mant, 1); exp++; }
        exp += rsh;
    }

    /* Handle subnormal range */
    if (exp <= -16383) {
        /* subnormal: shift mant right to keep true exponent = -16382 */
        int subnorm_shift = -16382 - exp + 1;
        if (subnorm_shift >= 113) {
            /* underflow to zero */
            hi = (uint64_t)sign << 63;
            lo = 0;
            memcpy(r.bytes,   &lo, 8);
            memcpy(r.bytes+8, &hi, 8);
            return r;
        }
        mant = u128_shr(mant, subnorm_shift);
        /* biased = 0 for subnormals */
        uint64_t mhi = u128_hi(mant) & 0x0000FFFFFFFFFFFFull;
        uint64_t mlo = u128_lo(mant);
        hi = ((uint64_t)sign << 63) | mhi;
        lo = mlo;
        memcpy(r.bytes,   &lo, 8);
        memcpy(r.bytes+8, &hi, 8);
        return r;
    }

    int32_t biased = exp + 16383;
    if (biased >= 0x7FFF) {
        /* overflow to infinity */
        hi = ((uint64_t)sign << 63) | ((uint64_t)0x7FFF << 48);
        lo = 0;
        memcpy(r.bytes,   &lo, 8);
        memcpy(r.bytes+8, &hi, 8);
        return r;
    }

    /* strip implicit leading 1 (bit 112) and store */
    mant = u128_and(mant, u128_sub(u128_shl(u128_one(), 112), u128_one()));
    uint64_t mhi = u128_hi(mant) & 0x0000FFFFFFFFFFFFull;
    uint64_t mlo = u128_lo(mant);
    hi = ((uint64_t)sign << 63) | ((uint64_t)biased << 48) | mhi;
    lo = mlo;
    memcpy(r.bytes,   &lo, 8);
    memcpy(r.bytes+8, &hi, 8);
    return r;
}

/* Build special values */
static lf_float128 f128_make_nan(void) {
    lf_float128 r;
    uint64_t hi = 0x7FFF800000000000ull, lo = 0;
    memcpy(r.bytes,   &lo, 8);
    memcpy(r.bytes+8, &hi, 8);
    return r;
}
static lf_float128 f128_make_inf(int sign) {
    lf_float128 r;
    uint64_t hi = ((uint64_t)sign << 63) | ((uint64_t)0x7FFF << 48);
    uint64_t lo = 0;
    memcpy(r.bytes,   &lo, 8);
    memcpy(r.bytes+8, &hi, 8);
    return r;
}
static lf_float128 f128_make_zero(int sign) {
    lf_float128 r;
    uint64_t hi = (uint64_t)sign << 63, lo = 0;
    memcpy(r.bytes,   &lo, 8);
    memcpy(r.bytes+8, &hi, 8);
    return r;
}

/* Is v a NaN / Inf / Zero? */
static int f128_is_nan (lf_float128 v) { return f128_unpack(v).is_nan;  }
static int f128_is_inf (lf_float128 v) { return f128_unpack(v).is_inf;  }
static int f128_is_zero(lf_float128 v) { return f128_unpack(v).is_zero; }
static int f128_sign   (lf_float128 v) { return f128_unpack(v).sign;    }

/* ========================================================================
 * C. Constants (commonly-needed fp128 values)
 * ======================================================================== */

/* These are computed lazily and cached. We build them from double. */
/* Exact constant packer: given biased exp and 112-bit mantissa as two u64 */
static lf_float128 f128_const(int sign, int32_t biased,
                               uint64_t mhi, uint64_t mlo) {
    lf_float128 r;
    uint64_t hi = ((uint64_t)sign << 63) | ((uint64_t)biased << 48) | (mhi & 0x0000FFFFFFFFFFFFull);
    uint64_t lo = mlo;
    memcpy(r.bytes,   &lo, 8);
    memcpy(r.bytes+8, &hi, 8);
    return r;
}

/* 1.0 */
static lf_float128 f128_one(void)  { return f128_const(0, 16383, 0, 0); }
/* 0.5 */
static lf_float128 f128_half(void) { return f128_const(0, 16382, 0, 0); }
/* 2.0 */
static lf_float128 f128_two(void)  { return f128_const(0, 16384, 0, 0); }

/* ========================================================================
 * D. Comparisons
 * ======================================================================== */

/* Internal compare: returns -1, 0, +1. NaN inputs return +2 (unordered). */
static int f128_cmp_internal(lf_float128 a, lf_float128 b) {
    f128_parts pa = f128_unpack(a), pb = f128_unpack(b);
    if (pa.is_nan || pb.is_nan) return 2;

    int as = pa.sign, bs = pb.sign;

    /* Both zero: equal regardless of sign */
    if (pa.is_zero && pb.is_zero) return 0;
    if (pa.is_zero) return bs ? 1 : -1;  /* b<0: a>b; b>0: a<b */
    if (pb.is_zero) return as ? -1 : 1;

    /* Different signs */
    if (as != bs) return as ? -1 : 1;

    /* Same sign: compare magnitude */
    int mag;
    if (pa.is_inf && pb.is_inf) mag = 0;
    else if (pa.is_inf)         mag = 1;
    else if (pb.is_inf)         mag = -1;
    else if (pa.exp != pb.exp)  mag = pa.exp > pb.exp ? 1 : -1;
    else                        mag = u128_gt(pa.mant, pb.mant) ? 1 : u128_lt(pa.mant, pb.mant) ? -1 : 0;

    return as ? -mag : mag;  /* if negative, flip */
}

#if !defined(__wasm32__)
int __eqtf2   (lf_float128 a, lf_float128 b) { return f128_cmp_internal(a,b) == 0 ? 0 : 1; }
int __netf2   (lf_float128 a, lf_float128 b) { return f128_cmp_internal(a,b) != 0 ? 1 : 0; }
int __lttf2   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?1:c; }
int __letf2   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?1:c; }
int __gttf2   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?1:c; }
int __getf2   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?1:c; }
int __unordtf2(lf_float128 a, lf_float128 b) { return (f128_is_nan(a)||f128_is_nan(b))?1:0; }
#endif

/* ========================================================================
 * E. Negation / Abs
 * ======================================================================== */

lf_float128 __negtf2_lf_impl(lf_float128 a) {
    lf_float128 r = a;
    r.bytes[15] ^= 0x80;  /* flip sign bit */
    return r;
}

lf_float128 lf_f128_abs(lf_float128 a) {
    lf_float128 r = a;
    r.bytes[15] &= 0x7F;  /* clear sign bit */
    return r;
}

/* ========================================================================
 * F. Addition / Subtraction
 * ======================================================================== */

/* Add two values with the same sign. pa.exp >= pb.exp guaranteed. */
static lf_float128 f128_add_same_sign(f128_parts pa, f128_parts pb) {
    /* Align significands */
    int shift = pa.exp - pb.exp;
    u128 mb = (shift < 128) ? u128_shr(pb.mant, shift) : u128_zero();
    u128 sum = u128_add(pa.mant, mb);
    int32_t exp = pa.exp;

    /* Check carry out of bit 113 */
    if (!u128_is_zero(u128_shr(sum, 113))) {
        sum = u128_shr(sum, 1);
        exp++;
    }
    return f128_pack_parts(pa.sign, exp, sum);
}

/* Subtract smaller from larger (same sign but different magnitudes).
 * |pa| > |pb|, result sign = result_sign. */
static lf_float128 f128_sub_mag(f128_parts pa, f128_parts pb, int result_sign) {
    int shift = pa.exp - pb.exp;
    u128 mb = (shift < 128) ? u128_shr(pb.mant, shift) : u128_zero();
    u128 diff = u128_sub(pa.mant, mb);
    return f128_pack_parts(result_sign, pa.exp, diff);
}

lf_float128 __addtf3_lf_impl(lf_float128 a, lf_float128 b) {
    f128_parts pa = f128_unpack(a), pb = f128_unpack(b);

    if (pa.is_nan) return a;
    if (pb.is_nan) return b;
    if (pa.is_inf && pb.is_inf) {
        if (pa.sign != pb.sign) return f128_make_nan();  /* +inf + -inf */
        return a;
    }
    if (pa.is_inf) return a;
    if (pb.is_inf) return b;
    if (pa.is_zero && pb.is_zero) return f128_make_zero(pa.sign & pb.sign);
    if (pa.is_zero) return b;
    if (pb.is_zero) return a;

    /* Sort so pa has the larger (or equal) magnitude */
    int swapped = 0;
    if (pa.exp < pb.exp || (pa.exp == pb.exp && u128_lt(pa.mant, pb.mant))) {
        f128_parts tmp = pa; pa = pb; pb = tmp;
        swapped = 1;
    }

    if (pa.sign == pb.sign) {
        return f128_add_same_sign(pa, pb);
    } else {
        /* Subtraction: pa.mant >= pb.mant (by sort above) */
        if (u128_eq(pa.mant, pb.mant) && pa.exp == pb.exp) return f128_make_zero(0);
        int rsign = swapped ? pb.sign : pa.sign;
        return f128_sub_mag(pa, pb, rsign);
    }
}

lf_float128 __subtf3_lf_impl(lf_float128 a, lf_float128 b) {
    /* flip sign of b then add */
    return __addtf3_lf_impl(a, __negtf2_lf_impl(b));
}

/* ========================================================================
 * G. Multiplication
 * ======================================================================== */

lf_float128 __multf3_lf_impl(lf_float128 a, lf_float128 b) {
    f128_parts pa = f128_unpack(a), pb = f128_unpack(b);
    int rsign = pa.sign ^ pb.sign;

    if (pa.is_nan) return a;
    if (pb.is_nan) return b;
    if (pa.is_inf || pb.is_inf) {
        if (pa.is_zero || pb.is_zero) return f128_make_nan();  /* 0 * inf */
        return f128_make_inf(rsign);
    }
    if (pa.is_zero || pb.is_zero) return f128_make_zero(rsign);

    /* Full 113×113 -> 226-bit product via 128×128 -> 256-bit */
    u128 prod_hi, prod_lo;
    u128_mul128(pa.mant, pb.mant, &prod_hi, &prod_lo);

    /* Product has 226 significant bits (bit 224 is the leading 1 for max case).
     * We need 113 bits. The product's binary point is at position 224
     * (since each operand has its 1 in bit 112).
     * Normalise: shift right so leading 1 is at bit 112. */
    int32_t exp = pa.exp + pb.exp;

    /* Leading bit of product is in prod_hi. Find its position. */
    int lz = u128_clz(prod_hi);
    /* bit position of leading 1 in 256-bit: (127 - lz) + 128 = 255 - lz */
    /* We want it at bit 112 of the final 128-bit mant: shift right by (255-lz-112) */
    int rsh = 255 - lz - 112;  /* how many bits to shift the 256-bit product right */

    u128 result_mant;
    if (rsh >= 128) {
        /* result fits entirely in prod_hi */
        int rsh2 = rsh - 128;
        result_mant = u128_shr(prod_hi, rsh2);
        /* rounding bit is (prod_hi >> (rsh2-1)) & 1 if rsh2>0, else prod_lo's top */
        int round_bit = rsh2 > 0 ? u128_bit(prod_hi, rsh2 - 1) : u128_bit(prod_lo, 127);
        u128 sticky    = rsh2 > 1
                       ? u128_and(prod_hi, u128_sub(u128_shl(u128_one(), rsh2 - 1), u128_one()))
                       : u128_zero();
        sticky = u128_or(sticky, prod_lo);
        if (round_bit && (!u128_is_zero(sticky) || !u128_is_zero(u128_and(result_mant, u128_one())))) {
            result_mant = u128_inc(result_mant);
        }
    } else {
        /* straddling prod_hi and prod_lo */
        result_mant = u128_or(u128_shl(prod_hi, 128 - rsh), u128_shr(prod_lo, rsh));
        int round_bit = u128_bit(prod_lo, rsh - 1);
        u128 sticky    = u128_and(prod_lo, u128_sub(u128_shl(u128_one(), rsh - 1), u128_one()));
        if (round_bit && (!u128_is_zero(sticky) || !u128_is_zero(u128_and(result_mant, u128_one())))) {
            result_mant = u128_inc(result_mant);
        }
    }
    exp += rsh - 112;  /* correct for the shift */

    return f128_pack_parts(rsign, exp, result_mant);
}

/* ========================================================================
 * H. Division
 * ======================================================================== */

lf_float128 __divtf3_lf_impl(lf_float128 a, lf_float128 b) {
    f128_parts pa = f128_unpack(a), pb = f128_unpack(b);
    int rsign = pa.sign ^ pb.sign;

    if (pa.is_nan) return a;
    if (pb.is_nan) return b;
    if (pb.is_zero) {
        if (pa.is_zero) return f128_make_nan();   /* 0/0 */
        return f128_make_inf(rsign);
    }
    if (pa.is_inf) {
        if (pb.is_inf) return f128_make_nan();    /* inf/inf */
        return f128_make_inf(rsign);
    }
    if (pa.is_zero) return f128_make_zero(rsign);
    if (pb.is_inf)  return f128_make_zero(rsign);

    /* Long division via Newton-Raphson reciprocal approximation:
     * q = a/b = a * (1/b)
     * Start from double-precision reciprocal, refine with 3 NR steps
     * (each step doubles precision; 53 + 2*53 = 159 > 113 bits after 2 steps,
     *  a third step gives ~200 bits sufficient for correct rounding).
     *
     * NR step: r_{n+1} = r_n * (2 - b * r_n)
     */

    /* Initial approximation: 1/b in double */
    int32_t exp_a = pa.exp, exp_b = pb.exp;
    u128  mant_a  = pa.mant, mant_b = pb.mant;

    /* Scale both to a common exponent for the reciprocal computation.
     * We compute (mant_a / mant_b) as a 113-bit fixed-point value,
     * then adjust the exponent. */

    /* Use 256-bit shifted dividend for accuracy:
     * Shift mant_a left by 112 bits into a 256-bit dividend.
     * Divide by mant_b (113 bits) to get a 113-bit quotient + remainder. */

    /* Represent dividend as (mant_a << 112) */
    /* Long division: quotient = (div_hi:div_lo) / mant_b
     * We need 113 bits of quotient. Use iterative bit-by-bit or
     * restoring division with 128-bit intermediates. */

    /* Simpler: use the compiler's 128-bit division where possible.
     * div_hi < mant_b (since mant_a ~ mant_b in magnitude for normalised inputs),
     * so we can use: quotient = (div_hi * 2^64 + div_hi_lo) / mant_b
     * iteratively in 64-bit chunks.
     *
     * Actually, for our case mant_a and mant_b are both in [2^112, 2^113).
     * So mant_a/mant_b ∈ [0.5, 2).
     * After one shift: (mant_a << 112) / mant_b gives a 112-bit or 113-bit quotient.
     */

    /* Method: 113-step binary restoring division */
    u128 remainder = mant_a;
    u128 quotient  = u128_zero();
    /* We want quotient = floor(mant_a * 2^113 / mant_b), giving 113 bits */
    for (int i = 0; i < 113; i++) {
        remainder = u128_shl1(remainder);
        quotient  = u128_shl1(quotient);
        if (u128_gte(remainder, mant_b)) {
            remainder = u128_sub(remainder, mant_b);
            quotient  = u128_or(quotient, u128_one());
        }
    }
    /* Round: if remainder*2 >= mant_b, round up */
    if (u128_gte(u128_shl1(remainder), mant_b)) quotient = u128_inc(quotient);

    int32_t rexp = exp_a - exp_b - 1;  /* -1 because quotient has an extra factor of 2 */

    return f128_pack_parts(rsign, rexp, quotient);
}

/* ========================================================================
 * I. Conversions
 * ======================================================================== */

lf_float128 __extenddftf2_lf_impl(double d) {
    /* Reinterpret double as bits */
    uint64_t bits;
    memcpy(&bits, &d, 8);
    int      sign   = (int)(bits >> 63);
    int32_t  dexp   = (int32_t)((bits >> 52) & 0x7FF);
    uint64_t dmant  = bits & 0x000FFFFFFFFFFFFFull;

    if (dexp == 0x7FF) {
        if (dmant) return f128_make_nan();
        return f128_make_inf(sign);
    }
    if (!dexp && !dmant) return f128_make_zero(sign);

    u128 mant;
    int32_t exp;
    if (dexp) {
        /* normal double */
        mant = u128_shl(u128_from_u64(dmant | (1ull << 52)), 112 - 52);  /* align to bit 112 */
        exp  = dexp - 1023;
    } else {
        /* subnormal double */
        mant = u128_shl(u128_from_u64(dmant), 112 - 52);
        exp  = -1022 - 52;  /* will be corrected by pack_parts normalisation */
    }
    return f128_pack_parts(sign, exp, mant);
}

lf_float128 __extendsftf2_lf_impl(float f) {
    uint32_t bits;
    memcpy(&bits, &f, 4);
    int      sign  = (int)(bits >> 31);
    int32_t  fexp  = (int32_t)((bits >> 23) & 0xFF);
    uint32_t fmant = bits & 0x007FFFFFu;

    if (fexp == 0xFF) {
        if (fmant) return f128_make_nan();
        return f128_make_inf(sign);
    }
    if (!fexp && !fmant) return f128_make_zero(sign);

    u128 mant;
    int32_t exp;
    if (fexp) {
        mant = u128_shl(u128_from_u64(fmant | (1u << 23)), 112 - 23);
        exp  = fexp - 127;
    } else {
        mant = u128_shl(u128_from_u64(fmant), 112 - 23);
        exp  = -126 - 23;
    }
    return f128_pack_parts(sign, exp, mant);
}

double __trunctfdf2_lf_impl(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return NAN;
    if (p.is_inf) return p.sign ? -INFINITY : INFINITY;
    if (p.is_zero) return p.sign ? -0.0 : 0.0;

    /* Shift mant right to get 52 bits + rounding */
    int32_t dexp = p.exp + 1023;
    if (dexp >= 0x7FF) return p.sign ? -INFINITY : INFINITY;  /* overflow */
    if (dexp <= 0) {
        /* subnormal double: shift further */
        int extra_shift = 1 - dexp;
        int rsh = (112 - 52) + extra_shift;
        if (rsh >= 128) return p.sign ? -0.0 : 0.0;
        uint64_t dmant = u128_lo(u128_shr(p.mant, rsh));
        uint64_t bits  = ((uint64_t)p.sign << 63) | dmant;
        double r; memcpy(&r, &bits, 8); return r;
    }
    int rsh = 112 - 52;
    uint64_t dmant = u128_lo(u128_shr(p.mant, rsh)) & 0x000FFFFFFFFFFFFFull;
    /* rounding */
    int round_bit = u128_bit(p.mant, rsh - 1);
    u128 sticky    = u128_and(p.mant, u128_sub(u128_shl(u128_one(), rsh - 1), u128_one()));
    if (round_bit && (!u128_is_zero(sticky) || (dmant & 1))) dmant++;
    if (dmant >> 52) { dmant >>= 1; dexp++; }
    uint64_t bits = ((uint64_t)p.sign << 63) | ((uint64_t)dexp << 52) | dmant;
    double r; memcpy(&r, &bits, 8); return r;
}

float __trunctfsf2_lf_impl(lf_float128 a) {
    return (float)__trunctfdf2_lf_impl(a);  /* double handles the precision */
}

lf_float128 __floatsitf_lf_impl(int32_t x) {
    if (!x) return f128_make_zero(0);
    int sign = x < 0 ? 1 : 0;
    uint64_t abs_x = sign ? (uint64_t)(-(int64_t)x) : (uint64_t)x;
    u128 mant = u128_from_u64(abs_x);
    int clz   = u128_clz(mant);
    mant    = u128_shl(mant, clz);        /* normalise to have leading 1 in bit 127 */
    int32_t exp = 127 - clz;
    /* pack_parts expects leading 1 in bit 112 */
    mant = u128_shl(mant, 112 - 127 + clz);  /* re-align */
    /* Actually, simpler: */
    int lz32 = lf_clz32((uint32_t)abs_x);
    mant = u128_shl(u128_from_u64(abs_x), 112 - (31 - lz32));
    exp  = 31 - lz32;
    return f128_pack_parts(sign, exp, mant);
}

lf_float128 __floatditf_lf_impl(int64_t x) {
    if (!x) return f128_make_zero(0);
    int sign = x < 0 ? 1 : 0;
    uint64_t abs_x = sign ? (uint64_t)(-x) : (uint64_t)x;
    int lz   = lf_clz64(abs_x);
    int32_t exp = 63 - lz;
    u128 mant = u128_shl(u128_from_u64(abs_x), 112 - exp);
    return f128_pack_parts(sign, exp, mant);
}

lf_float128 __floatunditf_lf_impl(uint64_t x) {
    if (!x) return f128_make_zero(0);
    int lz  = lf_clz64(x);
    int32_t exp = 63 - lz;
    u128 mant = u128_shl(u128_from_u64(x), 112 - exp);
    return f128_pack_parts(0, exp, mant);
}

int32_t __fixtfsi_lf_impl(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf || p.is_zero) return 0;
    if (p.exp < 0) return 0;
    if (p.exp > 30) return p.sign ? (int32_t)0x80000000 : (int32_t)0x7FFFFFFF;
    uint32_t v = (uint32_t)u128_lo(u128_shr(p.mant, 112 - p.exp));
    return p.sign ? -(int32_t)v : (int32_t)v;
}

int64_t __fixtfdi_lf_impl(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf || p.is_zero) return 0;
    if (p.exp < 0) return 0;
    if (p.exp > 62) return p.sign ? (int64_t)0x8000000000000000LL : (int64_t)0x7FFFFFFFFFFFFFFFLL;
    uint64_t v = u128_lo(u128_shr(p.mant, 112 - p.exp));
    return p.sign ? -(int64_t)v : (int64_t)v;
}

/* ========================================================================
 * J. Math: sqrt, floor, ceiling, mod, pow
 * ======================================================================== */

lf_float128 lf_f128_sqrt(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.sign) return f128_make_nan();
    if (p.is_zero) return a;
    if (p.is_inf) return a;

    /* Newton-Raphson: x_{n+1} = (x_n + a/x_n) / 2
     * Start from double sqrt, then refine. */

    /* Initial guess from double */
    double da = __trunctfdf2_lf_impl(a);
    double dsqrt;
    /* Use __builtin_sqrt if available, else manual Newton in double */
    {
        /* 2 Newton steps in double to get ~53 bits */
        dsqrt = da;
        /* rough initial: just use the double approximation */
        /* We'll use the fp128 Newton-Raphson from here */
    }
    /* Better: start from exponent midpoint */
    int32_t half_exp = (p.exp >> 1);
    u128 half_mant   = u128_shl(u128_one(), 112);  /* 1.0 */
    lf_float128 x    = f128_pack_parts(0, half_exp, half_mant);

    /* Refine with Newton: x = (x + a/x) / 2, 7 iterations for 113+ bits */
    lf_float128 two = f128_two();
    for (int i = 0; i < 7; i++) {
        lf_float128 ax = __divtf3_lf_impl(a, x);
        lf_float128 s  = __addtf3_lf_impl(x, ax);
        x = __divtf3_lf_impl(s, two);
    }
    return x;
}

lf_float128 lf_f128_floor(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf || p.is_zero) return a;

    if (p.exp >= 112) return a;  /* already an integer */
    if (p.exp < 0) {
        /* |a| < 1: floor is 0 or -1 */
        if (!p.sign) return f128_make_zero(0);
        return __negtf2_lf_impl(f128_one());  /* -1.0 */
    }

    /* Zero the fractional bits (bits 0 .. 111-exp) */
    int frac_bits = 112 - p.exp;
    u128 mask = u128_not(u128_sub(u128_shl(u128_one(), frac_bits), u128_one()));
    u128 trunc_mant = u128_and(p.mant, mask);
    lf_float128 trunc = f128_pack_parts(p.sign, p.exp, trunc_mant);

    if (!p.sign || u128_eq(trunc_mant, p.mant)) return trunc;
    /* negative and had fractional part: floor = trunc - 1 */
    return __subtf3_lf_impl(trunc, f128_one());
}

lf_float128 lf_f128_ceiling(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf || p.is_zero) return a;
    if (p.exp >= 112) return a;
    if (p.exp < 0) {
        if (p.sign) return f128_make_zero(0);
        return f128_one();
    }
    int frac_bits = 112 - p.exp;
    u128 mask     = u128_not(u128_sub(u128_shl(u128_one(), frac_bits), u128_one()));
    u128 trunc_mant = u128_and(p.mant, mask);
    lf_float128 trunc = f128_pack_parts(p.sign, p.exp, trunc_mant);

    if (p.sign || u128_eq(trunc_mant, p.mant)) return trunc;
    return __addtf3_lf_impl(trunc, f128_one());
}

lf_float128 lf_f128_mod(lf_float128 a, lf_float128 b) {
    /* a - floor(a/b)*b */
    lf_float128 q = __divtf3_lf_impl(a, b);
    lf_float128 qf = lf_f128_floor(q);
    return __subtf3_lf_impl(a, __multf3_lf_impl(qf, b));
}

lf_float128 lf_f128_pow(lf_float128 base, lf_float128 exp_v) {
    f128_parts ep = f128_unpack(exp_v);
    /* Special cases */
    if (ep.is_zero) return f128_one();         /* x^0 = 1 */
    if (f128_is_zero(base)) return f128_make_zero(0);

    /* Integer exponent: use fast repeated squaring */
    if (ep.exp >= 0 && ep.exp < 63) {
        int64_t n = __fixtfdi_lf_impl(exp_v);
        lf_float128 frac_part = __subtf3_lf_impl(exp_v, __floatditf_lf_impl(n));
        int is_int = f128_is_zero(frac_part);
        if (is_int) {
            int neg = (n < 0);
            uint64_t un = neg ? (uint64_t)(-n) : (uint64_t)n;
            lf_float128 result = f128_one();
            lf_float128 b2 = base;
            while (un) {
                if (un & 1) result = __multf3_lf_impl(result, b2);
                b2 = __multf3_lf_impl(b2, b2);
                un >>= 1;
            }
            if (neg) result = __divtf3_lf_impl(f128_one(), result);
            return result;
        }
    }
    /* General: a^b = exp(b * ln(a)) */
    return lf_f128_exp(__multf3_lf_impl(exp_v, lf_f128_log(base)));
}

/* ========================================================================
 * K. Math: exp, log, log2, log10
 *
 * Algorithm: argument reduction + Taylor/Padé series.
 *
 * exp(x): reduce x = n*ln2 + r, |r| <= ln2/2.
 *         exp(x) = 2^n * exp(r). Use Taylor for exp(r).
 *
 * log(x): reduce x = 2^n * m, m ∈ [1,2).
 *         log(x) = n*log(2) + log(m).
 *         log(m): let u=(m-1)/(m+1), log(m)=2*atanh(u).
 *
 * Constants are given as exact binary128 bit patterns.
 * ======================================================================== */

/* ln(2) = 0.6931471805599453094172321214581765680755... */
/* biased exp: 16382 (true exp -1), mant bits */
static lf_float128 f128_ln2(void) {
    return f128_const(0, 16382,
        0x00002C5C85FDF473U, 0xDE3A68C90C02396EU);
}

/* 1/ln(2) */
static lf_float128 f128_inv_ln2(void) {
    return f128_const(0, 16383,
        0x000071547652B82FU, 0xE1777D0FFDA0D23AU);
}

/* log10(e) = 1/ln(10) = 0.4342944819... */
static lf_float128 f128_log10e(void) {
    return f128_const(0, 16381,
        0x0000BCB7B1526E50U, 0xE32A6AB7555F5A68U);
}

/* log2(e) = 1/ln(2) -- same as inv_ln2 */

/* pi = 3.14159265358979... */
static lf_float128 f128_pi(void) {
    return f128_const(0, 16384,
        0x00001921FB54442DU, 0x18469898CC51701BU);
}

/* pi/2 */
static lf_float128 f128_pi_2(void) {
    return f128_const(0, 16383,
        0x00001921FB54442DU, 0x18469898CC51701BU);
}

/* pi/4 */
static lf_float128 f128_pi_4(void) {
    return f128_const(0, 16382,
        0x00001921FB54442DU, 0x18469898CC51701BU);
}

/* Evaluate polynomial: coeffs[0] + coeffs[1]*x + ... + coeffs[n-1]*x^(n-1)
 * Uses Horner's method. coeffs[n-1] is the highest-degree coefficient. */
static lf_float128 poly_eval(lf_float128 x, const lf_float128 *c, int n) {
    lf_float128 r = c[n-1];
    for (int i = n-2; i >= 0; i--) {
        r = __addtf3_lf_impl(__multf3_lf_impl(r, x), c[i]);
    }
    return r;
}

lf_float128 lf_f128_exp(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (p.is_inf) return p.sign ? f128_make_zero(0) : a;
    if (p.is_zero) return f128_one();

    /* Overflow/underflow bounds: exp(11356) > max_f128, exp(-11357) < min_subnormal */
    if (p.exp > 13) {   /* |a| > 8192 roughly */
        double da = __trunctfdf2_lf_impl(a);
        if (da > 11356.0) return f128_make_inf(0);
        if (da < -11357.0) return f128_make_zero(0);
    }

    /* Reduce: a = n*ln2 + r, n = round(a/ln2), |r| <= ln2/2 */
    lf_float128 inv_ln2 = f128_inv_ln2();
    lf_float128 ln2     = f128_ln2();

    /* n = nint(a / ln2) -- round to nearest integer */
    lf_float128 fn = __multf3_lf_impl(a, inv_ln2);
    int64_t n = __fixtfdi_lf_impl(__addtf3_lf_impl(fn,
                    f128_const(p.sign, 16382, 0, 0)));  /* fn + 0.5*sign, then trunc */
    /* simpler: */
    {
        double dv = __trunctfdf2_lf_impl(fn);
        n = (int64_t)(dv >= 0 ? (int64_t)(dv + 0.5) : (int64_t)(dv - 0.5));
    }
    lf_float128 fn128 = __floatditf_lf_impl(n);
    /* r = a - n*ln2 */
    lf_float128 r = __subtf3_lf_impl(a, __multf3_lf_impl(fn128, ln2));

    /* Taylor series for exp(r) - 1, r is small (|r| <= ln2/2 ~ 0.347):
     * exp(r) = 1 + r + r^2/2! + r^3/3! + ... + r^k/k!
     * For |r| < 0.5, 28 terms give > 115 bits of accuracy.
     */
    /* Coefficients: 1/k! for k=0..27 */
    /* We precompute as double and convert — sufficient since we're summing small terms */
    /* Actually: compute iteratively: term = term * r / k */
    lf_float128 sum  = f128_one();
    lf_float128 term = f128_one();
    for (int k = 1; k <= 28; k++) {
        /* term *= r / k */
        lf_float128 kf = __floatsitf_lf_impl(k);
        term = __divtf3_lf_impl(__multf3_lf_impl(term, r), kf);
        sum  = __addtf3_lf_impl(sum, term);
        /* early termination when term becomes negligible */
        f128_parts tp = f128_unpack(term);
        f128_parts sp = f128_unpack(sum);
        if (!tp.is_zero && !sp.is_zero && (sp.exp - tp.exp) > 115) break;
    }

    /* Scale: exp(a) = exp(r) * 2^n */
    f128_parts rp = f128_unpack(sum);
    if (rp.is_zero || rp.is_nan || rp.is_inf) return sum;
    return f128_pack_parts(rp.sign, rp.exp + (int32_t)n, rp.mant);
}

lf_float128 lf_f128_log(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (p.sign && !p.is_zero) return f128_make_nan();  /* log(negative) */
    if (p.is_zero) return f128_make_inf(1);             /* log(0) = -inf */
    if (p.is_inf) return a;

    /* Reduce: a = 2^n * m, m ∈ [1, 2)
     * log(a) = n*ln(2) + log(m)
     * Let u = (m-1)/(m+1), log(m) = 2*atanh(u) = 2*(u + u^3/3 + u^5/5 + ...)
     * u = (m-1)/(m+1) ∈ [0, 1/3) since m ∈ [1, 2).
     */
    int32_t n = p.exp;  /* exponent, so m = a * 2^(-n) ∈ [1,2) */
    /* Build m = a with exponent forced to 0 */
    lf_float128 m = f128_pack_parts(0, 0, p.mant);  /* m ∈ [1, 2) */

    /* u = (m - 1) / (m + 1) */
    lf_float128 one = f128_one();
    lf_float128 num = __subtf3_lf_impl(m, one);
    lf_float128 den = __addtf3_lf_impl(m, one);
    lf_float128 u   = __divtf3_lf_impl(num, den);
    lf_float128 u2  = __multf3_lf_impl(u, u);

    /* log(m) = 2 * (u + u^3/3 + u^5/5 + ... + u^(2k-1)/(2k-1))
     * |u| < 1/3, so convergence is fast. 57 terms give > 115 bits. */
    lf_float128 term  = u;
    lf_float128 sum   = u;
    for (int k = 1; k <= 57; k++) {
        term = __multf3_lf_impl(term, u2);
        lf_float128 kf = __floatsitf_lf_impl(2*k + 1);
        lf_float128 t  = __divtf3_lf_impl(term, kf);
        sum = __addtf3_lf_impl(sum, t);
        f128_parts tp = f128_unpack(t), sp = f128_unpack(sum);
        if (!tp.is_zero && !sp.is_zero && (sp.exp - tp.exp) > 115) break;
    }
    /* log(m) = 2*sum */
    lf_float128 log_m = __multf3_lf_impl(f128_two(), sum);

    /* log(a) = n*ln(2) + log(m) */
    lf_float128 n128 = __floatditf_lf_impl((int64_t)n);
    return __addtf3_lf_impl(__multf3_lf_impl(n128, f128_ln2()), log_m);
}

lf_float128 lf_f128_log2(lf_float128 a) {
    /* log2(a) = log(a) / log(2) = log(a) * inv_ln2 */
    return __multf3_lf_impl(lf_f128_log(a), f128_inv_ln2());
}

lf_float128 lf_f128_log10(lf_float128 a) {
    return __multf3_lf_impl(lf_f128_log(a), f128_log10e());
}

/* ========================================================================
 * L. Math: sin, cos, tan, asin, acos, atan, atan2
 * ======================================================================== */

/* sin/cos via argument reduction to [-pi/4, pi/4] + Taylor series.
 *
 * Reduce x to [0, 2*pi) first, then to [0, pi/4] with octant tracking.
 * Taylor: sin(r) = r - r^3/3! + r^5/5! - ...
 *         cos(r) = 1 - r^2/2! + r^4/4! - ...
 * |r| <= pi/4 ~ 0.785: 35 terms each give > 115 bits.
 */
static void sincos_kernel(lf_float128 r, lf_float128 *sn, lf_float128 *cs) {
    /* sin(r) = r*(1 - r^2/6*(1 - r^2/20*(1 - ...))) -- Horner-like */
    lf_float128 r2 = __multf3_lf_impl(r, r);

    lf_float128 s_term = r;
    lf_float128 s_sum  = r;
    lf_float128 c_term = f128_one();
    lf_float128 c_sum  = f128_one();

    for (int k = 1; k <= 35; k++) {
        /* sin: next term = prev_term * (-r^2) / ((2k)(2k+1)) */
        lf_float128 denom_s = __floatsitf_lf_impl((2*k)*(2*k+1));
        s_term = __divtf3_lf_impl(__multf3_lf_impl(s_term, __negtf2_lf_impl(r2)), denom_s);
        s_sum  = __addtf3_lf_impl(s_sum, s_term);

        /* cos: next term = prev_term * (-r^2) / ((2k-1)(2k)) */
        lf_float128 denom_c = __floatsitf_lf_impl((2*k-1)*(2*k));
        c_term = __divtf3_lf_impl(__multf3_lf_impl(c_term, __negtf2_lf_impl(r2)), denom_c);
        c_sum  = __addtf3_lf_impl(c_sum, c_term);

        f128_parts sp = f128_unpack(s_sum), st = f128_unpack(s_term);
        if (!st.is_zero && !sp.is_zero && (sp.exp - st.exp) > 115) break;
    }
    *sn = s_sum;
    *cs = c_sum;
}

/* Reduce x to octant. Returns reduced r ∈ [0,pi/4] and octant 0..7. */
static int sincos_reduce(lf_float128 x, lf_float128 *r) {
    lf_float128 two_pi = __multf3_lf_impl(f128_two(), __multf3_lf_impl(f128_two(), f128_pi_2()));
    lf_float128 pi     = __multf3_lf_impl(f128_two(), f128_pi_2());
    lf_float128 pi_2   = f128_pi_2();
    lf_float128 pi_4   = f128_pi_4();

    /* x modulo 2*pi */
    lf_float128 k = lf_f128_floor(__divtf3_lf_impl(x, two_pi));
    lf_float128 xr = __subtf3_lf_impl(x, __multf3_lf_impl(k, two_pi));

    /* Octant */
    int octant = 0;
    if (f128_cmp_internal(xr, pi) > 0)        { xr = __subtf3_lf_impl(xr, pi);  octant += 4; }
    if (f128_cmp_internal(xr, pi_2) > 0)      { xr = __subtf3_lf_impl(xr, pi_2); octant += 2; }
    if (f128_cmp_internal(xr, pi_4) > 0)      { xr = __subtf3_lf_impl(xr, pi_4); xr = __subtf3_lf_impl(pi_4, xr); octant += 1; }
    *r = xr;
    return octant;
}

lf_float128 lf_f128_sin(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return a;

    int sign = p.sign;
    lf_float128 ax = lf_f128_abs(a);
    lf_float128 r;
    int oct = sincos_reduce(ax, &r);
    lf_float128 s, c;
    sincos_kernel(r, &s, &c);

    /* Map octant to result */
    lf_float128 result;
    switch (oct & 3) {
        case 0: result = s; break;
        case 1: result = c; break;
        case 2: result = c; break;  /* sin(pi-x) = sin(x), but pi-x -> c */
        case 3: result = s; break;
        default: result = s; break;
    }
    /* Octant-based sign: sin is negative in [pi, 2*pi] (oct 4-7) */
    int rsign = sign ^ ((oct >= 4) ? 1 : 0) ^ ((oct == 2 || oct == 3) ? 1 : 0);
    if (rsign) result = __negtf2_lf_impl(result);
    return result;
}

lf_float128 lf_f128_cos(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return f128_one();

    lf_float128 ax = lf_f128_abs(a);
    lf_float128 r;
    int oct = sincos_reduce(ax, &r);
    lf_float128 s, c;
    sincos_kernel(r, &s, &c);

    lf_float128 result;
    switch (oct & 3) {
        case 0: result = c; break;
        case 1: result = s; break;
        case 2: result = s; break;
        case 3: result = c; break;
        default: result = c; break;
    }
    int rsign = (oct == 2 || oct == 3 || oct == 4 || oct == 5) ? 1 : 0;
    if (rsign) result = __negtf2_lf_impl(result);
    return result;
}

lf_float128 lf_f128_tan(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return a;
    /* tan = sin/cos */
    lf_float128 s = lf_f128_sin(a);
    lf_float128 c = lf_f128_cos(a);
    if (f128_is_zero(c)) return f128_make_inf(f128_sign(s) ^ f128_sign(c));
    return __divtf3_lf_impl(s, c);
}

/* atan via series: atan(x) = x - x^3/3 + x^5/5 - ...  for |x| <= 1
 * Reduce to |x|<=1 using atan(x)=pi/2-atan(1/x) for x>1. */
static lf_float128 atan_kernel(lf_float128 x) {
    /* |x| <= 1 guaranteed */
    lf_float128 x2   = __multf3_lf_impl(x, x);
    lf_float128 term  = x;
    lf_float128 sum   = x;
    for (int k = 1; k <= 57; k++) {
        term = __multf3_lf_impl(term, __negtf2_lf_impl(x2));
        lf_float128 kf = __floatsitf_lf_impl(2*k+1);
        lf_float128 t  = __divtf3_lf_impl(term, kf);
        sum = __addtf3_lf_impl(sum, t);
        f128_parts sp = f128_unpack(sum), tp = f128_unpack(t);
        if (!tp.is_zero && !sp.is_zero && (sp.exp - tp.exp) > 115) break;
    }
    return sum;
}

lf_float128 lf_f128_atan(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (p.is_inf) return p.sign ? __negtf2_lf_impl(f128_pi_2()) : f128_pi_2();
    if (p.is_zero) return a;

    int sign = p.sign;
    lf_float128 ax = lf_f128_abs(a);
    lf_float128 one = f128_one();

    lf_float128 result;
    if (f128_cmp_internal(ax, one) > 0) {
        /* |x| > 1: atan(x) = sign(x)*(pi/2 - atan(1/x)) */
        result = __subtf3_lf_impl(f128_pi_2(),
                     atan_kernel(__divtf3_lf_impl(one, ax)));
    } else {
        result = atan_kernel(ax);
    }
    return sign ? __negtf2_lf_impl(result) : result;
}

lf_float128 lf_f128_atan2(lf_float128 y, lf_float128 x) {
    f128_parts px = f128_unpack(x), py = f128_unpack(y);
    if (px.is_nan || py.is_nan) return f128_make_nan();

    int sx = px.sign, sy = py.sign;
    lf_float128 zero = f128_make_zero(0);
    lf_float128 pi   = __multf3_lf_impl(f128_two(), f128_pi_2());

    if (py.is_zero) {
        if (px.is_zero) return f128_make_nan();
        if (!sx) return sy ? __negtf2_lf_impl(zero) : zero;
        return sy ? __negtf2_lf_impl(pi) : pi;
    }
    if (px.is_zero) return sy ? __negtf2_lf_impl(f128_pi_2()) : f128_pi_2();
    if (px.is_inf && py.is_inf) {
        lf_float128 pi_4 = f128_pi_4();
        lf_float128 r = sx ? __multf3_lf_impl(f128_const(0,16385,0,0), pi_4)
                           : pi_4;
        return sy ? __negtf2_lf_impl(r) : r;
    }

    lf_float128 base = lf_f128_atan(__divtf3_lf_impl(y, x));
    if (!sx) return base;
    /* x < 0: add or subtract pi */
    lf_float128 adj = sy ? __negtf2_lf_impl(pi) : pi;
    return __addtf3_lf_impl(base, adj);
}

lf_float128 lf_f128_asin(lf_float128 a) {
    /* asin(x) = atan(x / sqrt(1-x^2)) */
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    lf_float128 one = f128_one();
    lf_float128 ax  = lf_f128_abs(a);
    if (f128_cmp_internal(ax, one) > 0) return f128_make_nan();
    if (f128_cmp_internal(ax, one) == 0) {
        return p.sign ? __negtf2_lf_impl(f128_pi_2()) : f128_pi_2();
    }
    lf_float128 t = lf_f128_sqrt(__subtf3_lf_impl(one, __multf3_lf_impl(a, a)));
    return lf_f128_atan(__divtf3_lf_impl(a, t));
}

lf_float128 lf_f128_acos(lf_float128 a) {
    /* acos(x) = pi/2 - asin(x) */
    return __subtf3_lf_impl(f128_pi_2(), lf_f128_asin(a));
}

/* ========================================================================
 * M. Math: sinh, cosh, tanh
 * ======================================================================== */

lf_float128 lf_f128_sinh(lf_float128 a) {
    /* sinh(x) = (exp(x) - exp(-x)) / 2 */
    lf_float128 ex  = lf_f128_exp(a);
    lf_float128 emx = lf_f128_exp(__negtf2_lf_impl(a));
    return __divtf3_lf_impl(__subtf3_lf_impl(ex, emx), f128_two());
}

lf_float128 lf_f128_cosh(lf_float128 a) {
    lf_float128 ex  = lf_f128_exp(a);
    lf_float128 emx = lf_f128_exp(__negtf2_lf_impl(a));
    return __divtf3_lf_impl(__addtf3_lf_impl(ex, emx), f128_two());
}

lf_float128 lf_f128_tanh(lf_float128 a) {
    /* tanh(x) = (e^2x - 1) / (e^2x + 1) */
    lf_float128 e2x = lf_f128_exp(__multf3_lf_impl(f128_two(), a));
    lf_float128 one = f128_one();
    return __divtf3_lf_impl(
        __subtf3_lf_impl(e2x, one),
        __addtf3_lf_impl(e2x, one));
}

/* ========================================================================
 * N. Formatting (Dragon4) — verbatim from lf_float128_to_str.c
 * ======================================================================== */

/* [Dragon4 big-integer infrastructure and lf_float128_to_str() are
 *  #include'd from the existing lf_float128_to_str.c to avoid duplication.
 *  In the final build, either include it here or compile both .c files.] */

#define BIG_MAX 640
typedef struct { uint64_t d[BIG_MAX]; int n; } Big;

static void big_zero(Big *a)               { a->n=1; a->d[0]=0; }
static void big_from_u128b(Big *a, uint64_t hi, uint64_t lo) {
    a->d[0]=lo; a->d[1]=hi; a->n=hi?2:1;
}
static void big_shl(Big *a, int shift) {
    if (!shift) return;
    int ws=shift>>6, bs=shift&63;
    int nn=a->n+ws+(bs?1:0); if(nn>BIG_MAX) nn=BIG_MAX;
    for(int i=nn-1;i>=0;i--){
        int src=i-ws;
        uint64_t hi_w=(src>=0&&src<a->n)?a->d[src]:0;
        uint64_t lo_w=(src-1>=0&&src-1<a->n)?a->d[src-1]:0;
        a->d[i]=bs?((hi_w<<bs)|(lo_w>>(64-bs))):hi_w;
    }
    a->n=nn; while(a->n>1&&!a->d[a->n-1]) a->n--;
}
static void big_mul_small(Big *a, uint32_t k) {
    uint64_t carry=0;
    for(int i=0;i<a->n;i++){
        uint64_t t_lo, t_hi;
        {
            u128 prod = mul64x64(a->d[i], (uint64_t)k);
            prod = u128_add(prod, u128_from_u64(carry));
            t_lo = prod.lo;
            t_hi = prod.hi;
        }
        a->d[i]=t_lo; carry=t_hi;
    }
    if(carry&&a->n<BIG_MAX) a->d[a->n++]=carry;
}
static int big_cmp(const Big *a, const Big *b) {
    int n=a->n>b->n?a->n:b->n;
    for(int i=n-1;i>=0;i--){
        uint64_t av=(i<a->n)?a->d[i]:0, bv=(i<b->n)?b->d[i]:0;
        if(av>bv) return 1; if(av<bv) return -1;
    }
    return 0;
}
static void big_sub(Big *a, const Big *b) {
    uint64_t borrow=0;
    for(int i=0;i<a->n;i++){
        uint64_t bv=(i<b->n)?b->d[i]:0;
        uint64_t sub = bv + borrow;
        uint64_t av = a->d[i];
        a->d[i] = av - sub;
        borrow = (av < sub) ? 1 : 0;
    }
    while(a->n>1&&!a->d[a->n-1]) a->n--;
}
static int big_digit(Big *a, const Big *b) {
    int q=0;
    while(big_cmp(a,b)>=0&&q<9){big_sub(a,(Big*)b);q++;}
    return q;
}
static int32_t floor_log10_pow2b(int32_t n) {
    if(!n) return 0;
    if(n>0) return (int32_t)((uint32_t)n*30103u/100000u);
    return -(int32_t)((uint32_t)(-n)*30103u/100000u+1);
}

void lf_float128_to_str(char *result, const uint8_t bytes[16]) {
    uint64_t lo_w, hi_w;
    memcpy(&lo_w, bytes, 8); memcpy(&hi_w, bytes+8, 8);
    int vsign=(int)(hi_w>>63);
    int32_t biased=(int32_t)((hi_w>>48)&0x7FFFu);
    uint64_t mhi=hi_w&0x0000FFFFFFFFFFFFull, mlo=lo_w;

    if(biased==0x7FFF){
        sprintf(result,(mhi||mlo)?"NaN":"%sInfinity",vsign?"-":"");
        return;
    }
    if(!biased&&!mhi&&!mlo){
        sprintf(result,"%s0.000000000000000000000000000000000E+0000",vsign?"-":"");
        return;
    }
    uint64_t sig_hi=mhi|(biased?(1ull<<48):0ull);
    uint64_t sig_lo=mlo;
    int32_t bin_exp=biased?(biased-16383-112):(1-16383-112);
    int32_t dec_exp=floor_log10_pow2b(112+bin_exp);

    Big R,S;
    big_from_u128b(&R,sig_hi,sig_lo); big_zero(&S); S.d[0]=1; S.n=1;
    if(bin_exp>=0) big_shl(&R,bin_exp); else big_shl(&S,-bin_exp);

    Big S_scaled=S;
    if(dec_exp>0){for(int32_t i=0;i<dec_exp;i++) big_mul_small(&S_scaled,10);}
    else if(dec_exp<0){for(int32_t i=0;i<-dec_exp;i++) big_mul_small(&R,10);}

    {Big S10=S_scaled; big_mul_small(&S10,10);
     if(big_cmp(&R,&S10)>=0){dec_exp++;S_scaled=S10;}
     else if(big_cmp(&R,&S_scaled)<0){dec_exp--;big_mul_small(&R,10);}}

    char digits[36]; int nd=0;
    digits[nd++]=(char)('0'+big_digit(&R,&S_scaled));
    for(int i=1;i<34;i++){big_mul_small(&R,10);digits[nd++]=(char)('0'+big_digit(&R,&S_scaled));}

    big_mul_small(&R,10);
    if(big_digit(&R,&S_scaled)>=5){
        for(int i=nd-1;i>=0;i--){
            int dv=(digits[i]-'0')+1; digits[i]=(char)('0'+dv%10);
            if(dv<10) break;
            if(i==0){memmove(digits+1,digits,nd-1);digits[0]='1';dec_exp++;break;}
        }
    }

    char *p=result;
    if(vsign) *p++='-';
    *p++=digits[0]; *p++='.';
    for(int i=1;i<34;i++) *p++=digits[i];
    int ae=dec_exp>=0?dec_exp:-dec_exp;
    sprintf(p,"E%c%04d",dec_exp>=0?'+':'-',ae);
}

/* ========================================================================
 * O. Parsing: decimal string -> binary128
 * ======================================================================== */

lf_float128 lf_float128_from_str(const char *s) {
    if (!s || !*s) return f128_make_zero(0);

    /* Skip whitespace */
    while (*s == ' ' || *s == '\t') s++;

    int sign = 0;
    if (*s == '-') { sign = 1; s++; }
    else if (*s == '+') { s++; }

    /* Special values */
    if (s[0]=='i'||s[0]=='I') return f128_make_inf(sign);
    if (s[0]=='n'||s[0]=='N') return f128_make_nan();

    /* Parse digits: integer part + optional fractional part */
    char intbuf[100]  = {0};
    char fracbuf[100] = {0};
    int  int_len  = 0;
    int  frac_len = 0;

    while (*s >= '0' && *s <= '9' && int_len < 99)  intbuf[int_len++]  = *s++;
    if (*s == '.') {
        s++;
        while (*s >= '0' && *s <= '9' && frac_len < 99) fracbuf[frac_len++] = *s++;
    }

    /* Exponent */
    int32_t dec_exp = 0;
    if (*s == 'e' || *s == 'E' || *s == 'd' || *s == 'D') {
        s++;
        int esign = 1;
        if (*s == '-') { esign = -1; s++; }
        else if (*s == '+') s++;
        int32_t ev = 0;
        while (*s >= '0' && *s <= '9') ev = ev*10 + (*s++ - '0');
        dec_exp = esign * ev;
    }

    /* Combine: number = intbuf.fracbuf * 10^dec_exp
     * Effective exponent after incorporating fracbuf length:
     *   value = (intbuf_as_int * 10^frac_len + fracbuf_as_int) * 10^(dec_exp - frac_len)
     */
    int32_t eff_exp = dec_exp - frac_len;

    /* Build the integer significand from intbuf+fracbuf using big integers */
    Big sig;
    big_zero(&sig);
    for (int i = 0; i < int_len; i++) {
        big_mul_small(&sig, 10);
        uint64_t carry = intbuf[i] - '0';
        for (int j = 0; j < sig.n || carry; j++) {
            if (j >= sig.n) sig.d[sig.n++] = 0;
            uint64_t sum_lo = sig.d[j] + carry;
            uint64_t sum_carry = (sum_lo < sig.d[j]) ? 1 : 0;
            sig.d[j] = sum_lo;
            carry = sum_carry;
        }
    }
    for (int i = 0; i < frac_len; i++) {
        big_mul_small(&sig, 10);
        uint64_t carry = fracbuf[i] - '0';
        for (int j = 0; j < sig.n || carry; j++) {
            if (j >= sig.n) sig.d[sig.n++] = 0;
            uint64_t sum_lo = sig.d[j] + carry;
            uint64_t sum_carry = (sum_lo < sig.d[j]) ? 1 : 0;
            sig.d[j] = sum_lo;
            carry = sum_carry;
        }
    }

    /* Check for zero */
    int all_zero = 1;
    for (int i = 0; i < sig.n; i++) if (sig.d[i]) { all_zero = 0; break; }
    if (all_zero) return f128_make_zero(sign);

    /* Multiply or divide sig by 10^|eff_exp| */
    if (eff_exp > 0) {
        for (int32_t i = 0; i < eff_exp && i < 5000; i++) big_mul_small(&sig, 10);
    }
    /* For eff_exp < 0: we need to divide. Represent as R/S = sig / 10^|eff_exp|
     * and use Dragon4-like extraction to get a binary128. */

    /* Convert big integer sig (possibly multiplied by 10^eff_exp) to binary128.
     * Strategy: find the leading bit position, extract 114 bits, then pack. */
    if (eff_exp >= 0) {
        /* sig is the integer value. Find its bit length. */
        int top_limb = sig.n - 1;
        int bit_pos  = top_limb * 64 + (63 - lf_clz64(sig.d[top_limb]));
        /* bit_pos = position of leading 1 (0-indexed) */
        int32_t exp = bit_pos;  /* true exponent */

        /* Extract 113 bits starting from bit_pos */
        u128 mant = u128_zero();
        int shift = bit_pos - 112;  /* how many bits to shift sig right */
        if (shift < 0) {
            /* sig has fewer than 113 bits: shift left */
            /* Build sig as u128 directly */
            uint64_t lo2 = sig.d[0];
            uint64_t hi2 = sig.n > 1 ? sig.d[1] : 0;
            mant = u128_shl(u128_make(hi2, lo2), -shift);
        } else {
            /* shift right: read relevant limbs */
            int limb = shift / 64;
            int bit  = shift % 64;
            uint64_t lo2 = (limb < sig.n) ? sig.d[limb] : 0;
            uint64_t hi2 = (limb+1 < sig.n) ? sig.d[limb+1] : 0;
            uint64_t hi3 = (limb+2 < sig.n) ? sig.d[limb+2] : 0;
            mant = u128_make(hi2, lo2);
            if (bit) mant = u128_or(u128_shr(mant, bit), u128_shl(u128_from_u64(hi3), 64 - bit));
            else     mant = u128_make(hi2, lo2);
            /* Simplified: just take top 2 limbs and shift */
            mant = u128_shr(u128_make(hi2, lo2), bit);
        }
        return f128_pack_parts(sign, exp, mant);
    } else {
        /* eff_exp < 0: value = sig / 10^|eff_exp|
         * Use a 128-bit fixed-point approach via Dragon4 in reverse:
         * Treat as R=sig, S=10^|eff_exp|, extract 113 binary bits. */
        Big S2;
        big_zero(&S2); S2.d[0]=1; S2.n=1;
        for (int32_t i = 0; i < -eff_exp && i < 5000; i++) big_mul_small(&S2, 10);

        /* Scale R so that R/S ∈ [2^112, 2^113): multiply R by 2^k */
        /* Estimate k: bit_len(S) - bit_len(sig) + 112 */
        int top_s = S2.n - 1;
        int top_r = sig.n - 1;
        int bl_s = top_s*64 + (63 - lf_clz64(S2.d[top_s]));
        int bl_r = top_r*64 + (63 - lf_clz64(sig.d[top_r]));
        int k    = bl_s - bl_r + 112;
        if (k < 0) {
            big_shl(&S2, -k);
        } else {
            big_shl(&sig, k);
        }

        /* Correct by ±1 if needed */
        {
            Big S113 = S2; big_shl(&S113, 113);
            if (big_cmp(&sig, &S113) >= 0) {
                /* sig/S >= 2^113: shift sig right (k--) */
                /* Actually: scale down sig by 2 */
                int carry = 0;
                for (int i = sig.n-1; i >= 0; i--) {
                    int new_carry = sig.d[i] & 1;
                    sig.d[i] = (sig.d[i] >> 1) | ((uint64_t)carry << 63);
                    carry = new_carry;
                }
                while (sig.n > 1 && !sig.d[sig.n-1]) sig.n--;
                k--;
            }
        }
        /* Now sig/S2 is close to [2^112, 2^113). Extract the integer
         * quotient directly; this quotient is the binary128 significand. */
        u128 mant2 = u128_zero();
        for (int i = 113; i >= 0; i--) {
            Big shifted = S2;
            big_shl(&shifted, i);
            if (big_cmp(&sig, &shifted) >= 0) {
                big_sub(&sig, &shifted);
                mant2 = u128_or(mant2, u128_shl(u128_one(), i));
            }
        }

        /* Round: if remainder*2 >= denominator, round up. */
        big_shl(&sig, 1);
        if (big_cmp(&sig, &S2) >= 0) mant2 = u128_inc(mant2);

        int32_t exp2 = 112 - k;
        if (!u128_is_zero(u128_shr(mant2, 113))) {
            mant2 = u128_shr(mant2, 1);
            exp2++;
        }
        return f128_pack_parts(sign, exp2, mant2);
    }
}

/* ========================================================================
 * P. Non-ARM64 thin wrappers (restore public names)
 * ======================================================================== */
#if !defined(__aarch64__) && !defined(__wasm32__)
lf_float128 __addtf3(lf_float128 a, lf_float128 b)  { return __addtf3_lf_impl(a, b); }
lf_float128 __subtf3(lf_float128 a, lf_float128 b)  { return __subtf3_lf_impl(a, b); }
lf_float128 __multf3(lf_float128 a, lf_float128 b)  { return __multf3_lf_impl(a, b); }
lf_float128 __divtf3(lf_float128 a, lf_float128 b)  { return __divtf3_lf_impl(a, b); }
lf_float128 __negtf2(lf_float128 a)                 { return __negtf2_lf_impl(a); }
lf_float128 __extenddftf2(double a)                 { return __extenddftf2_lf_impl(a); }
lf_float128 __extendsftf2(float a)                  { return __extendsftf2_lf_impl(a); }
double      __trunctfdf2(lf_float128 a)             { return __trunctfdf2_lf_impl(a); }
float       __trunctfsf2(lf_float128 a)             { return __trunctfsf2_lf_impl(a); }
lf_float128 __floatsitf(int32_t a)                  { return __floatsitf_lf_impl(a); }
lf_float128 __floatditf(int64_t a)                  { return __floatditf_lf_impl(a); }
lf_float128 __floatunditf(uint64_t a)               { return __floatunditf_lf_impl(a); }
int32_t     __fixtfsi(lf_float128 a)                { return __fixtfsi_lf_impl(a); }
int64_t     __fixtfdi(lf_float128 a)                { return __fixtfdi_lf_impl(a); }
/* On non-Apple-ARM64 platforms the fp128 SIMD ABI and the struct ABI coincide
 * (both pass 16 bytes contiguously), so lf_sqrtq/lf_powq are thin wrappers. */
lf_float128 lf_sqrtq(lf_float128 a)                 { return lf_f128_sqrt(a); }
lf_float128 lf_powq(lf_float128 a, lf_float128 b)   { return lf_f128_pow(a, b); }
#endif

/* ========================================================================
 * P2. WASM32 fp128 ABI wrappers
 *
 * LLVM lowers fp128 soft-float calls on wasm32 as low/high i64 words, not as
 * byval C structs. Keep the portable implementation struct-based internally,
 * but export the compiler-rt symbols with the ABI LLVM expects.
 * ======================================================================== */
#if defined(__wasm32__) && !defined(__EMSCRIPTEN__)

#define LF_WASM_TF2_ATTR __attribute__((weak))

static lf_float128 f128_from_words(uint64_t lo, uint64_t hi) {
    lf_float128 r;
    memcpy(r.bytes, &lo, 8);
    memcpy(r.bytes + 8, &hi, 8);
    return r;
}

static void f128_store_words(lf_float128 *out, lf_float128 v) {
    memcpy(out->bytes, v.bytes, 16);
}

LF_WASM_TF2_ATTR void __addtf3(lf_float128 *out, uint64_t a_lo, uint64_t a_hi,
        uint64_t b_lo, uint64_t b_hi) {
    f128_store_words(out, __addtf3_lf_impl(
        f128_from_words(a_lo, a_hi), f128_from_words(b_lo, b_hi)));
}

LF_WASM_TF2_ATTR void __subtf3(lf_float128 *out, uint64_t a_lo, uint64_t a_hi,
        uint64_t b_lo, uint64_t b_hi) {
    f128_store_words(out, __subtf3_lf_impl(
        f128_from_words(a_lo, a_hi), f128_from_words(b_lo, b_hi)));
}

LF_WASM_TF2_ATTR void __multf3(lf_float128 *out, uint64_t a_lo, uint64_t a_hi,
        uint64_t b_lo, uint64_t b_hi) {
    f128_store_words(out, __multf3_lf_impl(
        f128_from_words(a_lo, a_hi), f128_from_words(b_lo, b_hi)));
}

LF_WASM_TF2_ATTR void __divtf3(lf_float128 *out, uint64_t a_lo, uint64_t a_hi,
        uint64_t b_lo, uint64_t b_hi) {
    f128_store_words(out, __divtf3_lf_impl(
        f128_from_words(a_lo, a_hi), f128_from_words(b_lo, b_hi)));
}

LF_WASM_TF2_ATTR void __negtf2(lf_float128 *out, uint64_t a_lo, uint64_t a_hi) {
    f128_store_words(out, __negtf2_lf_impl(f128_from_words(a_lo, a_hi)));
}

LF_WASM_TF2_ATTR void __extenddftf2(lf_float128 *out, double a) {
    f128_store_words(out, __extenddftf2_lf_impl(a));
}

LF_WASM_TF2_ATTR void __extendsftf2(lf_float128 *out, float a) {
    f128_store_words(out, __extendsftf2_lf_impl(a));
}

LF_WASM_TF2_ATTR double __trunctfdf2(uint64_t a_lo, uint64_t a_hi) {
    return __trunctfdf2_lf_impl(f128_from_words(a_lo, a_hi));
}

LF_WASM_TF2_ATTR float __trunctfsf2(uint64_t a_lo, uint64_t a_hi) {
    return __trunctfsf2_lf_impl(f128_from_words(a_lo, a_hi));
}

LF_WASM_TF2_ATTR void __floatsitf(lf_float128 *out, int32_t a) {
    f128_store_words(out, __floatsitf_lf_impl(a));
}

LF_WASM_TF2_ATTR void __floatditf(lf_float128 *out, int64_t a) {
    f128_store_words(out, __floatditf_lf_impl(a));
}

LF_WASM_TF2_ATTR void __floatunditf(lf_float128 *out, uint64_t a) {
    f128_store_words(out, __floatunditf_lf_impl(a));
}

LF_WASM_TF2_ATTR int32_t __fixtfsi(uint64_t a_lo, uint64_t a_hi) {
    return __fixtfsi_lf_impl(f128_from_words(a_lo, a_hi));
}

LF_WASM_TF2_ATTR int64_t __fixtfdi(uint64_t a_lo, uint64_t a_hi) {
    return __fixtfdi_lf_impl(f128_from_words(a_lo, a_hi));
}

LF_WASM_TF2_ATTR int __eqtf2(uint64_t a_lo, uint64_t a_hi, uint64_t b_lo, uint64_t b_hi) {
    return f128_cmp_internal(f128_from_words(a_lo, a_hi),
        f128_from_words(b_lo, b_hi)) == 0 ? 0 : 1;
}

LF_WASM_TF2_ATTR int __netf2(uint64_t a_lo, uint64_t a_hi, uint64_t b_lo, uint64_t b_hi) {
    return f128_cmp_internal(f128_from_words(a_lo, a_hi),
        f128_from_words(b_lo, b_hi)) != 0 ? 1 : 0;
}

LF_WASM_TF2_ATTR int __lttf2(uint64_t a_lo, uint64_t a_hi, uint64_t b_lo, uint64_t b_hi) {
    int c = f128_cmp_internal(f128_from_words(a_lo, a_hi),
        f128_from_words(b_lo, b_hi));
    return c >= 2 ? 1 : c;
}

LF_WASM_TF2_ATTR int __letf2(uint64_t a_lo, uint64_t a_hi, uint64_t b_lo, uint64_t b_hi) {
    int c = f128_cmp_internal(f128_from_words(a_lo, a_hi),
        f128_from_words(b_lo, b_hi));
    return c >= 2 ? 1 : c;
}

LF_WASM_TF2_ATTR int __gttf2(uint64_t a_lo, uint64_t a_hi, uint64_t b_lo, uint64_t b_hi) {
    int c = f128_cmp_internal(f128_from_words(a_lo, a_hi),
        f128_from_words(b_lo, b_hi));
    return c >= 2 ? 1 : c;
}

LF_WASM_TF2_ATTR int __getf2(uint64_t a_lo, uint64_t a_hi, uint64_t b_lo, uint64_t b_hi) {
    int c = f128_cmp_internal(f128_from_words(a_lo, a_hi),
        f128_from_words(b_lo, b_hi));
    return c >= 2 ? 1 : c;
}

LF_WASM_TF2_ATTR int __unordtf2(uint64_t a_lo, uint64_t a_hi, uint64_t b_lo, uint64_t b_hi) {
    return (f128_is_nan(f128_from_words(a_lo, a_hi)) ||
        f128_is_nan(f128_from_words(b_lo, b_hi))) ? 1 : 0;
}

#undef LF_WASM_TF2_ATTR

#endif

/* ========================================================================
 * Q. ARM64 calling-convention shims
 *
 * On macOS ARM64, LLVM lowers fp128 ops to runtime calls using the Apple
 * ARM64 ABI for __float128:
 *   fp128 args in q0/q1 (128-bit SIMD); fp128 result in q0.
 *   int->fp128 conversions: int arg in w0/x0; fp128 result in q0.
 *   fp128->int conversions: fp128 arg in q0; int result in w0/x0.
 *
 * Our _lf_impl C functions use the struct ABI (lf_float128 = 16-byte struct):
 *   Two-arg: args in x0/x1, x2/x3; result in x0/x1.
 *   One-arg:  arg  in x0/x1;       result in x0/x1.
 *   double->fp128: arg in d0;       result in x0/x1.
 *   fp128->int:    arg in x0/x1;    result in x0/w0.
 *
 * Each shim marshals registers in both directions so the two ABIs meet.
 * All shims are strong symbols (no `weak`) to override system versions.
 *
 * macOS asm symbol mangling: leading underscore required.
 * Linux/ELF: no leading underscore.
 * ======================================================================== */
#if defined(__aarch64__) && !defined(__wasm32__)

#if defined(__APPLE__)
#  define _LF_SYM(n) "_" #n
#else
#  define _LF_SYM(n) #n
#endif

/* Two-operand: fp128 args q0,q1 -> x0/x1,x2/x3; result x0/x1 -> q0 */
#define _LF_SHIM2(pub, impl)                                 \
__attribute__((naked))                                       \
void pub(void) {                                             \
    __asm__ volatile(                                        \
        "stp x29, x30, [sp, #-16]!\n\t"                     \
        "mov x29, sp\n\t"                                    \
        "fmov x0, d0\n\t"                                    \
        "mov  x1, v0.d[1]\n\t"                               \
        "fmov x2, d1\n\t"                                    \
        "mov  x3, v1.d[1]\n\t"                               \
        "bl  " _LF_SYM(impl) "\n\t"                         \
        "fmov d0, x0\n\t"                                    \
        "mov  v0.d[1], x1\n\t"                               \
        "ldp x29, x30, [sp], #16\n\t"                       \
        "ret\n\t"                                            \
    );                                                       \
}

/* One-operand fp128: arg q0 -> x0/x1; result x0/x1 -> q0 */
#define _LF_SHIM1(pub, impl)                                 \
__attribute__((naked))                                       \
void pub(void) {                                             \
    __asm__ volatile(                                        \
        "stp x29, x30, [sp, #-16]!\n\t"                     \
        "mov x29, sp\n\t"                                    \
        "fmov x0, d0\n\t"                                    \
        "mov  x1, v0.d[1]\n\t"                               \
        "bl  " _LF_SYM(impl) "\n\t"                         \
        "fmov d0, x0\n\t"                                    \
        "mov  v0.d[1], x1\n\t"                               \
        "ldp x29, x30, [sp], #16\n\t"                       \
        "ret\n\t"                                            \
    );                                                       \
}

/* scalar->fp128: arg already in d0/s0/w0/x0; result x0/x1 -> q0 */
#define _LF_SHIM_FROM_SCALAR(pub, impl)                      \
__attribute__((naked))                                       \
void pub(void) {                                             \
    __asm__ volatile(                                        \
        "stp x29, x30, [sp, #-16]!\n\t"                     \
        "mov x29, sp\n\t"                                    \
        "bl  " _LF_SYM(impl) "\n\t"                         \
        "fmov d0, x0\n\t"                                    \
        "mov  v0.d[1], x1\n\t"                               \
        "ldp x29, x30, [sp], #16\n\t"                       \
        "ret\n\t"                                            \
    );                                                       \
}

/* fp128->int: arg q0 -> x0/x1; int result already in x0/w0 */
#define _LF_SHIM_TO_INT(pub, impl)                           \
__attribute__((naked))                                       \
void pub(void) {                                             \
    __asm__ volatile(                                        \
        "stp x29, x30, [sp, #-16]!\n\t"                     \
        "mov x29, sp\n\t"                                    \
        "fmov x0, d0\n\t"                                    \
        "mov  x1, v0.d[1]\n\t"                               \
        "bl  " _LF_SYM(impl) "\n\t"                         \
        "ldp x29, x30, [sp], #16\n\t"                       \
        "ret\n\t"                                            \
    );                                                       \
}

/* fp128->double: arg q0 -> x0/x1; result already in d0 from the impl */
#define _LF_SHIM_TO_DOUBLE(pub, impl)                        \
__attribute__((naked))                                       \
void pub(void) {                                             \
    __asm__ volatile(                                        \
        "stp x29, x30, [sp, #-16]!\n\t"                     \
        "mov x29, sp\n\t"                                    \
        "fmov x0, d0\n\t"                                    \
        "mov  x1, v0.d[1]\n\t"                               \
        "bl  " _LF_SYM(impl) "\n\t"                         \
        "ldp x29, x30, [sp], #16\n\t"                       \
        "ret\n\t"                                            \
    );                                                       \
}

/* fp128->float: arg q0 -> x0/x1; result already in s0 from the impl */
#define _LF_SHIM_TO_FLOAT(pub, impl)                         \
__attribute__((naked))                                       \
void pub(void) {                                             \
    __asm__ volatile(                                        \
        "stp x29, x30, [sp, #-16]!\n\t"                     \
        "mov x29, sp\n\t"                                    \
        "fmov x0, d0\n\t"                                    \
        "mov  x1, v0.d[1]\n\t"                               \
        "bl  " _LF_SYM(impl) "\n\t"                         \
        "ldp x29, x30, [sp], #16\n\t"                       \
        "ret\n\t"                                            \
    );                                                       \
}

_LF_SHIM2(__addtf3,          __addtf3_lf_impl)
_LF_SHIM2(__subtf3,          __subtf3_lf_impl)
_LF_SHIM2(__multf3,          __multf3_lf_impl)
_LF_SHIM2(__divtf3,          __divtf3_lf_impl)
/* SIMD-ABI public entry points for the transcendentals called by LLVM-emitted
 * `call fp128 @lf_sqrtq(fp128)` / `lf_powq(fp128, fp128)`. They bridge to the
 * struct-ABI implementations in lf_f128_sqrt / lf_f128_pow. */
_LF_SHIM1(lf_sqrtq,          lf_f128_sqrt)
_LF_SHIM2(lf_powq,           lf_f128_pow)
_LF_SHIM1(__negtf2,          __negtf2_lf_impl)
_LF_SHIM_FROM_SCALAR(__extenddftf2,   __extenddftf2_lf_impl)
_LF_SHIM_FROM_SCALAR(__extendsftf2,   __extendsftf2_lf_impl)
_LF_SHIM_FROM_SCALAR(__floatsitf,     __floatsitf_lf_impl)
_LF_SHIM_FROM_SCALAR(__floatditf,     __floatditf_lf_impl)
_LF_SHIM_FROM_SCALAR(__floatunditf,   __floatunditf_lf_impl)
_LF_SHIM_TO_INT(__fixtfsi,            __fixtfsi_lf_impl)
_LF_SHIM_TO_INT(__fixtfdi,            __fixtfdi_lf_impl)
_LF_SHIM_TO_DOUBLE(__trunctfdf2,      __trunctfdf2_lf_impl)
_LF_SHIM_TO_FLOAT(__trunctfsf2,       __trunctfsf2_lf_impl)

#undef _LF_SHIM2
#undef _LF_SHIM1
#undef _LF_SHIM_FROM_SCALAR
#undef _LF_SHIM_TO_INT
#undef _LF_SHIM_TO_DOUBLE
#undef _LF_SHIM_TO_FLOAT
#undef _LF_SYM

#endif /* __aarch64__ */

/* ========================================================================
 * Q. Linux / HAVE_REAL128 path helpers
 *
 * On Linux with __float128 (LFORTRAN_HAVE_REAL128=1), keep the public
 * lf_float128 byte representation and convert at the quadmath boundary.
 * These wrappers expose the same API used by lfortran_intrinsics.c.
 * ======================================================================== */

#if LFORTRAN_HAVE_REAL128

static __float128 lf_f128_to_quad(lf_float128 v) {
    __float128 q;
    memcpy(&q, v.bytes, 16);
    return q;
}

static lf_float128 lf_f128_from_quad(__float128 q) {
    lf_float128 v;
    memcpy(v.bytes, &q, 16);
    return v;
}

lf_float128 lf_f128_from_double(double d)  { return lf_f128_from_quad((__float128)d); }
lf_float128 lf_f128_from_int64(int64_t i)  { return lf_f128_from_quad((__float128)i); }
double       lf_f128_to_double(lf_float128 v) { return (double)lf_f128_to_quad(v); }
lf_float128 lf_f128_add(lf_float128 a, lf_float128 b) { return lf_f128_from_quad(lf_f128_to_quad(a) + lf_f128_to_quad(b)); }
lf_float128 lf_f128_sub(lf_float128 a, lf_float128 b) { return lf_f128_from_quad(lf_f128_to_quad(a) - lf_f128_to_quad(b)); }
lf_float128 lf_f128_mul(lf_float128 a, lf_float128 b) { return lf_f128_from_quad(lf_f128_to_quad(a) * lf_f128_to_quad(b)); }
lf_float128 lf_f128_div(lf_float128 a, lf_float128 b) { return lf_f128_from_quad(lf_f128_to_quad(a) / lf_f128_to_quad(b)); }
lf_float128 lf_f128_neg(lf_float128 a) { return lf_f128_from_quad(-lf_f128_to_quad(a)); }
int lf_f128_isnan   (lf_float128 v) { return isnanq(lf_f128_to_quad(v)); }
int lf_f128_isinf   (lf_float128 v) { return isinfq(lf_f128_to_quad(v)); }
int lf_f128_signbit (lf_float128 v) {
    uint64_t hi; memcpy(&hi, v.bytes + 8, 8); return (int)(hi >> 63);
}
int lf_f128_cmp(lf_float128 a, lf_float128 b) {
    __float128 aq = lf_f128_to_quad(a), bq = lf_f128_to_quad(b);
    if (isnanq(aq) || isnanq(bq)) return 1;
    return (aq > bq) - (aq < bq);
}
int lf_f128_eq(lf_float128 a, lf_float128 b) {
    __float128 aq = lf_f128_to_quad(a), bq = lf_f128_to_quad(b);
    if (isnanq(aq) || isnanq(bq)) return 0;
    return aq == bq;
}

/* Compiler-rt comparison ABI (weak on ELF so libgcc can override) */
#if defined(__ELF__)
#  define LF_TF2_ATTR __attribute__((weak))
#else
#  define LF_TF2_ATTR
#endif

LF_TF2_ATTR int __eqtf2   (lf_float128 a, lf_float128 b) { return lf_f128_eq(a,b) ? 0 : 1; }
LF_TF2_ATTR int __netf2   (lf_float128 a, lf_float128 b) { return lf_f128_eq(a,b) ? 0 : 1; }
LF_TF2_ATTR int __lttf2   (lf_float128 a, lf_float128 b) { return lf_f128_cmp(a,b); }
LF_TF2_ATTR int __letf2   (lf_float128 a, lf_float128 b) { return lf_f128_cmp(a,b); }
LF_TF2_ATTR int __gttf2   (lf_float128 a, lf_float128 b) { return lf_f128_cmp(a,b); }
LF_TF2_ATTR int __getf2   (lf_float128 a, lf_float128 b) { return lf_f128_cmp(a,b); }
LF_TF2_ATTR int __unordtf2(lf_float128 a, lf_float128 b) { return (lf_f128_isnan(a)||lf_f128_isnan(b))?1:0; }

#else /* !LFORTRAN_HAVE_REAL128 — expose the struct-based helpers used by intrinsics.c */

lf_float128 lf_f128_from_double(double d)     { return __extenddftf2_lf_impl(d); }
lf_float128 lf_f128_from_int64(int64_t i)     { return __floatditf_lf_impl(i); }
double       lf_f128_to_double(lf_float128 v) { return __trunctfdf2_lf_impl(v); }
lf_float128 lf_f128_add(lf_float128 a, lf_float128 b) { return __addtf3_lf_impl(a, b); }
lf_float128 lf_f128_sub(lf_float128 a, lf_float128 b) { return __subtf3_lf_impl(a, b); }
lf_float128 lf_f128_mul(lf_float128 a, lf_float128 b) { return __multf3_lf_impl(a, b); }
lf_float128 lf_f128_div(lf_float128 a, lf_float128 b) { return __divtf3_lf_impl(a, b); }
lf_float128 lf_f128_neg(lf_float128 a) { return __negtf2_lf_impl(a); }
int lf_f128_isnan   (lf_float128 v) { return f128_is_nan(v); }
int lf_f128_isinf   (lf_float128 v) { return f128_is_inf(v); }
int lf_f128_signbit (lf_float128 v) { return f128_sign(v); }
int lf_f128_cmp     (lf_float128 a, lf_float128 b) {
    int c = f128_cmp_internal(a, b);
    return (c >= 2) ? 1 : c;   /* NaN -> 1 (unordered) */
}
int lf_f128_eq(lf_float128 a, lf_float128 b) {
    return f128_cmp_internal(a, b) == 0;
}

#endif /* LFORTRAN_HAVE_REAL128 */

/* ========================================================================
 * R. format_float128_fortran — Fortran print formatting for real(16)
 *
 * Output: [-]d.dddddddddddddddddddddddddddddddddE+eee  (34 sig digits)
 *
 * HAVE_REAL128: uses quadmath_snprintf (exact, native).
 * !HAVE_REAL128: delegates to lf_float128_to_str (Dragon4, correct).
 *   The old u256-based implementation that lived here overflowed: for
 *   values like 1.5, multiplying the 113-bit significand by 5^112
 *   requires 373 bits — beyond any u256.
 * ======================================================================== */

void format_float128_fortran(char *result, lf_float128 val) {
#if LFORTRAN_HAVE_REAL128
    char buf[64];
    int n = quadmath_snprintf(buf, sizeof(buf), "%.33Qe", lf_f128_to_quad(val));
    if (n < 0 || n >= (int)sizeof(buf)) { snprintf(result, 64, "NaN"); return; }
    char *ep = strchr(buf, 'e'); if (!ep) ep = strchr(buf, 'E');
    if (ep) {
        int w = (int)(ep - buf); char es = ep[1]; int ev = atoi(ep + 2);
        sprintf(result, "%.*sE%c%04d", w, buf, es=='-'?'-':'+', ev<0?-ev:ev);
    } else { strcpy(result, buf); }
#else
    lf_float128_to_str(result, val.bytes);
#endif
}
