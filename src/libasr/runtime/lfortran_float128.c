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
 *   J. Math: sqrt, floor, ceiling, mod
 *   K. Math: exp, expm1, exp2, log, log2, log10, pow
 *   L. Math: sin, cos, tan, asin, acos, atan, atan2 and degree variants
 *   M. Math: sinh, cosh, tanh, asinh, acosh, atanh
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
#include <float.h>   /* LDBL_MANT_DIG */
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

/* Struct-ABI implementations. The public __*tf2 entry points that LLVM calls
 * are defined per platform further down, because LLVM passes their operands
 * with the native fp128 calling convention rather than as a 16-byte struct. */
#if !defined(__wasm32__)
int __eqtf2_lf_impl   (lf_float128 a, lf_float128 b) { return f128_cmp_internal(a,b) == 0 ? 0 : 1; }
int __netf2_lf_impl   (lf_float128 a, lf_float128 b) { return f128_cmp_internal(a,b) != 0 ? 1 : 0; }
int __lttf2_lf_impl   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?1:c; }
int __letf2_lf_impl   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?1:c; }
int __gttf2_lf_impl   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?-1:c; }
int __getf2_lf_impl   (lf_float128 a, lf_float128 b) { int c=f128_cmp_internal(a,b); return c>=2?-1:c; }
int __unordtf2_lf_impl(lf_float128 a, lf_float128 b) { return (f128_is_nan(a)||f128_is_nan(b))?1:0; }
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

/* Number of extra low bits (guard, round, sticky) carried by the
 * significands while adding, so that the sum can be rounded correctly. */
enum { F128_ADD_GRS = 3 };

/* Round and pack the result of an addition. `m` is the significand with
 * F128_ADD_GRS extra low bits, the lowest of which is sticky; its value is
 * m * 2^(exp - 112 - F128_ADD_GRS). exp >= -16382 and m < 2^117. */
static lf_float128 f128_round_pack_grs(int sign, int32_t exp, u128 m) {
    const int top = 112 + F128_ADD_GRS;
    /* A carry out of the top bit: shift right by one, keeping the sticky. */
    if (u128_bit(m, top + 1)) {
        uint64_t sticky = u128_lo(m) & 1;
        m = u128_shr1(m);
        m.lo |= sticky;
        exp++;
    }
    /* After a cancellation, shift left up to the smallest normal exponent.
     * This happens only when no bits were lost while aligning. */
    int lsh = u128_clz(m) - (127 - top);
    if (lsh > exp + 16382) lsh = exp + 16382;
    if (lsh > 0) {
        m = u128_shl(m, lsh);
        exp -= lsh;
    }
    /* Round to nearest, ties to even. */
    uint64_t rest = u128_lo(m) & ((1u << F128_ADD_GRS) - 1);
    uint64_t half = (uint64_t)1 << (F128_ADD_GRS - 1);
    u128 q = u128_shr(m, F128_ADD_GRS);
    if (rest > half || (rest == half && (u128_lo(q) & 1))) {
        q = u128_inc(q);
        if (u128_bit(q, 113)) {
            q = u128_shr1(q);
            exp++;
        }
    }
    if (exp > 16383) return f128_make_inf(sign);
    if (!u128_bit(q, 112)) {
        /* Subnormal (exp is -16382 here) or zero. */
        return f128_const(sign, 0, u128_hi(q), u128_lo(q));
    }
    return f128_const(sign, exp + 16383, u128_hi(q), u128_lo(q));
}

/* Add the magnitudes of pa and pb (subtract them if `subtract`), with
 * |pa| >= |pb|, giving the correctly rounded result with sign `sign`. */
static lf_float128 f128_add_mag(f128_parts pa, f128_parts pb, int subtract,
                                int sign) {
    int shift = pa.exp - pb.exp;
    u128 ma = u128_shl(pa.mant, F128_ADD_GRS);
    u128 mb = u128_shl(pb.mant, F128_ADD_GRS);
    if (shift > 0) {
        /* Align, collecting the bits shifted out in the sticky bit. */
        int sticky;
        if (shift >= 128) {
            sticky = !u128_is_zero(mb);
            mb = u128_zero();
        } else {
            u128 lost = u128_and(mb, u128_sub(u128_shl(u128_one(), shift),
                                              u128_one()));
            sticky = !u128_is_zero(lost);
            mb = u128_shr(mb, shift);
        }
        if (sticky) mb.lo |= 1;
    }
    u128 m = subtract ? u128_sub(ma, mb) : u128_add(ma, mb);
    if (u128_is_zero(m)) return f128_make_zero(0);
    return f128_round_pack_grs(sign, pa.exp, m);
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

    /* Sort so pa has the larger (or equal) magnitude; the result takes its
     * sign. */
    if (pa.exp < pb.exp || (pa.exp == pb.exp && u128_lt(pa.mant, pb.mant))) {
        f128_parts tmp = pa; pa = pb; pb = tmp;
    }
    return f128_add_mag(pa, pb, pa.sign != pb.sign, pa.sign);
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

    /* Normalise subnormal operands so that both significands lie in
     * [2^112, 2^113). */
    int32_t exp_a = pa.exp, exp_b = pb.exp;
    u128 mant_a = pa.mant, mant_b = pb.mant;
    int nsh = u128_clz(mant_a) - 15;
    mant_a = u128_shl(mant_a, nsh);
    exp_a -= nsh;
    nsh = u128_clz(mant_b) - 15;
    mant_b = u128_shl(mant_b, nsh);
    exp_b -= nsh;

    /* Scale the dividend so that mant_b <= mant_a < 2*mant_b, which puts
     * the quotient mant_a/mant_b in [1, 2) with its exponent rexp. */
    if (u128_lt(mant_a, mant_b)) {
        mant_a = u128_shl1(mant_a);
        exp_a--;
    }
    int32_t rexp = exp_a - exp_b;

    /* Restoring division producing the 113 significand bits followed by
     * `guard` extra bits: quotient = floor(mant_a * 2^(112+guard) / mant_b),
     * with its leading 1 in bit 112+guard. The leading quotient bit is 1 by
     * the scaling above; afterwards the remainder stays below mant_b, so the
     * shifted remainder stays below 2^114. */
    const int guard = 2;
    u128 remainder = u128_sub(mant_a, mant_b);
    u128 quotient  = u128_one();
    for (int i = 0; i < 112 + guard; i++) {
        remainder = u128_shl1(remainder);
        quotient  = u128_shl1(quotient);
        if (u128_gte(remainder, mant_b)) {
            remainder = u128_sub(remainder, mant_b);
            quotient  = u128_or(quotient, u128_one());
        }
    }
    int sticky = !u128_is_zero(remainder);

    /* Number of low quotient bits to round off: the guard bits, plus the
     * bits that fall below the least significant subnormal bit. Subnormal
     * results are rounded here, in a single step, rather than by
     * f128_pack_parts, which expects an already rounded significand. */
    int32_t drop = guard;
    if (rexp < -16382) drop += -16382 - rexp;
    /* Dropping all 115 quotient bits and more leaves less than half of the
     * smallest subnormal: round to zero. */
    if (drop > 115) return f128_make_zero(rsign);

    /* Round to nearest, ties to even. */
    u128 rest = u128_and(quotient, u128_sub(u128_shl(u128_one(), drop), u128_one()));
    u128 half = u128_shl(u128_one(), drop - 1);
    u128 mant = u128_shr(quotient, drop);
    if (u128_gt(rest, half) ||
        (u128_eq(rest, half) && (sticky || (u128_lo(mant) & 1)))) {
        mant = u128_inc(mant);
    }

    if (rexp < -16382) {
        /* Subnormal result: mant holds the stored significand bits. If
         * rounding carried into bit 112, that bit becomes biased exponent 1,
         * the smallest normal number. */
        return f128_const(rsign, (int32_t)(u128_hi(mant) >> 48),
                          u128_hi(mant), u128_lo(mant));
    }
    /* Normal result (mant is in [2^112, 2^113]); f128_pack_parts handles a
     * rounding carry to 2^113 and overflow to infinity. */
    return f128_pack_parts(rsign, rexp, mant);
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
    /* A carry out of the 52 stored bits gives the next power of two. */
    if (dmant >> 52) { dmant = 0; dexp++; }
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
 * J. Math: sqrt, floor, ceiling, mod
 * ======================================================================== */

/* Correctly rounded square root (round to nearest, ties to even).
 * The significand is normalised to m in [2^112, 2^113), with a = m * 2^(e-112).
 * Then N = m * 2^(114 + (e & 1)) and a = N * 2^(2E - 226), E = (e - (e & 1))/2,
 * so sqrt(a) = sqrt(N) * 2^(E - 113). The integer square root q = isqrt(N)
 * lies in [2^113, 2^114): its top 113 bits are the result significand, its
 * lowest bit the round bit, and a nonzero remainder N - q^2 the sticky bit. */
lf_float128 lf_f128_sqrt(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) {
        a.bytes[13] |= 0x80;  /* quiet the NaN */
        return a;
    }
    if (p.is_zero) return a;  /* sqrt(+-0) = +-0 */
    if (p.sign) return f128_make_nan();
    if (p.is_inf) return a;

    /* Normalise subnormal significands so that bit 112 is set. */
    int shift = u128_clz(p.mant) - (127 - 112);
    u128 m = u128_shl(p.mant, shift);
    int32_t e = p.exp - shift;
    int odd = (int)(e & 1);
    int nshift = 114 + odd;

    /* Bit-by-bit integer square root of N = m << nshift (at most 228 bits),
     * taking two bits of N per step. The remainder stays below 2^116. */
    u128 q = u128_zero(), rem = u128_zero();
    for (int j = 113; j >= 0; j--) {
        int k = 2 * j - nshift;
        uint64_t two = 0;
        if (k + 1 >= 0) two |= (uint64_t)u128_bit(m, k + 1) << 1;
        if (k >= 0)     two |= (uint64_t)u128_bit(m, k);
        rem = u128_or(u128_shl(rem, 2), u128_from_u64(two));
        u128 t = u128_or(u128_shl(q, 2), u128_one());
        q = u128_shl1(q);
        if (u128_gte(rem, t)) {
            rem = u128_sub(rem, t);
            q = u128_or(q, u128_one());
        }
    }

    int32_t res_exp = (e - odd) / 2;
    int round_bit = (int)(u128_lo(q) & 1);
    u128 mant = u128_shr1(q);
    if (round_bit && (!u128_is_zero(rem) || (u128_lo(mant) & 1))) {
        mant = u128_inc(mant);
        if (u128_bit(mant, 113)) {
            mant = u128_shr1(mant);
            res_exp++;
        }
    }
    return f128_const(0, res_exp + 16383, u128_hi(mant), u128_lo(mant));
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

/* ========================================================================
 * K. Math: exp, expm1, exp2, log, log2, log10, pow
 *
 * The functions are evaluated with the soft-float operations above, which
 * round to nearest. Intermediate results that need more than binary128
 * precision are kept as unevaluated sums hi + lo ("double-word" numbers,
 * f128_dw), built with the error-free transformations two_sum and
 * two_prod. This keeps the results within about an ulp of the exact value.
 *
 * The constants and tables are correctly rounded binary128 values, given as
 * {high word, low word} bit patterns; the low part of a {hi, lo} pair is the
 * rounded remainder of the exact value minus the high part. The block from
 * F128_PIO2_1 through F128_TWO_OVER_PI_BITS is printed by
 * gen_float128_consts.py (next to this file; Python standard library only,
 * exact rational arithmetic, not run by the build):
 *     python src/libasr/runtime/gen_float128_consts.py > tables.txt
 * To change a table, edit the script and paste its output over the block.
 * ======================================================================== */

/* pi/2 = PIO2_1 + PIO2_2 + PIO2_3 to 339 bits */
static const uint64_t F128_PIO2_1[2] = {0x3FFF921FB54442D1ULL, 0x8469898CC51701B8ULL};
static const uint64_t F128_PIO2_2[2] = {0x3F8CCD129024E088ULL, 0xA67CC74020BBEA64ULL};
static const uint64_t F128_PIO2_3[2] = {0xBF1A3B19376BAD7DULL, 0xE19C72FEC8841ABAULL};
/* 2/pi */
static const uint64_t F128_TWO_OVER_PI[2] = {0x3FFE45F306DC9C88ULL, 0x2A53F84EAFA3EA6AULL};
/* log(2) = LN2_HI98 + LN2_LO98, LN2_HI98 with 98 significant bits so that
 * k*LN2_HI98 is exact for |k| < 2^15 */
static const uint64_t F128_LN2_HI98[2] = {0x3FFE62E42FEFA39EULL, 0xF35793C767300000ULL};
static const uint64_t F128_LN2_LO98[2] = {0x3F98F97B57A079A1ULL, 0x93394C5B16C5068CULL};
/* pi, {hi, lo} */
static const uint64_t F128_PI[2][2] = {{0x4000921FB54442D1ULL, 0x8469898CC51701B8ULL}, {0x3F8DCD129024E088ULL, 0xA67CC74020BBEA64ULL}};
/* log(2), {hi, lo} */
static const uint64_t F128_LN2[2][2] = {{0x3FFE62E42FEFA39EULL, 0xF35793C7673007E6ULL}, {0xBF8A2A17E1979B31ULL, 0xACE93A4EBE5D148FULL}};
/* 1/log(2), {hi, lo} */
static const uint64_t F128_LOG2E[2][2] = {{0x3FFF71547652B82FULL, 0xE1777D0FFDA0D23AULL}, {0x3F8DF4475ABBD546ULL, 0xEB4AD2C45928B367ULL}};
/* 1/log(10), {hi, lo} */
static const uint64_t F128_LOG10E[2][2] = {{0x3FFDBCB7B1526E50ULL, 0xE32A6AB7555F5A68ULL}, {0xBF8B1E6E08E5CFEDULL, 0xD1B2EFEE2E0695D8ULL}};
/* pi/180, {hi, lo} */
static const uint64_t F128_D2R[2][2] = {{0x3FF91DF46A2529D3ULL, 0x915C1D8BECDD290CULL}, {0xBF87D937FA428857ULL, 0xF250CA38D7EAB25AULL}};
/* 180/pi, {hi, lo} */
static const uint64_t F128_R2D[2][2] = {{0x4004CA5DC1A63C1FULL, 0x7B86152EA6FE81A5ULL}, {0xBF9241465BEAED04ULL, 0x1A07E91992EC5EB2ULL}};
/* log(1 + i/64) for i = -16..32, as {hi, lo} pairs */
static const uint64_t F128_LOG_TABLE[49][2][2] = {
    {{0xBFFD269621134DB9ULL, 0x2783BEB7676C0AAAULL}, {0x3F8AEAE785B7919AULL, 0x55E432A52FB80667ULL}},
    {{0xBFFD1178E8227E47ULL, 0xBDE338B41FC72DE8ULL}, {0xBF89E3BB51FD8784ULL, 0x96F146F7E0FDAA98ULL}},
    {{0xBFFCF991C6CB3B37ULL, 0x97D99419BE602863ULL}, {0xBF8AB07644EDC7C9ULL, 0xE7A659550555EE18ULL}},
    {{0xBFFCD1037F2655E7ULL, 0xB5818A49091C6888ULL}, {0x3F89FC03DD427012ULL, 0x1911CAE6C0B7CF74ULL}},
    {{0xBFFCA93ED3C8AD9EULL, 0x36F2BEA77A5C80DDULL}, {0x3F899188E775070BULL, 0x0BA4611813F28405ULL}},
    {{0xBFFC823C16551A3CULL, 0x1BB734C63D061FA6ULL}, {0xBF88207AB3DB6D1EULL, 0xD67EC773DBC2C5DAULL}},
    {{0xBFFC5BF406B543DBULL, 0x1FB8292ECFC82063ULL}, {0x3F892D3D5F1E333AULL, 0xA986BB41A195FD4EULL}},
    {{0xBFFC365FCB015901ULL, 0x62FA8234B7289595ULL}, {0xBF87C206C033F255ULL, 0xA6BB3F632D2E097DULL}},
    {{0xBFFC1178E8227E47ULL, 0xBDE338B41FC72DE8ULL}, {0xBF88E3BB51FD8784ULL, 0x96F146F7E0FDAA98ULL}},
    {{0xBFFBDA727638446AULL, 0x25007E9C5CCC0630ULL}, {0x3F895504628BF8B4ULL, 0x28C1F1BA8AAFC285ULL}},
    {{0xBFFB9335E5D59498ULL, 0x8AE1D5EA3ECCD251ULL}, {0x3F89DA329F340343ULL, 0xA04118ED8D3C5036ULL}},
    {{0xBFFB4D3115D207EAULL, 0xC5DA7D0B1E10B2F6ULL}, {0xBF89D34A76DE4FDCULL, 0x95E0F352095BC59BULL}},
    {{0xBFFB08598B59E3A0ULL, 0x688A3FD9BF503373ULL}, {0x3F88F681C9D38072ULL, 0x2E71A34D377A144FULL}},
    {{0xBFFA894AA149FB34ULL, 0x33517D2ECC144799ULL}, {0x3F885F973F27591EULL, 0x57988B68F247887DULL}},
    {{0xBFFA0415D89E7444ULL, 0x470173C75D4D888AULL}, {0x3F870F8F9ECC43D9ULL, 0xA0FCF1197B9ED79EULL}},
    {{0xBFF9020565893584ULL, 0x749F23A105B9C7BCULL}, {0x3F879642AAC1310EULL, 0xC916C9ED469C6CD0ULL}},
    {{0x0000000000000000ULL, 0x0000000000000000ULL}, {0x0000000000000000ULL, 0x0000000000000000ULL}},
    {{0x3FF8FC0A8B0FC03EULL, 0x3CF9EDA74D37ABD5ULL}, {0x3F86B7D6077D207BULL, 0xB8A66BA281FC0AEEULL}},
    {{0x3FF9F829B0E78330ULL, 0x04CF8FC13C7BC8A8ULL}, {0xBF854544147ACC88ULL, 0x3294B8824E29C072ULL}},
    {{0x3FFA77458F632DCFULL, 0xC4634F2A1EE4A583ULL}, {0xBF87C36408FE4D57ULL, 0x62734F8298B2145CULL}},
    {{0x3FFAF0A30C01162AULL, 0x6617CC9716EEB32FULL}, {0x3F8631A68E9A66EBULL, 0x6A4B2CE30A3E152EULL}},
    {{0x3FFB341D7961BD1DULL, 0x092998376104D137ULL}, {0x3F89408683B296DEULL, 0x119E5439840E2017ULL}},
    {{0x3FFB6F0D28AE56B4ULL, 0xB9BE499B9ED19B64ULL}, {0x3F869CA183DECA10ULL, 0xFFBE47025E714069ULL}},
    {{0x3FFBA926D3A4AD56ULL, 0x3650BD22A9C3AA4CULL}, {0x3F89E6A7D9F88BB4ULL, 0xE634077AAC47E90AULL}},
    {{0x3FFBE27076E2AF2EULL, 0x5E9EA87FFE1FE9E1ULL}, {0x3F89576E53AF1005ULL, 0xFDBE55F74225F080ULL}},
    {{0x3FFC0D77E7CD08E5ULL, 0x96697717A40C2AE9ULL}, {0xBF88DBD9A581397EULL, 0x3CF883BF9C74BCBAULL}},
    {{0x3FFC29552F81FF52ULL, 0x34C05DC7101F6FA7ULL}, {0xBF8ADEC086DF1E10ULL, 0x3AC08A593F81BB24ULL}},
    {{0x3FFC44D2B6CCB7D1ULL, 0xE67D3D950F87E1E1ULL}, {0xBF89DF4D419BA2C2ULL, 0x7D56C1FD27F5F87FULL}},
    {{0x3FFC5FF3070A793DULL, 0x3C873E20A072123CULL}, {0xBF8A406554719540ULL, 0x418339C7EA602525ULL}},
    {{0x3FFC7AB890210D90ULL, 0x91BE36B2D6A06080ULL}, {0x3F8A91FF85253620ULL, 0x42CAD5D8C59740EFULL}},
    {{0x3FFC9525A9CF456BULL, 0x47641307538B8967ULL}, {0x3F8A899417DA79EEULL, 0x2ECD28E27B5433DBULL}},
    {{0x3FFCAF3C94E80BFFULL, 0x2D8CE601937CCF5DULL}, {0xBF8A1312C8E8CB45ULL, 0x5E6BD6B13BA29B40ULL}},
    {{0x3FFCC8FF7C79A9A2ULL, 0x1AC25D81EF2FFB9AULL}, {0x3F892571D97A9D04ULL, 0x6B706C5C3C4CFD59ULL}},
    {{0x3FFCE27076E2AF2EULL, 0x5E9EA87FFE1FE9E1ULL}, {0x3F8A576E53AF1005ULL, 0xFDBE55F74225F080ULL}},
    {{0x3FFCFB9186D5E3E2ULL, 0xA8D55466C3794D2DULL}, {0xBF8A7DB7F4F77549ULL, 0xBA6FD09D34116553ULL}},
    {{0x3FFD0A324E27390EULL, 0x35F73F7A018700C4ULL}, {0xBF8BD721FCA5041FULL, 0x68D21C9221C5E238ULL}},
    {{0x3FFD1675CABABA60ULL, 0xE039CC7D57106E2EULL}, {0x3F881F833E82521EULL, 0x0CFA9A61E46E1CCBULL}},
    {{0x3FFD22941FBCF796ULL, 0x5A242853DA75D433ULL}, {0xBF8868B2A9642C3CULL, 0x4E8C5C0EC4D68002ULL}},
    {{0x3FFD2E8E2BAE11D3ULL, 0x09C2CC91A85081B4ULL}, {0xBF8A90910A5209BBULL, 0x21D5BD9661B3155AULL}},
    {{0x3FFD3A64C556945EULL, 0x9C72F35CD740D08EULL}, {0xBF88485C31181FD6ULL, 0x1B91ECCB201B9831ULL}},
    {{0x3FFD4618BC21C5ECULL, 0x27D0B7B37B33C734ULL}, {0x3F8B05772CD24C00ULL, 0xB44393CB5EEB55CBULL}},
    {{0x3FFD51AAD872DF82ULL, 0xD09C93D60CFAAF19ULL}, {0xBF8BC56161950680ULL, 0xA3ED41DE065A3A51ULL}},
    {{0x3FFD5D1BDBF5809CULL, 0xA508D8E0F71FF845ULL}, {0x3F8B9F380B4A6B42ULL, 0x9A4BB094DF9CBB76ULL}},
    {{0x3FFD686C81E9B14AULL, 0xEC442BE1014E3869ULL}, {0x3F8B92DEDED1C339ULL, 0x5996523FA9AE6167ULL}},
    {{0x3FFD739D7F6BBD00ULL, 0x69CE24C53FAD3EF8ULL}, {0xBF8BA55221EF2315ULL, 0xA68A844436D87B2EULL}},
    {{0x3FFD7EAF83B82AFCULL, 0x364B3A5E7B4A5411ULL}, {0xBF8A1D3E6806811CULL, 0x22E569865F313334ULL}},
    {{0x3FFD89A3386C1425ULL, 0xAB5A71881103C157ULL}, {0xBF8A800739AFC97FULL, 0x5D4B9C15FC118C67ULL}},
    {{0x3FFD947941C2116FULL, 0xABA4CDD147D106EBULL}, {0xBF895B58CF688D7CULL, 0x1796073398734CF1ULL}},
    {{0x3FFD9F323ECBF984ULL, 0xBF2B68D766F40522ULL}, {0x3F89819F483FECD1ULL, 0x51F5F0ACE2B5E3B1ULL}},
};
/* atan(i/8) for i = 0..8, as {hi, lo} pairs */
static const uint64_t F128_ATAN_TABLE[9][2][2] = {
    {{0x0000000000000000ULL, 0x0000000000000000ULL}, {0x0000000000000000ULL, 0x0000000000000000ULL}},
    {{0x3FFBFD5BA9AAC2F6ULL, 0xDC65912F313E7D11ULL}, {0x3F87DEF1672AFB2BULL, 0xB35B245D926AEFBFULL}},
    {{0x3FFCF5B75F92C80DULL, 0xD62ADB8F3DEBEF44ULL}, {0x3F897E5AA7FA9038ULL, 0x8B3836B7A3A767C9ULL}},
    {{0x3FFD6F61941E4DEFULL, 0x08E715464245B9FDULL}, {0xBF8BDBE613DE3442ULL, 0x8D5D519DC660D1AEULL}},
    {{0x3FFDDAC670561BB4ULL, 0xF68ADFC88BD97875ULL}, {0x3F89A06DC282B0E4ULL, 0xC39BE01C59E2DCDDULL}},
    {{0x3FFE1E00BABDEFEBULL, 0x3F36B906BC2CCB88ULL}, {0x3F8CBA3C8C533F03ULL, 0x2959BAE7A9ED2369ULL}},
    {{0x3FFE4978FA3269EEULL, 0x12483350FE548AFBULL}, {0x3F8C64F71F84344FULL, 0x7595CF390A43332CULL}},
    {{0x3FFE700A7C578463ULL, 0x3CE7965B4AA42149ULL}, {0xBF8CDE16142899F7ULL, 0x59C32BFAFF6F5505ULL}},
    {{0x3FFE921FB54442D1ULL, 0x8469898CC51701B8ULL}, {0x3F8BCD129024E088ULL, 0xA67CC74020BBEA64ULL}},
};
/* 1/k! for k = 0..31 */
static const uint64_t F128_INV_FACT[32][2] = {
    {0x3FFF000000000000ULL, 0x0000000000000000ULL},
    {0x3FFF000000000000ULL, 0x0000000000000000ULL},
    {0x3FFE000000000000ULL, 0x0000000000000000ULL},
    {0x3FFC555555555555ULL, 0x5555555555555555ULL},
    {0x3FFA555555555555ULL, 0x5555555555555555ULL},
    {0x3FF8111111111111ULL, 0x1111111111111111ULL},
    {0x3FF56C16C16C16C1ULL, 0x6C16C16C16C16C17ULL},
    {0x3FF2A01A01A01A01ULL, 0xA01A01A01A01A01AULL},
    {0x3FEFA01A01A01A01ULL, 0xA01A01A01A01A01AULL},
    {0x3FEC71DE3A556C73ULL, 0x38FAAC1C88E50017ULL},
    {0x3FE927E4FB7789F5ULL, 0xC72EF016D3EA6679ULL},
    {0x3FE5AE64567F544EULL, 0x38FE747E4B837DC7ULL},
    {0x3FE21EED8EFF8D89ULL, 0x7B544DA987ACFE85ULL},
    {0x3FDE6124613A86D0ULL, 0x97CA38331D23AF68ULL},
    {0x3FDA93974A8C07C9ULL, 0xD20BADF145DFA3E5ULL},
    {0x3FD6AE7F3E733B81ULL, 0xF11D8656B0EE8CB0ULL},
    {0x3FD2AE7F3E733B81ULL, 0xF11D8656B0EE8CB0ULL},
    {0x3FCE952C77030AD4ULL, 0xA6B2605197771B00ULL},
    {0x3FCA6827863B97D9ULL, 0x77BB004886A2C2ABULL},
    {0x3FC62F49B4681415ULL, 0x724CA1EC3B7B9675ULL},
    {0x3FC1E542BA402022ULL, 0x507A9CAD2BF8F0BBULL},
    {0x3FBD71B8EF6DCF57ULL, 0x18BEF146FCEE6E45ULL},
    {0x3FB90CE396DB7F85ULL, 0x29450C90B7F338ECULL},
    {0x3FB4761B41316381ULL, 0x9D97B8704DD7F628ULL},
    {0x3FAFF2CF01972F57ULL, 0x7CCA4B4067CA9D8AULL},
    {0x3FAB3F3CCDD165FAULL, 0x8D4E44A419776F11ULL},
    {0x3FA688E85FC6A4E5ULL, 0x9A38F2050BA6B015ULL},
    {0x3FA1D1AB1C2DCCEAULL, 0x320A9A18F15D4277ULL},
    {0x3F9D0A18A2635085ULL, 0xD373C5C51C354A8DULL},
    {0x3F98259F98B4358AULL, 0xD7ABE30E7766F129ULL},
    {0x3F933932C5047D60ULL, 0xE60CADED4C2989C5ULL},
    {0x3F8E434D2E783F5BULL, 0xC42E1EE46FA6BFC4ULL},
};
/* 1/(2k+1) for k = 0..31 */
static const uint64_t F128_INV_ODD[32][2] = {
    {0x3FFF000000000000ULL, 0x0000000000000000ULL},
    {0x3FFD555555555555ULL, 0x5555555555555555ULL},
    {0x3FFC999999999999ULL, 0x999999999999999AULL},
    {0x3FFC249249249249ULL, 0x2492492492492492ULL},
    {0x3FFBC71C71C71C71ULL, 0xC71C71C71C71C71CULL},
    {0x3FFB745D1745D174ULL, 0x5D1745D1745D1746ULL},
    {0x3FFB3B13B13B13B1ULL, 0x3B13B13B13B13B14ULL},
    {0x3FFB111111111111ULL, 0x1111111111111111ULL},
    {0x3FFAE1E1E1E1E1E1ULL, 0xE1E1E1E1E1E1E1E2ULL},
    {0x3FFAAF286BCA1AF2ULL, 0x86BCA1AF286BCA1BULL},
    {0x3FFA861861861861ULL, 0x8618618618618618ULL},
    {0x3FFA642C8590B216ULL, 0x42C8590B21642C86ULL},
    {0x3FFA47AE147AE147ULL, 0xAE147AE147AE147BULL},
    {0x3FFA2F684BDA12F6ULL, 0x84BDA12F684BDA13ULL},
    {0x3FFA1A7B9611A7B9ULL, 0x611A7B9611A7B961ULL},
    {0x3FFA084210842108ULL, 0x4210842108421084ULL},
    {0x3FF9F07C1F07C1F0ULL, 0x7C1F07C1F07C1F08ULL},
    {0x3FF9D41D41D41D41ULL, 0xD41D41D41D41D41DULL},
    {0x3FF9BACF914C1BACULL, 0xF914C1BACF914C1CULL},
    {0x3FF9A41A41A41A41ULL, 0xA41A41A41A41A41AULL},
    {0x3FF98F9C18F9C18FULL, 0x9C18F9C18F9C18FAULL},
    {0x3FF97D05F417D05FULL, 0x417D05F417D05F41ULL},
    {0x3FF96C16C16C16C1ULL, 0x6C16C16C16C16C17ULL},
    {0x3FF95C9882B93105ULL, 0x72620AE4C415C988ULL},
    {0x3FF94E5E0A72F053ULL, 0x97829CBC14E5E0A7ULL},
    {0x3FF9414141414141ULL, 0x4141414141414141ULL},
    {0x3FF93521CFB2B78CULL, 0x13521CFB2B78C135ULL},
    {0x3FF929E4129E4129ULL, 0xE4129E4129E4129EULL},
    {0x3FF91F7047DC11F7ULL, 0x047DC11F7047DC12ULL},
    {0x3FF915B1E5F75270ULL, 0xD0456C797DD49C34ULL},
    {0x3FF90C9714FBCDA3ULL, 0xAC10C9714FBCDA3BULL},
    {0x3FF9041041041041ULL, 0x0410410410410410ULL},
};
/* Bits of 2/pi after the binary point, most significant first */
static const uint64_t F128_TWO_OVER_PI_BITS[262] = {
    0xA2F9836E4E441529ULL, 0xFC2757D1F534DDC0ULL, 0xDB6295993C439041ULL, 0xFE5163ABDEBBC561ULL,
    0xB7246E3A424DD2E0ULL, 0x06492EEA09D1921CULL, 0xFE1DEB1CB129A73EULL, 0xE88235F52EBB4484ULL,
    0xE99C7026B45F7E41ULL, 0x3991D639835339F4ULL, 0x9C845F8BBDF9283BULL, 0x1FF897FFDE05980FULL,
    0xEF2F118B5A0A6D1FULL, 0x6D367ECF27CB09B7ULL, 0x4F463F669E5FEA2DULL, 0x7527BAC7EBE5F17BULL,
    0x3D0739F78A5292EAULL, 0x6BFB5FB11F8D5D08ULL, 0x56033046FC7B6BABULL, 0xF0CFBC209AF4361DULL,
    0xA9E391615EE61B08ULL, 0x6599855F14A06840ULL, 0x8DFFD8804D732731ULL, 0x06061556CA73A8C9ULL,
    0x60E27BC08C6B47C4ULL, 0x19C367CDDCE8092AULL, 0x8359C4768B961CA6ULL, 0xDDAF44D15719053EULL,
    0xA5FF07053F7E33E8ULL, 0x32C2DE4F98327DBBULL, 0xC33D26EF6B1E5EF8ULL, 0x9F3A1F35CAF27F1DULL,
    0x87F121907C7C246AULL, 0xFA6ED5772D30433BULL, 0x15C614B59D19C3C2ULL, 0xC4AD414D2C5D000CULL,
    0x467D862D71E39AC6ULL, 0x9B0062337CD2B497ULL, 0xA7B4D55537F63ED7ULL, 0x1810A3FC764D2A9DULL,
    0x64ABD770F87C6357ULL, 0xB07AE715175649C0ULL, 0xD9D63B3884A7CB23ULL, 0x24778AD623545AB9ULL,
    0x1F001B0AF1DFCE19ULL, 0xFF319F6A1E666157ULL, 0x9947FBACD87F7EB7ULL, 0x652289E83260BFE6ULL,
    0xCDC4EF09366CD43FULL, 0x5DD7DE16DE3B5892ULL, 0x9BDE2822D2E88628ULL, 0x4D58E232CAC616E3ULL,
    0x08CB7DE050C017A7ULL, 0x1DF35BE01834132EULL, 0x6212830148835B8EULL, 0xF57FB0ADF2E91E43ULL,
    0x4A48D36710D8DDAAULL, 0x425FAECE616AA428ULL, 0x0AB499D3F2A6067FULL, 0x775C83C2A3883C61ULL,
    0x78738A5A8CAFBDD7ULL, 0x6F63A62DCBBFF4EFULL, 0x818D67C12645CA55ULL, 0x36D9CAD2A8288D61ULL,
    0xC277C9121426049BULL, 0x4612C459C444C5C8ULL, 0x91B24DF31700AD43ULL, 0xD4E5492910D5FDFCULL,
    0xBE00CC941EEECE70ULL, 0xF53E1380F1ECC3E7ULL, 0xB328F8C79405933EULL, 0x71C1B3092EF3450BULL,
    0x9C12887B20AB9FB5ULL, 0x2EC292472F327B6DULL, 0x550C90A7721FE76BULL, 0x96CB314A1679E279ULL,
    0x4189DFF49794E884ULL, 0xE6E29731996BED88ULL, 0x365F5F0EFDBBB49AULL, 0x486CA46742727132ULL,
    0x5D8DB8159F09E5BCULL, 0x25318D3974F71C05ULL, 0x30010C0D68084B58ULL, 0xEE2C90AA4702E774ULL,
    0x24D6BDA67DF77248ULL, 0x6EEF169FA6948EF6ULL, 0x91B45153D1F20ACFULL, 0x3398207E4BF56863ULL,
    0xB25F3EDD035D407FULL, 0x8985295255C06437ULL, 0x10D86D324832754CULL, 0x5BD4714E6E5445C1ULL,
    0x090B69F52AD56614ULL, 0x9D072750045DDB3BULL, 0xB4C576EA17F9877DULL, 0x6B49BA271D296996ULL,
    0xACCCC65414AD6AE2ULL, 0x9089D98850722CBEULL, 0xA4049407777030F3ULL, 0x27FC00A871EA49C2ULL,
    0x663DE06483DD9797ULL, 0x3FA3FD94438C860DULL, 0xDE41319D39928C70ULL, 0xDDE7B7173BDF082BULL,
    0x3715A0805C93805AULL, 0x921110D8E80FAF80ULL, 0x6C4BFFDB0F903876ULL, 0x185915A562BBCB61ULL,
    0xB989C7BD401004F2ULL, 0xD2277549F6B6EBBBULL, 0x22DBAA140A2F2689ULL, 0x768364333B091A94ULL,
    0x0EAA3A51C2A31DAEULL, 0xEDAF12265C4DC26DULL, 0x9C7A2D9756C0833FULL, 0x03F6F0098C402B99ULL,
    0x316D07B43915200CULL, 0x5BC3D8C492F54BADULL, 0xC6A5CA4ECD37A736ULL, 0xA9E69492AB6842DDULL,
    0xDE6319EF8C76528BULL, 0x6837DBFCABA1AE31ULL, 0x15DFA1AE00DAFB0CULL, 0x664D64B705ED3065ULL,
    0x29BF56573AFF47B9ULL, 0xF96AF3BE75DF9328ULL, 0x3080ABF68C6615CBULL, 0x040622FA1DE4D9A4ULL,
    0xB33D8F1B5709CD36ULL, 0xE9424EA4BE13B523ULL, 0x331AAAF0A8654FA5ULL, 0xC1D20F3F0BCD785BULL,
    0x76F923048B7B7217ULL, 0x8953A6C6E26E6F00ULL, 0xEBEF584A9BB7DAC4ULL, 0xBA66AACFCF761D02ULL,
    0xD12DF1B1C1998C77ULL, 0xADC3DA4886A05DF7ULL, 0xF480C62FF0AC9AECULL, 0xDDBC5C3F6DDED01FULL,
    0xC790B6DB2A3A25A3ULL, 0x9AAF009353AD0457ULL, 0xB6B42D297E804BA7ULL, 0x07DA0EAA76A1597BULL,
    0x2A12162DB7DCFDE5ULL, 0xFAFEDB89FDBE896CULL, 0x76E4FCA90670803EULL, 0x156E85FF87FD073EULL,
    0x2833676186182AEAULL, 0xBD4DAFE7B36E6D8FULL, 0x3967955BBF3148D7ULL, 0x8416DF30432DC735ULL,
    0x6125CE70C9B8CB30ULL, 0xFD6CBFA200A4E46CULL, 0x05A0DD5A476F21D2ULL, 0x1262845CB9496170ULL,
    0xE0566B0152993755ULL, 0x50B7D51EC4F1335FULL, 0x6E13E4305DA92E85ULL, 0xC3B21D3632A1A4B7ULL,
    0x08D4B1EA21F716E4ULL, 0x698F77FF2780030CULL, 0x2D408DA0CD4F99A5ULL, 0x20D3A2B30A5D2F42ULL,
    0xF9B4CBDA11D0BE7DULL, 0xC1DB9BBD17AB81A2ULL, 0xCA5C6A0817552E55ULL, 0x0027F0147F8607E1ULL,
    0x640B148D4196DEBEULL, 0x872AFDDAB6256B34ULL, 0x897BFEF3059EBFB9ULL, 0x4F6A68A82A4A5AC4ULL,
    0x4FBCF82D985AD795ULL, 0xC7F48D4D0DA63A20ULL, 0x5F57A4B13F149538ULL, 0x800120CC86DD71B6ULL,
    0xDEC9F560BF11654DULL, 0x6B0701ACB08CD0C0ULL, 0xB24855510EFB1EC3ULL, 0x72953B06A33540C0ULL,
    0x7BDC06CC45E0FA29ULL, 0x4EC8CAD641F3E8DEULL, 0x647CD8649B31BED9ULL, 0xC397A4D45877C5E3ULL,
    0x6913DAF03C3ABA46ULL, 0x18465F7555F5BDD2ULL, 0xC6926E5D2EACED44ULL, 0x0E423E1C87C461E9ULL,
    0xFD29F3D6E7CA7C22ULL, 0x35916FC5E0088DD7ULL, 0xFFE26A6EC6FDB0C1ULL, 0x0893745D7CB2AD6BULL,
    0x9D6ECD7B723E6A11ULL, 0xC6A9CFF7DF7329BAULL, 0xC9B55100B70DB2E2ULL, 0x24BA74607DE58AD8ULL,
    0x742C150D0C188194ULL, 0x667E162901767A9FULL, 0xBEFDFDEF4556367EULL, 0xD913D9ECB9BA8BFCULL,
    0x97C427A831C36EF1ULL, 0x36C59456A8D8B5A8ULL, 0xB40ECCCF2D891234ULL, 0x576F89562CE3CE99ULL,
    0xB920D6AA5E6B9C2AULL, 0x3ECC5F114A0BFDFBULL, 0xF4E16D3B8E2C86E2ULL, 0x84D4E9A9B4FCD1EEULL,
    0xEFC9352E61392F44ULL, 0x2138C8D91B0AFC81ULL, 0x6A4AFBD81C2F84B4ULL, 0x538C994ECC2254DCULL,
    0x552AD6C6C096190BULL, 0xB8701A649569605AULL, 0x26EE523F0F117F11ULL, 0xB5F4F5CBFC2DBC34ULL,
    0xEEBC34CC5DE8605EULL, 0xDD9B8E67EF3392B8ULL, 0x17C99B5861BC57E1ULL, 0xC68351103ED84871ULL,
    0xDDDD1C2DA118AF46ULL, 0x2C21D7F359987AD9ULL, 0xC0549EFA864FFC06ULL, 0x56AE79E536228922ULL,
    0xAD38DC9367AAE855ULL, 0x3826829BE7CAA40DULL, 0x51B133990ED7A948ULL, 0x0569F0B265A7887FULL,
    0x974C8836D1F9B392ULL, 0x214A827B21CF98DCULL, 0x9F405547DC3A74E1ULL, 0x42EB67DF9DFE5FD4ULL,
    0x5EA4677B7AACBAA2ULL, 0xF65523882B55BA41ULL, 0x086E59862A218347ULL, 0x39E6E389D49EE540ULL,
    0xFB49E956FFCA0F1CULL, 0x8A59C52BFA94C5C1ULL, 0xD3CFC50FAE5ADB86ULL, 0xC5476243853B8621ULL,
    0x94792C8761107B4CULL, 0x2A1A2C8012BF4390ULL, 0x2688893C78E4C4A8ULL, 0x7BDBE5C23AC4EAF4ULL,
    0x268A67F7BF920D2BULL, 0xA365B1933D0B7CBDULL, 0xDC51A463DD27DDE1ULL, 0x6919949A9529A828ULL,
    0xCE68B4ED09209F44ULL, 0xCA984E638270237CULL, 0x7E32B90F8EF5A7E7ULL, 0x561408F1212A9DB5ULL,
    0x4D7E6F5119A5ABF9ULL, 0xB5D6DF8261DD9602ULL, 0x36169F3AC4A1A283ULL, 0x6DED727A8D39A9B8ULL,
    0x825C326B5B2746EDULL, 0x34007700D255F4FCULL, 0x4D59018071E0E13FULL, 0x89B295F364A8F1AEULL,
    0xA74B38FC4CEAB2BBULL, 0x47270BABC3A734BAULL,
};

static lf_float128 f128_bits(const uint64_t w[2]) {
    lf_float128 r;
    memcpy(r.bytes,     &w[1], 8);
    memcpy(r.bytes + 8, &w[0], 8);
    return r;
}

static inline lf_float128 f_add(lf_float128 a, lf_float128 b) { return __addtf3_lf_impl(a, b); }
static inline lf_float128 f_sub(lf_float128 a, lf_float128 b) { return __subtf3_lf_impl(a, b); }
static inline lf_float128 f_mul(lf_float128 a, lf_float128 b) { return __multf3_lf_impl(a, b); }
static inline lf_float128 f_div(lf_float128 a, lf_float128 b) { return __divtf3_lf_impl(a, b); }
static inline lf_float128 f_neg(lf_float128 a) { return __negtf2_lf_impl(a); }
static inline lf_float128 f_int(int64_t i) { return __floatditf_lf_impl(i); }
static inline double      f_dbl(lf_float128 a) { return __trunctfdf2_lf_impl(a); }
static inline lf_float128 f_sign(lf_float128 a, int neg) { return neg ? f_neg(a) : a; }

/* x * 2^n, correctly rounded also when the result is subnormal. */
static lf_float128 f128_scale(lf_float128 x, int32_t n) {
    f128_parts p = f128_unpack(x);
    if (p.is_nan || p.is_inf || p.is_zero || n == 0) return x;
    int nsh = u128_clz(p.mant) - 15;
    u128 m = u128_shl(p.mant, nsh);
    int64_t e = (int64_t)p.exp - nsh + n;
    if (e > 16383) return f128_make_inf(p.sign);
    if (e >= -16382) {
        return f128_const(p.sign, (int32_t)e + 16383, u128_hi(m), u128_lo(m));
    }
    int64_t shift = -16382 - e;
    if (shift > 120) return f128_make_zero(p.sign);
    m = u128_shl(m, F128_ADD_GRS);
    u128 lost = u128_and(m, u128_sub(u128_shl(u128_one(), (int)shift),
                                     u128_one()));
    m = u128_shr(m, (int)shift);
    if (!u128_is_zero(lost)) m.lo |= 1;
    return f128_round_pack_grs(p.sign, -16382, m);
}

/* Unevaluated sum hi + lo with |lo| <= ulp(hi)/2 (after normalisation). */
typedef struct { lf_float128 hi, lo; } f128_dw;

static f128_dw dw_make(lf_float128 hi, lf_float128 lo) {
    f128_dw r; r.hi = hi; r.lo = lo; return r;
}

static f128_dw dw_from_bits(const uint64_t w[2][2]) {
    return dw_make(f128_bits(w[0]), f128_bits(w[1]));
}

/* s + e = a + b exactly, s = fl(a + b). */
static f128_dw two_sum(lf_float128 a, lf_float128 b) {
    lf_float128 s  = f_add(a, b);
    lf_float128 bb = f_sub(s, a);
    lf_float128 e  = f_add(f_sub(a, f_sub(s, bb)), f_sub(b, bb));
    return dw_make(s, e);
}

/* Same as two_sum, for |a| >= |b| (or a == 0). */
static f128_dw fast_two_sum(lf_float128 a, lf_float128 b) {
    lf_float128 s = f_add(a, b);
    return dw_make(s, f_sub(b, f_sub(s, a)));
}

/* Veltkamp splitting a = hi + lo, each part with at most 56 significant
 * bits, so that the products of two parts are exact. */
static void f128_split(lf_float128 a, lf_float128 *hi, lf_float128 *lo) {
    /* 2^57 + 1 */
    lf_float128 c = f_mul(f128_const(0, 16383 + 57, 0, (uint64_t)1 << 55), a);
    *hi = f_sub(c, f_sub(c, a));
    *lo = f_sub(a, *hi);
}

/* p + e = a * b exactly, p = fl(a * b) (Dekker). |a|, |b| < 2^16000. */
static f128_dw two_prod(lf_float128 a, lf_float128 b) {
    lf_float128 p = f_mul(a, b);
    lf_float128 ah, al, bh, bl;
    f128_split(a, &ah, &al);
    f128_split(b, &bh, &bl);
    lf_float128 e = f_sub(f_mul(ah, bh), p);
    e = f_add(e, f_mul(ah, bl));
    e = f_add(e, f_mul(al, bh));
    e = f_add(e, f_mul(al, bl));
    return dw_make(p, e);
}

static f128_dw dw_add(f128_dw a, f128_dw b) {
    f128_dw s = two_sum(a.hi, b.hi);
    return fast_two_sum(s.hi, f_add(s.lo, f_add(a.lo, b.lo)));
}

static f128_dw dw_neg(f128_dw a) { return dw_make(f_neg(a.hi), f_neg(a.lo)); }

static f128_dw dw_mul(f128_dw a, f128_dw b) {
    f128_dw p = two_prod(a.hi, b.hi);
    lf_float128 e = f_add(f_mul(a.hi, b.lo), f_mul(a.lo, b.hi));
    return fast_two_sum(p.hi, f_add(p.lo, e));
}

static f128_dw dw_mul_q(f128_dw a, lf_float128 b) {
    f128_dw p = two_prod(a.hi, b);
    return fast_two_sum(p.hi, f_add(p.lo, f_mul(a.lo, b)));
}

static f128_dw dw_div(f128_dw a, f128_dw b) {
    lf_float128 q = f_div(a.hi, b.hi);
    f128_dw p = two_prod(q, b.hi);
    lf_float128 r = f_sub(f_sub(a.hi, p.hi), p.lo);
    r = f_add(r, f_sub(a.lo, f_mul(q, b.lo)));
    return fast_two_sum(q, f_div(r, b.hi));
}

/* sqrt(a.hi + a.lo) for a positive double-word a. */
static f128_dw dw_sqrt(f128_dw a) {
    lf_float128 s = lf_f128_sqrt(a.hi);
    f128_dw p = two_prod(s, s);
    lf_float128 r = f_add(f_sub(f_sub(a.hi, p.hi), p.lo), a.lo);
    return fast_two_sum(s, f_div(r, f128_scale(s, 1)));
}

/* c[first] + x*(c[first+step] + x*(... + x*c[last])) */
static lf_float128 f128_poly(lf_float128 x, const uint64_t (*c)[2],
                             int first, int last, int step) {
    lf_float128 r = f128_bits(c[last]);
    for (int k = last - step; k >= first; k -= step) {
        r = f_add(f_mul(r, x), f128_bits(c[k]));
    }
    return r;
}

/* exp(r) - 1 for |r| <= 0.36 (Taylor series). */
static lf_float128 f128_expm1_kernel(lf_float128 r) {
    lf_float128 q = f128_poly(r, F128_INV_FACT, 2, 26, 1);
    return f_add(r, f_mul(f_mul(r, r), q));
}

/* exp(xh + xl) = 2^n * (1 + p) for |xh| < 11500; returns p, sets n. */
static lf_float128 f128_exp_core(lf_float128 xh, lf_float128 xl,
                                 int32_t *n) {
    *n = (int32_t)floor(f_dbl(xh) * 1.4426950408889634 + 0.5);
    lf_float128 nf = f_int(*n);
    /* n * LN2_HI98 is exact and so is the difference. */
    lf_float128 r = f_sub(xh, f_mul(nf, f128_bits(F128_LN2_HI98)));
    r = f_add(f_sub(r, f_mul(nf, f128_bits(F128_LN2_LO98))), xl);
    return f128_expm1_kernel(r);
}

lf_float128 lf_f128_exp(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (p.is_inf) return p.sign ? f128_make_zero(0) : a;
    if (p.is_zero) return f128_one();
    double d = f_dbl(a);
    if (d > 11357.0) return f128_make_inf(0);
    if (d < -11434.0) return f128_make_zero(0);
    int32_t n;
    lf_float128 q = f128_exp_core(a, f128_make_zero(0), &n);
    return f128_scale(f_add(f128_one(), q), n);
}

lf_float128 lf_f128_expm1(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_zero) return a;
    if (p.is_inf) return p.sign ? f_neg(f128_one()) : a;
    double d = f_dbl(a);
    if (d > -0.35 && d < 0.35) return f128_expm1_kernel(a);
    if (d > 11357.0) return f128_make_inf(0);
    if (d < -80.0) return f_neg(f128_one());
    int32_t n;
    lf_float128 q = f128_exp_core(a, f128_make_zero(0), &n);
    if (n > 113) return f_sub(f128_scale(f_add(f128_one(), q), n), f128_one());
    /* 2^n - 1 is exact for -113 <= n <= 113. */
    lf_float128 two_n = f128_scale(f128_one(), n);
    return f_add(f_sub(two_n, f128_one()), f128_scale(q, n));
}

lf_float128 lf_f128_exp2(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (p.is_inf) return p.sign ? f128_make_zero(0) : a;
    if (p.is_zero) return f128_one();
    double d = f_dbl(a);
    if (d > 16384.0) return f128_make_inf(0);
    if (d < -16495.0) return f128_make_zero(0);
    int32_t n = (int32_t)floor(d + 0.5);
    lf_float128 r = f_sub(a, f_int(n));   /* exact */
    f128_dw y = dw_mul_q(dw_from_bits(F128_LN2), r);
    int32_t m;
    lf_float128 q = f128_exp_core(y.hi, y.lo, &m);
    return f128_scale(f_add(f128_one(), q), n + m);
}

/* log(a) as a double-word, for a finite and positive.
 *
 * a = 2^k * m with m in [0.75, 1.5), m = F * (1 + f/F) with F = 1 + i/64
 * and |f| <= 1/128, so that log(a) = k*log(2) + log(F) + 2*atanh(u) with
 * u = f / (F + m), |u| < 2^-7.5. */
static f128_dw f128_log_dw(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    int nsh = u128_clz(p.mant) - 15;
    u128 mant = u128_shl(p.mant, nsh);
    int32_t k = p.exp - nsh;
    int big = u128_bit(mant, 111);   /* significand >= 1.5 */
    lf_float128 m = f128_const(0, big ? 16382 : 16383, u128_hi(mant),
                               u128_lo(mant));
    if (big) k++;
    int i = (int)floor((f_dbl(m) - 1.0) * 64.0 + 0.5);
    if (i < -16) i = -16;
    if (i > 32) i = 32;
    lf_float128 F = f128_scale(f_int(64 + i), -6);
    lf_float128 f = f_sub(m, F);       /* exact */
    f128_dw s = two_sum(F, m);
    /* u = f / s as a double-word */
    lf_float128 uh = f_div(f, s.hi);
    f128_dw pr = two_prod(uh, s.hi);
    lf_float128 rr = f_sub(f_sub(f, pr.hi), pr.lo);
    rr = f_sub(rr, f_mul(uh, s.lo));
    lf_float128 ul = f_div(rr, s.hi);
    /* 2*atanh(u) = 2u + 2u * v * (1/3 + v/5 + ...), v = u^2 */
    lf_float128 v = f_mul(uh, uh);
    lf_float128 tail = f_mul(f_mul(f128_scale(uh, 1), v),
                             f128_poly(v, F128_INV_ODD, 1, 9, 1));
    f128_dw lf = dw_from_bits(F128_LOG_TABLE[i + 16]);
    lf_float128 kf = f_int(k);
    f128_dw s1 = two_sum(f_mul(kf, f128_bits(F128_LN2_HI98)), lf.hi);
    f128_dw s2 = two_sum(s1.hi, f128_scale(uh, 1));
    lf_float128 lo = f_add(s1.lo, s2.lo);
    lo = f_add(lo, f_mul(kf, f128_bits(F128_LN2_LO98)));
    lo = f_add(lo, lf.lo);
    lo = f_add(lo, f_add(f128_scale(ul, 1), tail));
    return fast_two_sum(s2.hi, lo);
}

/* NaN, -inf, +inf or a result for the special arguments of log;
 * returns 0 if a is finite and positive. */
static int f128_log_special(lf_float128 a, lf_float128 *r) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) { *r = a; return 1; }
    if (p.is_zero) { *r = f128_make_inf(1); return 1; }
    if (p.sign) { *r = f128_make_nan(); return 1; }
    if (p.is_inf) { *r = a; return 1; }
    return 0;
}

lf_float128 lf_f128_log(lf_float128 a) {
    lf_float128 r;
    if (f128_log_special(a, &r)) return r;
    return f128_log_dw(a).hi;
}

lf_float128 lf_f128_log2(lf_float128 a) {
    lf_float128 r;
    if (f128_log_special(a, &r)) return r;
    return dw_mul(f128_log_dw(a), dw_from_bits(F128_LOG2E)).hi;
}

lf_float128 lf_f128_log10(lf_float128 a) {
    lf_float128 r;
    if (f128_log_special(a, &r)) return r;
    return dw_mul(f128_log_dw(a), dw_from_bits(F128_LOG10E)).hi;
}

/* log(1 + a) */
static lf_float128 f128_log1p(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_zero) return a;
    if (p.is_inf) return p.sign ? f128_make_nan() : a;
    f128_dw s = two_sum(f128_one(), a);
    lf_float128 r;
    if (f128_log_special(s.hi, &r)) return r;
    f128_dw l = f128_log_dw(s.hi);
    return f_add(l.hi, f_add(l.lo, f_div(s.lo, s.hi)));
}

/* 0 if b is not an integer, 1 if it is an odd and 2 if an even integer.
 * b is finite and nonzero. */
static int f128_integer_kind(f128_parts b) {
    if (b.exp >= 113) return 2;
    if (b.exp < 0) return 0;
    int frac_bits = 112 - b.exp;
    u128 mask = u128_sub(u128_shl(u128_one(), frac_bits), u128_one());
    if (!u128_is_zero(u128_and(b.mant, mask))) return 0;
    return u128_bit(b.mant, frac_bits) ? 1 : 2;
}

lf_float128 lf_f128_pow(lf_float128 base, lf_float128 exp_v) {
    f128_parts pa = f128_unpack(base), pb = f128_unpack(exp_v);
    lf_float128 one = f128_one();
    if (pb.is_zero) return one;                       /* x^0 = 1 */
    if (f128_cmp_internal(base, one) == 0) return one;  /* 1^y = 1 */
    if (pa.is_nan || pb.is_nan) return f128_make_nan();
    if (pb.is_inf) {
        int c = f128_cmp_internal(lf_f128_abs(base), one);
        if (c == 0) return one;                       /* (-1)^inf = 1 */
        return ((c < 0) == pb.sign) ? f128_make_inf(0) : f128_make_zero(0);
    }
    int ikind = f128_integer_kind(pb);
    int odd = (ikind == 1);
    if (pa.is_zero) {
        int s = odd ? pa.sign : 0;
        return pb.sign ? f128_make_inf(s) : f128_make_zero(s);
    }
    if (pa.is_inf) {
        int s = pa.sign && odd;
        return pb.sign ? f128_make_zero(s) : f128_make_inf(s);
    }
    if (pa.sign && ikind == 0) return f128_make_nan();  /* (-x)^y */
    int rsign = pa.sign && odd;
    lf_float128 ax = lf_f128_abs(base);
    /* (-1)^y = +-1, also for the huge (even) y the large path below
     * would otherwise overflow. */
    if (f128_cmp_internal(ax, one) == 0) return f_sign(one, rsign);

    /* Small integer exponents: repeated multiplication. */
    if (ikind && pb.exp <= 2) {
        int64_t n = __fixtfdi_lf_impl(exp_v);
        int64_t un = n < 0 ? -n : n;
        if (un <= 4) {
            lf_float128 r = ax;
            for (int64_t i = 1; i < un; i++) r = f_mul(r, ax);
            if (n < 0) r = f_div(one, r);
            return f_sign(r, rsign);
        }
    }

    /* x^y = exp(y * log(x)), with log(x) and the product in double-word
     * precision. |y| > 2^16000 always overflows or underflows. */
    int large = pb.exp > 16000;
    f128_dw y = large ? dw_make(exp_v, f128_make_zero(0))
                      : dw_mul_q(f128_log_dw(ax), exp_v);
    if (large && (f128_cmp_internal(ax, one) < 0)) y.hi = f_neg(y.hi);
    double d = f_dbl(y.hi);
    if (d > 11357.0) return f128_make_inf(rsign);
    if (d < -11434.0) return f128_make_zero(rsign);
    int32_t n;
    lf_float128 q = f128_exp_core(y.hi, y.lo, &n);
    return f_sign(f128_scale(f_add(one, q), n), rsign);
}

/* ========================================================================
 * L. Math: sin, cos, tan, asin, acos, atan, atan2 and their degree
 *    variants sind, cosd, tand, asind, acosd, atand
 * ======================================================================== */

/* sin(r.hi + r.lo) for |r| <= ~pi/4 (Taylor series). */
static lf_float128 f128_sin_kernel(f128_dw r) {
    lf_float128 x = r.hi;
    lf_float128 z = f_mul(x, x);
    /* sin(x) = x - x*z*(1/3! - z/5! + ...),
     * sin(x + lo) ~= sin(x) + lo*(1 - z/2) */
    lf_float128 q = f128_poly(f_neg(z), F128_INV_FACT, 3, 31, 2);
    lf_float128 t = f_mul(r.lo, f_sub(f128_one(), f128_scale(z, -1)));
    t = f_sub(t, f_mul(f_mul(x, z), q));
    return f_add(x, t);
}

/* cos(r.hi + r.lo) for |r| <= ~pi/4 (Taylor series). */
static lf_float128 f128_cos_kernel(f128_dw r) {
    lf_float128 x = r.hi;
    lf_float128 z = f_mul(x, x);
    lf_float128 hz = f128_scale(z, -1);
    lf_float128 w = f_sub(f128_one(), hz);
    lf_float128 corr = f_sub(f_sub(f128_one(), w), hz);   /* exact */
    /* cos(x) = 1 - z/2 + z^2*(1/4! - z/6! + ...),
     * cos(x + lo) ~= cos(x) - lo*x */
    lf_float128 q = f128_poly(f_neg(z), F128_INV_FACT, 4, 30, 2);
    lf_float128 t = f_sub(f_mul(f_mul(z, z), q), f_mul(x, r.lo));
    return f_add(w, f_add(corr, t));
}

/* The bit of a little-endian multiword integer, 0 for negative positions. */
static int f128_mw_bit(const uint64_t *a, int n_words, int pos) {
    if (pos < 0 || pos >= 64 * n_words) return 0;
    return (int)((a[pos / 64] >> (pos % 64)) & 1);
}

/* The `len` (<= 113) bits of a multiword integer starting at bit `pos`. */
static u128 f128_mw_bits(const uint64_t *a, int n_words, int pos, int len) {
    u128 r = u128_zero();
    for (int i = len - 1; i >= 0; i--) {
        r = u128_shl1(r);
        if (f128_mw_bit(a, n_words, pos + i)) r.lo |= 1;
    }
    return r;
}

/* Payne-Hanek reduction of a finite |x| >= 2^60: x = n*pi/2 + r with
 * |r| <= pi/4. Returns n mod 4 and r as a double-word. */
static int f128_rem_pio2_large(f128_parts p, f128_dw *r) {
    /* |x| = M * 2^E with the integer M = p.mant. The bits b_i of 2/pi
     * (2/pi = sum b_i 2^-i) with i < E - 1 only add multiples of 4 to
     * x * 2/pi, so a window of 320 bits from b_s, s = max(E - 1, 1), gives
     * x * 2/pi mod 4 to within 2^-205. */
    enum { WIN = 320, NW = WIN / 64, NP = NW + 2 };
    int32_t E = p.exp - 112;
    int32_t s = E - 1 > 1 ? E - 1 : 1;
    int start = s - 1, wi = start / 64, sh = start % 64;
    uint64_t w[NW];
    for (int j = 0; j < NW; j++) {
        uint64_t hi = F128_TWO_OVER_PI_BITS[wi + j];
        uint64_t lo = F128_TWO_OVER_PI_BITS[wi + j + 1];
        w[NW - 1 - j] = sh ? (hi << sh) | (lo >> (64 - sh)) : hi;
    }
    /* prod = M * window */
    uint64_t prod[NP];
    uint64_t m[2];
    m[0] = u128_lo(p.mant);
    m[1] = u128_hi(p.mant);
    for (int i = 0; i < NP; i++) prod[i] = 0;
    for (int i = 0; i < 2; i++) {
        uint64_t carry = 0;
        for (int j = 0; j < NW; j++) {
            u128 t = mul64x64(m[i], w[j]);
            t = u128_add(t, u128_from_u64(prod[i + j]));
            t = u128_add(t, u128_from_u64(carry));
            prod[i + j] = u128_lo(t);
            carry = u128_hi(t);
        }
        prod[i + NW] += carry;
    }
    /* x * 2/pi mod 4 = prod / 2^k */
    int k = s + WIN - 1 - E;
    int n = f128_mw_bit(prod, NP, k) | (f128_mw_bit(prod, NP, k + 1) << 1);
    /* Keep the fraction only. */
    for (int i = 0; i < 64 * NP; i++) {
        if (i >= k) prod[i / 64] &= ~((uint64_t)1 << (i % 64));
    }
    int neg = f128_mw_bit(prod, NP, k - 1);
    if (neg) {
        /* fraction >= 1/2: use fraction - 1 = -(2^k - fraction) */
        uint64_t borrow = 0;
        for (int i = 0; i < NP; i++) {
            uint64_t v = prod[i];
            uint64_t res = (uint64_t)0 - v - borrow;
            borrow = (v != 0 || borrow) ? 1 : 0;
            prod[i] = res;
        }
        for (int i = k; i < 64 * NP; i++) {
            prod[i / 64] &= ~((uint64_t)1 << (i % 64));
        }
        n = (n + 1) & 3;
    }
    int h = k - 1;
    while (h >= 0 && !f128_mw_bit(prod, NP, h)) h--;
    if (h < 0) {
        *r = dw_make(f128_make_zero(0), f128_make_zero(0));
    } else {
        /* fraction = fh + fl, the top 226 bits */
        lf_float128 fh = f128_pack_parts(0, h - k,
            f128_mw_bits(prod, NP, h - 112, 113));
        lf_float128 fl = f128_pack_parts(0, h - k - 113,
            f128_mw_bits(prod, NP, h - 225, 113));
        f128_dw t = two_prod(fh, f128_bits(F128_PIO2_1));
        lf_float128 e = f_add(f_mul(fh, f128_bits(F128_PIO2_2)),
                              f_mul(fl, f128_bits(F128_PIO2_1)));
        *r = fast_two_sum(t.hi, f_add(t.lo, e));
    }
    if (neg) *r = dw_neg(*r);
    if (p.sign) {
        *r = dw_neg(*r);
        n = (4 - n) & 3;
    }
    return n;
}

/* x = n*pi/2 + r with |r| <= ~pi/4 for finite x. Returns n mod 4. */
static int f128_rem_pio2(lf_float128 x, f128_dw *r) {
    f128_parts p = f128_unpack(x);
    if (p.is_zero || f_dbl(lf_f128_abs(x)) <= 0.785398163397448) {
        *r = dw_make(x, f128_make_zero(0));
        return 0;
    }
    if (p.exp >= 60) return f128_rem_pio2_large(p, r);
    /* Cody-Waite with pi/2 = PIO2_1 + PIO2_2 + PIO2_3 (339 bits) */
    lf_float128 q = f_mul(x, f128_bits(F128_TWO_OVER_PI));
    int64_t n = __fixtfdi_lf_impl(lf_f128_floor(f_add(q, f128_half())));
    lf_float128 nf = f_int(n);
    f128_dw p1 = two_prod(nf, f128_bits(F128_PIO2_1));
    f128_dw p2 = two_prod(nf, f128_bits(F128_PIO2_2));
    lf_float128 p3 = f_mul(nf, f128_bits(F128_PIO2_3));
    lf_float128 t = f_sub(x, p1.hi);    /* exact */
    f128_dw a = two_sum(t, f_neg(p1.lo));
    f128_dw b = two_sum(a.hi, f_neg(p2.hi));
    lf_float128 e = f_add(a.lo, b.lo);
    e = f_sub(e, f_add(p2.lo, p3));
    *r = fast_two_sum(b.hi, e);
    return (int)(n & 3);
}

lf_float128 lf_f128_sin(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return a;
    f128_dw r;
    switch (f128_rem_pio2(a, &r)) {
        case 0:  return f128_sin_kernel(r);
        case 1:  return f128_cos_kernel(r);
        case 2:  return f_neg(f128_sin_kernel(r));
        default: return f_neg(f128_cos_kernel(r));
    }
}

lf_float128 lf_f128_cos(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return f128_one();
    f128_dw r;
    switch (f128_rem_pio2(a, &r)) {
        case 0:  return f128_cos_kernel(r);
        case 1:  return f_neg(f128_sin_kernel(r));
        case 2:  return f_neg(f128_cos_kernel(r));
        default: return f128_sin_kernel(r);
    }
}

/* tan for x = n*pi/2 + r */
static lf_float128 f128_tan_reduced(int n, f128_dw r) {
    lf_float128 s = f128_sin_kernel(r);
    lf_float128 c = f128_cos_kernel(r);
    if (n & 1) return f_neg(f_div(c, s));
    return f_div(s, c);
}

lf_float128 lf_f128_tan(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return a;
    f128_dw r;
    int n = f128_rem_pio2(a, &r);
    return f128_tan_reduced(n, r);
}

/* atan(x.hi + x.lo) for 0 <= x <= 1: atan(x) = atan(c) + atan(t) with
 * c = i/8 and t = (x - c)/(1 + x*c), |t| <= 1/16. */
static f128_dw f128_atan_core(f128_dw x) {
    int i = (int)floor(f_dbl(x.hi) * 8.0 + 0.5);
    if (i < 0) i = 0;
    if (i > 8) i = 8;
    lf_float128 t;
    if (i == 0) {
        t = x.hi;
    } else {
        lf_float128 c = f128_scale(f_int(i), -3);
        lf_float128 num = f_add(f_sub(x.hi, c), x.lo);   /* x.hi - c is exact */
        lf_float128 den = f_add(f128_one(), f_mul(x.hi, c));
        t = f_div(num, den);
    }
    /* atan(t) = t - t*w*(1/3 - w/5 + ...), w = t^2 */
    lf_float128 w = f_mul(t, t);
    lf_float128 at = f_sub(t, f_mul(f_mul(t, w),
                           f128_poly(f_neg(w), F128_INV_ODD, 1, 16, 1)));
    if (i == 0) {
        at = f_add(at, x.lo);
        return fast_two_sum(at, f128_make_zero(0));
    }
    f128_dw ac = dw_from_bits(F128_ATAN_TABLE[i]);
    return fast_two_sum(ac.hi, f_add(ac.lo, at));
}

static f128_dw f128_pio2_dw(void) {
    return dw_make(f128_bits(F128_PIO2_1), f128_bits(F128_PIO2_2));
}

static f128_dw f128_pi_dw(void) {
    return dw_from_bits(F128_PI);
}

/* atan2(y, x) in [0, pi/2] for y, x >= 0, not both zero. */
static f128_dw f128_atan2_pos(f128_dw y, f128_dw x) {
    f128_parts py = f128_unpack(y.hi), px = f128_unpack(x.hi);
    if (py.is_zero) return dw_make(f128_make_zero(0), f128_make_zero(0));
    if (px.is_zero) return f128_pio2_dw();
    /* Far apart: atan(t) = t and pi/2 - atan(1/t) = pi/2 - 1/t. */
    if (px.exp - py.exp > 120) {
        return dw_make(f_div(y.hi, x.hi), f128_make_zero(0));
    }
    if (py.exp - px.exp > 120) {
        f128_dw pio2 = f128_pio2_dw();
        return fast_two_sum(pio2.hi, f_sub(pio2.lo, f_div(x.hi, y.hi)));
    }
    /* Scale both close to 1, so that the double-word operations below
     * neither overflow nor underflow. */
    int32_t e = px.exp > py.exp ? px.exp : py.exp;
    y = dw_make(f128_scale(y.hi, -e), f128_scale(y.lo, -e));
    x = dw_make(f128_scale(x.hi, -e), f128_scale(x.lo, -e));
    if (f128_cmp_internal(y.hi, x.hi) <= 0) {
        return f128_atan_core(dw_div(y, x));
    }
    return dw_add(f128_pio2_dw(), dw_neg(f128_atan_core(dw_div(x, y))));
}

static f128_dw f128_atan_dw(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    f128_dw r;
    if (p.is_inf) {
        r = f128_pio2_dw();
    } else if (p.exp < -57) {
        r = dw_make(lf_f128_abs(a), f128_make_zero(0));  /* atan(x) = x */
    } else {
        r = f128_atan2_pos(dw_make(lf_f128_abs(a), f128_make_zero(0)),
                           dw_make(f128_one(), f128_make_zero(0)));
    }
    return p.sign ? dw_neg(r) : r;
}

lf_float128 lf_f128_atan(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_zero) return a;
    return f128_atan_dw(a).hi;
}

lf_float128 lf_f128_atan2(lf_float128 y, lf_float128 x) {
    f128_parts px = f128_unpack(x), py = f128_unpack(y);
    if (px.is_nan || py.is_nan) return f128_make_nan();
    f128_dw r;
    if (py.is_zero) {
        /* atan2(+-0, x) = +-0 for x > 0 or x = +0, +-pi otherwise */
        if (!px.sign) return y;
        r = f128_pi_dw();
    } else if (px.is_inf) {
        f128_dw pi = f128_pi_dw();
        if (py.is_inf) {
            /* pi/4 or 3*pi/4 */
            r = px.sign ? dw_mul_q(pi, f128_const(0, 16382, (uint64_t)1 << 47, 0))
                        : dw_make(f128_scale(pi.hi, -2), f128_scale(pi.lo, -2));
        } else {
            r = px.sign ? pi : dw_make(f128_make_zero(0), f128_make_zero(0));
        }
    } else if (py.is_inf) {
        r = f128_pio2_dw();
    } else {
        lf_float128 zero = f128_make_zero(0);
        r = f128_atan2_pos(dw_make(lf_f128_abs(y), zero),
                           dw_make(lf_f128_abs(x), zero));
        if (px.sign) r = dw_add(f128_pi_dw(), dw_neg(r));
    }
    return py.sign ? f_neg(r.hi) : r.hi;
}

/* 1 - a^2 as a double-word, for |a| < 1 */
static f128_dw f128_one_minus_sq(lf_float128 a) {
    lf_float128 one = f128_one();
    lf_float128 ax = lf_f128_abs(a);
    if (f128_cmp_internal(ax, f128_half()) >= 0) {
        /* (1 - |a|) * (1 + |a|), where 1 - |a| is exact */
        lf_float128 d = f_sub(one, ax);
        return dw_mul_q(two_sum(one, ax), d);
    }
    f128_dw p = two_prod(ax, ax);
    f128_dw s = two_sum(one, f_neg(p.hi));
    return fast_two_sum(s.hi, f_sub(s.lo, p.lo));
}

/* asin(a) for |a| <= 1, as a double-word. */
static f128_dw f128_asin_dw(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    lf_float128 zero = f128_make_zero(0);
    f128_dw r;
    if (p.exp < -57) return dw_make(a, zero);   /* asin(x) = x */
    if (f128_cmp_internal(lf_f128_abs(a), f128_one()) == 0) {
        r = f128_pio2_dw();
    } else {
        f128_dw s = dw_sqrt(f128_one_minus_sq(a));
        r = f128_atan2_pos(dw_make(lf_f128_abs(a), zero), s);
    }
    return p.sign ? dw_neg(r) : r;
}

/* acos(a) for |a| <= 1, as a double-word. */
static f128_dw f128_acos_dw(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    lf_float128 zero = f128_make_zero(0);
    f128_dw r;
    if (f128_cmp_internal(lf_f128_abs(a), f128_one()) == 0) {
        r = dw_make(zero, zero);
    } else {
        f128_dw s = dw_sqrt(f128_one_minus_sq(a));
        r = f128_atan2_pos(s, dw_make(lf_f128_abs(a), zero));
    }
    return p.sign ? dw_add(f128_pi_dw(), dw_neg(r)) : r;
}

lf_float128 lf_f128_asin(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_zero) return a;
    if (f128_cmp_internal(lf_f128_abs(a), f128_one()) > 0) return f128_make_nan();
    return f128_asin_dw(a).hi;
}

lf_float128 lf_f128_acos(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (f128_cmp_internal(lf_f128_abs(a), f128_one()) > 0) return f128_make_nan();
    return f128_acos_dw(a).hi;
}

/* a mod 360 for finite a >= 2^62, computed exactly. */
static lf_float128 f128_mod360_large(f128_parts p) {
    int32_t E = p.exp - 112;   /* a = M * 2^E, E >= -50 */
    uint64_t md = E < 0 ? (uint64_t)360 << (-E) : 360;
    /* M mod md by binary long division */
    uint64_t rem = 0;
    for (int i = 112; i >= 0; i--) {
        rem = (rem << 1) | (uint64_t)u128_bit(p.mant, i);
        if (rem >= md) rem -= md;
    }
    if (E < 0) return f128_scale(__floatunditf_lf_impl(rem), E);
    /* (M mod 360) * (2^E mod 360) mod 360 */
    uint64_t pw = 1, b = 2;
    for (int32_t e = E; e > 0; e >>= 1) {
        if (e & 1) pw = (pw * b) % 360;
        b = (b * b) % 360;
    }
    return __floatunditf_lf_impl((rem * pw) % 360);
}

/* |a| = 90*n + r with r in [-45, 45] exactly, for finite a; returns
 * n mod 4. */
static int f128_rem_90(lf_float128 a, lf_float128 *r) {
    f128_parts p = f128_unpack(a);
    lf_float128 ax = lf_f128_abs(a);
    if (p.is_zero) { *r = ax; return 0; }
    if (p.exp >= 62) ax = f128_mod360_large(p);
    lf_float128 ninety = f_int(90);
    int64_t n = __fixtfdi_lf_impl(lf_f128_floor(f_add(f_div(ax, ninety),
                                                      f128_half())));
    *r = f_sub(ax, f_mul(f_int(n), ninety));   /* exact */
    return (int)(n & 3);
}

/* r * pi/180 as a double-word */
static f128_dw f128_deg_to_rad(lf_float128 r) {
    return dw_mul_q(dw_from_bits(F128_D2R), r);
}

lf_float128 lf_f128_sind(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return a;
    lf_float128 rd;
    int n = f128_rem_90(a, &rd);
    f128_dw r = f128_deg_to_rad(rd);
    lf_float128 s;
    switch (n) {
        case 0:  s = f128_sin_kernel(r); break;
        case 1:  s = f128_cos_kernel(r); break;
        case 2:  s = f_neg(f128_sin_kernel(r)); break;
        default: s = f_neg(f128_cos_kernel(r)); break;
    }
    return f_sign(s, p.sign);
}

lf_float128 lf_f128_cosd(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    lf_float128 rd;
    int n = f128_rem_90(a, &rd);
    /* cos(90 + 180k degrees) is exactly +0 (IEEE 754 cosPi(k + 1/2)). */
    if ((n & 1) && f128_is_zero(rd)) return f128_make_zero(0);
    f128_dw r = f128_deg_to_rad(rd);
    switch (n) {
        case 0:  return f128_cos_kernel(r);
        case 1:  return f_neg(f128_sin_kernel(r));
        case 2:  return f_neg(f128_cos_kernel(r));
        default: return f128_sin_kernel(r);
    }
}

lf_float128 lf_f128_tand(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf) return f128_make_nan();
    if (p.is_zero) return a;
    lf_float128 rd;
    int n = f128_rem_90(a, &rd);
    /* An odd multiple of 90 degrees is a pole: +inf at 90, -inf at 270. */
    if ((n & 1) && f128_is_zero(rd)) return f128_make_inf(p.sign ^ (n == 3));
    return f_sign(f128_tan_reduced(n, f128_deg_to_rad(rd)), p.sign);
}

static lf_float128 f128_rad_to_deg(f128_dw r) {
    return dw_mul(r, dw_from_bits(F128_R2D)).hi;
}

lf_float128 lf_f128_asind(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_zero) return a;
    if (f128_cmp_internal(lf_f128_abs(a), f128_one()) > 0) return f128_make_nan();
    return f128_rad_to_deg(f128_asin_dw(a));
}

lf_float128 lf_f128_acosd(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (f128_cmp_internal(lf_f128_abs(a), f128_one()) > 0) return f128_make_nan();
    return f128_rad_to_deg(f128_acos_dw(a));
}

lf_float128 lf_f128_atand(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_zero) return a;
    return f128_rad_to_deg(f128_atan_dw(a));
}

/* ========================================================================
 * M. Math: sinh, cosh, tanh, asinh, acosh, atanh
 * ======================================================================== */

lf_float128 lf_f128_sinh(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf || p.is_zero) return a;
    if (p.exp < -57) return a;              /* sinh(x) = x */
    lf_float128 ax = lf_f128_abs(a), one = f128_one(), r;
    double d = f_dbl(ax);
    if (d < 40.0) {
        /* t = expm1(|x|): sinh = (2t - t^2/(t+1))/2 for |x| < 1,
         * (t + t/(t+1))/2 otherwise */
        lf_float128 t = lf_f128_expm1(ax);
        lf_float128 tp1 = f_add(t, one);
        if (d < 1.0) {
            r = f_sub(f128_scale(t, 1), f_div(f_mul(t, t), tp1));
        } else {
            r = f_add(t, f_div(t, tp1));
        }
        r = f128_scale(r, -1);
    } else if (d > 11358.0) {
        r = f128_make_inf(0);
    } else {
        int32_t n;
        lf_float128 q = f128_exp_core(ax, f128_make_zero(0), &n);
        r = f128_scale(f_add(one, q), n - 1);
    }
    return f_sign(r, p.sign);
}

lf_float128 lf_f128_cosh(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan) return a;
    if (p.is_inf) return f128_make_inf(0);
    lf_float128 ax = lf_f128_abs(a), one = f128_one();
    if (p.is_zero || p.exp < -57) return one;
    double d = f_dbl(ax);
    if (d < 0.3465) {
        /* t = expm1(|x|): cosh = 1 + t^2/(2(1+t)) */
        lf_float128 t = lf_f128_expm1(ax);
        lf_float128 w = f128_scale(f_add(one, t), 1);
        return f_add(one, f_div(f_mul(t, t), w));
    }
    if (d < 40.0) {
        lf_float128 t = lf_f128_exp(ax);
        return f_add(f128_scale(t, -1), f_div(f128_half(), t));
    }
    if (d > 11358.0) return f128_make_inf(0);
    int32_t n;
    lf_float128 q = f128_exp_core(ax, f128_make_zero(0), &n);
    return f128_scale(f_add(one, q), n - 1);
}

lf_float128 lf_f128_tanh(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    lf_float128 one = f128_one(), two = f128_two(), r;
    if (p.is_nan || p.is_zero) return a;
    if (p.is_inf) return f_sign(one, p.sign);
    if (p.exp < -57) return a;              /* tanh(x) = x */
    lf_float128 ax = lf_f128_abs(a);
    double d = f_dbl(ax);
    if (d > 40.0) {
        r = one;
    } else if (d >= 1.0) {
        /* 1 - 2/(expm1(2|x|) + 2) */
        lf_float128 t = lf_f128_expm1(f128_scale(ax, 1));
        r = f_sub(one, f_div(two, f_add(t, two)));
    } else {
        /* -t/(t + 2), t = expm1(-2|x|) */
        lf_float128 t = lf_f128_expm1(f_neg(f128_scale(ax, 1)));
        r = f_neg(f_div(t, f_add(t, two)));
    }
    return f_sign(r, p.sign);
}

lf_float128 lf_f128_asinh(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    if (p.is_nan || p.is_inf || p.is_zero) return a;
    if (p.exp < -57) return a;              /* asinh(x) = x */
    lf_float128 ax = lf_f128_abs(a), one = f128_one(), r;
    if (p.exp > 57) {
        /* log(2|x|) */
        f128_dw l = dw_add(f128_log_dw(ax),
                           dw_from_bits(F128_LN2));
        r = l.hi;
    } else if (f_dbl(ax) > 2.0) {
        /* log(2|x| + 1/(sqrt(x^2 + 1) + |x|)) */
        lf_float128 s = lf_f128_sqrt(f_add(f_mul(ax, ax), one));
        r = lf_f128_log(f_add(f128_scale(ax, 1), f_div(one, f_add(s, ax))));
    } else {
        /* log1p(|x| + x^2/(1 + sqrt(1 + x^2))) */
        lf_float128 t = f_mul(ax, ax);
        lf_float128 s = lf_f128_sqrt(f_add(one, t));
        r = f128_log1p(f_add(ax, f_div(t, f_add(one, s))));
    }
    return f_sign(r, p.sign);
}

lf_float128 lf_f128_acosh(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    lf_float128 one = f128_one();
    if (p.is_nan) return a;
    int c = f128_cmp_internal(a, one);
    if (c < 0) return f128_make_nan();
    if (c == 0) return f128_make_zero(0);
    if (p.is_inf) return a;
    if (p.exp > 57) {
        /* log(2x) */
        return dw_add(f128_log_dw(a),
                      dw_from_bits(F128_LN2)).hi;
    }
    if (f_dbl(a) > 2.0) {
        /* log(2x - 1/(x + sqrt(x^2 - 1))) */
        lf_float128 s = lf_f128_sqrt(f_sub(f_mul(a, a), one));
        return lf_f128_log(f_sub(f128_scale(a, 1), f_div(one, f_add(a, s))));
    }
    /* log1p(t + sqrt(2t + t^2)), t = x - 1 (exact) */
    lf_float128 t = f_sub(a, one);
    lf_float128 s = lf_f128_sqrt(f_add(f128_scale(t, 1), f_mul(t, t)));
    return f128_log1p(f_add(t, s));
}

lf_float128 lf_f128_atanh(lf_float128 a) {
    f128_parts p = f128_unpack(a);
    lf_float128 one = f128_one(), r;
    if (p.is_nan || p.is_zero) return a;
    lf_float128 ax = lf_f128_abs(a);
    int c = f128_cmp_internal(ax, one);
    if (c > 0) return f128_make_nan();
    if (c == 0) return f128_make_inf(p.sign);
    if (p.exp < -57) return a;              /* atanh(x) = x */
    lf_float128 t = f128_scale(ax, 1);
    lf_float128 d = f_sub(one, ax);
    if (f128_cmp_internal(ax, f128_half()) < 0) {
        /* log1p(2|x| + 2|x|*|x|/(1 - |x|))/2 */
        r = f128_log1p(f_add(t, f_div(f_mul(t, ax), d)));
    } else {
        /* log1p(2|x|/(1 - |x|))/2, 1 - |x| is exact */
        r = f128_log1p(f_div(t, d));
    }
    return f_sign(f128_scale(r, -1), p.sign);
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
 * P. Non-ARM64 public entry points
 *
 * LLVM calls these with the target's native fp128 calling convention, which
 * is not the convention a 16-byte C struct gets. On x86-64 a binary128 value
 * travels in a single SSE register (System V classes SSE + SSEUP), whereas
 * `lf_float128` is classified INTEGER + INTEGER and travels in a pair of
 * general purpose registers; the same mismatch applies to return values
 * (xmm0 versus rax:rdx). Passing the struct straight through would therefore
 * make the implementations operate on whatever the argument registers
 * happened to hold.
 *
 * A 16-byte vector of doubles is classified exactly like __float128, so use
 * it to spell the ABI and convert to the struct the implementations expect.
 * On Windows x64 the same trick works with MSVC's __m128d: LLVM passes fp128
 * indirectly by pointer and returns it in XMM0, which is exactly how MSVC
 * treats __m128d parameters and return values.
 * The conversions are pure memcpy, so no fp128 arithmetic is generated here
 * and these wrappers cannot recurse into themselves.
 *
 * On other targets whose C compiler has a binary128 type, that type spells
 * the fp128 calling convention: `long double` where it is binary128 (e.g.
 * wasm32, RISC-V, MIPS64) and otherwise `__float128` (e.g. 32-bit x86, or
 * PowerPC with -mfloat128). Clang passes both as plain fp128 values, the
 * same way LLVM-emitted code passes them, so only the math entry points
 * (lf_sqrtq, lf_expq, ...) are defined there; the compiler-rt routines come
 * from the system's compiler-rt/libgcc (or section P2 on WASI). s390x is
 * left out because it passes `long double` indirectly. ARM64 uses the shims
 * of section Q. On targets without a binary128 C type, the math entry points
 * are not provided.
 * ======================================================================== */
#if !defined(__aarch64__) && ( \
    (defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))) \
    || (defined(_MSC_VER) && defined(_M_X64)) \
    || (LDBL_MANT_DIG == 113 && !defined(__s390x__)) \
    || defined(__SIZEOF_FLOAT128__) || defined(__FLOAT128__))

#if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
typedef double lf_f128_abi __attribute__((vector_size(16)));
#elif defined(_MSC_VER) && defined(_M_X64)
/* On Windows x64 LLVM passes fp128 arguments indirectly through a pointer to
 * a 16-byte aligned slot and returns fp128 in XMM0. MSVC gives __m128d
 * parameters and return values exactly that treatment, so it spells the same
 * ABI the vector typedef above does for the System V targets. */
#include <emmintrin.h>
typedef __m128d lf_f128_abi;
#elif LDBL_MANT_DIG == 113 && !defined(__s390x__)
typedef long double lf_f128_abi;
#else
typedef __float128 lf_f128_abi;
#endif

static lf_f128_abi lf_f128_to_abi(lf_float128 v) {
    lf_f128_abi r;
    memcpy(&r, v.bytes, 16);
    return r;
}

static lf_float128 lf_f128_from_abi(lf_f128_abi v) {
    lf_float128 r;
    memcpy(r.bytes, &v, 16);
    return r;
}

#if (defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))) \
    || (defined(_MSC_VER) && defined(_M_X64))
lf_f128_abi __addtf3(lf_f128_abi a, lf_f128_abi b)  { return lf_f128_to_abi(__addtf3_lf_impl(lf_f128_from_abi(a), lf_f128_from_abi(b))); }
lf_f128_abi __subtf3(lf_f128_abi a, lf_f128_abi b)  { return lf_f128_to_abi(__subtf3_lf_impl(lf_f128_from_abi(a), lf_f128_from_abi(b))); }
lf_f128_abi __multf3(lf_f128_abi a, lf_f128_abi b)  { return lf_f128_to_abi(__multf3_lf_impl(lf_f128_from_abi(a), lf_f128_from_abi(b))); }
lf_f128_abi __divtf3(lf_f128_abi a, lf_f128_abi b)  { return lf_f128_to_abi(__divtf3_lf_impl(lf_f128_from_abi(a), lf_f128_from_abi(b))); }
lf_f128_abi __negtf2(lf_f128_abi a)                 { return lf_f128_to_abi(__negtf2_lf_impl(lf_f128_from_abi(a))); }
lf_f128_abi __extenddftf2(double a)                 { return lf_f128_to_abi(__extenddftf2_lf_impl(a)); }
lf_f128_abi __extendsftf2(float a)                  { return lf_f128_to_abi(__extendsftf2_lf_impl(a)); }
double      __trunctfdf2(lf_f128_abi a)             { return __trunctfdf2_lf_impl(lf_f128_from_abi(a)); }
float       __trunctfsf2(lf_f128_abi a)             { return __trunctfsf2_lf_impl(lf_f128_from_abi(a)); }
lf_f128_abi __floatsitf(int32_t a)                  { return lf_f128_to_abi(__floatsitf_lf_impl(a)); }
lf_f128_abi __floatditf(int64_t a)                  { return lf_f128_to_abi(__floatditf_lf_impl(a)); }
lf_f128_abi __floatunditf(uint64_t a)               { return lf_f128_to_abi(__floatunditf_lf_impl(a)); }
int32_t     __fixtfsi(lf_f128_abi a)                { return __fixtfsi_lf_impl(lf_f128_from_abi(a)); }
int64_t     __fixtfdi(lf_f128_abi a)                { return __fixtfdi_lf_impl(lf_f128_from_abi(a)); }

int __eqtf2   (lf_f128_abi a, lf_f128_abi b) { return __eqtf2_lf_impl   (lf_f128_from_abi(a), lf_f128_from_abi(b)); }
int __netf2   (lf_f128_abi a, lf_f128_abi b) { return __netf2_lf_impl   (lf_f128_from_abi(a), lf_f128_from_abi(b)); }
int __lttf2   (lf_f128_abi a, lf_f128_abi b) { return __lttf2_lf_impl   (lf_f128_from_abi(a), lf_f128_from_abi(b)); }
int __letf2   (lf_f128_abi a, lf_f128_abi b) { return __letf2_lf_impl   (lf_f128_from_abi(a), lf_f128_from_abi(b)); }
int __gttf2   (lf_f128_abi a, lf_f128_abi b) { return __gttf2_lf_impl   (lf_f128_from_abi(a), lf_f128_from_abi(b)); }
int __getf2   (lf_f128_abi a, lf_f128_abi b) { return __getf2_lf_impl   (lf_f128_from_abi(a), lf_f128_from_abi(b)); }
int __unordtf2(lf_f128_abi a, lf_f128_abi b) { return __unordtf2_lf_impl(lf_f128_from_abi(a), lf_f128_from_abi(b)); }
#endif

/* SIMD-ABI public entry points for the math functions called by
 * LLVM-emitted code such as `call fp128 @lf_sqrtq(fp128)`,
 * `lf_powq(fp128, fp128)` or `lf_sinq(fp128)`. */
lf_f128_abi lf_sqrtq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_sqrt(lf_f128_from_abi(a))); }
lf_f128_abi lf_powq(lf_f128_abi a, lf_f128_abi b)   { return lf_f128_to_abi(lf_f128_pow(lf_f128_from_abi(a), lf_f128_from_abi(b))); }
lf_f128_abi lf_atan2q(lf_f128_abi y, lf_f128_abi x) { return lf_f128_to_abi(lf_f128_atan2(lf_f128_from_abi(y), lf_f128_from_abi(x))); }
lf_f128_abi lf_expq(lf_f128_abi a)                  { return lf_f128_to_abi(lf_f128_exp(lf_f128_from_abi(a))); }
lf_f128_abi lf_expm1q(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_expm1(lf_f128_from_abi(a))); }
lf_f128_abi lf_exp2q(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_exp2(lf_f128_from_abi(a))); }
lf_f128_abi lf_logq(lf_f128_abi a)                  { return lf_f128_to_abi(lf_f128_log(lf_f128_from_abi(a))); }
lf_f128_abi lf_log10q(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_log10(lf_f128_from_abi(a))); }
lf_f128_abi lf_sinq(lf_f128_abi a)                  { return lf_f128_to_abi(lf_f128_sin(lf_f128_from_abi(a))); }
lf_f128_abi lf_cosq(lf_f128_abi a)                  { return lf_f128_to_abi(lf_f128_cos(lf_f128_from_abi(a))); }
lf_f128_abi lf_tanq(lf_f128_abi a)                  { return lf_f128_to_abi(lf_f128_tan(lf_f128_from_abi(a))); }
lf_f128_abi lf_asinq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_asin(lf_f128_from_abi(a))); }
lf_f128_abi lf_acosq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_acos(lf_f128_from_abi(a))); }
lf_f128_abi lf_atanq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_atan(lf_f128_from_abi(a))); }
lf_f128_abi lf_sinhq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_sinh(lf_f128_from_abi(a))); }
lf_f128_abi lf_coshq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_cosh(lf_f128_from_abi(a))); }
lf_f128_abi lf_tanhq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_tanh(lf_f128_from_abi(a))); }
lf_f128_abi lf_asinhq(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_asinh(lf_f128_from_abi(a))); }
lf_f128_abi lf_acoshq(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_acosh(lf_f128_from_abi(a))); }
lf_f128_abi lf_atanhq(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_atanh(lf_f128_from_abi(a))); }
lf_f128_abi lf_sindq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_sind(lf_f128_from_abi(a))); }
lf_f128_abi lf_cosdq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_cosd(lf_f128_from_abi(a))); }
lf_f128_abi lf_tandq(lf_f128_abi a)                 { return lf_f128_to_abi(lf_f128_tand(lf_f128_from_abi(a))); }
lf_f128_abi lf_asindq(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_asind(lf_f128_from_abi(a))); }
lf_f128_abi lf_acosdq(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_acosd(lf_f128_from_abi(a))); }
lf_f128_abi lf_atandq(lf_f128_abi a)                { return lf_f128_to_abi(lf_f128_atand(lf_f128_from_abi(a))); }
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

/* Two fp128 operands -> int: args q0,q1 -> x0/x1,x2/x3; result already in w0 */
#define _LF_SHIM2_TO_INT(pub, impl)                          \
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
/* SIMD-ABI public entry points for the math functions called by LLVM-emitted
 * code such as `call fp128 @lf_sqrtq(fp128)`, `lf_powq(fp128, fp128)` or
 * `lf_sinq(fp128)`. They bridge to the struct-ABI implementations lf_f128_*. */
_LF_SHIM1(lf_sqrtq,          lf_f128_sqrt)
_LF_SHIM2(lf_powq,           lf_f128_pow)
_LF_SHIM2(lf_atan2q,         lf_f128_atan2)
_LF_SHIM1(lf_expq,          lf_f128_exp)
_LF_SHIM1(lf_expm1q,        lf_f128_expm1)
_LF_SHIM1(lf_exp2q,         lf_f128_exp2)
_LF_SHIM1(lf_logq,          lf_f128_log)
_LF_SHIM1(lf_log10q,        lf_f128_log10)
_LF_SHIM1(lf_sinq,          lf_f128_sin)
_LF_SHIM1(lf_cosq,          lf_f128_cos)
_LF_SHIM1(lf_tanq,          lf_f128_tan)
_LF_SHIM1(lf_asinq,         lf_f128_asin)
_LF_SHIM1(lf_acosq,         lf_f128_acos)
_LF_SHIM1(lf_atanq,         lf_f128_atan)
_LF_SHIM1(lf_sinhq,         lf_f128_sinh)
_LF_SHIM1(lf_coshq,         lf_f128_cosh)
_LF_SHIM1(lf_tanhq,         lf_f128_tanh)
_LF_SHIM1(lf_asinhq,        lf_f128_asinh)
_LF_SHIM1(lf_acoshq,        lf_f128_acosh)
_LF_SHIM1(lf_atanhq,        lf_f128_atanh)
_LF_SHIM1(lf_sindq,         lf_f128_sind)
_LF_SHIM1(lf_cosdq,         lf_f128_cosd)
_LF_SHIM1(lf_tandq,         lf_f128_tand)
_LF_SHIM1(lf_asindq,        lf_f128_asind)
_LF_SHIM1(lf_acosdq,        lf_f128_acosd)
_LF_SHIM1(lf_atandq,        lf_f128_atand)
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
_LF_SHIM2_TO_INT(__eqtf2,             __eqtf2_lf_impl)
_LF_SHIM2_TO_INT(__netf2,             __netf2_lf_impl)
_LF_SHIM2_TO_INT(__lttf2,             __lttf2_lf_impl)
_LF_SHIM2_TO_INT(__letf2,             __letf2_lf_impl)
_LF_SHIM2_TO_INT(__gttf2,             __gttf2_lf_impl)
_LF_SHIM2_TO_INT(__getf2,             __getf2_lf_impl)
_LF_SHIM2_TO_INT(__unordtf2,          __unordtf2_lf_impl)

#undef _LF_SHIM2
#undef _LF_SHIM1
#undef _LF_SHIM_FROM_SCALAR
#undef _LF_SHIM_TO_INT
#undef _LF_SHIM2_TO_INT
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

/* The public compiler-rt comparison entry points are defined once per
 * platform above; this branch only supplies the lf_f128_* helpers. */

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
