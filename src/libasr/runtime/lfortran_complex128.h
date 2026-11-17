/*
 * lfortran_complex128.h
 *
 * complex(16) arithmetic and elementary functions on pairs of binary128
 * numbers, written against the lf_float128 API of
 * lfortran_float128_quadmath.h.
 *
 * The runtime (lfortran_intrinsics.c) computes complex(16) operations with
 * these functions and the compiler folds complex(16) constant expressions
 * with them, so that a folded constant equals the value computed at run time.
 */

#ifndef LFORTRAN_COMPLEX128_H
#define LFORTRAN_COMPLEX128_H

#include <stdint.h>
#include "lfortran_float128_quadmath.h"

typedef struct {
    lf_float128 re, im;
} lf_complex128;

static inline lf_complex128 lf_c128_make(lf_float128 re, lf_float128 im) {
    lf_complex128 r;
    r.re = re;
    r.im = im;
    return r;
}

static inline lf_complex128 lf_c128_from_double(double re, double im) {
    return lf_c128_make(lf_f128_from_double(re), lf_f128_from_double(im));
}

static inline lf_complex128 lf_c128_add(lf_complex128 a, lf_complex128 b) {
    return lf_c128_make(lf_f128_add(a.re, b.re), lf_f128_add(a.im, b.im));
}

static inline lf_complex128 lf_c128_sub(lf_complex128 a, lf_complex128 b) {
    return lf_c128_make(lf_f128_sub(a.re, b.re), lf_f128_sub(a.im, b.im));
}

static inline lf_complex128 lf_c128_mul(lf_complex128 a, lf_complex128 b) {
    return lf_c128_make(
        lf_f128_sub(lf_f128_mul(a.re, b.re), lf_f128_mul(a.im, b.im)),
        lf_f128_add(lf_f128_mul(a.re, b.im), lf_f128_mul(a.im, b.re)));
}

static inline lf_complex128 lf_c128_div(lf_complex128 a, lf_complex128 b) {
    // Smith's algorithm, as for the lower precisions.
    lf_float128 zero = lf_f128_from_double(0.0);
    if (lf_f128_cmp(lf_f128_abs(b.re), lf_f128_abs(b.im)) >= 0) {
        if (lf_f128_eq(b.re, zero) && lf_f128_eq(b.im, zero)) {
            return lf_c128_make(lf_f128_div(a.re, zero),
                lf_f128_div(a.im, zero));
        }
        lf_float128 ratio = lf_f128_div(b.im, b.re);
        lf_float128 denom = lf_f128_add(b.re, lf_f128_mul(b.im, ratio));
        return lf_c128_make(
            lf_f128_div(lf_f128_add(a.re, lf_f128_mul(a.im, ratio)), denom),
            lf_f128_div(lf_f128_sub(a.im, lf_f128_mul(a.re, ratio)), denom));
    }
    lf_float128 ratio = lf_f128_div(b.re, b.im);
    lf_float128 denom = lf_f128_add(lf_f128_mul(b.re, ratio), b.im);
    return lf_c128_make(
        lf_f128_div(lf_f128_add(lf_f128_mul(a.re, ratio), a.im), denom),
        lf_f128_div(lf_f128_sub(lf_f128_mul(a.im, ratio), a.re), denom));
}

static inline lf_float128 lf_c128_abs(lf_complex128 z) {
    lf_float128 zero = lf_f128_from_double(0.0);
    lf_float128 x = lf_f128_abs(z.re), y = lf_f128_abs(z.im);
    if (lf_f128_isinf(x) || lf_f128_isinf(y)) {
        return lf_f128_div(lf_f128_from_double(1.0), zero);
    }
    if (lf_f128_isnan(x) || lf_f128_isnan(y)) {
        return lf_f128_div(zero, zero);
    }
    if (lf_f128_cmp(x, y) < 0) {
        lf_float128 t = x;
        x = y;
        y = t;
    }
    if (lf_f128_eq(x, zero)) {
        return zero;
    }
    // Scale by the larger part so that the squares do not overflow.
    lf_float128 r = lf_f128_div(y, x);
    return lf_f128_mul(x, lf_f128_sqrt(
        lf_f128_add(lf_f128_from_double(1.0), lf_f128_mul(r, r))));
}

static inline lf_complex128 lf_c128_mul_i(lf_complex128 z) {
    return lf_c128_make(lf_f128_neg(z.im), z.re);
}

static inline lf_complex128 lf_c128_sqrt(lf_complex128 z) {
    lf_float128 zero = lf_f128_from_double(0.0);
    lf_float128 half = lf_f128_from_double(0.5);
    if (lf_f128_eq(z.re, zero) && lf_f128_eq(z.im, zero)) {
        return lf_c128_make(zero, z.im);
    }
    lf_float128 t = lf_f128_sqrt(lf_f128_mul(half,
        lf_f128_add(lf_c128_abs(z), lf_f128_abs(z.re))));
    lf_float128 two_t = lf_f128_add(t, t);
    if (lf_f128_signbit(z.re) == 0) {
        return lf_c128_make(t, lf_f128_div(z.im, two_t));
    }
    lf_float128 re = lf_f128_div(lf_f128_abs(z.im), two_t);
    return lf_c128_make(re, lf_f128_signbit(z.im) ? lf_f128_neg(t) : t);
}

static inline lf_complex128 lf_c128_exp(lf_complex128 z) {
    lf_float128 e = lf_f128_exp(z.re);
    return lf_c128_make(lf_f128_mul(e, lf_f128_cos(z.im)),
        lf_f128_mul(e, lf_f128_sin(z.im)));
}

static inline lf_complex128 lf_c128_log(lf_complex128 z) {
    return lf_c128_make(lf_f128_log(lf_c128_abs(z)),
        lf_f128_atan2(z.im, z.re));
}

static inline lf_complex128 lf_c128_sin(lf_complex128 z) {
    return lf_c128_make(lf_f128_mul(lf_f128_sin(z.re), lf_f128_cosh(z.im)),
        lf_f128_mul(lf_f128_cos(z.re), lf_f128_sinh(z.im)));
}

static inline lf_complex128 lf_c128_cos(lf_complex128 z) {
    return lf_c128_make(lf_f128_mul(lf_f128_cos(z.re), lf_f128_cosh(z.im)),
        lf_f128_neg(lf_f128_mul(lf_f128_sin(z.re), lf_f128_sinh(z.im))));
}

static inline lf_complex128 lf_c128_sinh(lf_complex128 z) {
    return lf_c128_make(lf_f128_mul(lf_f128_sinh(z.re), lf_f128_cos(z.im)),
        lf_f128_mul(lf_f128_cosh(z.re), lf_f128_sin(z.im)));
}

static inline lf_complex128 lf_c128_cosh(lf_complex128 z) {
    return lf_c128_make(lf_f128_mul(lf_f128_cosh(z.re), lf_f128_cos(z.im)),
        lf_f128_mul(lf_f128_sinh(z.re), lf_f128_sin(z.im)));
}

static inline lf_complex128 lf_c128_tanh(lf_complex128 z) {
    // The C99 ctanh algorithm (as in glibc and libquadmath). With
    // den = sinh(x)^2 + cos(y)^2,
    //   tanh(x + i y) = (sinh(x) cosh(x) + i sin(y) cos(y)) / den,
    // which does not cancel. Where sinh(x)^2 would overflow,
    //   tanh(x + i y) = sign(x) + 4 i sin(y) cos(y) exp(-2|x|)
    // to binary128 precision.
    lf_float128 zero = lf_f128_from_double(0.0);
    lf_float128 one = lf_f128_from_double(1.0);
    lf_float128 x = z.re, y = z.im;
    if (lf_f128_isinf(x)) {
        lf_float128 re = lf_f128_signbit(x) ? lf_f128_neg(one) : one;
        lf_float128 im = y;
        if (!lf_f128_isinf(y) && !lf_f128_isnan(y)
                && lf_f128_cmp(lf_f128_abs(y), one) > 0) {
            im = lf_f128_mul(lf_f128_sin(y), lf_f128_cos(y));
        }
        return lf_c128_make(re,
            lf_f128_signbit(im) ? lf_f128_neg(zero) : zero);
    }
    if (lf_f128_isnan(x) || lf_f128_isinf(y) || lf_f128_isnan(y)) {
        if (lf_f128_eq(y, zero)) {
            return z;
        }
        lf_float128 nan = lf_f128_div(zero, zero);
        return lf_c128_make(lf_f128_eq(x, zero) ? x : nan, nan);
    }
    lf_float128 sin_y = lf_f128_sin(y), cos_y = lf_f128_cos(y);
    // floor((MAX_EXP - 1) log(2) / 2) for binary128
    lf_float128 t = lf_f128_from_double(5677.0);
    lf_float128 ax = lf_f128_abs(x);
    if (lf_f128_cmp(ax, t) > 0) {
        lf_float128 exp_2t = lf_f128_exp(lf_f128_add(t, t));
        lf_float128 im = lf_f128_div(lf_f128_mul(lf_f128_from_double(4.0),
            lf_f128_mul(sin_y, cos_y)), exp_2t);
        ax = lf_f128_sub(ax, t);
        im = lf_f128_div(im, lf_f128_cmp(ax, t) > 0 ? exp_2t
            : lf_f128_exp(lf_f128_add(ax, ax)));
        return lf_c128_make(lf_f128_signbit(x) ? lf_f128_neg(one) : one, im);
    }
    lf_float128 sinh_x = lf_f128_sinh(x);
    lf_float128 den = lf_f128_add(lf_f128_mul(sinh_x, sinh_x),
        lf_f128_mul(cos_y, cos_y));
    return lf_c128_make(
        lf_f128_div(lf_f128_mul(sinh_x, lf_f128_cosh(x)), den),
        lf_f128_div(lf_f128_mul(sin_y, cos_y), den));
}

static inline lf_complex128 lf_c128_tan(lf_complex128 z) {
    // tan(z) = -i tanh(i z)
    lf_complex128 r = lf_c128_tanh(lf_c128_mul_i(z));
    return lf_c128_make(r.im, lf_f128_neg(r.re));
}

static inline lf_float128 lf_c128_copysign(lf_float128 mag, lf_float128 sgn) {
    lf_float128 a = lf_f128_abs(mag);
    return lf_f128_signbit(sgn) ? lf_f128_neg(a) : a;
}

static inline lf_float128 lf_c128_log1p(lf_float128 u) {
    // log(1 + u), accurate for small u: the rounding error of w = 1 + u
    // is compensated by the factor u / (w - 1).
    lf_float128 one = lf_f128_from_double(1.0);
    lf_float128 w = lf_f128_add(one, u);
    if (lf_f128_eq(w, one)) {
        return u;
    }
    if (lf_f128_isinf(w)) {
        return w;
    }
    return lf_f128_mul(lf_f128_log(w), lf_f128_div(u, lf_f128_sub(w, one)));
}

// The inverse functions place their branch cuts as C99 Annex G does: the
// sign of a zero real or imaginary part selects the side of a cut and is kept
// in the result. On the real and imaginary axes they are computed exactly
// from the real functions. Elsewhere they use the formulas of Kahan, "Branch
// Cuts for Complex Elementary Functions" (1987), in terms of s = sqrt(1 - z)
// and t = sqrt(1 + z), which do not cancel. For a NaN and a finite part they
// return the Annex G values. atanh and atan also handle infinite and huge
// parts as Annex G does; asin, acos, asinh and acosh do not yet (their
// square roots overflow for parts near the overflow threshold).

static inline lf_complex128 lf_c128_asin(lf_complex128 z) {
    lf_float128 zero = lf_f128_from_double(0.0);
    lf_float128 one = lf_f128_from_double(1.0);
    if (lf_f128_eq(z.im, zero) && !lf_f128_isnan(z.re)) {
        lf_float128 ax = lf_f128_abs(z.re);
        if (lf_f128_cmp(ax, one) <= 0) {
            return lf_c128_make(lf_f128_asin(z.re), z.im);
        }
        // On the cut: asin(+-1) + i (+-acosh|x|), signed by Im(z)
        return lf_c128_make(lf_f128_asin(lf_c128_copysign(one, z.re)),
            lf_c128_copysign(lf_f128_acosh(ax), z.im));
    }
    if (lf_f128_eq(z.re, zero)) {
        return lf_c128_make(z.re, lf_f128_asinh(z.im));
    }
    // asin(z) = atan2(x, Re(s t)) + i asinh(Im(conj(s) t))
    lf_complex128 s = lf_c128_sqrt(lf_c128_make(lf_f128_sub(one, z.re),
        lf_f128_neg(z.im)));
    lf_complex128 t = lf_c128_sqrt(lf_c128_make(lf_f128_add(one, z.re),
        z.im));
    return lf_c128_make(
        lf_f128_atan2(z.re, lf_f128_sub(lf_f128_mul(s.re, t.re),
            lf_f128_mul(s.im, t.im))),
        lf_f128_asinh(lf_f128_sub(lf_f128_mul(s.re, t.im),
            lf_f128_mul(s.im, t.re))));
}

static inline lf_complex128 lf_c128_acos(lf_complex128 z) {
    lf_float128 zero = lf_f128_from_double(0.0);
    lf_float128 one = lf_f128_from_double(1.0);
    if (lf_f128_eq(z.im, zero) && !lf_f128_isnan(z.re)) {
        lf_float128 ax = lf_f128_abs(z.re);
        if (lf_f128_cmp(ax, one) <= 0) {
            return lf_c128_make(lf_f128_acos(z.re), lf_f128_neg(z.im));
        }
        // On the cut: acos(+-1) - i (+-acosh|x|), signed by Im(z)
        return lf_c128_make(lf_f128_acos(lf_c128_copysign(one, z.re)),
            lf_f128_neg(lf_c128_copysign(lf_f128_acosh(ax), z.im)));
    }
    if (lf_f128_eq(z.re, zero)) {
        return lf_c128_make(lf_f128_acos(zero),
            lf_f128_neg(lf_f128_asinh(z.im)));
    }
    // acos(z) = 2 atan2(Re(s), Re(t)) + i asinh(Im(conj(t) s))
    lf_complex128 s = lf_c128_sqrt(lf_c128_make(lf_f128_sub(one, z.re),
        lf_f128_neg(z.im)));
    lf_complex128 t = lf_c128_sqrt(lf_c128_make(lf_f128_add(one, z.re),
        z.im));
    lf_float128 a = lf_f128_atan2(s.re, t.re);
    return lf_c128_make(lf_f128_add(a, a),
        lf_f128_asinh(lf_f128_sub(lf_f128_mul(t.re, s.im),
            lf_f128_mul(t.im, s.re))));
}

static inline lf_complex128 lf_c128_asinh(lf_complex128 z) {
    // asinh(z) = i asin(-i z)
    lf_complex128 r = lf_c128_asin(lf_c128_make(z.im, lf_f128_neg(z.re)));
    return lf_c128_make(lf_f128_neg(r.im), r.re);
}

static inline lf_complex128 lf_c128_acosh(lf_complex128 z) {
    lf_float128 zero = lf_f128_from_double(0.0);
    lf_float128 one = lf_f128_from_double(1.0);
    if (lf_f128_eq(z.im, zero) && !lf_f128_isnan(z.re)) {
        lf_float128 ax = lf_f128_abs(z.re);
        if (lf_f128_cmp(ax, one) <= 0) {
            return lf_c128_make(zero,
                lf_c128_copysign(lf_f128_acos(z.re), z.im));
        }
        // acosh|x| + i acos(+-1), signed by Im(z)
        return lf_c128_make(lf_f128_acosh(ax), lf_c128_copysign(
            lf_f128_acos(lf_c128_copysign(one, z.re)), z.im));
    }
    if (lf_f128_eq(z.re, zero)) {
        return lf_c128_make(lf_f128_asinh(lf_f128_abs(z.im)),
            lf_c128_copysign(lf_f128_acos(zero), z.im));
    }
    // acosh(z) = asinh(Re(conj(u) t)) + 2 i atan2(Im(u), Re(t)),
    // u = sqrt(z - 1)
    lf_complex128 u = lf_c128_sqrt(lf_c128_make(lf_f128_sub(z.re, one),
        z.im));
    lf_complex128 t = lf_c128_sqrt(lf_c128_make(lf_f128_add(z.re, one),
        z.im));
    lf_float128 a = lf_f128_atan2(u.im, t.re);
    return lf_c128_make(
        lf_f128_asinh(lf_f128_add(lf_f128_mul(u.re, t.re),
            lf_f128_mul(u.im, t.im))),
        lf_f128_add(a, a));
}

static inline lf_complex128 lf_c128_atanh(lf_complex128 z) {
    lf_float128 zero = lf_f128_from_double(0.0);
    lf_float128 one = lf_f128_from_double(1.0);
    lf_float128 half_pi = lf_f128_acos(zero);
    lf_float128 x = lf_f128_abs(z.re), y = lf_f128_abs(z.im);
    if (lf_f128_isnan(x) || lf_f128_isnan(y)) {
        // Annex G: atanh(+-Inf + i NaN) = +-0 + i NaN,
        // atanh(NaN +- i Inf) = +-0 +- i pi/2, atanh(+-0 + i NaN) = +-0 + i NaN
        if (lf_f128_isinf(y)) {
            return lf_c128_make(lf_c128_copysign(zero, z.re),
                lf_c128_copysign(half_pi, z.im));
        }
        if (lf_f128_isinf(x) || lf_f128_eq(x, zero)) {
            return lf_c128_make(lf_c128_copysign(zero, z.re),
                lf_f128_add(y, y));
        }
        return lf_c128_make(lf_f128_add(x, y), lf_f128_add(x, y));
    }
    if (lf_f128_eq(y, zero) && lf_f128_cmp(x, one) < 0) {
        return lf_c128_make(lf_f128_atanh(z.re), z.im);
    }
    if (lf_f128_eq(x, zero)) {
        return lf_c128_make(z.re, lf_f128_atan(z.im));
    }
    // atanh is odd and atanh(conj(z)) = conj(atanh(z)): evaluate it at
    // (|x|, |y|) and restore the signs.
    lf_float128 quarter = lf_f128_from_double(0.25);
    lf_float128 half = lf_f128_from_double(0.5);
    // big = 1e2400: below it the squares do not overflow, and above it
    // atanh(z) = Re(1/z) + i pi/2 to working precision.
    lf_float128 big = lf_f128_from_double(1.0e300);
    big = lf_f128_mul(big, big);
    big = lf_f128_mul(big, big);
    big = lf_f128_mul(big, big);
    lf_float128 re, im;
    if (lf_f128_cmp(x, big) > 0 || lf_f128_cmp(y, big) > 0) {
        if (lf_f128_isinf(x) || lf_f128_isinf(y)) {
            re = zero;
        } else if (lf_f128_cmp(x, y) >= 0) {
            // Re(1/z) = (1/x) / (1 + (y/x)^2)
            lf_float128 r = lf_f128_div(y, x);
            re = lf_f128_div(lf_f128_div(one, x),
                lf_f128_add(one, lf_f128_mul(r, r)));
        } else {
            // Re(1/z) = (x/y) / y / (1 + (x/y)^2)
            lf_float128 r = lf_f128_div(x, y);
            re = lf_f128_div(lf_f128_div(r, y),
                lf_f128_add(one, lf_f128_mul(r, r)));
        }
        im = half_pi;
    } else if (lf_f128_eq(x, one) && !lf_f128_eq(y, zero)
            && lf_f128_cmp(y, one) < 0) {
        // (log(4 + y^2) - 2 log y) / 4 + i atan2(2 y, -y^2) / 2, where the
        // general formula would form 4 / y^2, which overflows for tiny y
        re = lf_f128_mul(quarter, lf_f128_sub(
            lf_f128_log(lf_f128_add(lf_f128_from_double(4.0),
                lf_f128_mul(y, y))),
            lf_f128_mul(lf_f128_from_double(2.0), lf_f128_log(y))));
        im = lf_f128_mul(half, lf_f128_atan2(lf_f128_add(y, y),
            lf_f128_neg(lf_f128_mul(y, y))));
    } else {
        // log1p(4 x / ((1 - x)^2 + y^2)) / 4
        // + i atan2(2 y, (1 - x)(1 + x) - y^2) / 2
        lf_float128 omx = lf_f128_sub(one, x);
        lf_float128 x4 = lf_f128_mul(lf_f128_from_double(4.0), x);
        re = lf_f128_mul(quarter, lf_c128_log1p(lf_f128_div(x4,
            lf_f128_add(lf_f128_mul(omx, omx), lf_f128_mul(y, y)))));
        im = lf_f128_mul(half, lf_f128_atan2(lf_f128_add(y, y),
            lf_f128_sub(lf_f128_mul(omx, lf_f128_add(one, x)),
                lf_f128_mul(y, y))));
    }
    return lf_c128_make(lf_c128_copysign(re, z.re),
        lf_c128_copysign(im, z.im));
}

static inline lf_complex128 lf_c128_atan(lf_complex128 z) {
    // atan(z) = -i atanh(i z)
    lf_complex128 r = lf_c128_atanh(lf_c128_mul_i(z));
    return lf_c128_make(r.im, lf_f128_neg(r.re));
}

static inline lf_complex128 lf_c128_pow(lf_complex128 a, lf_complex128 b) {
    lf_float128 zero = lf_f128_from_double(0.0);
    if (lf_f128_eq(a.re, zero) && lf_f128_eq(a.im, zero)) {
        if (lf_f128_eq(b.re, zero) && lf_f128_eq(b.im, zero)) {
            return lf_c128_from_double(1.0, 0.0);
        }
        return lf_c128_make(zero, zero);
    }
    return lf_c128_exp(lf_c128_mul(b, lf_c128_log(a)));
}

// a**n by repeated multiplication (binary exponentiation); a negative `n`
// gives 1/a**(-n). This is the algorithm of the lower precisions, with which
// the compiler also unrolls a constant exponent.
static inline lf_complex128 lf_c128_pow_int(lf_complex128 a, int64_t n) {
    if (n == 0) {
        return lf_c128_from_double(1.0, 0.0);
    }
    uint64_t u = n < 0 ? (uint64_t)0 - (uint64_t)n : (uint64_t)n;
    lf_complex128 base = a;
    while ((u & 1) == 0) {
        base = lf_c128_mul(base, base);
        u >>= 1;
    }
    lf_complex128 result = base;
    u >>= 1;
    while (u != 0) {
        base = lf_c128_mul(base, base);
        if (u & 1) {
            result = lf_c128_mul(result, base);
        }
        u >>= 1;
    }
    if (n < 0) {
        result = lf_c128_div(lf_c128_from_double(1.0, 0.0), result);
    }
    return result;
}

#endif /* LFORTRAN_COMPLEX128_H */
