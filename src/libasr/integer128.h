#ifndef LFORTRAN_INTEGER128_H
#define LFORTRAN_INTEGER128_H

// A signed 128-bit integer stored as a little-endian pair of words.
// Arithmetic wraps in two's complement, which is what the LLVM backend
// does for integer(16), and values that fit in int64 stay plain int64
// constants unless that bit pattern would be mistaken for a BigInt pointer.
// The arithmetic is portable C++. It does not use __int128, so compile-time
// folding is the same on MSVC as on GCC and Clang.

#include <cmath>
#include <cstdint>
#include <string>

#include <libasr/bigint.h>

namespace LCompilers {

struct Int128 {
    uint64_t lo;
    uint64_t hi;

    static Int128 from_i64(int64_t v) {
        Int128 r;
        r.lo = (uint64_t)v;
        r.hi = v < 0 ? ~(uint64_t)0 : 0;
        return r;
    }
};

inline bool int128_is_negative(Int128 v) {
    return (v.hi >> 63) != 0;
}

inline bool int128_is_zero(Int128 v) {
    return v.lo == 0 && v.hi == 0;
}

inline int int128_ucmp(Int128 a, Int128 b) {
    if (a.hi < b.hi) return -1;
    if (a.hi > b.hi) return 1;
    if (a.lo < b.lo) return -1;
    if (a.lo > b.lo) return 1;
    return 0;
}

inline int int128_cmp(Int128 a, Int128 b) {
    if ((int64_t)a.hi < (int64_t)b.hi) return -1;
    if ((int64_t)a.hi > (int64_t)b.hi) return 1;
    if (a.lo < b.lo) return -1;
    if (a.lo > b.lo) return 1;
    return 0;
}

inline Int128 int128_add(Int128 a, Int128 b) {
    Int128 r;
    r.lo = a.lo + b.lo;
    r.hi = a.hi + b.hi + (r.lo < a.lo ? 1u : 0u);
    return r;
}

inline Int128 int128_sub(Int128 a, Int128 b) {
    Int128 r;
    r.lo = a.lo - b.lo;
    r.hi = a.hi - b.hi - (a.lo < b.lo ? 1u : 0u);
    return r;
}

inline Int128 int128_neg(Int128 v) {
    return int128_sub(Int128::from_i64(0), v);
}

inline Int128 int128_and(Int128 a, Int128 b) {
    Int128 r;
    r.lo = a.lo & b.lo;
    r.hi = a.hi & b.hi;
    return r;
}

inline Int128 int128_or(Int128 a, Int128 b) {
    Int128 r;
    r.lo = a.lo | b.lo;
    r.hi = a.hi | b.hi;
    return r;
}

inline Int128 int128_xor(Int128 a, Int128 b) {
    Int128 r;
    r.lo = a.lo ^ b.lo;
    r.hi = a.hi ^ b.hi;
    return r;
}

inline Int128 int128_lshl(Int128 a, unsigned n) {
    if (n == 0) return a;
    if (n >= 128) return Int128::from_i64(0);
    Int128 r;
    if (n >= 64) {
        r.lo = 0;
        r.hi = a.lo << (n - 64);
    } else {
        r.lo = a.lo << n;
        r.hi = (a.hi << n) | (a.lo >> (64 - n));
    }
    return r;
}

inline Int128 int128_lshr(Int128 a, unsigned n) {
    if (n == 0) return a;
    if (n >= 128) return Int128::from_i64(0);
    Int128 r;
    if (n >= 64) {
        r.lo = a.hi >> (n - 64);
        r.hi = 0;
    } else {
        r.lo = (a.lo >> n) | (a.hi << (64 - n));
        r.hi = a.hi >> n;
    }
    return r;
}

// Low 128 bits of a two's-complement product. The high half is discarded,
// which is the wrap of an i128 multiply.
inline Int128 int128_mul(Int128 a, Int128 b) {
    uint32_t aw[4] = {
        (uint32_t)a.lo, (uint32_t)(a.lo >> 32),
        (uint32_t)a.hi, (uint32_t)(a.hi >> 32)
    };
    uint32_t bw[4] = {
        (uint32_t)b.lo, (uint32_t)(b.lo >> 32),
        (uint32_t)b.hi, (uint32_t)(b.hi >> 32)
    };
    uint32_t acc[4] = {0, 0, 0, 0};
    for (int i = 0; i < 4; i++) {
        uint64_t carry = 0;
        for (int j = 0; i + j < 4; j++) {
            uint64_t cur = (uint64_t)acc[i + j] + (uint64_t)aw[i] * bw[j] + carry;
            acc[i + j] = (uint32_t)cur;
            carry = cur >> 32;
        }
    }
    Int128 r;
    r.lo = (uint64_t)acc[0] | ((uint64_t)acc[1] << 32);
    r.hi = (uint64_t)acc[2] | ((uint64_t)acc[3] << 32);
    return r;
}

// Unsigned division. The caller rejects a zero divisor.
inline Int128 int128_udiv(Int128 a, Int128 b) {
    Int128 q = Int128::from_i64(0);
    Int128 r = Int128::from_i64(0);
    for (int i = 127; i >= 0; i--) {
        r = int128_lshl(r, 1);
        uint64_t bit = (i >= 64) ? ((a.hi >> (i - 64)) & 1u) : ((a.lo >> i) & 1u);
        r.lo |= bit;
        if (int128_ucmp(r, b) >= 0) {
            r = int128_sub(r, b);
            if (i >= 64) q.hi |= (uint64_t)1 << (i - 64);
            else q.lo |= (uint64_t)1 << i;
        }
    }
    return q;
}

// Truncates toward zero, matching signed i128 division.
inline Int128 int128_div(Int128 a, Int128 b) {
    if (int128_is_zero(b)) return Int128::from_i64(0);
    bool neg = int128_is_negative(a) != int128_is_negative(b);
    Int128 ua = int128_is_negative(a) ? int128_neg(a) : a;
    Int128 ub = int128_is_negative(b) ? int128_neg(b) : b;
    Int128 q = int128_udiv(ua, ub);
    return neg ? int128_neg(q) : q;
}

inline Int128 int128_shl(Int128 a, Int128 b) {
    unsigned shift = (unsigned)b.lo & 127u;
    return int128_lshl(a, shift);
}

// Arithmetic right shift, matching a signed i128 >>.
inline Int128 int128_shr(Int128 a, Int128 b) {
    unsigned shift = (unsigned)b.lo & 127u;
    if (shift == 0 || !int128_is_negative(a)) return int128_lshr(a, shift);
    Int128 ones;
    ones.lo = ~(uint64_t)0;
    ones.hi = ~(uint64_t)0;
    return int128_or(int128_lshr(a, shift), int128_lshl(ones, 128 - shift));
}

// Fortran integer power. A negative exponent is 0 unless the base is ±1.
inline Int128 int128_pow(Int128 base, Int128 exp) {
    if (int128_is_negative(exp)) {
        if (int128_cmp(base, Int128::from_i64(1)) == 0) return Int128::from_i64(1);
        if (int128_cmp(base, Int128::from_i64(-1)) == 0) {
            return Int128::from_i64((exp.lo & 1u) ? -1 : 1);
        }
        return Int128::from_i64(0);
    }
    Int128 r = Int128::from_i64(1);
    Int128 ub = base;
    Int128 ue = exp;
    while (!int128_is_zero(ue)) {
        if (ue.lo & 1u) r = int128_mul(r, ub);
        ue = int128_lshr(ue, 1);
        if (!int128_is_zero(ue)) ub = int128_mul(ub, ub);
    }
    return r;
}

// True when the signed value fits in int64.
inline bool int128_fits_i64(Int128 v) {
    if (v.hi == 0) return (int64_t)v.lo >= 0;
    if (v.hi == ~(uint64_t)0) return (int64_t)v.lo < 0;
    return false;
}

// A plain int64 in 2^62 .. 2^63-1 has the BigInt pointer tag. A kind-16
// constant in that range is stored as a payload so the tag stays unambiguous.
inline bool int128_needs_payload(Int128 v) {
    if (!int128_fits_i64(v)) return true;
    return BigInt::is_int_ptr((int64_t)v.lo);
}

// Decimal magnitude, no sign. Rejects a value above 2^127-1.
inline bool int128_from_decimal(const std::string &digits, Int128 &out) {
    if (digits.empty()) return false;
    uint32_t w[4] = {0, 0, 0, 0};
    for (char c : digits) {
        if (c < '0' || c > '9') return false;
        uint64_t carry = (unsigned)(c - '0');
        for (int i = 0; i < 4; i++) {
            uint64_t cur = (uint64_t)w[i] * 10u + carry;
            w[i] = (uint32_t)cur;
            carry = cur >> 32;
        }
        if (carry != 0) return false;
    }
    // 2^127-1 is the largest magnitude a positive integer(16) can hold.
    if (w[3] & 0x80000000u) return false;
    out.lo = (uint64_t)w[0] | ((uint64_t)w[1] << 32);
    out.hi = (uint64_t)w[2] | ((uint64_t)w[3] << 32);
    return true;
}

inline std::string int128_to_decimal(Int128 v) {
    bool neg = int128_is_negative(v);
    Int128 mag = neg ? int128_neg(v) : v;
    if (int128_is_zero(mag)) return "0";
    char rev[48];
    int n = 0;
    while (!int128_is_zero(mag)) {
        Int128 q = int128_udiv(mag, Int128::from_i64(10));
        Int128 prod = int128_mul(q, Int128::from_i64(10));
        uint64_t digit = int128_sub(mag, prod).lo;
        rev[n++] = (char)('0' + (int)digit);
        mag = q;
    }
    std::string s;
    if (neg) s.push_back('-');
    while (n > 0) s.push_back(rev[--n]);
    return s;
}

inline Int128 int128_not(Int128 v) {
    Int128 r;
    r.lo = ~v.lo;
    r.hi = ~v.hi;
    return r;
}

inline Int128 int128_abs(Int128 v) {
    return int128_is_negative(v) ? int128_neg(v) : v;
}

inline Int128 int128_mod(Int128 a, Int128 b) {
    return int128_sub(a, int128_mul(int128_div(a, b), b));
}

inline Int128 int128_modulo(Int128 a, Int128 b) {
    Int128 r = int128_mod(a, b);
    if (!int128_is_zero(r) && int128_is_negative(r) != int128_is_negative(b)) {
        r = int128_add(r, b);
    }
    return r;
}

inline bool int128_btest(Int128 v, unsigned pos) {
    if (pos >= 128) return false;
    return pos < 64 ? (((v.lo >> pos) & 1u) != 0)
                    : (((v.hi >> (pos - 64)) & 1u) != 0);
}

inline Int128 int128_ibset(Int128 v, unsigned pos) {
    if (pos < 64) v.lo |= (uint64_t{1} << pos);
    else if (pos < 128) v.hi |= (uint64_t{1} << (pos - 64));
    return v;
}

inline Int128 int128_ibclr(Int128 v, unsigned pos) {
    if (pos < 64) v.lo &= ~(uint64_t{1} << pos);
    else if (pos < 128) v.hi &= ~(uint64_t{1} << (pos - 64));
    return v;
}

inline int int128_popcnt_u64(uint64_t x) {
    int c = 0;
    while (x) {
        x &= x - 1;
        c++;
    }
    return c;
}

inline int int128_popcnt(Int128 v) {
    return int128_popcnt_u64(v.lo) + int128_popcnt_u64(v.hi);
}

inline int int128_leadz(Int128 v) {
    if (v.hi != 0) {
        int c = 0;
        for (int b = 63; b >= 0; b--) {
            if ((v.hi >> b) & 1u) break;
            c++;
        }
        return c;
    }
    if (v.lo != 0) {
        int c = 64;
        for (int b = 63; b >= 0; b--) {
            if ((v.lo >> b) & 1u) break;
            c++;
        }
        return c;
    }
    return 128;
}

inline int int128_trailz(Int128 v) {
    if (v.lo != 0) {
        int c = 0;
        for (int b = 0; b < 64; b++) {
            if ((v.lo >> b) & 1u) break;
            c++;
        }
        return c;
    }
    if (v.hi != 0) {
        int c = 64;
        for (int b = 0; b < 64; b++) {
            if ((v.hi >> b) & 1u) break;
            c++;
        }
        return c;
    }
    return 128;
}

inline double int128_to_double(Int128 v) {
    bool neg = int128_is_negative(v);
    Int128 mag = neg ? int128_neg(v) : v;
    double r = std::ldexp(static_cast<double>(mag.hi), 64) + static_cast<double>(mag.lo);
    return neg ? -r : r;
}

inline long double int128_to_long_double(Int128 v) {
    bool neg = int128_is_negative(v);
    Int128 mag = neg ? int128_neg(v) : v;
    long double r = std::ldexp(static_cast<long double>(mag.hi), 64) + static_cast<long double>(mag.lo);
    return neg ? -r : r;
}

} // namespace LCompilers

#endif
