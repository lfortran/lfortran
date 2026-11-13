#ifndef LFORTRAN_INTEGER128_H
#define LFORTRAN_INTEGER128_H

// A signed 128-bit integer stored as a little-endian pair of words.
// Arithmetic wraps in two's complement, which is what the LLVM backend
// does for integer(16), and values that fit in int64 stay plain int64
// constants unless that bit pattern would be mistaken for a BigInt pointer.

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

#if defined(__SIZEOF_INT128__)

inline __int128 int128_as_native(Int128 v) {
    return ((__int128)v.hi << 64) | (__int128)v.lo;
}

inline Int128 int128_from_native(__int128 v) {
    Int128 r;
    r.lo = (uint64_t)v;
    r.hi = (uint64_t)((unsigned __int128)v >> 64);
    return r;
}

inline bool int128_is_negative(Int128 v) {
    return (v.hi >> 63) != 0;
}

inline int int128_cmp(Int128 a, Int128 b) {
    __int128 na = int128_as_native(a);
    __int128 nb = int128_as_native(b);
    if (na < nb) return -1;
    if (na > nb) return 1;
    return 0;
}

inline Int128 int128_neg(Int128 v) {
    unsigned __int128 u = (unsigned __int128)int128_as_native(v);
    // Negating the most negative value wraps back to itself.
    u = 0 - u;
    return int128_from_native((__int128)u);
}

inline Int128 int128_add(Int128 a, Int128 b) {
    return int128_from_native(int128_as_native(a) + int128_as_native(b));
}

inline Int128 int128_sub(Int128 a, Int128 b) {
    return int128_from_native(int128_as_native(a) - int128_as_native(b));
}

inline Int128 int128_mul(Int128 a, Int128 b) {
    return int128_from_native(int128_as_native(a) * int128_as_native(b));
}

inline Int128 int128_div(Int128 a, Int128 b) {
    return int128_from_native(int128_as_native(a) / int128_as_native(b));
}

inline Int128 int128_and(Int128 a, Int128 b) {
    return int128_from_native(int128_as_native(a) & int128_as_native(b));
}

inline Int128 int128_or(Int128 a, Int128 b) {
    return int128_from_native(int128_as_native(a) | int128_as_native(b));
}

inline Int128 int128_xor(Int128 a, Int128 b) {
    return int128_from_native(int128_as_native(a) ^ int128_as_native(b));
}

inline Int128 int128_shl(Int128 a, Int128 b) {
    unsigned __int128 u = (unsigned __int128)int128_as_native(a);
    unsigned shift = (unsigned)int128_as_native(b) & 127u;
    return int128_from_native((__int128)(u << shift));
}

inline Int128 int128_shr(Int128 a, Int128 b) {
    unsigned shift = (unsigned)int128_as_native(b) & 127u;
    return int128_from_native(int128_as_native(a) >> shift);
}

// Fortran integer power. A negative exponent is 0 unless the base is ±1.
inline Int128 int128_pow(Int128 base, Int128 exp) {
    __int128 b = int128_as_native(base);
    __int128 e = int128_as_native(exp);
    if (e < 0) {
        if (b == 1) return Int128::from_i64(1);
        if (b == -1) return Int128::from_i64((e & 1) ? -1 : 1);
        return Int128::from_i64(0);
    }
    unsigned __int128 ub = (unsigned __int128)b;
    unsigned __int128 ue = (unsigned __int128)e;
    unsigned __int128 r = 1;
    while (ue != 0) {
        if (ue & 1) r *= ub;
        ue >>= 1;
        if (ue != 0) ub *= ub;
    }
    return int128_from_native((__int128)r);
}

inline bool int128_is_zero(Int128 v) {
    return v.lo == 0 && v.hi == 0;
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
    unsigned __int128 maxv = ((unsigned __int128)1 << 127) - 1;
    unsigned __int128 v = 0;
    for (char c : digits) {
        if (c < '0' || c > '9') return false;
        unsigned __int128 nv = v * 10u + (unsigned)(c - '0');
        if (nv < v || nv > maxv) return false;
        v = nv;
    }
    out = int128_from_native((__int128)v);
    return true;
}

inline std::string int128_to_decimal(Int128 v) {
    bool neg = int128_is_negative(v);
    unsigned __int128 mag = (unsigned __int128)int128_as_native(neg ? int128_neg(v) : v);
    if (mag == 0) return "0";
    char rev[48];
    int n = 0;
    while (mag != 0) {
        rev[n++] = (char)('0' + (int)(mag % 10));
        mag /= 10;
    }
    std::string s;
    if (neg) s.push_back('-');
    while (n > 0) s.push_back(rev[--n]);
    return s;
}

#else

inline bool int128_from_decimal(const std::string &, Int128 &) { return false; }
inline std::string int128_to_decimal(Int128) { return "0"; }
inline bool int128_needs_payload(Int128) { return false; }
inline bool int128_fits_i64(Int128) { return true; }
inline bool int128_is_zero(Int128 v) { return v.lo == 0 && v.hi == 0; }
inline int int128_cmp(Int128, Int128) { return 0; }
inline Int128 int128_neg(Int128 v) { return v; }
inline Int128 int128_add(Int128 a, Int128) { return a; }
inline Int128 int128_sub(Int128 a, Int128) { return a; }
inline Int128 int128_mul(Int128 a, Int128) { return a; }
inline Int128 int128_div(Int128 a, Int128) { return a; }
inline Int128 int128_pow(Int128 a, Int128) { return a; }
inline Int128 int128_and(Int128 a, Int128) { return a; }
inline Int128 int128_or(Int128 a, Int128) { return a; }
inline Int128 int128_xor(Int128 a, Int128) { return a; }
inline Int128 int128_shl(Int128 a, Int128) { return a; }
inline Int128 int128_shr(Int128 a, Int128) { return a; }

#endif

} // namespace LCompilers

#endif
