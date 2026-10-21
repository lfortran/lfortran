"""Generate the binary128 constants and tables of lfortran_float128.c.

Section K of lfortran_float128.c starts with a block of constants and tables
(from "pi/2 = PIO2_1 + PIO2_2 + PIO2_3" through F128_TWO_OVER_PI_BITS). This
script prints that block. It uses only the Python standard library: exact
rational arithmetic (fractions), 140-digit decimal logarithms (decimal) and
integer arithmetic for the bits of 2/pi. Every value is rounded to nearest
(ties to even) to binary128; a {hi, lo} pair holds the rounded value and the
rounded remainder. The script is not run by the build. To regenerate the
block, run

    python src/libasr/runtime/gen_float128_consts.py > tables.txt

and replace the block in lfortran_float128.c with the contents of tables.txt.
"""
import math
import sys
from decimal import Decimal as D, getcontext
from fractions import Fraction as Fr

getcontext().prec = 140

def dec_to_fr(d):
    return Fr(d)

def rn(v, bits=113):
    """Round Fraction v to nearest binary float with `bits` significant bits (ties even)."""
    if v == 0: return Fr(0)
    s = -1 if v < 0 else 1
    a = abs(v)
    e = a.numerator.bit_length() - a.denominator.bit_length()
    if Fr(2)**e > a: e -= 1
    if Fr(2)**(e+1) <= a: e += 1
    sc = Fr(2)**(e - bits + 1)
    m = a / sc
    q, r = divmod(m.numerator, m.denominator)
    rem = Fr(r, m.denominator)
    if rem > Fr(1,2) or (rem == Fr(1,2) and q % 2 == 1): q += 1
    return s * q * sc

def words(v):
    if v == 0: return (0, 0)
    s = 1 if v < 0 else 0
    a = abs(v)
    e = a.numerator.bit_length() - a.denominator.bit_length()
    if Fr(2)**e > a: e -= 1
    if Fr(2)**(e+1) <= a: e += 1
    m = a / Fr(2)**(e - 112)
    assert m.denominator == 1, v
    m = m.numerator
    assert 2**112 <= m < 2**113
    hi = (s << 63) | ((e + 16383) << 48) | ((m >> 64) & ((1 << 48) - 1))
    lo = m & ((1 << 64) - 1)
    return hi, lo

def cw(v):
    hi, lo = words(v)
    return "{0x%016XULL, 0x%016XULL}" % (hi, lo)

def dw(x):
    h = rn(x); l = rn(x - h); return h, l

def atan_dec(x):
    # x: Decimal, |x| <= 1; argument halving twice then Taylor
    for _ in range(3):
        x = x / (1 + (1 + x*x).sqrt())
    s = D(0); t = x; k = 0; x2 = x*x
    while True:
        term = t / (2*k + 1)
        if abs(term) < D(10) ** -135: break
        s += term if k % 2 == 0 else -term
        t *= x2; k += 1
    return s * 8

pi_d = 16*atan_dec(D(1)/5) - 4*atan_dec(D(1)/239)
pi = dec_to_fr(pi_d)
ln2 = dec_to_fr(D(2).ln())
ln10 = dec_to_fr(D(10).ln())

out = []
def const(name, v):
    out.append("static const uint64_t %s[2] = %s;" % (name, cw(v)))

def const_dw(name, comment, v):
    h, l = dw(v)
    out.append("/* %s, {hi, lo} */" % comment)
    out.append("static const uint64_t %s[2][2] = {%s, %s};" % (name, cw(h), cw(l)))
out.append("/* pi/2 = PIO2_1 + PIO2_2 + PIO2_3 to 339 bits */")
p1 = rn(pi/2); p2 = rn(pi/2 - p1); p3 = rn(pi/2 - p1 - p2)
const("F128_PIO2_1", p1); const("F128_PIO2_2", p2); const("F128_PIO2_3", p3)
out.append("/* 2/pi */")
const("F128_TWO_OVER_PI", rn(2/pi))
out.append("/* log(2) = LN2_HI98 + LN2_LO98, LN2_HI98 with 98 significant bits so that")
out.append(" * k*LN2_HI98 is exact for |k| < 2^15 */")
l2h = rn(ln2, 98); assert l2h * 2**15 == rn(l2h * 2**15)
const("F128_LN2_HI98", l2h); const("F128_LN2_LO98", rn(ln2 - l2h))
const_dw("F128_PI", "pi", pi)
const_dw("F128_LN2", "log(2)", ln2)
const_dw("F128_LOG2E", "1/log(2)", 1/ln2)
const_dw("F128_LOG10E", "1/log(10)", 1/ln10)
const_dw("F128_D2R", "pi/180", pi/180)
const_dw("F128_R2D", "180/pi", 180/pi)
out.append("/* log(1 + i/64) for i = -16..32, as {hi, lo} pairs */")
out.append("static const uint64_t F128_LOG_TABLE[49][2][2] = {")
for i in range(-16, 33):
    v = dec_to_fr((1 + D(i)/64).ln()) if i != 0 else Fr(0)
    h, l = dw(v)
    out.append("    {%s, %s}," % (cw(h), cw(l)))
out.append("};")
out.append("/* atan(i/8) for i = 0..8, as {hi, lo} pairs */")
out.append("static const uint64_t F128_ATAN_TABLE[9][2][2] = {")
for i in range(0, 9):
    v = dec_to_fr(atan_dec(D(i)/8)) if i else Fr(0)
    if i == 8: v = pi/4
    h, l = dw(v)
    out.append("    {%s, %s}," % (cw(h), cw(l)))
out.append("};")
out.append("/* 1/k! for k = 0..31 */")
out.append("static const uint64_t F128_INV_FACT[32][2] = {")
for k in range(32):
    out.append("    %s," % cw(rn(Fr(1, math.factorial(k)))))
out.append("};")
out.append("/* 1/(2k+1) for k = 0..31 */")
out.append("static const uint64_t F128_INV_ODD[32][2] = {")
for k in range(32):
    out.append("    %s," % cw(rn(Fr(1, 2*k + 1))))
out.append("};")

# 2/pi bits, integer Machin
NB = 262 * 64 + 128
prec = NB + 64
def arctan_inv(x, one):
    s = t = one // x; x2 = x*x; k = 1; sign = -1
    while t:
        t //= x2
        s += sign * (t // (2*k + 1)); sign = -sign; k += 1
    return s
one = 1 << (prec + 32)
pi_int = 16*arctan_inv(5, one) - 4*arctan_inv(239, one)   # pi * 2^(prec+32)
assert abs(Fr(pi_int, 1 << (prec+32)) - pi) < Fr(1, 10**130)
# 2/pi * 2^(262*64) floor
tp = (2 << (prec + 32 + 262*64)) // pi_int
wordsl = []
for j in range(262):
    wordsl.append((tp >> (64*(261 - j))) & ((1 << 64) - 1))
assert tp >> (262*64) == 0
out.append("/* Bits of 2/pi after the binary point, most significant first */")
out.append("static const uint64_t F128_TWO_OVER_PI_BITS[262] = {")
for j in range(0, 262, 4):
    out.append("    " + ", ".join("0x%016XULL" % w for w in wordsl[j:j+4]) + ",")
out.append("};")
sys.stdout.write("\n".join(out) + "\n")
