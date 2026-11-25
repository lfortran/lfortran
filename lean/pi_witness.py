#!/usr/bin/env python3
"""Witnesses for `pi_lower_bound` / `pi_upper_bound` (LFortran/PiBounds.lean).

A Python port of the Mathematica generator documented in Mathlib's
Mathlib/Analysis/Real/Pi/Bounds.lean, plus an exact rational check of the
inequalities the Lean tactics prove, and a search over the generator's
parameters. Lean re-checks every witness, so this script is not trusted.

    python3 lean/pi_witness.py
"""
import decimal
from decimal import Decimal as D
from fractions import Fraction as F
decimal.getcontext().prec = 120

def simplest_between(lo, hi):
    # simplest rational in [lo, hi] (lo < hi, positive), via continued fractions
    lo, hi = F(lo), F(hi)
    fl = lo.numerator // lo.denominator
    if F(fl) == lo: return F(fl)
    if fl + 1 <= hi: return F(fl + 1)
    rest = simplest_between(1 / (hi - fl), 1 / (lo - fl))
    return fl + 1 / rest

def rationalize(x, dx):
    x = F(x); dx = F(abs(dx))
    return simplest_between(x - dx, x + dx)

def bound(a, n, extra):
    a = D(a)
    PI = D('3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798')
    sign = 1 if a >= PI else -1
    r0 = 2 - ((a - D((sign + 1) // 2) / 2 / D(4) ** n) / D(2) ** (n + 1)) ** 2
    xs = [r0]
    for _ in range(n - 1): xs.append(xs[-1] ** 2 - 2)
    r = [(2 - x).ln() for x in xs]
    diff = (r[-1] - (2 - D(2).sqrt()).ln()) / (len(r) + 1)
    assert sign * diff > 0, "insufficient iterations"
    ts = [r[i] - diff * (i + 1) for i in range(len(r))]
    r2 = [rationalize(t.exp(), D(extra) * (t.exp() - (t - sign * diff).exp())) for t in ts]
    # checks (exact rationals, sqrt via squares)
    vals = [F(v) for v in r2]   # these are exp(r2) = 2 - x_i
    out = list(reversed([2 - v for v in vals]))
    return out

def fmt(q):
    s1 = f"{q.numerator}/{q.denominator}"
    t = 2 - q
    s2 = f"2-{t.numerator}/{t.denominator}"
    return s2 if len(s2) < len(s1) else s1

def check(a, xs, upper):
    """The inequalities `pi_lower_bound` / `pi_upper_bound` check, exactly."""
    a = F(a); n = len(xs); prev = F(0)
    for x in xs:
        if upper:
            if not (x * x <= 2 + prev): return False
        else:
            if not (2 + prev <= x * x): return False
        prev = x
    if upper:
        z = 2 - ((a - F(1, 4 ** n)) / F(2) ** (n + 1)) ** 2
        return z <= prev and F(1, 4 ** n) <= a
    return prev <= 2 - (a / F(2) ** (n + 1)) ** 2


def search(a, upper):
    for n in range(10, 80):
        for extra in ['0.9', '0.8', '0.7', '0.6', '0.5', '0.4', '0.3', '0.2', '0.1']:
            try:
                xs = bound(a, n, extra)
            except AssertionError:
                continue
            if check(F(D(a)), xs, upper):
                return xs
    raise SystemExit("no witness found")


if __name__ == "__main__":
    lo = '3.14159265358979323846264338327950'
    hi = '3.14159265358979323846264338327951'
    print(f"theorem pi_gt_d32 : {lo} < π := by")
    print("  pi_lower_bound [" + ", ".join(fmt(q) for q in search(lo, False)) + "]")
    print(f"theorem pi_lt_d32 : π < {hi} := by")
    print("  pi_upper_bound [" + ", ".join(fmt(q) for q in search(hi, True)) + "]")
