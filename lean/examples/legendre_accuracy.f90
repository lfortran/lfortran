! Measures the absolute error of legendre_poly(n, x) for 0 <= n <= 100 and
! x in [-1, 1] against a double-double (~106 bit) evaluation of the same
! recurrence, which is accurate to ~1e-29 here.
program legendre_accuracy
use legendre_mod, only: dp, legendre_poly
implicit none
integer, parameter :: nmax = 100, nx = 20001
integer :: n, i, n_worst
real(dp) :: x, p, ph, pl, err, max_err, x_worst
real(dp) :: max_err_n(0:nmax)

max_err = 0
max_err_n = 0
n_worst = -1
x_worst = 0
do i = 0, nx - 1
    ! uniform grid plus points clustered near the endpoints
    if (mod(i, 2) == 0) then
        x = -1 + 2*real(i, dp)/(nx - 1)
    else
        x = 1 - 2.0_dp**(-real(mod(i, 1000), dp)/20)
        if (mod(i, 4) == 1) x = -x
    end if
    do n = 0, nmax
        p = legendre_poly(n, x)
        call legendre_dd(n, x, ph, pl)
        err = abs((p - ph) - pl)
        if (err > max_err_n(n)) max_err_n(n) = err
        if (err > max_err) then
            max_err = err
            n_worst = n
            x_worst = x
        end if
    end do
end do

do n = 0, nmax, 10
    print "(a,i4,a,es10.3)", "n =", n, "   max abs error =", max_err_n(n)
end do
print "(a,es10.3,a,i4,a,es24.16)", "overall max abs error =", max_err, &
    "  at n =", n_worst, ", x =", x_worst
print "(a,es10.3)", "unit roundoff u = 2^-53 =", 2.0_dp**(-53)
print "(a,f10.1)", "max error / u =", max_err / 2.0_dp**(-53)
if (max_err > 1e-11_dp) error stop "accuracy requirement 1e-11 violated"

contains

    subroutine two_sum(a, b, s, e)
    real(dp), intent(in) :: a, b
    real(dp), intent(out) :: s, e
    real(dp) :: v
    s = a + b
    v = s - a
    e = (a - (s - v)) + (b - v)
    end subroutine

    subroutine split(a, hi, lo)
    real(dp), intent(in) :: a
    real(dp), intent(out) :: hi, lo
    real(dp) :: t
    t = 134217729.0_dp*a
    hi = t - (t - a)
    lo = a - hi
    end subroutine

    subroutine two_prod(a, b, p, e)
    real(dp), intent(in) :: a, b
    real(dp), intent(out) :: p, e
    real(dp) :: ah, al, bh, bl
    p = a*b
    call split(a, ah, al)
    call split(b, bh, bl)
    e = ((ah*bh - p) + ah*bl + al*bh) + al*bl
    end subroutine

    ! (ch, cl) = (ah, al) * b
    subroutine dd_mul_d(ah, al, b, ch, cl)
    real(dp), intent(in) :: ah, al, b
    real(dp), intent(out) :: ch, cl
    real(dp) :: p, e
    call two_prod(ah, b, p, e)
    e = e + al*b
    call two_sum(p, e, ch, cl)
    end subroutine

    ! (ch, cl) = (ah, al) + (bh, bl)
    subroutine dd_add(ah, al, bh, bl, ch, cl)
    real(dp), intent(in) :: ah, al, bh, bl
    real(dp), intent(out) :: ch, cl
    real(dp) :: s, e
    call two_sum(ah, bh, s, e)
    e = e + al + bl
    call two_sum(s, e, ch, cl)
    end subroutine

    ! (ch, cl) = (ah, al) / b
    subroutine dd_div_d(ah, al, b, ch, cl)
    real(dp), intent(in) :: ah, al, b
    real(dp), intent(out) :: ch, cl
    real(dp) :: q1, q2, ph, pl, rh, rl
    q1 = ah / b
    call dd_mul_d(q1, 0.0_dp, b, ph, pl)
    call dd_add(ah, al, -ph, -pl, rh, rl)
    q2 = rh / b
    call two_sum(q1, q2, ch, cl)
    end subroutine

    subroutine legendre_dd(n, x, ph, pl)
    integer, intent(in) :: n
    real(dp), intent(in) :: x
    real(dp), intent(out) :: ph, pl
    real(dp) :: ah, al, bh, bl, th, tl, uh, ul, sh, sl, vh, vl, ch, cl
    integer :: k
    ah = 1; al = 0
    bh = x; bl = 0
    if (n == 0) then
        ph = ah; pl = al
        return
    end if
    do k = 2, n
        call dd_mul_d(bh, bl, x, th, tl)
        call dd_mul_d(th, tl, real(2*k-1, dp), uh, ul)
        call dd_mul_d(ah, al, real(-(k-1), dp), sh, sl)
        call dd_add(uh, ul, sh, sl, vh, vl)
        call dd_div_d(vh, vl, real(k, dp), ch, cl)
        ah = bh; al = bl
        bh = ch; bl = cl
    end do
    ph = bh; pl = bl
    end subroutine

end program
