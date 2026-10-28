! DOT_PRODUCT of elementwise expressions is accumulated in a loop, without
! an array temporary for the expressions, and its sections are indexed
! through their base arrays.
subroutine dot_products(a, b, ia, ca, cb, la, lb, i, j, s, is, cs, ls)
real, intent(in) :: a(:), b(10)
integer, intent(in) :: ia(:)
complex, intent(in) :: ca(:), cb(:)
logical, intent(in) :: la(:), lb(:)
integer, intent(in) :: i, j
real, intent(out) :: s
integer, intent(out) :: is
complex, intent(out) :: cs
logical, intent(out) :: ls
s = dot_product(a(i:j) * 2, b(j:i:-1))
is = dot_product(ia(1:7:3) + 1, ia(7:1:-3))
cs = dot_product(ca * 2, cb)
ls = dot_product(la .and. lb, lb)
end subroutine
