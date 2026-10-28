! A reduction with a scalar result over an elementwise array expression is
! accumulated in one loop over the elements of the expression, without
! evaluating the expression into an allocatable array temporary first.
subroutine array_op_reduction_loop(a, b, i, j, s, n)
implicit none
real, intent(in) :: a(:), b(:)
integer, intent(in) :: i, j
real, intent(out) :: s
integer, intent(out) :: n
s = sum(a(i:j) * b(i:j)) + 1
n = 0
if (any(a(i:j) > b(i:j))) n = count(a(i:j) > 2 * b(i:j))
end subroutine

! The operations in the arguments of elemental intrinsic functions are
! accumulated element by element as well, and MAXVAL and MINVAL evaluate
! each element once.
subroutine array_op_reduction_elemental(a, b, n, s, m, c, e)
implicit none
real, intent(in) :: a(:), b(:)
integer, intent(in) :: n
real, intent(out) :: s, m, e
integer, intent(out) :: c
s = sum(abs(a(1:n) - b(1:n)))
m = maxval(abs(a(2:n) - a(1:n-1)))
c = count(abs(a - b) > 0.5)
e = minval(exp(a) * log(b + 1))
end subroutine
