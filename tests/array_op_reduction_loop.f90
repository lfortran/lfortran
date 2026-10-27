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
