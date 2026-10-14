! The loops that array_op creates for these assignments index every operand
! within its bounds. The shapes of `x` and `y` are known to conform at compile
! time, so the first assignment needs no shape check at all; the second one
! still needs a runtime shape check because the extent of `z` is not known.
! Nothing checks the shapes of the arguments of the elemental call, so only
! its loop-driving argument `x` is known to be indexed within its bounds.
subroutine array_op_in_bounds(x, y, z)
implicit none
real, intent(out) :: x(4)
real, intent(in) :: y(4), z(:)
x = y + 1.
x = z
call scale(x, z)
contains
elemental subroutine scale(a, b)
real, intent(inout) :: a
real, intent(in) :: b
a = a * b
end subroutine
end subroutine
