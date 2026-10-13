! The loops that array_op creates for these assignments index every operand
! within its bounds. The shapes of `x` and `y` are known to conform at compile
! time, so the first assignment needs no shape check at all; the second one
! still needs a runtime shape check because the extent of `z` is not known.
subroutine array_op_in_bounds(x, y, z)
implicit none
real, intent(out) :: x(4)
real, intent(in) :: y(4), z(:)
x = y + 1.
x = z
end subroutine
