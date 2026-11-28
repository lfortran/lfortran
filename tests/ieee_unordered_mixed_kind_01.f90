program ieee_unordered_mixed_kind_01
  use :: ieee_arithmetic
  implicit none
  real :: x
  x = 1.0
  if (ieee_unordered(x, 0.0d0)) error stop 1
  if (ieee_unordered(0.0d0, x)) error stop 2
end program
