program common_32
  ! Test that COMMON block split across multiple statements in different
  ! program units is handled correctly (size check deferred until all
  ! statements for the block are processed).
  implicit none

  integer :: i1, i2
  real :: r1, r2
  common /block/ i1, i2
  real :: x
  common x /block/ r1, r2

  x = 42.42
  call sub ()
  if (i1 + i2 /= 579) error stop
  if (abs(r1 + r2) >= 0.0001) error stop

end program

subroutine sub ()
  implicit none

  integer :: i1, i2
  real :: r1, r2

  common /block/ i1, i2
  common /block/ r1, r2

  real :: x
  common // x

  if (abs(x - 42.42) >= 0.0001) error stop

  i1 = 123
  i2 = 456
  r1 = 3.14
  r2 = -3.14

end subroutine
