program parameter_19
  implicit none

  ! Integer parameter given a real literal through a standalone
  ! `parameter (...)` statement, then used in a real-valued expression.
  integer :: k
  parameter (k = 25.0)

  ! Real parameter given an integer literal through a standalone statement.
  real :: r
  parameter (r = 7)

  real :: x
  integer :: y

  x = k + 5.0
  print *, k, x
  if (k /= 25) error stop
  if (abs(x - 30.0) > 1e-6) error stop

  y = r + 1
  print *, r, y
  if (abs(r - 7.0) > 1e-6) error stop
  if (y /= 8) error stop
end program parameter_19
