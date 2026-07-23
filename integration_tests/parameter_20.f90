program parameter_20
  implicit none

  ! Integer parameter given a NEGATIVE real literal through a standalone
  ! `parameter (...)` statement, then used in a real-valued expression.
  ! The initializer is a RealUnaryMinus (not a bare RealConstant), so the
  ! RealToInteger conversion must still be folded to an IntegerConstant.
  integer :: k
  parameter (k = -125.0)

  ! Same, but with a kind-8 integer and a double precision literal.
  integer(8) :: k8
  parameter (k8 = -125.0d0)

  ! Real parameter given a NEGATIVE integer literal through a standalone
  ! statement (the symmetric case).
  real :: r
  parameter (r = -7)

  real :: x
  integer :: y

  x = k + 5.0
  print *, k, x
  if (k /= -125) error stop
  if (abs(x - (-120.0)) > 1e-6) error stop

  x = k8 + 5.0
  print *, k8, x
  if (k8 /= -125_8) error stop
  if (abs(x - (-120.0)) > 1e-6) error stop

  y = r + 1
  print *, r, y
  if (abs(r - (-7.0)) > 1e-6) error stop
  if (y /= -6) error stop
end program parameter_20
