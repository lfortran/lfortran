program common_26
  implicit none

  ! All Fortran standards allow the size of blank common to vary between
  ! program units - from F66 (7.2.1.3) to F2023 (8.10.2.5).

  real :: pi, epsilon, sq2
  common // pi, epsilon, sq2

  pi = 3.1415654
  epsilon = 2.718281828
  sq2 = 1.414213562

  call print_pi ()
  call print_eps ()
  call print_sq2 ()

  print *, "All tests passed"

end program

subroutine print_pi ()
  implicit none

  real :: pi
  common  pi

  if (abs(pi - 3.1415654) > 1.0e-6) error stop "Wrong pi value"

end subroutine

subroutine print_eps ()
  implicit none

  real :: dummy, eps
  common  dummy, eps

  if (abs(eps - 2.718281828) > 1.0e-6) error stop "Wrong epsilon value"

end subroutine

subroutine print_sq2 ()
  implicit none

  real :: dummy(2), sq2
  common  dummy,    sq2

  if (abs(sq2 - 1.414213562) > 1.0e-6) error stop "Wrong sqrt(2) value"

end subroutine
