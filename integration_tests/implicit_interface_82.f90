! A use-associated procedure with an implicit interface is called and
! referenced like a local one: through an interface built from each
! reference's actual arguments.
program implicit_interface_82
  use implicit_interface_82_mod
  implicit none
  real :: x
  x = 1.0
  if (abs(ii82_drive(x) - 4.0) > 1e-6) error stop 1
  if (abs(x - 2.0) > 1e-6) error stop 2
  call ii82_add1(x)
  if (abs(x - 3.0) > 1e-6) error stop 3
  if (abs(ii82_twice(x) - 6.0) > 1e-6) error stop 4
  print *, "ok"
end program

subroutine ii82_add1(x)
  real :: x
  x = x + 1
end subroutine

real function ii82_twice(x)
  real :: x
  ii82_twice = 2*x
end function
