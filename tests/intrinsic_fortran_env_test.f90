program test_fortran_env
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  real(real64) :: x = 3.14
  print *, x
end program
