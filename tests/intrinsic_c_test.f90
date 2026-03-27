program test_c_binding
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none
  integer(c_int) :: i = 42
  print *, i
end program
