program cpp_pre_14
  use iso_c_binding, only: c_int
#include "cpp_pre_14_include.h"
  implicit none
  integer(c_int) :: x
  x = CPP_PRE_14_VAL
  if (x /= 42) error stop
  print *, "PASSED"
end program
