module module_global_coarray_01_m
  implicit none
  integer :: x[*] = 1
end module module_global_coarray_01_m

subroutine module_global_coarray_01_set()
  use module_global_coarray_01_m, only: x
  implicit none
  x = 2
end subroutine module_global_coarray_01_set
