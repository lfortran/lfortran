module c_ptr_17_module
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
  implicit none
  integer, parameter :: N = 4
  type(c_ptr) :: ptrs(N) = c_null_ptr
end module c_ptr_17_module

