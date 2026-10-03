! a helper module for c_ptr_16
module c_ptr_16_module
   use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
   implicit none
   integer, parameter :: N = 4
contains
   subroutine clear(entries)
      type(c_ptr), intent(out) :: entries(N)
      entries = c_null_ptr     ! scalar-to-array broadcast of CPtr
   end subroutine
end module
