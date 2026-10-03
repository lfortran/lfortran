! companion module: c_ptr_16_module.f90
program c_ptr_16
   use c_ptr_16_module, only: clear, N
   use, intrinsic :: iso_c_binding, only: c_ptr, c_associated
   implicit none
   type(c_ptr) :: entries(N)
   integer :: i
   call clear(entries)
   do i = 1, N
      if (c_associated(entries(i))) error stop "clear did not null all entries"
   end do
   print *, "ok"
end program
