program c_ptr_17
  use c_ptr_17_module, only: ptrs, N
  use, intrinsic :: iso_c_binding, only: c_associated
  implicit none
  integer :: i
  if (size(ptrs) /= N) error stop "wrong size"
  do i = 1, N
    if (c_associated(ptrs(i))) error stop "default-init not null"
  end do
  print *, "ok"
end program
