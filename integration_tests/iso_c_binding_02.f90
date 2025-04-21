program iso_c_binding_02
  use iso_c_binding
  implicit none

  type(c_funptr) :: fptr
  type(c_ptr) :: ptr
  fptr = c_null_funptr
  ptr = c_null_ptr

  ! check if ptr is null
  if (c_associated(ptr)) error stop

  ! check if fptr is null
  if (c_associated(fptr)) error stop

  print *, "Null function pointer assigned successfully."
end program iso_c_binding_02
