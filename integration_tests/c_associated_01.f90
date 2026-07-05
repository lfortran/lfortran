program test
  use, intrinsic :: iso_c_binding, only: my_c_ptr => c_ptr, &
      my_c_associated => c_associated, my_c_null_ptr => c_null_ptr

  type(my_c_ptr) :: p
  p = my_c_null_ptr
  if (my_c_associated(p)) error stop
end program test
