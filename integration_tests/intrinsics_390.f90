program intrinsics_390 
  use iso_c_binding
  type(c_ptr) :: cptr
  character(len=5,kind=c_char), pointer :: sptr
  call c_f_pointer(cptr, sptr)
end program intrinsics_390