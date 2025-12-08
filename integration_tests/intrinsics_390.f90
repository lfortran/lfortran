program intrinsics_390 
  use iso_c_binding
  type(c_ptr) :: cptr
  character(len=5,kind=c_char), pointer :: sptr
  character(len= 6, kind=c_char), target :: cbuf
  cbuf = "Hello"// c_null_char

  cptr = c_loc(cbuf)
  call c_f_pointer(cptr, sptr)
  
  print *, sptr
  if(sptr /= "Hello") error stop
end program intrinsics_390