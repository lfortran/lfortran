program c_ptr_06
    use iso_c_binding
    implicit none
  
    character(len=1), pointer :: fchars(:)
    type(c_ptr) :: cptr
    integer, parameter :: N = 6
    character(kind=c_char), target :: cbuf(6)
  
    cbuf = [ 'H', 'E', 'L', 'L', 'O', C_NULL_CHAR ]
  
    cptr = c_loc(cbuf)
    call c_f_pointer(cptr, fchars, [N])
  
    print *, "First few elements of fchars:"
    print *, fchars
    if(any(fchars /= [ 'H', 'E', 'L', 'L', 'O', C_NULL_CHAR])) error stop
  
end program   