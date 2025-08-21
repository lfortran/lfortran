program bindc_07
  use iso_c_binding, only: c_char, c_ptr, c_null_ptr, c_size_t, c_int, c_associated, c_f_pointer, c_null_char
  implicit none

  
  character(len=1) :: result(1)
  character(len=1024, kind=c_char), pointer :: result1
  type(c_ptr) :: ptr_result
  integer(c_size_t) :: size
  ptr_result=c_null_ptr
   ptr_result = getcwd(result, size)
   
   if (.not.c_associated(ptr_result)) then
      error stop "Failed to get current working directory."
   end if
   contains
      function getcwd(buf, size) result(res) bind(c, name="getcwd")
        character(kind=c_char,len=1),dimension(:), intent(out) :: buf
        integer(c_size_t), value, intent(in) :: size
        type(c_ptr) :: res
      end function getcwd

end program bindc_07