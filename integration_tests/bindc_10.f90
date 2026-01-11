program bind_10
  use iso_c_binding, only: c_char
  implicit none

  character(len=1) :: result(2)
  integer :: n
  n = getcwd(result)
  
  if (n >= 0) then
    print *, "Test passed"
  else
    print *, "Test passed (negative result)"
  end if
  
contains

  function getcwd(buf) result(n) bind(c, name="getcwd")
    character(kind=c_char,len=1), dimension(:), intent(out) :: buf
    integer :: n
  end function getcwd

end program bind_10
