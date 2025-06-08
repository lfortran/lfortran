program intrinsics_218
    implicit none
    integer, parameter :: new_len = len(new_line('a'))
    character(len=*), parameter :: str = new_line('')
    print *, new_len
    if (new_len /= 1) error stop
    print*, "Hello, World!", new_line('a')
    call temp(str)
contains 
   subroutine temp(str)
      character(len=*) :: str
      print *, len(str)
      if(len(str) /= 1) error stop
    end subroutine
end program
