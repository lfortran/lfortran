! A character(kind=c_char) array dummy of a bind(c) procedure
module print_arr_14_m
use iso_c_binding, only: c_char
implicit none
contains
subroutine show(s) bind(c)
    character(kind=c_char), intent(in) :: s(3)
    character(len=10) :: buf
    print *, s
    write(buf, '(3A1)') s
    if (buf /= 'abc') error stop 1
end subroutine
end module

program print_arr_14
use print_arr_14_m
use iso_c_binding, only: c_char
implicit none
character(kind=c_char) :: s(3) = ['a', 'b', 'c']
call show(s)
end program
