module string_66_mod
    implicit none
    contains
    pure function str_int(i) result(s)
        integer, intent(in) :: i
        character(len=100) :: s
        write(s, '(i0)') i
    end function
    
    pure integer function str_real_len(r, fmt) result(sz)
        real, intent(in) :: r
        character(len=*), intent(in) :: fmt
        integer, parameter :: MAX_STR = 100
        character(MAX_STR) :: s
        write(s, fmt) r
        sz = len_trim(s)
    end function
 
    function str_real_n(r, n) result(s)
       real :: r
       integer :: n
        character(len=str_real_len(r, "(f0." // str_int(n) // ")")) :: s
       print *, len(s)
       if(len(s) /= 14) error stop
    end function   
end module
 
program string_66
use string_66_mod
character(:),allocatable :: strr
real :: r
integer :: i
r = 102.2
i = 10
strr = str_real_n(r,i)
end program