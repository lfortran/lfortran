program intrinsics_104
    implicit none
    
    integer, parameter :: new_len = len(new_line('a'))
    integer, parameter :: new_len2 = len(adjustl('a'))
    
    print *, new_len
    if (new_len /= 1) error stop
    
    print*, new_len2
    if (new_len2 /= 1) error stop
    
end program
