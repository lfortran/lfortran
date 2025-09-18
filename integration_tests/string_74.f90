
! This tests the speed of assigning substrings of a string to another string.
! It used to fill the memory and kills the process.
program string_74
    character(100000) :: str
    character(100000) :: str_2
    
    integer :: i
    str = "whatever"
    do i =1 , 100000
        str_2 = str_2(1:i)
    end do
    
end program string_74