program substring_read
    implicit none
    character(8) :: string = 'abcd', newstring = 'ABCD'

    read(string, '(A)') newstring(5:8)
    if (newstring /= 'ABCDabcd') error stop

    read(string(1:4), '(A)') newstring
    if (newstring /= 'abcd    ') error stop
    
end program substring_read
