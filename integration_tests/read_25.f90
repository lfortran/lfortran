program read_25
    implicit none
    character(8) :: string = 'abcd', newstring = 'ABCD'
    character(8):: newstring2 = 'ABCD'
    
    read(string, "(A)") newstring(5:8)
    read(string(1:4),"(A)") newstring2

    print "(A)", newstring
    print "(A)", newstring2
    if (newstring /= 'ABCDabcd') error stop
    if (newstring2 /= 'abcd') error stop
    
end program read_25