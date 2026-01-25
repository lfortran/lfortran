program format_52
    implicit none
    integer :: i
    character(2) :: which(2) = ['st','nd']
    character(10) :: output
    
    print "(I0,A,I0,A)", 1, which(1), 2, which(2)
    
    write(output, "(I0,A,I0,A)") 1, which(1), 2, which(2)
    if (trim(output) /= "1st2nd") error stop "Format reuse failed"
    
    write(output, "(I0,A)") 1, which(1)
    if (trim(output) /= "1st") error stop "Single format failed"
    
    print "(I0,A)", (i, which(i), i = 1, 2)
end program format_52
