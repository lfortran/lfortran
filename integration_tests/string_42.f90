program string_42

    implicit none
 
    character(len=2), allocatable :: line
    character(len=2) :: c
 
    allocate(character(len=2) :: line)
    line = "ab"
 
    read(line, *) c ! Test reading from allocatable string.
    
    print *, c
    if(c /= "ab") error stop
    
 end program string_42