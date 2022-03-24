program string_13
    implicit none

    integer, parameter :: ia0   = iachar('0')
    integer, parameter :: ia5   = iachar('5')
    integer, parameter :: ia9   = iachar('9')

    if (ia0 /= 48) error stop
    if (ia5 /= 53) error stop
    if (ia9 /= 57) error stop
    
    print *, ia0, ia5, ia9

end program