program string_35
    implicit none

    print *, index("pat", "patter")

    if (index("pat", "patter") /= 0) error stop

end program
