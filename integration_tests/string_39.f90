program string_39
    implicit none
    character(len=8) :: s
    s = "lfortran"
    print*, "s:", s(1:-1)
    if (s(1:-1) /= "") error stop
 end program
