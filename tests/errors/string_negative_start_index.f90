program expr2
    implicit none
    character(len=8) :: s
    s = "lfortran"
    print*, "s:", s(-1:4)
 end program