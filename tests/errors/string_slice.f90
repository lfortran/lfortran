program string_slice
    implicit none
    character(len=8) :: s = "lfortran"
    print*, s(-2:6)
end program string_slice