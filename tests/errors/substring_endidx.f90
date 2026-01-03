program substring_noninteger_endidx
    implicit none
    character(len=8) :: s = "lfortran"
    print*, s(1:5.2)
end program substring_noninteger_endidx
