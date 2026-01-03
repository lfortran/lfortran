program substring_noninteger_stride
    implicit none
    character(len=8) :: s = "lfortran"
    print*, s(1:5:2.2)
end program substring_noninteger_stride
