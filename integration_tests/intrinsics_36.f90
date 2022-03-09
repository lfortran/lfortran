program test_adjustl
    character(len=20) :: str = '   gfortran'
    str = adjustl(str)
    print *, str
    if (len(str) /= 8) error stop
end program test_adjustl