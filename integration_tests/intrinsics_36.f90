program test_adjustl
    character(len=20) :: str = '   gfortran'
    str = adjustl(str)
    print *, str
end program test_adjustl