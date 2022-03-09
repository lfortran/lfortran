program test_adjustl
    character(len=20) :: str = '   gfortran'
    integer :: one = 1
    str = adjustl(str)
    print *, str
    if (str(one:one) /= 'g') error stop
end program test_adjustl