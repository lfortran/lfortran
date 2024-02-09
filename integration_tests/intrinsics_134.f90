program intrinsics_134
    character(len=20) :: str = '   gfortran'
    str = adjustl(str)
    print *, str
    if (str /= 'gfortran') error stop
end program
