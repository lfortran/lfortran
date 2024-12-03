program test
    character(len=20) :: str
    character(len=:), allocatable :: sformat
    real :: x = 3.14159
    sformat = "(F6.2)"
    write(str, sformat) x
    print *, str
    if (trim(str) /= "  3.14") error stop
end program