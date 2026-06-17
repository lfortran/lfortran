program string_118
    implicit none
    character(20) :: i(2)
    character(20) :: str

    str = "LFortran"
    i(1)  = str
    print *, i(1)
    if (trim(i(1)) /= "LFortran") error stop 1

    str = "whatever"
    print *, str
    if (trim(str) /= "whatever") error stop 2

    print *, i(1)
    if (trim(i(1)) /= "LFortran") error stop 3
end program string_118