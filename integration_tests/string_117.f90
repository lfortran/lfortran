program string_117
    implicit none
    character(20) :: i(2)
    character(20) :: str

    str = "LFortran"
    i(1)  = str
    print *, i(1)

    str = "whatever"
    print *, str
    print *, i(1)
end program string_117
