program string_34
    implicit none

    character(len=20) :: a
    a = "hi hello"

    print *, a(int(1, 8):int(2, 8))
    print *, a(1_8:2_8)

    if (a(1_8:2_8) /= "hi") error stop
    if (a(int(1, 8):int(2, 8)) /= "hi") error stop

end program
