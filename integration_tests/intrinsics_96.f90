program intrinsics_96
    character c
    integer :: a1 = 35
    integer :: a2(3) = [39, 63, 66]

    print*, char(65)
    if (char(65) /= 'A') error stop

    ! print*, char([34, 39, 67]) ! Doesn't work yet

    print*, char(a1)
    if (char(a1) /= '#') error stop

    print*, char(a2)
    if (char(a2(1)) /= "'") error stop
    if (char(a2(2)) /= '?') error stop
    if (char(a2(3)) /= 'B') error stop

    c = char(32)
    print*, c
    if (c /= ' ') error stop

end program