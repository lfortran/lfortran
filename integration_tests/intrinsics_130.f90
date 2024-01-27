program intrinsics_130
    integer :: a, b
    a = 10
    b = 2
    print *, ishftc(a, b)
    if (ishftc(a, b) /= 40) error stop

    print*, ishftc(10, 2)
    if (ishftc(10, 2) /= 40) error stop

end program
