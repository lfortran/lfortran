program intrinsics_162
    integer :: a1 = 1
    integer :: a2 = -5
    integer :: a3 = 0

    print*, not(a1)
    if (not(a1) /= -2) error stop
    print*, not(a2)
    if (not(a2) /= 4) error stop
    print*, not(a3)
    if (not(a3) /= -1) error stop

    print*, not(1)
    if (not(1) /= -2) error stop
    print*, not(-5)
    if (not(-5) /= 4) error stop
    print*, not(0)
    if (not(0) /= -1) error stop

end
