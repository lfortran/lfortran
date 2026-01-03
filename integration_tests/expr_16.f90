program parconst
    implicit none
    integer a
    parameter (a = 8)
    integer (a) b

    b = 10_8
    print *, b
    if (b /= 10_8) error stop
end program parconst
