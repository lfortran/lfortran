program intrinsics_87
    implicit none
    integer, allocatable :: C(:, :)
    complex(4) :: arr(3)
    allocate(C(5, 10))
    C = -10
    C = abs(C)
    print *, C
    if (any(C /= 10)) error stop

    ! test if 'abs' of complex array results in a real array
    arr = [(3, 4), (5, 6), (7, 8)]
    print *, sum(abs(arr))
    if (sum(abs(arr)) - 23.4403954 > 1e-7) error stop
end program
