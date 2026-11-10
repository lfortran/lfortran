program intrinsics_510
    ! kind and len are inquiry functions with scalar results for array arguments (#3688)
    implicit none
    character(len=6) :: x(2)
    integer :: k, l

    k = kind([1, 2, 3])
    print *, kind([1, 2, 3])
    if (k /= kind(1)) error stop
    if (kind([1, 2, 3]) /= kind(0)) error stop
    if (kind([1.0d0, 2.0d0]) /= kind(1.0d0)) error stop

    l = len(x)
    print *, len(x)
    if (l /= 6) error stop
    if (len(x) /= 6) error stop
    if (len(['ab', 'cd', 'ef']) /= 2) error stop
end program
