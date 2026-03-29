program arrays_122
    implicit none
    real :: a(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    if (a(1, 1) /= 1) error stop
    if (a(2, 1) /= 2) error stop
    if (a(1, 2) /= 3) error stop
    if (a(2, 2) /= 4) error stop
    if (a(1, 3) /= 5) error stop
    if (a(2, 3) /= 6) error stop
    print *, a
end program arrays_122