program intrinsics_69
    implicit none
    integer :: x = 1
    real :: y(2) = [2., 4.]
    if (radix(3) /= 2) error stop
    if (radix(x) /= 2) error stop
    if (radix(y) /= 2) error stop
end program intrinsics_69
