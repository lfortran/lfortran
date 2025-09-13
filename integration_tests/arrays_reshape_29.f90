program arrays_reshape_29
    implicit none
    integer, parameter :: w(4) = [1, 2, 3, 4]
    real, parameter :: x(4) = w
    integer, parameter :: y(4) = x
    real :: b(2, 2) = reshape(y, [2, 2])

    if (b(1,1) /= 1.0) error stop "Mismatch at b(1,1)"
    if (b(2,1) /= 2.0)  error stop "Mismatch at b(2,1)"
    if (b(1,2) /= 3.0) error stop "Mismatch at b(1,2)"
    if (b(2,2) /= 4.0)  error stop "Mismatch at b(2,2)"
end program arrays_reshape_29
