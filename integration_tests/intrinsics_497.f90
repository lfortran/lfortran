program intrinsics_497
    ! spread with ncopies given by size() of a parameter array (#15327)
    implicit none
    real, parameter :: a(1) = [0.]
    integer, parameter :: b(3) = [1, 2, 3]
    integer, parameter :: c(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    real :: x(1)
    integer :: y(3, 2), z(2, 3)

    print *, spread(1., 1, size(a))
    x = spread(1., 1, size(a))
    if (size(x) /= 1) error stop
    if (abs(x(1) - 1.) > 1e-6) error stop

    if (size(spread(5, 1, size(b))) /= 3) error stop
    if (any(spread(5, 1, size(b)) /= 5)) error stop

    y = spread([7, 8], 1, size(b))
    if (any(y(:, 1) /= 7)) error stop
    if (any(y(:, 2) /= 8)) error stop

    z = spread([7, 8], 2, size(c, 1) + 1)
    if (any(z(1, :) /= 7)) error stop
    if (any(z(2, :) /= 8)) error stop

    z = spread([7, 8], 2, size(c) - 1)
    if (any(z(1, :) /= 7)) error stop

    y = spread([4, 6], 1, size(b, 1))
    if (any(y(:, 1) /= 4)) error stop
    if (any(y(:, 2) /= 6)) error stop
end program
