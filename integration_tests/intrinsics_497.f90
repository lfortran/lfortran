program intrinsics_497
    ! `size` of a parameter array as the `ncopies` argument of `spread`
    ! is a compile-time constant and must keep its value (#15327)
    implicit none
    real, parameter :: a(1) = [0.]
    integer, parameter :: e(2) = [0, 0]
    real :: r(1), rr(2, 2)
    integer :: s(2, 2)

    r = spread(1., 1, size(a))
    if (size(r) /= 1) error stop
    if (r(1) /= 1.0) error stop

    rr = spread([1.0, 2.0], 2, size(e))
    if (any(shape(rr) /= [2, 2])) error stop
    if (any(rr(:, 1) /= [1.0, 2.0])) error stop
    if (any(rr(:, 2) /= [1.0, 2.0])) error stop

    s = spread([3, 4], 1, size(e))
    if (any(shape(s) /= [2, 2])) error stop
    if (any(s(1, :) /= [3, 4])) error stop
    if (any(s(2, :) /= [3, 4])) error stop

    print *, spread(1., 1, size(a))
end program
