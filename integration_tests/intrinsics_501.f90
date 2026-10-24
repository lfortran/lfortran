program intrinsics_501
    ! Compile-time folding of sum/product/maxval/minval with `dim` on a
    ! constant array of rank > 1 must reduce along `dim` (#15391).
    implicit none
    integer, parameter :: a(2,3) = reshape([1, -2, 3, 4, -5, 6], [2, 3])
    integer, parameter :: c(2,3,2) = reshape([1, 2, 3, 4, 5, 6, &
        7, 8, 9, 10, 11, 12], [2, 3, 2])
    real, parameter :: x(2,2) = reshape([1.5, 2.0, -3.0, 4.0], [2, 2])
    logical, parameter :: m(2,3) = reshape([.true., .false., .true., &
        .true., .false., .true.], [2, 3])
    integer, parameter :: s1(3) = sum(a, dim=1)
    integer, parameter :: s2(2) = sum(a, dim=2)
    integer, parameter :: p2(2) = product(a, dim=2)
    integer, parameter :: mx(2) = maxval(a, dim=2, mask=m)
    integer, parameter :: c2(2,2) = sum(c, dim=2)
    real, parameter :: px(2) = product(x, dim=1)
    integer :: r(2)
    real :: rx(2)

    r = sum(reshape([1, 2, 3, 4], [2, 2]), dim=2)
    print *, r
    if (any(r /= [4, 6])) error stop

    r = sum(a, dim=2)
    if (any(r /= [-1, 8])) error stop
    r = product(a, dim=2)
    if (any(r /= [-15, -48])) error stop
    r = maxval(a, dim=2)
    if (any(r /= [3, 6])) error stop
    r = minval(a, dim=2)
    if (any(r /= [-5, -2])) error stop
    r = sum(a, dim=2, mask=m)
    if (any(r /= [4, 10])) error stop
    if (any(maxval(a, dim=1) /= [1, 4, 6])) error stop
    if (any(minval(a, dim=1, mask=m) /= [1, 3, 6])) error stop

    rx = sum(x, dim=1)
    if (any(abs(rx - [3.5, 1.0]) > 1e-6)) error stop
    rx = maxval(x, dim=2)
    if (any(abs(rx - [1.5, 4.0]) > 1e-6)) error stop

    print *, s1, s2, p2, mx
    if (any(s1 /= [-1, 7, 1])) error stop
    if (any(s2 /= [-1, 8])) error stop
    if (any(p2 /= [-15, -48])) error stop
    if (any(mx /= [3, 6])) error stop
    if (any(c2 /= reshape([9, 12, 27, 30], [2, 2]))) error stop
    if (any(abs(px - [3.0, -12.0]) > 1e-6)) error stop
    if (any(shape(sum(c, dim=3)) /= [2, 3])) error stop
end program
