program intrinsics_492
    ! spread of a named-constant array of rank > 1 must not be rejected
    ! as having an invalid `dim`
    implicit none
    integer, parameter :: c(3, 2) = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    integer, parameter :: d(2, 2, 2) = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
    integer :: r1(2, 3, 2), r3(3, 2, 2), s1(2, 2, 2, 2), s4(2, 2, 2, 2)

    r1 = spread(c, 1, 2)
    if (any(r1(1, :, :) /= c) .or. any(r1(2, :, :) /= c)) error stop
    r3 = spread(c, 3, 2)
    if (any(r3(:, :, 1) /= c) .or. any(r3(:, :, 2) /= c)) error stop
    s1 = spread(d, 1, 2)
    if (any(s1(1, :, :, :) /= d) .or. any(s1(2, :, :, :) /= d)) error stop
    s4 = spread(d, 4, 2)
    if (any(s4(:, :, :, 1) /= d) .or. any(s4(:, :, :, 2) /= d)) error stop
    if (sum(spread(d, 4, 3)) /= 108) error stop
    print *, "ok"
end program
