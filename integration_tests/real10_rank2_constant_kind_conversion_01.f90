program real10_rank2_constant_kind_conversion_01
    ! An elemental intrinsic of a rank-2 real(10) named constant folds to a
    ! real(10) constant of the same shape; converting it to the kind of the
    ! assigned variable keeps that shape and rounds each element once.
    implicit none
    real(10), parameter :: e = epsilon(1.0_10)
    real(10), parameter :: a(3,4) = reshape([1, -2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12], [3,4])
    real(10), parameter :: b(2,2) = reshape([1 + e, 2 - e, -3 + 2*e, 4.0_10], [2,2])
    real(4) :: x(3,4)
    real(8) :: y(3,4), w(2,2)
    real(16) :: q(2,2)
    integer :: k(3,4)
    x = abs(a)
    if (x(3,4) /= 12 .or. x(2,1) /= 2) error stop 1
    y = real(a, 16)
    if (any(y /= real(a, 8))) error stop 2
    if (y(1,2) /= -4) error stop 3
    k = real(a, 8)
    if (k(3,3) /= 9 .or. k(2,4) /= 11) error stop 4
    x = a
    if (x(3,4) /= -12) error stop 5
    ! real(10) -> real(16) is exact; the elements differ from integers by
    ! multiples of epsilon(1.0_10)
    q = abs(b)
    if (q(1,1) - 1 /= real(e, 16) .or. q(2,1) - 2 /= -real(e, 16)) error stop 6
    if (q(1,2) - 3 /= -2*real(e, 16) .or. q(2,2) /= 4) error stop 7
    w = abs(b)
    if (w(1,1) /= 1 .or. w(2,1) /= 2 .or. w(2,2) /= 4) error stop 8
    print *, 'ok'
end program
