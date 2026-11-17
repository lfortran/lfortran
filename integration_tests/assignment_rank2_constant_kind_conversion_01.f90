program assignment_rank2_constant_kind_conversion_01
    ! An elemental intrinsic of a rank-2 or rank-3 named constant folds to a
    ! constant of the same shape; converting it to the kind or type of the
    ! assigned variable keeps that shape.
    implicit none
    integer :: i_
    real(8), parameter :: a(3,4) = reshape([1, -2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12], [3,4])
    integer, parameter :: ia(2,3,2) = reshape([(i_, i_ = 1, 12)], [2,3,2])
    real(4) :: x(3,4), z(2,3,2)
    real(8) :: y(2,3,2)
    integer :: k(3,4)
    integer(8) :: k8(3,4)
    x = real(a, 8)
    if (any(x /= real(a, 4))) error stop 1
    if (x(3,4) /= -12 .or. x(1,2) /= -4) error stop 2
    x = abs(a)
    if (any(x /= real(abs(a), 4))) error stop 3
    if (x(2,4) /= 11) error stop 4
    x = int(a, 8)
    if (x(3,3) /= 9 .or. x(2,1) /= -2) error stop 5
    k = real(a, 4)
    if (any(k /= nint(a))) error stop 6
    k8 = sign(a, 1.0_8)
    if (k8(3,4) /= 12 .or. k8(1,1) /= 1) error stop 7
    y = int(ia, 8)
    if (any(y /= real(ia, 8))) error stop 8
    z = real(ia, 8)
    if (z(2,3,2) /= 12 .or. z(1,2,1) /= 3) error stop 9
    print *, 'ok'
end program
