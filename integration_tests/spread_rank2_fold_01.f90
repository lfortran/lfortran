program spread_rank2_fold_01
    ! spread of rank-2 and rank-3 constant arrays along the first and the
    ! last dimension, folded in PARAMETER initializers (#17876)
    implicit none
    type :: pt
        integer :: a
        real :: b
    end type
    integer, parameter :: m(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    real(8), parameter :: r(2, 3) = &
        reshape([1.5d0, 5d0, -2d0, 7d0, 0d0, -9.25d0], [2, 3])
    integer, parameter :: t(2, 2, 2) = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
    logical, parameter :: l(2, 2) = reshape([.true., .false., .false., .true.], [2, 2])
    character(2), parameter :: c(2, 2) = reshape(["ab", "cd", "ef", "gh"], [2, 2])

    integer, parameter :: s1(4, 2, 3) = spread(m, 1, 4)
    integer, parameter :: s3(2, 3, 2) = spread(m, 3, 2)
    real(8), parameter :: v1(3, 2, 3) = spread(r, 1, 3)
    real(8), parameter :: v3(2, 3, 2) = spread(r, 3, 2)
    integer, parameter :: u1(3, 2, 2, 2) = spread(t, 1, 3)
    integer, parameter :: u4(2, 2, 2, 3) = spread(t, 4, 3)
    logical, parameter :: l3(2, 2, 2) = spread(l, 3, 2)
    character(2), parameter :: c1(3, 2, 2) = spread(c, 1, 3)

    ! folded spreads used in further constant expressions
    integer, parameter :: g1 = sum(s1) + s3(2, 3, 2) + u4(2, 1, 2, 3)
    integer, parameter :: g2 = int(sum(spread(r, 3, 2)))
    integer, parameter :: g3(4) = shape(spread(t, 4, 3))
    integer, parameter :: g4 = size(spread(m, 1, 0))

    ! spread of a rank-1 derived-type constant
    type(pt), parameter :: p(2) = [pt(1, 1.5), pt(2, -3.0)]
    type(pt), parameter :: dp(2, 3) = spread(p, 2, 3)
    type(pt) :: de(2, 3)
    integer :: z(g1)
    integer :: i, j

    do i = 1, 4
        if (any(s1(i, :, :) /= m)) error stop 1
    end do
    do i = 1, 2
        if (any(s3(:, :, i) /= m)) error stop 2
        if (any(v3(:, :, i) /= r)) error stop 3
        if (any(l3(:, :, i) .neqv. l)) error stop 4
    end do
    do i = 1, 3
        if (any(v1(i, :, :) /= r)) error stop 5
        if (any(u1(i, :, :, :) /= t)) error stop 6
        if (any(u4(:, :, :, i) /= t)) error stop 7
        if (any(c1(i, :, :) /= c)) error stop 8
    end do
    if (s1(3, 2, 3) /= 6 .or. s3(2, 1, 2) /= 2) error stop 9
    if (v1(2, 2, 3) /= -9.25d0 .or. v3(1, 1, 2) /= 1.5d0) error stop 10
    if (u1(3, 1, 2, 2) /= 7 .or. u4(2, 2, 1, 3) /= 4) error stop 11
    if (c1(2, 2, 1) /= "cd" .or. .not. l3(2, 2, 2)) error stop 12
    if (g1 /= 4*21 + 6 + 6) error stop 13
    if (g2 /= 4) error stop 14
    if (any(g3 /= [2, 2, 2, 3])) error stop 15
    if (g4 /= 0) error stop 16
    if (size(z) /= 96) error stop 17
    de = spread(p, 2, 3)
    do i = 1, 3
        do j = 1, 2
            if (dp(j, i)%a /= p(j)%a .or. dp(j, i)%b /= p(j)%b) error stop 18
            if (de(j, i)%a /= p(j)%a .or. de(j, i)%b /= p(j)%b) error stop 19
        end do
    end do
    print *, g1, g2, g3, g4
end program
