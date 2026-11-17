program spread_rank2_fold_02
    ! spread of rank-2 and rank-3 constant arrays along an interior
    ! dimension (1 < dim <= rank) in PARAMETER initializers (#17876).
    ! Not labelled gfortran: GFortran 15.3 mis-folds spread of a constant
    ! along an interior dimension (wrong shape and values), so the results
    ! are checked against explicit reshape literals instead.
    implicit none
    integer :: i_
    integer, parameter :: k = 2
    integer, parameter :: m(2,3) = reshape([1, 2, 3, 4, 5, 6], [2,3])
    integer, parameter :: t(2,2,2) = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2,2,2])
    real(8), parameter :: x(3,2) = reshape([1.5d0, 2.5d0, 3.5d0, &
        4.5d0, 5.5d0, 6.5d0], [3,2])

    integer, parameter :: s1(2,3,3) = spread(m, 2, 3)
    integer, parameter :: s2(2,2,2,2) = spread(t, 2, k)
    integer, parameter :: s3(2,2,3,2) = spread(t, 3, 3)
    real(8), parameter :: s4(3,2,2) = spread(x, 2, k)
    integer, parameter :: n1 = sum(spread(m, 2, 3) * &
        reshape([(i_, i_ = 1, 18)], [2,3,3]))
    integer, parameter :: sh(4) = shape(spread(t, 3, 3))

    if (any(s1 /= reshape([1, 2, 1, 2, 1, 2, 3, 4, 3, 4, 3, 4, &
        5, 6, 5, 6, 5, 6], [2,3,3]))) error stop 1
    if (any(s2 /= reshape([1, 2, 1, 2, 3, 4, 3, 4, &
        5, 6, 5, 6, 7, 8, 7, 8], [2,2,2,2]))) error stop 2
    if (any(s3 /= reshape([1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, &
        5, 6, 7, 8, 5, 6, 7, 8, 5, 6, 7, 8], [2,2,3,2]))) error stop 3
    if (any(s4 /= reshape([1.5d0, 2.5d0, 3.5d0, 1.5d0, 2.5d0, 3.5d0, &
        4.5d0, 5.5d0, 6.5d0, 4.5d0, 5.5d0, 6.5d0], [3,2,2]))) error stop 4
    if (n1 /= 747) error stop 5
    if (any(sh /= [2, 2, 3, 2])) error stop 6
    if (s1(2,3,1) /= 2 .or. s1(1,1,3) /= 5) error stop 7
    if (s3(2,1,3,2) /= 6 .or. s3(1,2,2,1) /= 3) error stop 8

    print *, "ok"
end program
