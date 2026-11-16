program spread_rank2_fold_03
    ! spread of a rank-2 constant whose result is too large to be folded
    ! is evaluated at run time, so it compiles quickly (#17876); a result
    ! that is small enough is still folded in a PARAMETER
    implicit none
    integer :: i_
    integer, parameter :: m(64, 64) = reshape([(mod(i_, 7), i_ = 1, 4096)], [64, 64])
    integer, parameter :: q(16, 16) = reshape([(mod(i_, 5), i_ = 1, 256)], [16, 16])
    integer, parameter :: p(16, 16, 16) = spread(q, 3, 16)
    integer, parameter :: ps = sum(p)
    integer, allocatable :: x(:, :, :)
    integer :: n, k

    n = 8
    allocate(x(64, 64, n))
    x = spread(m, 3, 8)
    if (sum(x) /= 98288) error stop 1
    do k = 1, n
        if (any(x(:, :, k) /= m)) error stop 2
    end do
    deallocate(x)

    allocate(x(8, 64, 64))
    x = spread(m, 1, 8)
    if (sum(x) /= 98288) error stop 3
    if (x(5, 10, 20) /= 1 .or. x(8, 64, 64) /= 1) error stop 4

    if (sum(spread(m, 3, 8)) /= 98288) error stop 5
    if (any(shape(spread(m, 2, 8)) /= [64, 8, 64])) error stop 6
    if (ps /= 8176) error stop 7
    if (any(p(:, :, 16) /= q)) error stop 8
    print *, sum(x), ps
end program
