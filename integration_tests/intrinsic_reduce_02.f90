! reduce(array, operation, dim) used inside a larger expression, which needs
! a temporary for its array result
program intrinsic_reduce_02
    implicit none
    integer :: grid(2, 3)
    real :: cube(2, 3, 4)
    logical :: keep(2, 3)
    integer, allocatable :: res(:)
    integer :: i, n, total, big, shp(2)
    real :: rtotal
    grid = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    cube = reshape([(real(i), i = 1, 24)], [2, 3, 4])
    keep = grid /= 3

    n = size(reduce(grid, plus, 1))
    if (n /= 3) error stop 1
    total = sum(reduce(grid, plus, 2))
    if (total /= 21) error stop 2
    call check_cols(reduce(grid, plus, 1))
    big = maxval(reduce(grid, plus, 2))
    if (big /= 12) error stop 4
    total = sum(reduce(grid, plus, 1, mask=keep))
    if (total /= 18) error stop 5

    shp = shape(reduce(cube, rplus, 2))
    if (shp(1) /= 2 .or. shp(2) /= 4) error stop 6
    rtotal = sum(reduce(cube, rplus, 3))
    if (abs(rtotal - 300.0) > 1e-4) error stop 7

    allocate(res(2))
    res = reduce(grid, plus, 2)
    if (size(res) /= 2) error stop 8
    if (res(1) /= 9 .or. res(2) /= 12) error stop 9
    print *, "OK"
contains
    pure integer function plus(a, b)
        integer, intent(in) :: a, b
        plus = a + b
    end function

    subroutine check_cols(cols)
        integer, intent(in) :: cols(:)
        if (size(cols) /= 3) error stop 3
        if (cols(1) /= 3 .or. cols(2) /= 7 .or. cols(3) /= 11) error stop 3
    end subroutine

    pure real function rplus(a, b)
        real, intent(in) :: a, b
        rplus = a + b
    end function
end program
