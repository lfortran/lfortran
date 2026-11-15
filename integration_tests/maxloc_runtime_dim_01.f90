module maxloc_runtime_dim_01_m
    implicit none
contains
    function row_max(a, d) result(r)
        integer, intent(in) :: a(:, :), d
        integer :: r(size(a, 3 - d))
        r = maxloc(a, dim=d)
    end function

    subroutine find7(a, d, r)
        integer, intent(in) :: a(:, :), d
        integer, intent(out) :: r(:)
        r = findloc(a, 7, dim=d)
    end subroutine
end module

program maxloc_runtime_dim_01
    ! maxloc, minloc and findloc with a dim that is not a constant
    use maxloc_runtime_dim_01_m
    implicit none
    integer :: grid(2, 3), a(2, 3, 2), v(5), i, k, one
    integer(8) :: k8
    integer :: pos(2), r22(2, 2), r23(2, 3)
    integer, allocatable :: p(:), z(:, :)
    logical :: m(2, 3)
    real :: x(2, 3)

    grid = reshape([4, 9, 7, 1, 2, 8], [2, 3])
    k = 2
    one = 1
    pos = maxloc(grid, dim=k)
    if (any(pos /= [2, 1])) error stop 1
    pos = minloc(grid, dim=k)
    if (any(pos /= [3, 2])) error stop 2
    pos = findloc(grid, 7, dim=k)
    if (any(pos /= [2, 0])) error stop 3
    if (any(maxloc(grid, dim=one) /= [2, 1, 2])) error stop 4
    if (any(findloc(grid, 1, dim=one) /= [0, 2, 0])) error stop 5

    k8 = 2
    pos = maxloc(grid, dim=k8)
    if (any(pos /= [2, 1])) error stop 6
    x = real(grid)
    pos = minloc(x, dim=k)
    if (any(pos /= [3, 2])) error stop 7
    m = grid /= 7
    pos = maxloc(grid, dim=k, mask=m)
    if (any(pos /= [1, 1])) error stop 8
    pos = findloc(grid, 7, dim=k, mask=m)
    if (any(pos /= [0, 0])) error stop 9
    pos = minloc(grid, dim=k, back=.true.)
    if (any(pos /= [3, 2])) error stop 10

    a = reshape([(mod(i * 7, 11), i = 1, 12)], [2, 3, 2])
    r22 = maxloc(a, dim=k)
    if (any(r22 /= reshape([2, 3, 2, 3], [2, 2]))) error stop 11
    r23 = minloc(a, dim=k + 1)
    if (any(r23 /= reshape([2, 2, 2, 2, 2, 2], [2, 3]))) error stop 12
    r23 = findloc(a, 7, dim=k + 1)
    if (any(r23 /= reshape([1, 0, 0, 0, 0, 2], [2, 3]))) error stop 13

    v = [1, 7, 3, 7, 5]
    if (maxloc(v, dim=one) /= 2) error stop 14
    if (findloc(v, 7, dim=one, back=.true.) /= 4) error stop 15

    allocate(p(2))
    p = maxloc(grid, dim=k)
    if (size(p) /= 2) error stop 16
    if (any(p /= [2, 1])) error stop 17
    if (size(minloc(a, dim=one)) /= 6) error stop 18
    if (any(row_max(grid, k) /= [2, 1])) error stop 19
    if (any(row_max(grid, one) /= [2, 1, 2])) error stop 20
    call find7(grid, k, pos)
    if (any(pos /= [2, 0])) error stop 21

    ! the reduced dimension has no elements: the position is 0
    i = 0
    allocate(z(i, 3))
    if (any(maxloc(z, dim=one) /= 0)) error stop 22
    if (any(minloc(z, dim=one) /= 0)) error stop 23
    if (any(maxloc(z, dim=1) /= 0)) error stop 24
    if (any(minloc(z, dim=1, back=.true.) /= 0)) error stop 25
    if (size(maxloc(z, dim=k)) /= 0) error stop 26
    print *, "ok"
end program
