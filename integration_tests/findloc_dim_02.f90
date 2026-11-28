program findloc_dim_02
    ! findloc with dim on an allocatable array, where the result needs a temporary
    implicit none
    integer, allocatable :: m(:, :)
    integer :: n

    allocate(m(2, 3))
    m = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    print *, findloc(m, 3, dim=1)
    if (any(findloc(m, 3, dim=1) /= [0, 1, 0])) error stop 1
    if (any(findloc(m, 6, dim=2) /= [0, 3])) error stop 2
    if (size(findloc(m, 3, dim=1)) /= 3) error stop 3
    if (ubound(findloc(m, 3, dim=2), 1) /= 2) error stop 4
    n = sum(findloc(m, 4, dim=1)) + sum(findloc(m, 1))
    if (n /= 4) error stop 5
    print *, findloc(m, 5)
end program
