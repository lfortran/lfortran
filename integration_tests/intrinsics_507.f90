program intrinsics_507
    ! cshift and eoshift of an allocatable array into an allocated array
    ! with lower bound 0 (#16912)
    implicit none
    integer, allocatable :: x(:), y(:)

    allocate(x(3), y(0:2))
    x = [2, 4, 6]

    y = cshift(x, 1)
    if (lbound(y, 1) /= 0) error stop 1
    if (any(y /= [4, 6, 2])) error stop 2

    y = eoshift(x, 1)
    if (lbound(y, 1) /= 0) error stop 3
    if (any(y /= [4, 6, 0])) error stop 4
end program
