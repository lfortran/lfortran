! A BLOCK DATA unit initializes the blank COMMON block and a named block
! spelled in upper case. A program declaring both blocks with other names
! is associated with them from the start of each block.
block data common_48_data
    real :: a, b
    integer :: i, j
    common a, b
    COMMON /COMMON_48_BLK/ i, j
    data a, b / 1.0, 2.0 /
    data i, j / 3, 4 /
end block data common_48_data

program common_48
    implicit none
    real :: x, y
    integer :: m, n
    common x, y
    common /common_48_blk/ m, n
    print *, x, y, m, n
    if (abs(x - 1.0) > 1.0e-6) error stop
    if (abs(y - 2.0) > 1.0e-6) error stop
    if (m /= 3) error stop
    if (n /= 4) error stop
end program common_48
