block data bdata_1
    implicit none
    integer :: x, y
    common /blk1/ x, y
    data x, y/42, 43/
end

program block_data_01
    implicit none

    common /blk1/ x, y
    integer :: x, y

    if (x /= 42) error stop
    if (y /= 43) error stop
end program block_data_01
