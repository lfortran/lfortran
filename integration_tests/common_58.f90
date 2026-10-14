program common_58
    use file_common_block_common_58_blk, only: k
    implicit none
    integer :: v
    common /common_58_blk/ v
    v = k
    print *, v
    if (v /= 3) error stop
end program common_58
