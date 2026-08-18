program common_block_linkage_02
    implicit none
    character(32) :: value
    call read_name(value)
    if (value /= 'HELLO') error stop
end program common_block_linkage_02
