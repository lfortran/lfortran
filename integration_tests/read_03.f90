module read_03_module
    public :: bitset_type, bitset_large

    type, abstract :: bitset_type
        integer(4) :: num_bits
    end type bitset_type

    type, extends(bitset_type) :: bitset_large
        integer(8), allocatable :: blocks(:)
    end type bitset_large

end module read_03_module

program read_03
    use read_03_module
    implicit none

    type(bitset_large) :: bs
    integer :: unit

    bs%num_bits = 64
    allocate(bs%blocks(2))

    open(newunit=unit, file="read_03_data.txt")
    read(unit, *) bs%blocks
    if (bs%blocks(1) /= 123456789) error stop
    if (bs%blocks(2) /= 987654321) error stop
    close(unit)

    print *, "Read text data:", bs%blocks
end program read_03