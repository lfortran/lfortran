module read_03_module
    public ::         &
        bitset_type,  &
        bitset_large

    type, abstract :: bitset_type
        integer(4) :: num_bits
    end type bitset_type

    type, extends(bitset_type) :: bitset_large
        integer(8), allocatable :: blocks(:)
    contains
        procedure, pass(self)  :: input => input_large
    end type bitset_large

contains

    module subroutine input_large(self, unit)
        class(bitset_large), intent(out) :: self
        integer, intent(in) :: unit
        read(unit) self % blocks(:)
    end subroutine input_large

end module read_03_module

program read_03
    use read_03_module
    implicit none

    type(bitset_large) :: bs
    integer :: unit

    bs%num_bits = 64
    allocate(bs%blocks(2))

    print *, bs%blocks
end program read_03
