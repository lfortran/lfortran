module derived_types_53_m
    implicit none
    public ::         &
        bitset_type,  &
        bitset_64
    
    type, abstract :: bitset_type
        integer(4) :: num_bits
    end type bitset_type

    type, extends(bitset_type) :: bitset_64
    end type bitset_64

    type, extends(bitset_64) :: bitset_128
    contains
        procedure, pass(self)  :: write_bitset_string => write_bitset_string_128
    end type bitset_128

contains

    elemental function bits(self)
       integer(4)             :: bits
       class(bitset_type), intent(in) :: self
       bits = self%num_bits
       return
    end function bits

    module subroutine write_bitset_string_128(self)
        class(bitset_128), intent(in)               :: self
        integer(4) :: bit_count
        bit_count = bits(self)
        if (bit_count /= 4) error stop
    end subroutine write_bitset_string_128

end module derived_types_53_m

program derived_types_53
    use derived_types_53_m
    implicit none
    type(bitset_128) :: t
    t = bitset_128(num_bits=4)
    call t%write_bitset_string()
end program derived_types_53
