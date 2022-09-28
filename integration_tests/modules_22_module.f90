module modules_22_module

    type :: bitset_type
        integer(8) :: num_bits
    end type

contains

    function bits(self)
       integer(8)             :: bits
       type(bitset_type), intent(in) :: self

       bits = self % num_bits

       return
    end function bits
end module