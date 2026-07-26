program real128_module_01
    use real128_module_01_mod, only: set_value
    implicit none
    ! Raw binary128 payload of 1.0: the high 64 bits are 0x3FFF000000000000
    ! and the low 64 bits are zero. On the little-endian targets LFortran
    ! supports, transfer() yields the low half first.
    integer(8), parameter :: one_low = 0_8
    integer(8), parameter :: one_high = 4611404543450677248_8
    real(16) :: xl
    integer(8) :: bits(2)
    xl = 0.0_16
    call set_value(xl)
    print *, xl
    ! Check the payload that came back from the module file bit for bit,
    ! instead of using a real(16) relational operator: those lower to calls
    ! to __eqtf2 / __gttf2, whose runtime implementations take their
    ! arguments in the wrong registers, so their result cannot be relied on
    ! here. That is a separate bug from the serialization one under test.
    bits = transfer(xl, bits)
    if (bits(1) /= one_low) error stop 1
    if (bits(2) /= one_high) error stop 2
end program real128_module_01
