program real16_softfloat_mul_01
    ! The product of two real(16) values must be rounded once, also when the
    ! result is subnormal.
    implicit none
    call check(int(z'00153119352f933c', 8), 0_8, &
        int(z'3fea467ff71e83ba', 8), int(z'f8e0ee200aa097d5', 8), &
        int(z'0000c28f4ddfd42e', 8), int(z'28454fa8768f1b0f', 8), .true.)
    call check(int(z'002227b30af6ea3a', 8), int(z'121e4b293a39c5f1', 8), &
        int(z'3fdcc6910ceac5be', 8), int(z'c0b51703a3ce85f5', 8), &
        int(z'00008343c431aaf4', 8), int(z'97271a11db19f677', 8), .false.)
    call check(int(z'002bffffffffffff', 8), int(z'ffffffffffffffff', 8), &
        int(z'3fd1612dd0c1aefb', 8), int(z'4a042189874e5715', 8), &
        int(z'00002c25ba1835df', 8), int(z'6940843130e9cae3', 8), .true.)
contains
    ! Checks a * b == r for the magnitudes given by the bit patterns of
    ! their high and low 64-bit words; `negative` negates the first operand.
    subroutine check(ahi, alo, bhi, blo, rhi, rlo, negative)
        integer(8), intent(in) :: ahi, alo, bhi, blo, rhi, rlo
        logical, intent(in) :: negative
        real(16) :: a, b, r
        integer(8) :: bits(2)
        a = transfer([alo, ahi], a)
        b = transfer([blo, bhi], b)
        if (negative) a = -a
        r = a * b
        if (negative) r = -r
        bits = transfer(r, bits)
        if (bits(1) /= rlo .or. bits(2) /= rhi) error stop "real(16) subnormal product"
    end subroutine
end program
