program intrinsic_bit_shift_consumers
    implicit none
    integer :: n
    integer(4) :: low4(0:32), sign4, bit4
    integer(8) :: low8(0:64), sign8, bit8
    low4(0) = 0
    low8(0) = 0
    do n = 1, 31
        low4(n) = 2 * low4(n-1) + 1
    end do
    do n = 1, 63
        low8(n) = 2 * low8(n-1) + 1
    end do
    low4(32) = -1
    low8(64) = -1
    sign4 = not(huge(0_4))
    sign8 = not(huge(0_8))
    do n = 0, 32
        bit4 = sign4
        if (n > 1) bit4 = low4(32-n) + 1
        call check4(n, low4(n), not(low4(32-n)), sign4, bit4)
    end do
    do n = 0, 64
        bit8 = sign8
        if (n > 1) bit8 = low8(64-n) + 1
        call check8(n, low8(n), not(low8(64-n)), sign8, bit8)
    end do
contains
    subroutine check4(n, low, high, sign, bit)
        integer, intent(in) :: n
        integer(4), intent(in) :: low, high, sign, bit
        if (maskr(n) /= low) error stop
        if (maskr(n, 4) /= low) error stop
        if (maskl(n, 4) /= high) error stop
        if (ibits(-1_4, 0, n) /= low) error stop
        if (ibits(-1_4, 32-n, n) /= low) error stop
        if (n > 0) then
            if (ibits(sign, n-1, 33-n) /= bit) error stop
            if (shiftr(sign, n-1) /= bit) error stop
            if (shiftl(-1_4, 32-n) /= high) error stop
            if (shifta(-1_4, n-1) /= -1_4) error stop
        end if
        if (n > 0 .and. n < 32) then
            if (dshiftr(-1_4, -1_4, n) /= -1_4) error stop
            if (dshiftr(0_4, -1_4, n) /= not(high)) error stop
        else if (n == 0) then
            if (dshiftr(0_4, sign, n) /= sign) error stop
        else
            if (dshiftr(sign, 0_4, n) /= sign) error stop
        end if
    end subroutine
    subroutine check8(n, low, high, sign, bit)
        integer, intent(in) :: n
        integer(8), intent(in) :: low, high, sign, bit
        if (maskr(n, 8) /= low) error stop
        if (maskl(n, 8) /= high) error stop
        if (ibits(-1_8, 0, n) /= low) error stop
        if (ibits(-1_8, 64-n, n) /= low) error stop
        if (n > 0) then
            if (ibits(sign, n-1, 65-n) /= bit) error stop
            if (shiftr(sign, n-1) /= bit) error stop
            if (shiftl(-1_8, 64-n) /= high) error stop
            if (shifta(-1_8, n-1) /= -1_8) error stop
        end if
        if (n > 0 .and. n < 64) then
            if (dshiftr(-1_8, -1_8, n) /= -1_8) error stop
            if (dshiftr(0_8, -1_8, n) /= not(high)) error stop
        else if (n == 0) then
            if (dshiftr(0_8, sign, n) /= sign) error stop
        else
            if (dshiftr(sign, 0_8, n) /= sign) error stop
        end if
    end subroutine
end program
