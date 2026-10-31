program gpu_bit_shift_masks_8
    implicit none
    integer :: i, n(0:64)
    integer(8) :: low(0:64), out(0:64, 7), sign
    low(0) = 0
    do i = 1, 63
        low(i) = 2 * low(i-1) + 1
    end do
    low(64) = -1
    sign = not(huge(0_8))
    do i = 0, 64
        n(i) = i
    end do
    do concurrent (i = 0:64)
        out(i, 1) = maskr(n(i), 8)
        out(i, 2) = maskl(n(i), 8)
        out(i, 3) = ibits(-1_8, 64-n(i), n(i))
        out(i, 4) = shiftr(sign, min(n(i), 63))
        out(i, 5) = shiftl(-1_8, min(n(i), 63))
        out(i, 6) = shifta(-1_8, min(n(i), 63))
        out(i, 7) = dshiftr(0_8, -1_8, n(i))
    end do
    do i = 0, 64
        if (out(i, 1) /= low(i)) error stop 1
        if (out(i, 2) /= not(low(64-i))) error stop 2
        if (out(i, 3) /= low(i)) error stop 3
        if (i == 0) then
            if (out(i, 4) /= sign) error stop 4
        else
            if (out(i, 4) /= low(63-min(i,63))+1) error stop 5
        end if
        if (out(i, 5) /= not(low(min(i,63)))) error stop 6
        if (out(i, 6) /= -1_8) error stop 7
        if (out(i, 7) /= low(64-i)) error stop 8
    end do
end program
