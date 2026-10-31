program gpu_bit_shift_masks
    implicit none
    integer :: i, counts(4), masks(4), n4(0:32)
    integer(4) :: low4(0:32), out4(0:32, 7), sign4

    counts = [1, 2, 3, 4]
    do concurrent (i = 1:4)
        masks(i) = maskr(counts(i), 4)
    end do
    if (any(masks /= [1, 3, 7, 15])) error stop 1

    low4(0) = 0
    do i = 1, 31
        low4(i) = 2 * low4(i-1) + 1
    end do
    low4(32) = -1
    sign4 = not(huge(0_4))
    do i = 0, 32
        n4(i) = i
    end do
    do concurrent (i = 0:32)
        out4(i, 1) = maskr(n4(i), 4)
        out4(i, 2) = maskl(n4(i), 4)
        out4(i, 3) = ibits(-1_4, 32-n4(i), n4(i))
        out4(i, 4) = shiftr(sign4, min(n4(i), 31))
        out4(i, 5) = shiftl(-1_4, min(n4(i), 31))
        out4(i, 6) = shifta(-1_4, min(n4(i), 31))
        out4(i, 7) = dshiftr(0_4, -1_4, n4(i))
    end do
    do i = 0, 32
        if (out4(i, 1) /= low4(i)) error stop 2
        if (out4(i, 2) /= not(low4(32-i))) error stop 3
        if (out4(i, 3) /= low4(i)) error stop 4
        if (i == 0) then
            if (out4(i, 4) /= sign4) error stop 5
        else
            if (out4(i, 4) /= low4(31-min(i,31))+1) error stop 6
        end if
        if (out4(i, 5) /= not(low4(min(i,31)))) error stop 7
        if (out4(i, 6) /= -1_4) error stop 8
        if (out4(i, 7) /= low4(32-i)) error stop 9
    end do
end program
