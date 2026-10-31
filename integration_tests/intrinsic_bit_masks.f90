program intrinsic_bit_masks
    implicit none
    integer :: n
    do n = 0, 32
        call check4(n)
    end do
    do n = 0, 64
        call check8(n)
    end do
contains
    subroutine check4(n)
        integer, intent(in) :: n
        integer(4) :: right, left
        if (n == 32) then
            right = -1_4
            left = -1_4
        else
            right = not(ishft(-1_4, n))
            left = not(ishft(-1_4, -n))
        end if
        if (maskr(n, 4) /= right) error stop "maskr4"
        if (maskl(n, 4) /= left) error stop "maskl4"
        if (ibits(-1_4, 0, n) /= right) error stop "ibits4 low"
        if (ibits(-1_4, 32-n, n) /= right) error stop "ibits4 high"
        if (n == 0) then
            if (dshiftr(1_4, 0_4, n) /= 0_4) error stop "dshiftr4 zero"
            if (dshiftr(0_4, -1_4, n) /= -1_4) error stop "dshiftr4 zero negative"
        else if (n == 32) then
            if (dshiftr(1_4, 0_4, n) /= 1_4) error stop "dshiftr4 full"
            if (dshiftr(-1_4, 0_4, n) /= -1_4) error stop "dshiftr4 full negative"
        else
            if (dshiftr(1_4, 0_4, n) /= ishft(1_4, 32-n)) error stop "dshiftr4 low"
            if (dshiftr(0_4, -1_4, n) /= ishft(-1_4, -n)) error stop "dshiftr4 high"
            if (dshiftr(-1_4, huge(0_4), n) /= &
                ior(ishft(-1_4, 32-n), ishft(huge(0_4), -n))) error stop "dshiftr4 both"
        end if
    end subroutine
    subroutine check8(n)
        integer, intent(in) :: n
        integer(8) :: right, left
        if (n == 64) then
            right = -1_8
            left = -1_8
        else
            right = not(ishft(-1_8, n))
            left = not(ishft(-1_8, -n))
        end if
        if (maskr(n, 8) /= right) error stop "maskr8"
        if (maskl(n, 8) /= left) error stop "maskl8"
        if (ibits(-1_8, 0, n) /= right) error stop "ibits8 low"
        if (ibits(-1_8, 64-n, n) /= right) error stop "ibits8 high"
        if (n == 0) then
            if (dshiftr(1_8, 0_8, n) /= 0_8) error stop "dshiftr8 zero"
            if (dshiftr(0_8, -1_8, n) /= -1_8) error stop "dshiftr8 zero negative"
        else if (n == 64) then
            if (dshiftr(1_8, 0_8, n) /= 1_8) error stop "dshiftr8 full"
            if (dshiftr(-1_8, 0_8, n) /= -1_8) error stop "dshiftr8 full negative"
        else
            if (dshiftr(1_8, 0_8, n) /= ishft(1_8, 64-n)) error stop "dshiftr8 low"
            if (dshiftr(0_8, -1_8, n) /= ishft(-1_8, -n)) error stop "dshiftr8 high"
            if (dshiftr(-1_8, huge(0_8), n) /= &
                ior(ishft(-1_8, 64-n), ishft(huge(0_8), -n))) error stop "dshiftr8 both"
        end if
    end subroutine
end program
