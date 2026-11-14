program real16_softfloat_trunc_01
    ! Narrowing a real(16) value to real(4) or real(8) rounds to nearest, ties
    ! to even, exactly once, also when the result is subnormal.
    implicit none
    real(16) :: p150, p1075, tiny_above
    integer :: i
    p150 = 1.0_16
    do i = 1, 150
        p150 = p150 * 0.5_16
    end do
    p1075 = p150
    do i = 151, 1075
        p1075 = p1075 * 0.5_16
    end do
    tiny_above = 1.0_16
    do i = 1, 100
        tiny_above = tiny_above * 0.5_16
    end do
    tiny_above = 1.0_16 + tiny_above

    ! 1.5, 2.5 and 3.5 units of the smallest subnormal: ties go to even
    call check_single(1.5_16 * 2 * p150, 2)
    call check_single(2.5_16 * 2 * p150, 2)
    call check_single(3.5_16 * 2 * p150, 4)
    ! just above a tie between 0 and the smallest subnormal: rounds up
    call check_single(p150 * tiny_above, 1)
    call check_single(p150, 0)
    call check_double(1.5_16 * 2 * p1075, 2_8)
    call check_double(2.5_16 * 2 * p1075, 2_8)
    call check_double(p1075 * tiny_above, 1_8)
    call check_double(p1075, 0_8)
    call check_double(transfer([-1_8, int(z'3bdaffffffffffff', 8)], 1.0_16), &
        int(z'4000', 8))
    call check_double(transfer([int(z'5df2e5532f2331a1', 8), &
        int(z'3bd57ebc58813f82', 8)], 1.0_16), int(z'17f', 8))
    call check_double(transfer([-210_8, int(z'3bfe1321ea89b702', 8)], 1.0_16), &
        int(z'22643d5136e06', 8))
contains
    subroutine check_single(q, expected)
        real(16), intent(in) :: q
        integer, intent(in) :: expected
        real(4) :: f
        f = q
        if (transfer(f, 0) /= expected) error stop "real(16) to real(4) rounding"
        f = -q
        if (transfer(f, 0) /= ior(expected, ishft(1, 31))) &
            error stop "negative real(16) to real(4) rounding"
    end subroutine

    subroutine check_double(q, expected)
        real(16), intent(in) :: q
        integer(8), intent(in) :: expected
        real(8) :: d
        d = q
        if (transfer(d, 0_8) /= expected) error stop "real(16) to real(8) rounding"
    end subroutine
end program
