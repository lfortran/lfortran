program real16_softfloat_extend_01
    ! Widening a subnormal real(4) or real(8) value to real(16) is exact.
    implicit none
    real(16) :: p149, p1074
    integer :: i
    p149 = 1.0_16
    do i = 1, 149
        p149 = p149 * 0.5_16
    end do
    p1074 = p149
    do i = 150, 1074
        p1074 = p1074 * 0.5_16
    end do
    call check_single(1, p149)
    call check_single(666, p149)
    call check_single(8388607, p149)
    call check_double(1_8, p1074)
    call check_double(12345_8, p1074)
    call check_double(4503599627370495_8, p1074)
contains
    subroutine check_single(bits, unit)
        integer, intent(in) :: bits
        real(16), intent(in) :: unit
        real(4) :: f
        real(16) :: q
        f = transfer(bits, f)
        q = f
        if (q /= real(bits, 16) * unit) error stop "real(4) subnormal widening"
        q = -f
        if (q /= -real(bits, 16) * unit) error stop "negative real(4) subnormal"
    end subroutine

    subroutine check_double(bits, unit)
        integer(8), intent(in) :: bits
        real(16), intent(in) :: unit
        real(8) :: d
        real(16) :: q
        d = transfer(bits, d)
        q = d
        if (q /= real(bits, 16) * unit) error stop "real(8) subnormal widening"
        q = -d
        if (q /= -real(bits, 16) * unit) error stop "negative real(8) subnormal"
    end subroutine
end program
