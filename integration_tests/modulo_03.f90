program modulo_03
    implicit none
    integer(8), parameter :: m6 = int(z'3FECCCCCCCCCCCCE', 8)
    integer(8), parameter :: mo6 = int(z'3FE9999999999998', 8)
    integer(4), parameter :: m6s = int(z'3F666664', 4)
    integer(4), parameter :: mo6s = int(z'3F4CCCD0', 4)
    integer(8) :: e(12, 3)
    integer :: i, k
    real(8) :: x(12), p
    data e(:, 1) / z'3FD3333333333334', z'3FE3333333333334', z'3FECCCCCCCCCCCCE', &
        z'3FF3333333333334', z'3FF8000000000001', z'3FB99999999999B0', &
        z'3FD99999999999A0', z'3FE666666666666A', z'3FF0000000000002', &
        z'3FF4CCCCCCCCCCCF', z'3FF999999999999C', z'3FC99999999999B0' /
    data e(:, 2) / z'3FF4CCCCCCCCCCCD', z'3FECCCCCCCCCCCCE', z'3FE0000000000002', &
        z'3FB99999999999B0', z'3FF6666666666668', z'3FF0000000000002', &
        z'3FE3333333333338', z'3FC99999999999B0', z'3FF8000000000003', &
        z'3FF199999999999D', z'3FE666666666666E', z'3FD3333333333344' /
    data e(:, 3) / z'3FE3333333333334', z'3FF3333333333334', z'3FB99999999999B0', &
        z'3FE666666666666A', z'3FF4CCCCCCCCCCCF', z'3FC99999999999B0', &
        z'3FE99999999999A0', z'3FF666666666666A', z'3FD3333333333344', &
        z'3FECCCCCCCCCCCD6', z'3FF8000000000005', z'3FD99999999999B0' /

    ! run-time evaluation, compared bit for bit with GFortran
    p = 1.7d0
    do k = 2, 4
        do i = 1, 12
            x(i) = mod(real(i*k, 8), p)
        end do
        do i = 1, 12
            if (transfer(x(i), 1_8) /= e(i, k - 1)) error stop
        end do
    end do

    call check_r8(6.0d0, 1.7d0)
    call check_r4(6.0, 1.7)

    ! compile-time evaluation
    if (transfer(mod(6.0d0, 1.7d0), 1_8) /= m6) error stop
    if (transfer(modulo(-6.0d0, 1.7d0), 1_8) /= mo6) error stop
    if (transfer(mod(6.0, 1.7), 1_4) /= m6s) error stop
    if (mod(1d300, 3d0) /= 0d0) error stop

contains

    subroutine check_r8(a, b)
        real(8), intent(in) :: a, b
        real(8) :: r, big, three
        r = mod(a, b)
        if (transfer(r, 1_8) /= m6) error stop
        r = mod(-a, b)
        if (transfer(-r, 1_8) /= m6) error stop
        r = mod(a, -b)
        if (transfer(r, 1_8) /= m6) error stop
        r = mod(-a, -b)
        if (transfer(-r, 1_8) /= m6) error stop
        ! large quotients must not overflow
        big = 1d300
        three = 3d0
        r = mod(big, three)
        if (r /= 0d0) error stop
        r = mod(big, 1.7d0)
        if (transfer(r, 1_8) /= transfer(mod(1d300, 1.7d0), 1_8)) error stop
        if (r < 0d0 .or. r >= 1.7d0) error stop
        ! modulo with mixed signs
        r = modulo(a, b)
        if (transfer(r, 1_8) /= m6) error stop
        r = modulo(-a, b)
        if (transfer(r, 1_8) /= mo6) error stop
        r = modulo(a, -b)
        if (transfer(-r, 1_8) /= mo6) error stop
        r = modulo(-a, -b)
        if (transfer(-r, 1_8) /= m6) error stop
        r = modulo(big, three)
        if (r /= 0d0) error stop
    end subroutine

    subroutine check_r4(a, b)
        real(4), intent(in) :: a, b
        real(4) :: r
        r = mod(a, b)
        if (transfer(r, 1_4) /= m6s) error stop
        r = mod(-a, b)
        if (transfer(-r, 1_4) /= m6s) error stop
        r = modulo(-a, b)
        if (transfer(r, 1_4) /= mo6s) error stop
        r = modulo(a, -b)
        if (transfer(-r, 1_4) /= mo6s) error stop
        r = mod(1e30, 3.0 * b / b)
        if (r < 0.0 .or. r >= 3.0) error stop
    end subroutine

end program
