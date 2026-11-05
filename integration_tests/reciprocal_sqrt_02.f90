module reciprocal_sqrt_02_m
    ! Double precision reciprocal square roots, which --fast computes from
    ! the AVX-512 estimate and two Newton steps on x86, with the constant
    ! factor of the steps moved out of sums.
    implicit none
    integer, parameter :: dp = kind(1.d0)
contains
    ! A sum of products of reciprocal square roots (as in an N-body force).
    subroutine forces(n, x, m, ax)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n), m(n)
        real(dp), intent(out) :: ax(n)
        integer :: i, j
        real(dp) :: dx, inv_r, s, a
        do i = 1, n
            a = 0
            do j = 1, n
                dx = x(j) - x(i)
                inv_r = 1 / sqrt(dx*dx + 0.01_dp)
                s = m(j) * inv_r * inv_r * inv_r
                a = a + dx*s
            end do
            ax(i) = a
        end do
    end subroutine

    ! A variable and a constant numerator, a sum that does not start at
    ! zero and reciprocal square roots that are not summed.
    subroutine others(n, x, y, s1, s2, z)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n), y(n)
        real(dp), intent(out) :: s1, s2, z(n)
        integer :: i
        s1 = 0
        s2 = 1
        do i = 1, n
            s1 = s1 + y(i) / sqrt(x(i))
            s2 = s2 + 3 / sqrt(x(i))
            z(i) = 1 / sqrt(x(i))
        end do
    end subroutine

    ! A square root that is also used directly (left as it is).
    real(dp) function mixed(n, x) result(r)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n)
        integer :: i
        r = 0
        do i = 1, n
            r = r + sqrt(x(i)) + 1 / sqrt(x(i))
        end do
    end function
end module

program reciprocal_sqrt_02
    use reciprocal_sqrt_02_m
    implicit none
    integer, parameter :: n = 203
    real(dp) :: x(n), y(n), m(n), ax(n), s1, s2, z(n), r
    real(dp) :: e, a, dx, inv_r
    integer :: i, j
    do i = 1, n
        x(i) = 0.5_dp + mod(7*i, 31) * 0.25_dp
        y(i) = 1 + mod(3*i, 11) * 0.5_dp
        m(i) = 0.001_dp * (1 + mod(i, 5))
    end do

    call forces(n, x, m, ax)
    do i = 1, n
        a = 0
        do j = 1, n
            dx = x(j) - x(i)
            inv_r = 1 / sqrt(dx*dx + 0.01_dp)
            a = a + dx * m(j) * inv_r**3
        end do
        if (abs(ax(i) - a) > 1e-12_dp * (1 + abs(a))) error stop 1
    end do

    call others(n, x, y, s1, s2, z)
    e = sum(y / sqrt(x))
    if (abs(s1 - e) > 1e-12_dp * e) error stop 2
    e = 1 + sum(3 / sqrt(x))
    if (abs(s2 - e) > 1e-12_dp * e) error stop 3
    if (any(abs(z - 1 / sqrt(x)) > 1e-14_dp)) error stop 4

    r = mixed(n, x)
    e = sum(sqrt(x) + 1 / sqrt(x))
    if (abs(r - e) > 1e-12_dp * e) error stop 5
    print *, "ok"
end program
