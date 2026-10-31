module integer_arithmetic_nsw_m
    implicit none
contains
    subroutine arithmetic(a, b, s, d, p)
        integer, intent(in) :: a, b
        integer, intent(out) :: s, d, p
        s = a + b
        d = a - b
        p = a * b
    end subroutine

    integer function offset(a, i, c) result(r)
        integer, intent(in) :: a(-3:), i, c
        r = a(i + c)
    end function

    subroutine section(a, lo, hi, step, b)
        integer, intent(in) :: a(-3:), lo, hi, step
        integer, intent(out) :: b(:)
        b = a(lo:hi:step)
    end subroutine
end module

program integer_arithmetic_nsw
    use integer_arithmetic_nsw_m
    implicit none
    integer :: s, d, p, i, a(-3:4), b(4)
    call arithmetic(-7, 3, s, d, p)
    if (s /= -4 .or. d /= -10 .or. p /= -21) error stop
    call arithmetic(7, -3, s, d, p)
    if (s /= 4 .or. d /= 10 .or. p /= -21) error stop
    call arithmetic(-7, -3, s, d, p)
    if (s /= -10 .or. d /= -4 .or. p /= 21) error stop
    do i = -3, 4
        a(i) = 10 * i
    end do
    if (offset(a, -3, 2) /= -10) error stop
    if (offset(a, 4, -2) /= 20) error stop
    call section(a, -3, 3, 2, b)
    if (any(b /= [-30, -10, 10, 30])) error stop
    call section(a, 4, -2, -2, b)
    if (any(b /= [40, 20, 0, -20])) error stop
end program
