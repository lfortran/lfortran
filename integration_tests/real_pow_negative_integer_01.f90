! x**(-n) with a variable integer n where x**n overflows but x**(-n) is a
! finite subnormal or normal number.
program real_pow_negative_integer_01
    implicit none
    integer :: i
    real(8) :: x
    real(4) :: y
    do i = -1074, -1000
        x = 2.0d0**i
        if (x /= scale(1.0d0, i)) error stop "real(8)"
    end do
    do i = -149, -130
        y = 2.0**i
        if (y /= scale(1.0, i)) error stop "real(4)"
    end do
    i = -2
    print *, 2.0d0**(-1074), 10.0d0**i
    if (abs(10.0d0**i - 0.01d0) > 1d-17) error stop "ordinary"
end program
