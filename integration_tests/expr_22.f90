! Integer powers with negative exponents: 1/b**(-k) in integer arithmetic,
! which is nonzero only for b = 1 and b = -1.
program expr_22
    implicit none
    integer :: k, b, s
    integer(8) :: b8
    s = 0
    do k = -5, 5
        b = -1
        if (b**k /= merge(1, -1, mod(k, 2) == 0)) error stop "(-1)**k"
        b = 1
        if (b**k /= 1) error stop "1**k"
        b = 2
        if (k < 0 .and. b**k /= 0) error stop "2**k, k < 0"
        b8 = -1
        if (b8**k /= merge(1_8, -1_8, mod(k, 2) == 0)) error stop "(-1_8)**k"
        s = s + (-1)**k
    end do
    print *, s
    if (s /= -1) error stop "sum"
end program
