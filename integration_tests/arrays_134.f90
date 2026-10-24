! SIZE of an explicit-shape dummy whose extent is an integer(8)
! expression, and a whole-array assignment checked against that extent
module arrays_134_m
contains
    subroutine s(n, m, a, b, c)
        integer(8), intent(in) :: n
        integer, intent(in) :: m
        real, intent(inout) :: a(n), b(n), c(m, 2)
        real :: d(m, 2)
        integer :: k
        k = size(a, 1) + 1
        if (k /= 5) error stop
        if (size(c, 1) /= 3 .or. size(c, 2) /= 2) error stop
        a = a + b
        d = c * 2
        c = d + c
    end subroutine
end module

program arrays_134
    use arrays_134_m, only: s
    real :: a(4), b(4), c(3, 2)
    a = 1
    b = 2
    c = 3
    call s(4_8, 3, a, b, c)
    if (any(a /= 3)) error stop
    if (any(c /= 9)) error stop
    print *, a, c
end program
