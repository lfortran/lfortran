! Names that are implicitly declared by an earlier statement (an array
! bound, a DIMENSION, SAVE or COMMON statement) may still receive their one
! explicit type declaration later in the specification part.
program implicit_typing_14
    common /blk/ k
    integer(8) k
    integer :: b(3)
    real :: r
    k = 5_8
    call fill(b, 3)
    if (any(b /= [10, 20, 30])) error stop
    call twice(b, 3)
    if (any(b /= [20, 40, 60])) error stop
    r = acc(1.5)
    r = acc(2.0)
    if (abs(r - 3.5) > 1e-6) error stop
    if (kind(k) /= 8) error stop
    if (k /= 5_8) error stop
    print *, b, r, k
contains
    subroutine fill(a, n)
        dimension a(n)
        integer n
        integer a
        do i = 1, n
            a(i) = 10 * i
        end do
    end subroutine

    subroutine twice(c, m)
        integer :: c(m), m
        c = 2 * c
    end subroutine

    real function acc(x)
        save total
        double precision total
        data total /0.0d0/
        total = total + x
        acc = real(total)
    end function
end program
