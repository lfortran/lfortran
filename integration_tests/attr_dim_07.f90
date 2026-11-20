! A `dimension` statement before the type declaration applies only to the
! scoping unit it appears in: a later procedure that declares the same name
! gets its own entity, as an array with other bounds (#17337) or as a scalar
! (#15713).
module attr_dim_07_mod
    implicit none
contains
    subroutine s1(x)
        dimension x(1)
        real x
        if (size(x) /= 1) error stop
        x(1) = 1.0
    end subroutine s1

    subroutine s2(x)
        real x(2)
        if (size(x) /= 2) error stop
        x(2) = 2.0
    end subroutine s2

    subroutine s3(x)
        real :: x
        x = 3.0
    end subroutine s3

    subroutine s4(n)
        integer, intent(out) :: n
        dimension x(3)
        integer :: x
        integer :: m
        x = 2
        call inner(m)
        n = sum(x) + m
    contains
        subroutine inner(m)
            integer, intent(out) :: m
            integer :: x(5)
            x = 1
            m = sum(x)
        end subroutine inner
    end subroutine s4

    subroutine s5(n)
        integer, intent(out) :: n
        integer :: x(4)
        x = 3
        n = sum(x)
    end subroutine s5

    subroutine s6(n)
        integer, intent(out) :: n
        integer :: x
        x = 7
        n = x
    end subroutine s6
end module attr_dim_07_mod

program attr_dim_07
    use attr_dim_07_mod
    implicit none
    real :: c(2), r
    integer :: n
    c = 0.0
    call s1(c)
    call s2(c)
    if (c(1) /= 1.0 .or. c(2) /= 2.0) error stop
    call s3(r)
    if (r /= 3.0) error stop
    call s4(n)
    if (n /= 11) error stop
    call s5(n)
    if (n /= 12) error stop
    call s6(n)
    if (n /= 7) error stop
    print *, c, r, n
end program attr_dim_07
