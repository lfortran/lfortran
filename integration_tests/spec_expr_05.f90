module spec_expr_05_mod
    ! Generic function references in the array bounds of procedures that
    ! also have a dummy procedure argument.
    implicit none
    interface g
        procedure dbl
    end interface
contains
    subroutine s(f, n)
        interface
            pure integer function f(y)
                integer, intent(in) :: y
            end function
        end interface
        integer, intent(in) :: n
        integer :: a(g(n))
        print *, size(a), f(n)
        if (size(a) /= 2*n) error stop 1
        if (f(n) /= 2*n) error stop 2
    end subroutine

    integer function t(n, f, x)
        interface
            pure integer function f(y)
                integer, intent(in) :: y
            end function
        end interface
        integer, intent(in) :: n
        integer, intent(in) :: x(g(n))
        integer :: b(n, g(n))
        if (size(x) /= 2*n) error stop 3
        if (size(b, 2) /= 2*n) error stop 4
        t = sum(x) + f(size(b))
    end function

    pure integer function dbl(y)
        integer, intent(in) :: y
        dbl = 2*y
    end function
end module

program spec_expr_05
    use spec_expr_05_mod
    implicit none
    integer :: x(6), r
    call s(dbl, 3)
    x = 1
    r = t(3, dbl, x)
    print *, r
    if (r /= 42) error stop 5
end program
