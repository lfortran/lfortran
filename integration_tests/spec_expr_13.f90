! A specification expression in the signature of a module procedure calls
! a module procedure defined later.
module spec_expr_13_mod
implicit none
contains
    function f()
        integer :: f(f2())
        f = 1
    end function f

    pure function f2()
        integer :: f2
        f2 = 3
    end function f2

    ! a calls b, whose result has a length that calls c.
    function a(n) result(r)
        integer, intent(in) :: n
        integer :: r(len(b(n)))
        r = 7
    end function a

    pure function b(n) result(s)
        integer, intent(in) :: n
        character(len=c(n)) :: s
        s = repeat("b", c(n))
    end function b

    pure integer function c(n)
        integer, intent(in) :: n
        c = 2*n
    end function c

    subroutine s(n, x)
        integer, intent(in) :: n
        real, intent(out) :: x(twice(n))
        x = 1
    end subroutine s

    pure integer function twice(n)
        integer, intent(in) :: n
        twice = 2*n
    end function twice
end module spec_expr_13_mod

program spec_expr_13
use spec_expr_13_mod
implicit none
integer :: r(3), q(6)
real :: x(6)
r = f()
if (any(r /= 1)) error stop
if (size(f()) /= 3) error stop
q = a(3)
if (any(q /= 7)) error stop
if (size(a(3)) /= 6) error stop
call s(3, x)
if (any(x /= 1)) error stop
print *, size(f()), size(a(3)), sum(x)
end program spec_expr_13
