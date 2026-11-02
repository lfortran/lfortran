! An internal procedure that uses a host variable passed to a module
! procedure that only calls it: with --fast the call becomes a call of a
! copy of the module procedure in the host, which calls the internal
! procedure directly. The results are the same either way.
module procedure_argument_specialization_01_m
implicit none
abstract interface
    real function integrand(x)
        real, intent(in) :: x
    end function
end interface
contains
real function integrate(f, a, b, n) result(s)
    procedure(integrand) :: f
    real, intent(in) :: a, b
    integer, intent(in) :: n
    integer :: i
    real :: h
    h = (b - a)/n
    s = 0
    do i = 1, n
        s = s + f(a + (i - 0.5)*h)
    end do
    s = s*h
end function

subroutine apply(g, x)
    interface
        subroutine g(y)
            real, intent(inout) :: y
        end subroutine
    end interface
    real, intent(inout) :: x
    call g(x)
    call g(x)
end subroutine
end module

module procedure_argument_specialization_01_h
use procedure_argument_specialization_01_m, only: integrate, apply
implicit none
contains
real function host(c) result(r)
    real, intent(in) :: c
    real :: x
    r = integrate(linear, 0.0, 1.0, 100) + 2*integrate(linear, 0.0, 2.0, 100)
    x = 1
    call apply(scale, x)
    if (abs(x - c*c) > 1e-5) error stop 1
    r = r + sibling()
contains
    real function linear(x)
        real, intent(in) :: x
        linear = c*x
    end function

    subroutine scale(y)
        real, intent(inout) :: y
        y = c*y
    end subroutine

    real function sibling()
        sibling = integrate(linear, 0.0, 1.0, 10)
    end function
end function
end module

program procedure_argument_specialization_01
use procedure_argument_specialization_01_h, only: host
implicit none
real :: r
r = host(3.0)
! 3/2 + 2*6 + 3/2
if (abs(r - 15.0) > 1e-3) error stop 2
print *, r
end program
