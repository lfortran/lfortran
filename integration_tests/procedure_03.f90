module procedure_03_mod
implicit none

interface
    subroutine func(x, y, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: y(n)
    end subroutine
end interface

contains

    subroutine hybrd(fcn)
    procedure(func) :: fcn
    integer, parameter :: m = 3
    real, parameter :: eps = 1e-5
    real :: a(m), b(m)
    a = [1, 2, 3]
    call fcn(a, b, m)
    print *, a
    print *, b
    if ((b(1) - 3) > eps) error stop
    if ((b(2) - 6) > eps) error stop
    if ((b(3) - 9) > eps) error stop
    end subroutine

end module

program procedure_03
use procedure_03_mod, only: hybrd
implicit none

call hybrd(fn)

contains

    subroutine fn(x, y, k)
    integer, intent(in) :: k
    real, intent(in) :: x(k)
    real, intent(out) :: y(k)
    y = x * 3
    end subroutine

end program
