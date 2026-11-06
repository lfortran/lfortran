! With --fast, the copy of `integrate` made in `host` and the internal
! procedure `f` are both inlined into `host`. Internal procedures can only be
! called from their host, so they have internal linkage and LLVM deletes them
! once they are inlined, instead of leaving dead global copies in the binary.
module procedure_argument_specialization_09_integ
implicit none
abstract interface
    real(8) function fun(x)
        real(8), intent(in) :: x
    end function
end interface
contains
    real(8) function integrate(f, a, b, n) result(s)
        procedure(fun) :: f
        real(8), intent(in) :: a, b
        integer, intent(in) :: n
        integer :: i
        real(8) :: h
        h = (b - a)/n
        s = 0
        do i = 1, n
            s = s + f(a + (i - 0.5d0)*h)
        end do
        s = s*h
    end function
end module

module procedure_argument_specialization_09_host
use procedure_argument_specialization_09_integ, only: integrate
implicit none
contains
    subroutine host(p, q, r, n, res)
        real(8), intent(in) :: p, q, r
        integer, intent(in) :: n
        real(8), intent(out) :: res
        res = integrate(f, 0d0, 1d0, n)
    contains
        real(8) function f(x)
            real(8), intent(in) :: x
            f = (p*x + q)*x + r
        end function
    end subroutine
end module

program procedure_argument_specialization_09
use procedure_argument_specialization_09_host, only: host
implicit none
real(8) :: res
call host(1d0, 2d0, 0.5d0, 1000, res)
if (abs(res - 4d0/3d0 - 0.5d0) > 1d-6) error stop
print *, res
end program
