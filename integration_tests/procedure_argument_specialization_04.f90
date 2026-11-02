! The procedure that an internal procedure is passed to uses private
! entities of its module: a variable, a named constant, a procedure and a
! derived type. With --fast, its copy in the host refers to them too.
module procedure_argument_specialization_04_m
implicit none
private
public :: accumulate, calls

type :: tally
    integer :: n = 0
    real :: total = 0
end type

integer, parameter :: weight = 2
integer :: ncalls = 0

abstract interface
    real function fun(x)
        real, intent(in) :: x
    end function
end interface

contains
real function accumulate(f, n) result(s)
    procedure(fun) :: f
    integer, intent(in) :: n
    type(tally) :: t
    integer :: i
    do i = 1, n
        call add(t, f(real(i)))
    end do
    s = weight*t%total
    ncalls = ncalls + 1
end function

subroutine add(t, x)
    type(tally), intent(inout) :: t
    real, intent(in) :: x
    t%n = t%n + 1
    t%total = t%total + x
end subroutine

integer function calls()
    calls = ncalls
end function
end module

subroutine procedure_argument_specialization_04_host(c, r)
use procedure_argument_specialization_04_m, only: accumulate
implicit none
real, intent(in) :: c
real, intent(out) :: r
r = accumulate(times_c, 4)
contains
real function times_c(x)
    real, intent(in) :: x
    times_c = c*x
end function
end subroutine

program procedure_argument_specialization_04
use procedure_argument_specialization_04_m, only: calls
implicit none
interface
    subroutine procedure_argument_specialization_04_host(c, r)
        real, intent(in) :: c
        real, intent(out) :: r
    end subroutine
end interface
real :: r
call procedure_argument_specialization_04_host(3.0, r)
! 2*3*(1 + 2 + 3 + 4)
if (abs(r - 60.0) > 1e-5) error stop 1
call procedure_argument_specialization_04_host(1.0, r)
if (abs(r - 20.0) > 1e-5) error stop 2
if (calls() /= 2) error stop 3
print *, r, calls()
end program
