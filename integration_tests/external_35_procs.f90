! Compiled separately from external_35.f90, which uses the module
! external_35_mod. Its module procedures call external procedures defined
! in this file with no interface for them. The interfaces of the external
! procedures name named constants and a variable of a module that
! external_35_mod does not use; their locals name a derived type of that
! module and a named constant of a module that uses external_35_mod.
module external_35_consts
implicit none
integer, parameter :: n = 3, k = 4
integer :: m = 2
type :: external_35_box
    real :: v
end type
end module

module external_35_mod
implicit none
contains
    subroutine scale(a)
    real, intent(inout) :: a(3)
    call external_35_scale(a)
    end subroutine

    subroutine label(s)
    character(len=4), intent(out) :: s
    call external_35_label(s)
    end subroutine

    subroutine fill(c)
    integer, intent(out) :: c(2)
    call external_35_fill(c)
    end subroutine
end module

module external_35_user
use external_35_mod
implicit none
real, parameter :: offset = 5
end module

subroutine external_35_scale(a)
use external_35_consts, only: n, external_35_box
use external_35_user, only: offset
implicit none
real, intent(inout) :: a(n)
real, save :: big(1000)
integer, save :: calls = 0
type(external_35_box) :: b
calls = calls + 1
big(calls) = offset
b%v = n
a = a * b%v + big(calls)
end subroutine

subroutine external_35_label(s)
use external_35_consts, only: k
implicit none
character(len=k), intent(out) :: s
s = repeat("ab", k / 2)
end subroutine

subroutine external_35_fill(c)
use external_35_consts, only: m
implicit none
integer, intent(out) :: c(m)
c = m
end subroutine
