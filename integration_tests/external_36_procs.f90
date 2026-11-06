! Compiled separately from external_36.f90, which uses the module
! external_36_mod. Its module procedures call external procedures defined
! in this file with no interface for them. A dummy bound of one external
! procedure references a generic name of an interface block of that
! procedure; one of the other references a defined operator of a module
! that external_36_mod does not use.
module external_36_ops
implicit none
interface operator(.plus.)
    module procedure external_36_add
end interface
contains
    pure integer function external_36_add(i, j)
    integer, intent(in) :: i, j
    external_36_add = i + j
    end function
end module

module external_36_mod
implicit none
contains
    subroutine twice(a)
    real, intent(inout) :: a(6)
    call external_36_twice(a, 3)
    end subroutine

    subroutine plus(c)
    integer, intent(out) :: c(5)
    call external_36_plus(c, 2)
    end subroutine
end module

pure integer function external_36_double(n)
implicit none
integer, intent(in) :: n
external_36_double = 2*n
end function

subroutine external_36_twice(a, n)
implicit none
interface external_36_g
    pure integer function external_36_double(n)
    integer, intent(in) :: n
    end function
end interface
integer, intent(in) :: n
real, intent(inout) :: a(external_36_g(n))
a = a * size(a)
end subroutine

subroutine external_36_plus(c, n)
use external_36_ops, only: operator(.plus.)
implicit none
integer, intent(in) :: n
integer, intent(out) :: c(n .plus. 3)
c = size(c)
end subroutine
