! The result of an untyped external procedure pointer has the implicit type
! of the scoping unit that declares the pointer (#16414); letters that the
! scoping unit's own IMPLICIT statement does not name keep the host's mapping
module procedure_pointer_48_mod
implicit double precision (a-h, o-z)
contains
subroutine partial(y, c)
    implicit integer (i-n)
    pointer :: fp
    external :: fp
    fp => half_mod
    y = fp(3.0d0)
    c = 1d0 / 3
    if (kind(c) /= 8) error stop 10
end subroutine
double precision function half_mod(x)
    double precision :: x
    half_mod = x / 2
end function
end module

subroutine procedure_pointer_48_host(r)
implicit integer (p)
integer :: r
pointer :: pf
external :: pf
pf => twice
call inner()
contains
subroutine inner()
    implicit real (p)
    r = pf(21)
end subroutine
integer function twice(n)
    integer :: n
    twice = 2*n
end function
end subroutine

program procedure_pointer_48
use procedure_pointer_48_mod, only: partial
implicit double precision (a-h, o-z)
pointer :: fp
external :: fp
integer :: r
fp => half
x = fp(3.0d0)
if (abs(x - 1.5d0) > 1d-12) error stop 1
call procedure_pointer_48_host(r)
if (r /= 42) error stop 2
call partial(y, c)
if (abs(y - 1.5d0) > 1d-12) error stop 3
if (abs(c - 1d0 / 3) > 1d-15) error stop 4
call internal_partial(z)
if (abs(z - 1.5d0) > 1d-12) error stop 5
print *, "ok"
contains
subroutine internal_partial(y)
    implicit integer (i-n)
    pointer :: gp
    external :: gp
    gp => half
    y = gp(3.0d0)
end subroutine
double precision function half(y)
    double precision :: y
    half = y / 2
end function
end program
