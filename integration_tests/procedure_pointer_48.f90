! The result of an untyped external procedure pointer has the implicit type
! of the scoping unit that declares the pointer (#16414)
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
implicit double precision (a-h, o-z)
pointer :: fp
external :: fp
integer :: r
fp => half
x = fp(3.0d0)
if (abs(x - 1.5d0) > 1d-12) error stop 1
call procedure_pointer_48_host(r)
if (r /= 42) error stop 2
print *, "ok"
contains
double precision function half(y)
    double precision :: y
    half = y / 2
end function
end program
