module procedure_pointer_28_mod
implicit none
contains
real function f(x)
    real, intent(in) :: x
    f = 2*x
end function

subroutine s(k)
    integer, intent(inout) :: k
    k = k + 1
end subroutine
end module

program procedure_pointer_28
use procedure_pointer_28_mod
implicit none
real, external, pointer :: p3
external :: ps
pointer :: ps
integer :: k
! Procedure pointers declared with the EXTERNAL and POINTER attributes have an
! implicit interface, so they can point to procedures with explicit interfaces.
! They are not referenced afterwards (see #14357).
p3 => f
ps => s
k = 1
call s(k)
if (k /= 2) error stop 1
if (abs(f(1.5) - 3.0) > 1e-6) error stop 2
print *, "ok"
end program
