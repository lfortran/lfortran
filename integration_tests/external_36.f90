! The module external_36_mod, defined in external_36_procs.f90, calls
! external procedures defined in that file whose interfaces reference a
! generic name and a defined operator of another module.
program external_36
use external_36_mod
implicit none
real :: a(6) = 1
integer :: c(5)
call twice(a)
print *, a
if (any(a /= 6)) error stop 1
call plus(c)
print *, c
if (any(c /= 5)) error stop 2
end program
