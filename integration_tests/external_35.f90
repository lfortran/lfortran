! The module external_35_mod, defined in external_35_procs.f90, calls
! external procedures defined in that file through interfaces that name
! entities of another module.
program external_35
use external_35_mod
implicit none
real :: a(3) = [1, 2, 3]
character(len=4) :: s
integer :: c(2)
call scale(a)
print *, a
if (any(abs(a - [8, 11, 14]) > 1e-6)) error stop 1
call label(s)
print *, s
if (s /= "abab") error stop 2
call fill(c)
print *, c
if (any(c /= 2)) error stop 3
end program
