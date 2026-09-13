! Module procedures (implicit_interface_72_mod.f90) pass external subroutines
! defined in the module's file to a procedure defined in another file
! (implicit_interface_72b.f90). This program uses the module and is compiled
! separately, without those definitions.
program implicit_interface_72
use implicit_interface_72_mod, only: set_two_via_72, add_half_via_72
implicit none
real(8) :: y
y = 0
call set_two_via_72(y)
if (abs(y - 2.0d0) > 1.0d-12) error stop
call add_half_via_72(y)
if (abs(y - 2.5d0) > 1.0d-12) error stop
print *, "ok"
end program
