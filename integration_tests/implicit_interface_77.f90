! A module procedure (implicit_interface_77c.f90) passes an external defined in
! its own file to a procedure in another file (implicit_interface_77b.f90),
! compiled with --separate-compilation (#12835).
program implicit_interface_77
use implicit_interface_77_mod, only: via
implicit none
real(8) :: y
y = 0
call via(y)
if (abs(y - 2) > 1d-12) error stop
print *, "ok"
end program
