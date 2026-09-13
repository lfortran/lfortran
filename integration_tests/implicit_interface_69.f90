! An external subroutine with a derived-type argument, defined in this file,
! is passed to a procedure defined in another file (implicit_interface_69b.f90).
program implicit_interface_69
use implicit_interface_69_mod, only: t69
implicit none
external set_three
type(t69) :: z
z%v = 0
call apply_t69(set_three, z)
if (abs(z%v - 3.0d0) > 1.0d-12) error stop
print *, "ok"
end program

subroutine set_three(x)
use implicit_interface_69_mod, only: t69
type(t69) :: x
x%v = 3
end subroutine
