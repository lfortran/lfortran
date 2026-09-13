! An external subroutine with a derived-type argument, defined in this file,
! is passed to a procedure defined in another file (implicit_interface_71b.f90).
! The program does not use the module of the type, so the name of the type is
! free for an implicitly typed variable.
program implicit_interface_71
external set_three_71
call apply_t71(set_three_71)
t71 = 2.5
if (abs(t71 - 2.5) > 1e-6) error stop
print *, "ok"
end program

subroutine set_three_71(x)
use implicit_interface_71_mod, only: t71
type(t71) :: x
x%v = 3
end subroutine
