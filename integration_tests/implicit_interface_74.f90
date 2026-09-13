! An external subroutine with a derived-type argument, defined in this file, is
! passed to a procedure defined in another file (implicit_interface_74b.f90).
! Another external is passed to a procedure defined later in this file, so the
! procedure types are completed again after all bodies have been analyzed.
program implicit_interface_74
use implicit_interface_74_mod, only: t74
implicit none
external set_three_74, add_one_74
type(t74) :: z
real :: a(10)
z%v = 0
call apply_t74(set_three_74, z)
if (abs(z%v - 3.0d0) > 1.0d-12) error stop
a = 0
call later_74(add_one_74, a)
if (abs(a(1) - 1) > 1e-5) error stop
print *, "ok"
end program

subroutine later_74(q, a)
external q
real :: a(10)
call q(a)
end subroutine

subroutine add_one_74(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine

subroutine set_three_74(x)
use implicit_interface_74_mod, only: t74
type(t74) :: x
x%v = 3
end subroutine
