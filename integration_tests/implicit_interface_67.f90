! External procedures defined in this file are passed to procedures that are
! defined in another file (implicit_interface_67b.f90) and are not called
! directly here.
program implicit_interface_67
implicit none
external set_two, deriv
real(8) :: y, t, v(2), vp(2)
y = 0
call apply_scalar(set_two, y)
if (abs(y - 2.0d0) > 1.0d-12) error stop
t = 3
v = [1.0d0, 2.0d0]
call apply_rhs(deriv, t, v, vp)
if (abs(vp(1) - 2.0d0) > 1.0d-12) error stop
if (abs(vp(2) + 3.0d0) > 1.0d-12) error stop
call apply_scalar(set_two, y)
if (abs(y - 2.0d0) > 1.0d-12) error stop
print *, "ok"
end program

subroutine set_two(x)
real(8) :: x
x = 2
end subroutine

subroutine deriv(t, y, yp)
real(8) :: t, y(2), yp(2)
yp(1) = y(2)
yp(2) = -t*y(1)
end subroutine
