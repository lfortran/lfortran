! Two procedures each have a dummy external `f`, called with different argument
! lists (#12820).
program implicit_interface_73
external one, three
real(8) :: y, t, v(2), vp(2)
y = 0
call apply_scalar(one, y)
if (abs(y - 1) > 1d-12) error stop
t = 2
v = [1, 2]
call apply_rhs(three, t, v, vp)
if (any(abs(vp - [2, 4]) > 1d-12)) error stop
print *, "ok"
end program

subroutine apply_scalar(f, y)
external f
real(8) :: y
call f(y)
end subroutine

subroutine apply_rhs(f, t, y, yp)
external f
real(8) :: t, y(2), yp(2)
call f(t, y, yp)
end subroutine

subroutine one(x)
real(8) :: x
x = 1
end subroutine

subroutine three(t, y, yp)
real(8) :: t, y(2), yp(2)
yp = t * y
end subroutine
