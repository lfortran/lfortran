! A dummy procedure is passed to a function from another file
! (implicit_interface_79b.f90) after an external defined in this file
! (#12828).
program implicit_interface_79
external s
real :: a(10)
a = 0
call w(s, a)
if (abs(a(1) - 2) > 1e-5) error stop
print *, "ok"
end program

subroutine w(f, a)
external s, f
real :: a(10), r, lib
r = lib(s, a)
r = lib(f, a)
if (abs(r - 2) > 1e-5) error stop
end subroutine

subroutine s(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine
