! Two externals are passed to a procedure in another file
! (implicit_interface_78b.f90); the first is not defined in this file (#12826).
program implicit_interface_78
external s, u
real :: a(10)
a = 0
call lib(u, a)
call lib(s, a)
if (abs(a(1) - 100) > 1e-5) error stop
if (abs(a(2) - 7) > 1e-5) error stop
print *, "ok"
end program

subroutine s(x)
real :: x(5)
x(1) = 100
end subroutine
