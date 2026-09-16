! A module procedure (implicit_interface_71c.f90) and a dummy procedure are
! passed to an external in another file (implicit_interface_71b.f90), and the
! dummy is then called with an array element (#12830).
subroutine w(f, a)
use implicit_interface_71_mod, only: inner
external f
real :: a(10)
call lib(inner, a)
call lib(f, a)
call f(a(3))
end subroutine

program implicit_interface_71
external s
real :: a(10)
a = 0
call w(s, a)
if (abs(a(1) - 101) > 1e-5) error stop
if (abs(a(3) - 100) > 1e-5) error stop
print *, "ok"
end program

subroutine s(x)
real :: x(5)
x(1) = x(1) + 100
end subroutine
