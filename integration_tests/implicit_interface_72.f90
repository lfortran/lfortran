! A procedure pointer associated with an external is passed to an external in
! another file (implicit_interface_72b.f90), followed by a dummy procedure that
! is then called with an array element (#12831).
subroutine w(f, a)
interface
subroutine iface(x)
real :: x(10)
end subroutine
end interface
external f, u
procedure(iface), pointer :: pp
real :: a(10)
pp => u
call lib(pp, a)
call lib(f, a)
call f(a(3))
end subroutine

program implicit_interface_72
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

subroutine u(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine
