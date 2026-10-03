! Function references to an external function in another file
! (implicit_interface_76b.f90), passing an external and a dummy procedure, with
! a call to the external in between.
subroutine w(f, a)
external s, f
real :: a(10), r, lib
r = lib(s, a)
call s(a)
r = lib(f, a)
if (abs(r - a(1)) > 1e-5) error stop
end subroutine

subroutine w2(g, a)
external s, g
real :: a(10), r, lib
r = lib(s, a)
r = lib(g, a)
end subroutine

program implicit_interface_76
external s2
real :: a(10)
a = 0
call w(s2, a)
if (abs(a(1) - 102) > 1e-5) error stop
call w2(s2, a)
if (abs(a(1) - 203) > 1e-5) error stop
print *, "ok"
end program

subroutine s(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine

subroutine s2(x)
real :: x(10)
x(1) = x(1) + 100
end subroutine
