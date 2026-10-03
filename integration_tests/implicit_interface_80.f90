! A dummy procedure is called and passed to a procedure in another file
! (implicit_interface_80b.f90), followed by an external defined in this file
! (#12834).
subroutine w(g, a)
external g, s
real :: a(10)
call g(a)
call lib(g, a)
call lib(s, a)
end subroutine

program implicit_interface_80
external u
real :: a(10)
a = 0
call w(u, a)
if (abs(a(1) - 102) > 1e-5) error stop
print *, "ok"
end program

subroutine s(x)
real :: x(10)
x(1) = x(1) + 100
end subroutine

subroutine u(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine
