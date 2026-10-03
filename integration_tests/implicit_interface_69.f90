! A dummy procedure and an external are passed to a procedure in another file
! (implicit_interface_69b.f90), and the dummy is then called with an array
! element: sequence association with the explicit-shape dummy of its
! definition.
subroutine w(f, a, n)
external f, u
real :: a(10)
integer :: n
call lib(u, a)
call lib(f, a)
call f(a(n))
end subroutine

program implicit_interface_69
external s
real :: a(10)
a = 0
call w(s, a, 1)
if (abs(a(1) - 201) > 1e-5) error stop
print *, "ok"
end program

subroutine s(x)
real :: x(*)
x(1) = x(1) + 100
end subroutine

subroutine u(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine
