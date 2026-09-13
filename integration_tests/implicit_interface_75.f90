! A dummy procedure `f` is passed to a procedure defined in another file
! (implicit_interface_75b.f90) after a contained procedure or an interface
! block dummy was passed to it. `f` stays an implicit interface, so the later
! `call f(a(3))` passes an array element by sequence association.
subroutine contained_first_75(f, a)
external f
real :: a(10)
call apply_75(inner, a)
call apply_75(f, a)
call f(a(3))
contains
subroutine inner(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine
end subroutine

subroutine interface_first_75(g, f, a)
interface
subroutine g(x)
real :: x(10)
end subroutine
end interface
external f
real :: a(10)
call apply_75(g, a)
call apply_75(f, a)
call f(a(3))
end subroutine

subroutine interface_then_external_75(g, a)
interface
subroutine g(x)
real :: x(10)
end subroutine
end interface
external s_75
real :: a(10)
call apply_75(g, a)
call apply_75(s_75, a)
call s_75(a(3))
end subroutine

program implicit_interface_75
implicit none
external s_75, u_75
real :: a(10)
a = 0
call contained_first_75(s_75, a)
if (abs(a(1) - 101) > 1e-5) error stop
if (abs(a(3) - 100) > 1e-5) error stop
a = 0
call interface_first_75(u_75, s_75, a)
if (abs(a(1) - 101) > 1e-5) error stop
if (abs(a(3) - 100) > 1e-5) error stop
a = 0
call interface_then_external_75(u_75, a)
if (abs(a(1) - 101) > 1e-5) error stop
if (abs(a(3) - 100) > 1e-5) error stop
print *, "ok"
end program

subroutine s_75(x)
real :: x(5)
x(1) = x(1) + 100
end subroutine

subroutine u_75(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine
