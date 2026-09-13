! Dummy procedures `f` and `g` are passed to procedures defined in another
! file (implicit_interface_68b.f90). This file also defines unrelated global
! procedures named `f` and `g`, whose signatures must not be given to the
! dummies.
program implicit_interface_68
implicit none
external add_half
real(8), external :: dbl
real(8) :: y
y = 0
call wrap_sub(add_half, y)
if (abs(y - 1.0d0) > 1.0d-12) error stop
y = 1
call wrap_fun(dbl, y)
if (abs(y - 4.0d0) > 1.0d-12) error stop
print *, "ok"
end program

subroutine wrap_sub(f, y)
external f
real(8) :: y
call apply_sub(f, y)
call f(y)
end subroutine

subroutine wrap_fun(g, y)
external g
real(8) :: g
real(8) :: y
call apply_fun(g, y)
y = g(y)
end subroutine

subroutine add_half(x)
real(8) :: x
x = x + 0.5d0
end subroutine

real(8) function dbl(x)
real(8) :: x
dbl = 2*x
end function

subroutine f(a, b, c)
real(8) :: a, b, c
a = b + c
end subroutine

subroutine g(a, b, c)
real(8) :: a, b, c
a = b*c
end subroutine
