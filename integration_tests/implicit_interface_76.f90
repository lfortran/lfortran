! An external subroutine defined in this file is passed to a procedure
! defined in another file (implicit_interface_76b.f90), then an external
! subroutine defined in that other file is passed to it too.
program implicit_interface_76
implicit none
external s_76, other_76
real :: a(10)
a = 0
call apply_76(s_76, a)
call apply_76(other_76, a)
if (abs(a(1) - 11) > 1e-5) error stop
print *, "ok"
end program

subroutine s_76(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine
