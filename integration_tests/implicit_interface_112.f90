module implicit_interface_112_m
implicit none
intrinsic trunc
interface
module real function trunc_thrice(x)
real, intent(in) :: x
end function
end interface
contains
real function trunc_twice(x)
real, intent(in) :: x
trunc_twice = 2*trunc(x)
end function
end module

submodule (implicit_interface_112_m) implicit_interface_112_sm
contains
module real function trunc_thrice(x)
real, intent(in) :: x
trunc_thrice = 3*trunc(x)
end function
end submodule

program implicit_interface_112
! `trunc` and `exp2` are LFortran-specific intrinsics, not standard Fortran
! ones, so GFortran rejects the INTRINSIC statements below. With implicit
! interfaces, a reference to such a name without an INTRINSIC statement is
! a reference to an external procedure; with one, it is the intrinsic.
use implicit_interface_112_m, only: trunc_twice, trunc_thrice
intrinsic trunc, exp2
real :: x
double precision :: y
x = trunc(2.7)
if (abs(x - 2.0) > 1e-6) error stop 1
y = exp2(3.0d0)
if (abs(y - 8.0d0) > 1d-12) error stop 2
call sub(x)
if (abs(x - 3.0) > 1e-6) error stop 3
call host()
if (abs(trunc_twice(3.6) - 6.0) > 1e-6) error stop 5
if (abs(trunc_thrice(3.6) - 9.0) > 1e-6) error stop 6
call blk(x)
if (abs(x - 4.0) > 1e-5) error stop 7
print *, "ok"
contains
subroutine host()
real :: z
z = trunc(-4.5)
if (abs(z + 4.0) > 1e-6) error stop 4
end subroutine
end program

subroutine sub(x)
intrinsic trunc
real, intent(inout) :: x
x = trunc(x + 1.9)
end subroutine

subroutine blk(x)
real, intent(out) :: x
block
intrinsic exp2
x = exp2(2.0)
end block
end subroutine
