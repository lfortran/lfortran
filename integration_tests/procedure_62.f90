module proc_dummy_m
implicit none
abstract interface
function objfun(x)
real :: objfun
real, intent(in) :: x(:)
end function objfun
subroutine cb_interface(x)
real, intent(inout) :: x(:)
end subroutine cb_interface
end interface
contains
subroutine apply(f, x, n)
procedure(objfun) :: f
integer, intent(in) :: n
real, intent(inout) :: x(n)
x(1) = f(x)
end subroutine apply
subroutine apply_sub(f, x, n)
procedure(cb_interface) :: f
integer, intent(in) :: n
real, intent(inout) :: x(n)
call f(x)
end subroutine apply_sub
end module proc_dummy_m

program procedure_62
use proc_dummy_m
real :: a(3)
a(1) = 1.0
a(2) = 2.0
a(3) = 3.0
call apply(myfun, a, 3)
if (abs(a(1) - 2.0) > 1e-6) error stop
call apply_sub(mysub, a, 3)
if (abs(a(1) - 12.0) > 1e-6) error stop
print *, a(1)
contains
function myfun(x) result(y)
real, intent(in) :: x(:)
real :: y
y = x(1) + 1.0
end function myfun
subroutine mysub(x)
real, intent(inout) :: x(:)
x(1) = x(1) + 10.0
end subroutine mysub
end program procedure_62
