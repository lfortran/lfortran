! A host procedure passes an expression with its automatic array, sized by
! its dummy argument, to an internal procedure that also uses that array
subroutine host_call(a, r)
real, intent(in) :: a(:)
real, intent(out) :: r(:)
real :: s(size(a))
s = a
call inner(2*s)
r = r + sum(s)
contains
subroutine inner(x)
real, intent(in) :: x(:)
r = x + s(1)
end subroutine
end subroutine

subroutine host_func(a, r)
real, intent(in) :: a(:)
real, intent(out) :: r(:)
real :: s(size(a))
s = a
r = f(s + 1)
contains
function f(x) result(y)
real, intent(in) :: x(:)
real :: y(size(x))
y = x*s(size(s))
end function
end subroutine

program nested_vars_26
implicit none
interface
subroutine host_call(a, r)
real, intent(in) :: a(:)
real, intent(out) :: r(:)
end subroutine
subroutine host_func(a, r)
real, intent(in) :: a(:)
real, intent(out) :: r(:)
end subroutine
end interface
real :: a(3), r(3)

a = [1.0, 2.0, 3.0]
call host_call(a, r)
print *, r
! r = 2*a + a(1) + sum(a)
if (any(abs(r - [9.0, 11.0, 13.0]) > 1e-6)) error stop

call host_func(a, r)
print *, r
! r = (a + 1)*a(3)
if (any(abs(r - [6.0, 9.0, 12.0]) > 1e-6)) error stop
end program
