! A `procedure(real)` or `procedure(integer)` dummy has an implicit
! interface with a known result type; references to it are typed by their
! actual arguments.
module implicit_interface_99_mod
implicit none
contains
real function apply_mod(f, x)
real, intent(in) :: x
procedure(real) :: f
apply_mod = f(x)
end function
end module

real function apply_real(f, x)
real, intent(in) :: x
procedure(real) :: f
apply_real = f(x) + f(2 * x)
end function

integer function apply_int(f, i)
implicit none
integer, intent(in) :: i
procedure(integer) :: f
apply_int = f(i) + 1
end function

subroutine update_real(f, x)
real :: x
procedure(real) :: f
x = f(x)
end subroutine

subroutine print_real(f)
procedure(real) :: f
print *, f(1.0)
end subroutine

subroutine call_sub(s, x)
real :: x
procedure() :: s
call s(x)
end subroutine

program implicit_interface_99
use implicit_interface_99_mod
implicit none
real, external :: plus_one, apply_real
integer, external :: next_int, apply_int
external triple
real :: x
if (abs(apply_mod(plus_one, 1.0) - 2.0) > 1e-6) error stop
if (abs(apply_real(plus_one, 1.0) - 5.0) > 1e-6) error stop
if (apply_int(next_int, 1) /= 3) error stop
x = 1.0
call update_real(plus_one, x)
call call_sub(triple, x)
if (abs(x - 6.0) > 1e-6) error stop
call print_real(plus_one)
print *, apply_real(plus_one, 1.0), apply_int(next_int, 1), x
end program

real function plus_one(y)
real :: y
plus_one = y + 1.0
end function

integer function next_int(k)
integer :: k
next_int = k + 1
end function

subroutine triple(y)
real :: y
y = y * 3.0
end subroutine
