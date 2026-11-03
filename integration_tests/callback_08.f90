! A module procedure and capturing internal procedures are passed through
! implicit interfaces to C routines (without bind(c)) that call them. The C
! side sees plain code pointers with the GFortran calling convention.
module callback_08_m
implicit none
contains

subroutine square(x, y)
integer, intent(in) :: x
integer, intent(out) :: y
y = x * x
end subroutine

subroutine host(n, status)
integer, intent(in) :: n
integer, intent(out) :: status
integer :: offset, r, calls
real(8) :: scale, w
external :: callback_08_call_sub
integer, external :: callback_08_call_fun
real(8), external :: callback_08_call_real
status = 0
offset = 10 * n
calls = 0
scale = 0.5d0
call callback_08_call_sub(square, n, r)
if (r /= n * n) status = 1
call callback_08_call_sub(shift, n, r)
if (r /= n + 10 * n) status = 2
! The internal procedure sees the live host variable, not a copy.
offset = offset + 1
call callback_08_call_sub(shift, n, r)
if (r /= n + 10 * n + 1) status = 3
! The internal procedure updates host variables.
r = callback_08_call_fun(counted, n)
if (r /= 3 * n) status = 4
if (calls /= 1) status = 5
r = callback_08_call_fun(counted, n + 1)
if (r /= 3 * (n + 1) .or. calls /= 2) status = 6
w = callback_08_call_real(scaled, 3.0d0)
if (abs(w - 1.5d0) > 1d-12) status = 7
scale = 2.0d0
w = callback_08_call_real(scaled, 3.0d0)
if (abs(w - 6.0d0) > 1d-12) status = 8
contains
    subroutine shift(x, y)
    integer, intent(in) :: x
    integer, intent(out) :: y
    y = x + offset
    end subroutine
    integer function counted(x)
    integer, intent(in) :: x
    calls = calls + 1
    counted = 3 * x
    end function
    real(8) function scaled(x)
    real(8), intent(in) :: x
    scaled = scale * x
    end function
end subroutine

end module

program callback_08
use callback_08_m, only: host
implicit none
integer :: status
call host(4, status)
print *, status
if (status /= 0) error stop
call host(9, status)
print *, status
if (status /= 0) error stop
end program
