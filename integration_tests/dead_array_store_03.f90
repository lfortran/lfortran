! A pending asynchronous operation may read an ASYNCHRONOUS variable, and
! another agent a VOLATILE one, at a call that does not name it (F2018 8.5.4,
! 8.5.20, 18.10.4). So `x = 1` is not dead although `x = 2` assigns every
! element of x again before anything in the procedure reads it: the call in
! between sends the value.
module dead_array_store_03_m
implicit none
interface
    subroutine dead_array_store_03_send()
    end subroutine
end interface
contains

subroutine send_async(x, n)
    integer, intent(in) :: n
    real, asynchronous :: x(n)
    x = 1
    call dead_array_store_03_send()
    x = 2
end subroutine

subroutine send_volatile(x, n)
    integer, intent(in) :: n
    real, volatile :: x(n)
    x = 3
    call dead_array_store_03_send()
    x = 4
end subroutine

subroutine send_scoped(x, n)
    integer, intent(in) :: n
    real :: x(n)
    block
        asynchronous :: x
        x = 5
        call dead_array_store_03_send()
        x = 6
    end block
end subroutine

subroutine send_contained(x, n)
    integer, intent(in) :: n
    real :: x(n)
    call inner()
contains
    subroutine inner()
        volatile :: x
        x = 7
        call dead_array_store_03_send()
        x = 8
    end subroutine
end subroutine

end module

program dead_array_store_03
use iso_c_binding, only: c_loc
use dead_array_store_03_pending, only: pending_buffer, sent
use dead_array_store_03_m, only: send_async, send_volatile, send_scoped, send_contained
implicit none
real, target, asynchronous :: a(4)
real, target, volatile :: v(4)
a = 0
pending_buffer = c_loc(a)
call send_async(a, 4)
print *, sent, a(1)
if (abs(sent - 1) > 1e-6) error stop
if (abs(a(1) - 2) > 1e-6) error stop
v = 0
pending_buffer = c_loc(v)
call send_volatile(v, 4)
print *, sent, v(1)
if (abs(sent - 3) > 1e-6) error stop
if (abs(v(1) - 4) > 1e-6) error stop
pending_buffer = c_loc(a)
call send_scoped(a, 4)
if (abs(sent - 5) > 1e-6) error stop
if (abs(a(1) - 6) > 1e-6) error stop
pending_buffer = c_loc(v)
call send_contained(v, 4)
if (abs(sent - 7) > 1e-6) error stop
if (abs(v(1) - 8) > 1e-6) error stop
end program
