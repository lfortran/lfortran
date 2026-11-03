! `c_funloc` of a capturing internal procedure, called from C, in a recursive
! host. Callbacks of older and newer activations are live at the same time,
! and each one sees the variables of its own activation.
module callback_09_m
use iso_c_binding, only: c_int, c_funptr, c_funloc, c_associated
implicit none
interface
    subroutine c_register(depth, f) bind(c, name="callback_09_register")
    import :: c_int, c_funptr
    integer(c_int), value :: depth
    type(c_funptr), value :: f
    end subroutine
    integer(c_int) function c_call(depth, x) bind(c, name="callback_09_call")
    import :: c_int
    integer(c_int), value :: depth, x
    end function
    integer(c_int) function c_sum(n, x) bind(c, name="callback_09_sum")
    import :: c_int
    integer(c_int), value :: n, x
    end function
    integer(c_int) function c_distinct(n) bind(c, name="callback_09_distinct")
    import :: c_int
    integer(c_int), value :: n
    end function
end interface
contains

recursive subroutine host(depth, max_depth)
integer(c_int), intent(in) :: depth, max_depth
integer(c_int) :: captured, d
type(c_funptr) :: address
captured = 100 * depth
address = c_funloc(get)
call c_register(depth, address)
! The same activation gives the same address each time.
if (.not. c_associated(address, c_funloc(get))) error stop 1
! Every older activation is live and keeps its own variables.
do d = 1, depth
    if (c_call(d, 1_c_int) /= 100 * d + 1) error stop 2
end do
if (c_distinct(depth) /= 1) error stop 3
if (depth < max_depth) then
    call host(depth + 1, max_depth)
else
    if (c_sum(depth, 5_c_int) /= 50 * depth * (depth + 1) + 5 * depth) &
        error stop 4
end if
! The newer activations have returned; this one and the older ones are
! still live, and only this one has updated its variable.
captured = captured + 7
if (c_call(depth, 0_c_int) /= 100 * depth + 7) error stop 5
do d = 1, depth - 1
    if (c_call(d, 0_c_int) /= 100 * d) error stop 6
end do
contains
    integer(c_int) function get(x) bind(c)
    integer(c_int), value :: x
    get = captured + x
    end function
end subroutine

end module

program callback_09
use callback_09_m, only: host
use iso_c_binding, only: c_int
implicit none
call host(1_c_int, 5_c_int)
call host(1_c_int, 3_c_int)
print *, "ok"
end program
