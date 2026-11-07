module modules_92_a
implicit none
integer :: x = 5
integer, parameter :: k = 7
integer :: y = 9
contains
integer function f()
    f = 11
end function
end module

module modules_92_b
! A private re-export of use-associated names must not change their
! accessibility in the module they come from.
use modules_92_a, only: x, k, z => y, f
use iso_c_binding, only: c_int
implicit none
private :: x, k, z, f, c_int
contains
integer function get_only()
    use modules_92_a, only: x, k, y, f
    use iso_c_binding, only: c_int
    integer(c_int) :: i
    i = x + k + y + f()
    get_only = i
end function
integer function get_full()
    use modules_92_a
    get_full = x + k + y + f()
end function
end module

program modules_92
use modules_92_b
use modules_92_a, only: x, k, w => y, f
use iso_c_binding, only: c_int
implicit none
integer(c_int) :: i
i = x + k + w + f()
print *, i, get_only(), get_full()
if (x /= 5) error stop 1
if (k /= 7) error stop 2
if (w /= 9) error stop 3
if (f() /= 11) error stop 4
if (i /= 32) error stop 5
if (get_only() /= 32) error stop 6
if (get_full() /= 32) error stop 7
call check_hidden()
contains
subroutine check_hidden()
    use modules_92_b
    ! `z` is private in modules_92_b, so this declares a new variable
    integer :: z
    z = 1
    if (z /= 1) error stop 8
end subroutine
end program
