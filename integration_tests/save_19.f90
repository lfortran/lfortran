module save_19_mod
implicit none
type :: t
    integer :: h = 0
    character(len=3) :: c = 'abc'
    integer :: a(2) = [1, 2]
    real :: x = 1.5
end type
type(t), parameter :: z = t(9, 'xyz', [3, 4], 2.5)
type :: u_t
    integer :: h = 0
    real :: x = 0
    integer(8) :: k = 0
    complex :: w = (0, 0)
end type
integer, parameter :: n0 = 4
contains
integer function fm() result(r)
    type(t), save :: s = z
    s%h = s%h + 1
    r = s%h
end function

integer function f_folded() result(r)
    type(u_t), save :: s = u_t(-(-9), 1, n0, (1.0, -2.0))
    if (abs(s%x - 1.0) > 1e-6) error stop 8
    if (s%k /= 4_8) error stop 9
    if (abs(s%w - (1.0, -2.0)) > 1e-6) error stop 10
    s%h = s%h + 1
    r = s%h
end function

integer function f_negative() result(r)
    type(u_t), save :: s = u_t(-9)
    s%h = s%h + 1
    r = s%h
end function
end module

program save_19
use save_19_mod, only: t, fm, f_folded, f_negative
implicit none
type(t), parameter :: zp = t(9, 'xyz', [3, 4], 2.5)
integer :: i

do i = 1, 2
    if (f() /= 9 + i) error stop 1
    if (fm() /= 9 + i) error stop 2
    call b(i)
    if (f_folded() /= 9 + i) error stop 11
    if (f_negative() /= -9 + i) error stop 12
end do
print *, "ok"

contains

integer function f() result(r)
    type(t), save :: s = zp
    s%h = s%h + 1
    s%a(1) = s%a(1) + 1
    if (s%c /= 'xyz') error stop 3
    if (s%a(2) /= 4) error stop 4
    if (abs(s%x - 2.5) > 1e-6) error stop 5
    if (s%a(1) /= s%h - 6) error stop 6
    r = s%h
end function

subroutine b(i)
    integer, intent(in) :: i
    block
        type(t), save :: sb = zp
        sb%h = sb%h + 1
        if (sb%h /= 9 + i) error stop 7
    end block
end subroutine

end program
