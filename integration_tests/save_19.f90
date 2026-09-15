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
type :: a_t
    integer :: x = 1
end type
type, extends(a_t) :: c_t
    integer :: y = 2
end type
type(c_t), parameter :: pc = c_t(5, 6)
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

integer function f_extends() result(r)
    type(c_t), save :: v = c_t(10, 20)
    type(c_t), save :: w = pc
    if (v%x /= 10 .or. w%x /= 5) error stop 13
    v%y = v%y + 1
    w%y = w%y + 1
    if (w%y - 6 /= v%y - 20) error stop 14
    r = v%x + v%y
end function
end module

program save_19
use save_19_mod, only: t, fm, f_folded, f_negative, f_extends
implicit none
type(t), parameter :: zp = t(9, 'xyz', [3, 4], 2.5)
integer :: i

do i = 1, 2
    if (f() /= 9 + i) error stop 1
    if (fm() /= 9 + i) error stop 2
    call b(i)
    if (f_folded() /= 9 + i) error stop 11
    if (f_negative() /= -9 + i) error stop 12
    if (f_extends() /= 30 + i) error stop 15
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
