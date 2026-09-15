module save_19_mod
implicit none
type :: t
    integer :: h = 0
    character(len=3) :: c = 'abc'
    integer :: a(2) = [1, 2]
    real :: x = 1.5
end type
type(t), parameter :: z = t(9, 'xyz', [3, 4], 2.5)
contains
integer function fm() result(r)
    type(t), save :: s = z
    s%h = s%h + 1
    r = s%h
end function
end module

program save_19
use save_19_mod, only: t, fm
implicit none
type(t), parameter :: zp = t(9, 'xyz', [3, 4], 2.5)
integer :: i

do i = 1, 2
    if (f() /= 9 + i) error stop 1
    if (fm() /= 9 + i) error stop 2
    call b(i)
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
