module struct_constant_extends_01_mod
implicit none
type :: a_t
    real(8) :: r = 1
    integer :: x = 2
end type
type, extends(a_t) :: c_t
    integer :: y = 3
end type
type, extends(c_t) :: d_t
    integer :: z = 4
end type
type(c_t) :: gc = c_t(1.5d0, 20, 30)
type(d_t) :: gd = d_t(2.5d0, 21, 31, 41)
end module

program struct_constant_extends_01
use struct_constant_extends_01_mod
implicit none
type(c_t), parameter :: pc = c_t(3.5d0, 22, 32)
type(d_t), parameter :: pd = d_t(4.5d0, 23, 33, 43)
print *, gc%r, gc%x, gc%y
print *, gd%r, gd%x, gd%y, gd%z
if (gc%r /= 1.5d0) error stop 1
if (gc%x /= 20) error stop 2
if (gc%y /= 30) error stop 3
if (gd%r /= 2.5d0) error stop 4
if (gd%x /= 21) error stop 5
if (gd%y /= 31) error stop 6
if (gd%z /= 41) error stop 7
if (pc%r /= 3.5d0 .or. pc%x /= 22 .or. pc%y /= 32) error stop 8
if (pd%r /= 4.5d0 .or. pd%x /= 23 .or. pd%y /= 33 .or. pd%z /= 43) error stop 9
gc%y = gc%y + 1
gd%a_t%x = gd%a_t%x + 1
if (gc%y /= 31) error stop 10
if (gd%x /= 22) error stop 11
print *, "ok"
end program
