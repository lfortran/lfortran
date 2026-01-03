module common_01_mod
implicit none

contains

subroutine f()
common / c1 / a, b, c
integer :: a, b, c
a = b
b = 4
end subroutine

subroutine g()
common / c1 / x, y, z
integer :: x, y, z
x = y
y = 4
end subroutine

end module

program common_01
use common_01_mod, only: f, g
implicit none
common / c1 / a, b, c
integer :: a, b, c
a = 1
b = 2
print *, a, b
if (a /= 1) error stop
if (b /= 2) error stop
call f()
print *, a, b
if (a /= 2) error stop
if (b /= 4) error stop

a = 1
b = 2
print *, a, b
if (a /= 1) error stop
if (b /= 2) error stop
call g()
print *, a, b
if (a /= 2) error stop
if (b /= 4) error stop
end program
