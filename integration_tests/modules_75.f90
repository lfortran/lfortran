! An access statement of one module does not set the access of a same-named
! entity of a later module in the same file.
module modules_75_a
implicit none
integer :: x = 1
integer :: y = 2
private :: x
end module

module modules_75_b
implicit none
integer :: x = 5
end module

program modules_75
use modules_75_a
use modules_75_b
implicit none
print *, x, y
if (x /= 5) error stop 1
if (y /= 2) error stop 2
end program
