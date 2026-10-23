! A private specific of a use-associated assignment(=) does not take the
! name of a subroutine or function of the using module.
program modules_79
use modules_79_c
implicit none
type(ta) :: a
type(tb) :: b
integer :: i
a = 3
b = 4
call set_a(i, 5)
print *, a%x, b%x, i, set_b(2)
if (a%x /= 6) error stop 1
if (b%x /= 12) error stop 2
if (i /= 5) error stop 3
if (set_b(2) /= 20) error stop 4
end program
