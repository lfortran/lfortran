module constructor_effects
implicit none
integer :: calls
contains
integer function limit()
calls = calls + 1
limit = 3
end function
integer function decreasing_limit(k)
integer, intent(in) :: k
calls = calls + 1
decreasing_limit = 3-k
end function
end module
program constructor_once
use constructor_effects
implicit none
integer :: a(3), i, k
calls = 0
a = [(i, i=1,limit())]
print *, calls, a
if (calls /= 1) error stop 1
if (any(a /= [1,2,3])) error stop 2
call check([(i,i=1,limit())]*2)
if (calls /= 2) error stop 5
if (size([(i,i=1,limit())]) /= 3) error stop 6
if (calls /= 3) error stop 7
calls = 0
k = 0
do while (sum([(i,i=1,decreasing_limit(k))]) > 0)
    k = k + 1
    if (k > 3) error stop 3
end do
if (calls /= 4 .or. k /= 3) error stop 4
contains
subroutine check(x)
integer, intent(in) :: x(:)
if (size(x) /= 3) error stop 8
if (any(x /= [2,4,6])) error stop 9
end subroutine
end program
