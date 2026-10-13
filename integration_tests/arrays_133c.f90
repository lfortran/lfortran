subroutine arrays_133_sb(x, y)
use arrays_133_mod, only: n
implicit none
real, intent(out) :: x(n), y(n + 1)
if (size(x) /= 3) error stop
if (size(y) /= 4) error stop
x = 2
y = 3
end subroutine
