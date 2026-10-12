subroutine arrays_133_sa(x)
use arrays_133_mod, only: n
implicit none
real, intent(out) :: x(n)
if (size(x) /= 3) error stop
x = 1
end subroutine
