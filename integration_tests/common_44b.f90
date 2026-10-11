module common_44b_mod
use common_44a_mod, only: set_first
implicit none
contains
subroutine grow()
integer :: x, b
common /common_44_xb/ x, b
x = 1
b = 4
call set_first()
print *, x, b
if (x /= 5 .or. b /= 4) error stop
end subroutine
end module
