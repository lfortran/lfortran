module derived_types_214_mod
implicit none
type :: w
    character(len=2) :: c(3)
end type
contains
subroutine check_reshaped(x)
    class(w), intent(in) :: x(:, :)
    if (any(x(2, 2)%c /= ['ab', 'cd', 'zz'])) error stop 1
    if (any(x(1, 2)%c /= ['ab', 'cd', 'ef'])) error stop 2
end subroutine
end module

program derived_types_214
! reshape of a class(w) array copies every element of a character-array
! component
use derived_types_214_mod
implicit none
class(w), allocatable :: ca(:)
integer :: i

allocate(ca(4))
do i = 1, 4
    ca(i)%c = ['ab', 'cd', 'ef']
end do
ca(4)%c(3) = 'zz'
call check_reshaped(reshape(ca, [2, 2]))
end program
