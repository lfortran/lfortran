module class_155_mod
implicit none
type :: w
    class(*), pointer :: p(:) => null()
    class(*), allocatable :: a(:)
end type
contains
subroutine check(x, n, first)
    class(*), intent(in) :: x(:)
    integer, intent(in) :: n, first
    integer :: i
    if (size(x) /= n) error stop
    select type (x)
    type is (integer)
        do i = 1, n
            if (x(i) /= first + i - 1) error stop
        end do
    class default
        error stop
    end select
end subroutine
end module

program class_155
use class_155_mod
implicit none
integer, target :: u(4) = [1, 2, 3, 4]
class(*), pointer :: q(:)
type(w) :: x
q => u
call check(q, 4, 1)
x%p => u
if (size(x%p) /= 4) error stop
call check(x%p, 4, 1)
allocate(x%a, source=[5, 6, 7])
call check(x%a, 3, 5)
print *, "ok"
end program
