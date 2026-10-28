! A pointer component re-pointed inside the loop that writes its elements,
! so that the descriptor changes between the element stores.
module tbaa_04_m
implicit none

type :: view_t
    real, pointer :: p(:) => null()
    integer :: hits = 0
end type

contains

subroutine prefix_sum(self, a)
    type(view_t), intent(inout) :: self
    real, target, intent(inout) :: a(:)
    integer :: i
    do i = 1, size(a) - 1
        self%p => a(i:)
        self%p(2) = self%p(1) + self%p(2)
        self%hits = self%hits + 1
    end do
end subroutine

subroutine strided(self, a)
    class(view_t), intent(inout) :: self
    real, target, intent(inout) :: a(:)
    integer :: i, j
    do i = 1, 2
        self%p => a(i::2)
        do j = 2, size(self%p)
            self%p(j) = self%p(j) + self%p(j-1)
        end do
    end do
end subroutine

end module

program tbaa_04
use tbaa_04_m
implicit none
real, target :: a(5)
type(view_t) :: v
a = 1
call prefix_sum(v, a)
if (any(abs(a - [1.0, 2.0, 3.0, 4.0, 5.0]) > 1e-6)) error stop 1
if (v%hits /= 4) error stop 2
if (size(v%p) /= 2) error stop 3
v%p(1) = 100
if (a(4) /= 100) error stop 5
a = 1
call strided(v, a)
print *, a
if (any(abs(a - [1.0, 1.0, 2.0, 2.0, 3.0]) > 1e-6)) error stop 4
if (size(v%p) /= 2) error stop 6
v%p(2) = -1
if (a(4) /= -1) error stop 7
print *, "ok"
end program
