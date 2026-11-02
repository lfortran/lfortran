module tbaa_13_m
implicit none
type :: state_t
    integer :: width
    integer, pointer :: cursor => null()
    real :: fixed(4)
    real, allocatable :: values(:,:)
    real, allocatable :: scalar
end type
contains
subroutine accumulate(self, x, n)
    class(state_t), intent(inout) :: self
    integer, intent(in) :: n
    real, intent(in) :: x(n,n)
    integer :: i, j
    do j = 1, n
        do i = 1, self%width
            self%values(i,j) = self%values(i,j) + x(i,j) + self%scalar
        end do
        self%fixed(j) = self%values(1,j)
    end do
end subroutine

! A single TARGET receiver permits its pointer component to alias width.
! Do not turn this into overlapping non-TARGET INOUT dummy arguments.
subroutine target_alias(self)
    class(state_t), target, intent(inout) :: self
    integer :: i
    do i = 1, 4
        self%cursor = self%cursor + 1
        self%values(i,1) = real(self%width)
    end do
end subroutine

subroutine pointer_alias(self)
    class(state_t), pointer, intent(inout) :: self
    integer :: i
    do i = 1, 4
        self%cursor = self%cursor + 1
        self%values(i,1) = real(self%width)
    end do
end subroutine
end module

program tbaa_13
use tbaa_13_m
implicit none
type(state_t), target :: state
class(state_t), pointer :: p
real :: x(4,4)
integer :: i, j
allocate(state%values(4,4), state%scalar)
state%scalar = 2
state%width = 3
state%values = 1
state%fixed = 0
do j = 1, 4
    do i = 1, 4
        x(i,j) = real(10*j + i)
    end do
end do
call accumulate(state, x, 4)
if (state%width /= 3) error stop 1
do j = 1, 4
    do i = 1, 3
        if (state%values(i,j) /= x(i,j) + 3) error stop 2
    end do
    if (state%values(4,j) /= 1) error stop 3
    if (state%fixed(j) /= x(1,j) + 3) error stop 4
end do
state%cursor => state%width
call target_alias(state)
if (state%width /= 7) error stop 5
if (any(state%values(:,1) /= [4., 5., 6., 7.])) error stop 6
state%width = 3
p => state
call pointer_alias(p)
if (state%width /= 7) error stop 7
if (any(state%values(:,1) /= [4., 5., 6., 7.])) error stop 8
if (.not. associated(state%cursor, state%width)) error stop 9
nullify(p, state%cursor)
deallocate(state%values, state%scalar)
end program
