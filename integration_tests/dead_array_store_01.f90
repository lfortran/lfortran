! A whole-array assignment that is assigned again, every element, before
! anything reads it is removed (the dead_array_store pass); one that is read
! is kept.
module dead_array_store_01_m
implicit none
type workspace_t
    real, allocatable :: dcdw(:,:), dcdb(:)
end type
contains
subroutine learn(workspace, r, n)
    type(workspace_t), intent(inout) :: workspace
    real, intent(in) :: r(:,:,:)
    integer, intent(in) :: n
    integer :: iter
    associate(dcdw => workspace%dcdw, dcdb => workspace%dcdb)
        do iter = 1, n
            ! Both dead: assigned again below, read only for their shape.
            dcdw = 0.; dcdb = 0.
            block
                real :: reduce(size(dcdw, 1), size(dcdw, 2), size(r, 3))
                reduce = r * iter
                dcdw = sum(reduce, dim=3)
            end block
            dcdb = dcdw(:, 1)
        end do
    end associate
end subroutine
end module

program dead_array_store_01
use dead_array_store_01_m, only: workspace_t, learn
implicit none
type(workspace_t) :: workspace
real :: r(2, 3, 2), x(4)
integer :: i
r = 1
allocate(workspace%dcdw(2, 3), workspace%dcdb(2))
call learn(workspace, r, 3)
if (any(workspace%dcdw /= 6)) error stop
if (any(workspace%dcdb /= 6)) error stop
! Kept: the second assignment reads it.
x = 1
x = x + 1
if (any(x /= 2)) error stop
! Kept: an element is read in between.
x = 3
i = int(x(2))
x = 0
if (i /= 3 .or. any(x /= 0)) error stop
print *, "ok"
end program
