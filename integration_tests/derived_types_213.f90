module derived_types_213_mod
implicit none
type :: w
    character(len=2) :: c(3)
end type
type :: outer
    type(w) :: in
    character(len=3) :: d(2, 2)
end type
end module

program derived_types_213
use derived_types_213_mod
implicit none
type(w) :: rs(4), rb(2, 2)
type(w), allocatable :: ra(:), rab(:, :)
type(outer) :: ro(4), rob(2, 2)
integer :: i

! reshape of an array of structs with a character-array component
! copies every element of the component
do i = 1, 4
    rs(i)%c = ['ab', 'cd', 'ef']
end do
rs(4)%c(3) = 'zz'
rb = reshape(rs, [2, 2])
print *, rb(2, 2)%c
if (any(rb(2, 2)%c /= ['ab', 'cd', 'zz'])) error stop 1
if (any(rb(1, 1)%c /= ['ab', 'cd', 'ef'])) error stop 2
rb(:, :) = reshape(rs(4:1:-1), [2, 2])
if (any(rb(1, 1)%c /= ['ab', 'cd', 'zz'])) error stop 3

allocate(ra(4), rab(2, 2))
ra = rs
ra(3)%c(1) = 'yy'
rab = reshape(ra, [2, 2])
if (any(rab(1, 2)%c /= ['yy', 'cd', 'ef'])) error stop 4
if (any(rab(2, 2)%c /= ['ab', 'cd', 'zz'])) error stop 5

do i = 1, 4
    ro(i)%in = rs(i)
    ro(i)%d = reshape(['aaa', 'bbb', 'ccc', 'ddd'], [2, 2])
end do
ro(2)%d(1, 2) = 'xyz'
rob = reshape(ro, [2, 2])
if (any(rob(2, 2)%in%c /= ['ab', 'cd', 'zz'])) error stop 6
if (rob(2, 1)%d(1, 2) /= 'xyz' .or. rob(2, 1)%d(2, 2) /= 'ddd') error stop 7
end program
