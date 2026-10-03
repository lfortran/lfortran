! Assigning to a nested member of an array SECTION writes only the section.
! The section is reached through more than one component, so the pass that
! binds the section to a pointer temporary has to walk down the component
! chain to find it.
program derived_types_187
implicit none

type :: inner
    integer :: ii = 0
end type

type :: t
    integer :: id = 0
    type(inner) :: nest
end type

type(t) :: w(4), m(2, 3)
integer :: i, j

do i = 1, 4
    w(i)%id = 10*i
    w(i)%nest%ii = i
end do

! One component level: already worked, kept so it stays working.
w(2:3)%id = 7
if (w(1)%id /= 10) error stop "one level, below the section"
if (w(2)%id /= 7) error stop "one level, in the section"
if (w(3)%id /= 7) error stop "one level, in the section"
if (w(4)%id /= 40) error stop "one level, above the section"

! Two component levels over a contiguous section.
w(2:3)%nest%ii = 9
if (w(1)%nest%ii /= 1) error stop "two levels, below the section"
if (w(2)%nest%ii /= 9) error stop "two levels, in the section"
if (w(3)%nest%ii /= 9) error stop "two levels, in the section"
if (w(4)%nest%ii /= 4) error stop "two levels, above the section"

! Two component levels over a strided section.
do i = 1, 4
    w(i)%nest%ii = i
end do
w(1:4:2)%nest%ii = 5
if (w(1)%nest%ii /= 5) error stop "strided, in the section"
if (w(2)%nest%ii /= 2) error stop "strided, between strides"
if (w(3)%nest%ii /= 5) error stop "strided, in the section"
if (w(4)%nest%ii /= 4) error stop "strided, between strides"

! Two component levels over a rank-2 section.
do i = 1, 2
    do j = 1, 3
        m(i, j)%nest%ii = 0
    end do
end do
m(1, :)%nest%ii = 4
if (m(1, 1)%nest%ii /= 4) error stop "rank 2, in the section"
if (m(1, 3)%nest%ii /= 4) error stop "rank 2, in the section"
if (m(2, 1)%nest%ii /= 0) error stop "rank 2, outside the section"
if (m(2, 3)%nest%ii /= 0) error stop "rank 2, outside the section"

! The whole array through two levels still writes every element.
w%nest%ii = 6
do i = 1, 4
    if (w(i)%nest%ii /= 6) error stop "whole array, two levels"
end do

print *, "ok"

end program derived_types_187
