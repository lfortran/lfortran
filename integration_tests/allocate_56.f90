program allocate_56
! Test: allocate array with scalar source= and kind suffix on logical literal
! Also tests any()/all() with non-default logical kinds
use iso_c_binding, only: c_bool
implicit none
integer, parameter :: LP = c_bool
logical(LP), allocatable :: mask(:)
integer, allocatable :: arr(:)
integer :: i

! Scalar logical source with kind suffix
allocate(mask(1:5), source=.false._LP)
do i = 1, 5
    if (mask(i) .neqv. .false.) error stop
end do

! Test any() with non-default logical kind
if (any(mask)) error stop

! Scalar logical source without kind suffix (default kind)
deallocate(mask)
allocate(mask(1:3), source=.true._LP)
do i = 1, 3
    if (mask(i) .neqv. .true.) error stop
end do

! Test all() with non-default logical kind
if (.not. all(mask)) error stop

! Scalar integer source for array allocation
allocate(arr(1:4), source=42)
do i = 1, 4
    if (arr(i) /= 42) error stop
end do

print *, "PASS"
end program
