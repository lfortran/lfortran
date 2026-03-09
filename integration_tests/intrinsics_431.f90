program intrinsics_431
! Test shape() on class(*) allocatable arrays
implicit none
class(*), allocatable :: val(:,:)
integer :: s(2)
allocate(integer :: val(3,2))
s = shape(val)
if (s(1) /= 3) error stop
if (s(2) /= 2) error stop
deallocate(val)

allocate(real :: val(5,4))
s = shape(val)
if (s(1) /= 5) error stop
if (s(2) /= 4) error stop
deallocate(val)
end program
