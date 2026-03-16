program transfer_22
use iso_fortran_env, only: int8, int64
implicit none

integer(int8), allocatable :: arr1(:)
integer(int8), allocatable :: arr2(:)
integer(int8) :: mold(1)

! transfer array of int32 to array of int8 (array literal mold)
arr1 = transfer([1, 2], [0_int8])
if (size(arr1) /= 8) error stop
print *, size(arr1)

! transfer array of int64 to array of int8 (array literal mold)
arr2 = transfer([0_int64, 1_int64], [0_int8])
if (size(arr2) /= 16) error stop
print *, size(arr2)

! transfer array of int32 to array of int8 (variable mold)
mold(1) = 0
arr1 = transfer([3, 4], mold)
if (size(arr1) /= 8) error stop
print *, size(arr1)

end program
